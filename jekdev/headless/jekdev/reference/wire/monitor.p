/**
 * This module provides a HTTP object class to inspect the Prolog
 * threads, Prolog stack frames and Prolog variables of a
 * Prolog instance.
 *
 * Currently the HTTP object class only deals with GET requests
 * and provides a simple frame set based view. Extensions towards
 * web sockets and JSON RPC are planned.
 *
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 *
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 *
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 *
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- package(library(jekdev/reference/wire)).

:- module(monitor, []).
:- use_module(library(wire/httpsrv)).
:- use_module(library(system/thread)).
:- use_module(library(system/group)).
:- use_module(library(inspection/frame)).

/**
 * dispatch(O, P, A, S):
 * The predicate succeeds in dispatching the request for object
 * O, with path P, with parameter list A and the session S.
 */
% dispatch(+Object, +Spec, +Assoc, +Session)
:- public dispatch/4.
dispatch(_, '/index.html', _, Session) :- !,
   setup_call_cleanup(
      open(Session, write, Response),
      send_text(library(wire/index), Response),
      close(Response)).
dispatch(_, '/thread.jsp', _, Session) :- !,
   setup_call_cleanup(
      open(Session, write, Response),
      send_thread(Response),
      close(Response)).
dispatch(_, '/stack.jsp', Assoc, Session) :- !,
   setup_call_cleanup(
      open(Session, write, Response),
      send_stack(Assoc, Response),
      close(Response)).
dispatch(_, '/frame.html', _, Session) :- !,
   setup_call_cleanup(
      open(Session, write, Response),
      send_text(library(wire/frame), Response),
      close(Response)).
dispatch(_, '/closed.gif', _, Session) :- !,
   setup_call_cleanup(
      open(Session, write, Response, [type(binary)]),
      send_binary(library(wire/closed), Response),
      close(Response)).
dispatch(_, '/open.gif', _, Session) :- !,
   setup_call_cleanup(
      open(Session, write, Response, [type(binary)]),
      send_binary(library(wire/open), Response),
      close(Response)).
dispatch(_, '/blank.gif', _, Session) :- !,
   setup_call_cleanup(
      open(Session, write, Response, [type(binary)]),
      send_binary(library(wire/blank), Response),
      close(Response)).

/*************************************************************/
/* Thread Inspection                                         */
/*************************************************************/

/**
 * send_thread(O):
 * The predicate sends the threads to the output stream O.
 */
% send_thread(+Stream)
:- private send_thread/1.
send_thread(Response) :-
   html_begin(Response, 'Thread'),
   write(Response, '<h2>Threads</h2>\r\n'),
   thread_current(T),
   current_thread_flag(T, sys_thread_group, H),
   current_group_flag(H, sys_group_group, Root),
   send_group_list(Root, Response),
   html_end(Response).

% send_group_list(+Group, +Stream)
:- private send_group_list/2.
send_group_list(Group, Response) :-
   write(Response, '<dl>'),
   send_group_group_list(Group, Response),
   send_group_thread_list(Group, Response),
   write(Response, '</dl>\r\n').

% send_group_group_list(+Group, +Stream)
:- private send_group_group_list/2.
send_group_group_list(Group, Response) :-
   current_group(Group, Other),
   current_group_flag(Other, sys_group_name, Name),
   write(Response, '<dt><img src="open.gif">'),
   html_escape(Response, Name),
   write(Response, '</dt>\r\n'),
   write(Response, '<dd>'),
   send_group_list(Other, Response),
   write(Response, '</dd>\r\n'), fail.
send_group_group_list(_, _).

% send_group_thread_list(+Group, +Stream)
:- private send_group_thread_list/2.
send_group_thread_list(Group, Response) :-
   current_thread(Group, Thread),
   write(Response, '<dt><img src="blank.gif">'),
   send_thread(Thread, Response),
   write(Response, '</dt>\r\n'), fail.
send_group_thread_list(_, _).

% send_thread(+Thread, +Stream)
:- private send_thread/2.
send_thread(Thread, Response) :-
   current_thread(Thread), !,
   current_thread_flag(Thread, sys_thread_name, Name),
   write(Response, '<a href="stack.jsp?thread='),
   html_escape(Response, Name),
   write(Response, '" target="stack">'),
   html_escape(Response, Name),
   write(Response, '</a>').
send_thread(Thread, Response) :-
   current_thread_flag(Thread, sys_thread_name, Name),
   html_escape(Response, Name).

/*************************************************************/
/* Stack Inspection                                          */
/*************************************************************/

/**
 * send_stack(A, O):
 * The predicate sends the stack element for parameter list
 * A to the output stream O.
 */
% send_stack(+Assoc, +Stream)
:- private send_stack/2.
send_stack(Assoc, Response) :-
   http_parameter(Assoc, thread, Name),
   current_thread(Thread),
   current_thread_flag(Thread, sys_thread_name, Name), !,
   html_begin(Response, 'Stack'),
   write(Response, '<h2>Stack Elements</h2>\r\n'),
   current_thread_flag(Thread, sys_top_frame, Frame),
   write(Response, '<dl>'),
   send_call_stack(Frame, Response),
   write(Response, '</dl>\r\n'),
   html_end(Response).
send_stack(_, Response) :-
   html_begin(Response, 'Stack'),
   write(Response, '<h2>Stack Elements</h2>\r\n'),
   html_end(Response).

% send_call_strack(+Frame, +Stream)
:- private send_call_stack/2.
send_call_stack(null, _) :- !.
send_call_stack(Frame, Response) :-
   write(Response, '<dt>'),
   frame_property(Frame, sys_call_goal(Goal)),
   functor(Goal, Name, Arity),
   term_atom(Name/Arity, Atom),
   html_escape(Response, Atom),
   write(Response, '</dt>\r\n'),
   frame_property(Frame, sys_parent_frame(Other)),
   send_call_stack(Other, Response).

/*************************************************************/
/* Frame Inspection                                          */
/*************************************************************/




