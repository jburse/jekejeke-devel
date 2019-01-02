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
dispatch(Object, '/thread.jsp', _, Session) :- !,
   setup_call_cleanup(
      open(Session, write, Response),
      send_thread(Object, Response),
      close(Response)).
dispatch(Object, '/stack.jsp', Assoc, Session) :- !,
   setup_call_cleanup(
      open(Session, write, Response),
      send_stack(Object, Assoc, Response),
      close(Response)).
dispatch(_, '/frame.jsp', Assoc, Session) :- !,
   setup_call_cleanup(
      open(Session, write, Response),
      send_frame(Assoc, Response),
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

% frame_begin(+Stream, +Atom)
:- private frame_begin/2.
frame_begin(Response, Title) :-
   frame_begin(Response, Title, []).

% frame_begin(+Stream, +Atom, +List)
:- private frame_begin/3.
frame_begin(Response, Title, Opt) :-
   html_begin(Response, Title, Opt),
   write(Response, '<h2>'),
   html_escape(Response, Title),
   write(Response, '</h2>\r\n').

/*************************************************************/
/* Thread Inspection                                         */
/*************************************************************/

/**
 * send_thread(M, O):
 * The predicate sends the threads to the output stream O
 * from within the monitor object M.
 */
% send_thread(+Object, +Stream)
:- private send_thread/2.
send_thread(Object, Response) :-
   frame_begin(Response, 'Thread', [tree]),
   thread_current(T),
   current_thread_flag(T, sys_thread_group, H),
   current_group_flag(H, sys_group_group, Root),
   send_group_list(Object, Root, Response),
   html_end(Response).

% send_group_list(+Object, +Group, +Stream)
:- private send_group_list/3.
send_group_list(Object, Group, Response) :-
   write(Response, '<dl>'),
   send_group_group_list(Object, Group, Response),
   send_group_thread_list(Object, Group, Response),
   write(Response, '</dl>\r\n').

% send_group_group_list(+Object, +Group, +Stream)
:- private send_group_group_list/3.
send_group_group_list(Object, Group, Response) :-
   current_group(Group, Other),
   current_group_flag(Other, sys_group_name, Name),
   write(Response, '<dt><a onclick="openClose('''),
   html_escape(Response, Name),
   write(Response, ''');"><img src="closed.gif" id="'),
   html_escape(Response, Name),
   write(Response, '_img">'),
   html_escape(Response, Name),
   write(Response, '</dt>\r\n'),
   write(Response, '<dd style="display:none" id="'),
   html_escape(Response, Name),
   write(Response, '">'),
   send_group_list(Object, Other, Response),
   write(Response, '</dd>\r\n'), fail.
send_group_group_list(_, _, _).

% send_group_thread_list(+Object, +Group, +Stream)
:- private send_group_thread_list/3.
send_group_thread_list(Object, Group, Response) :-
   current_thread(Group, Thread),
   write(Response, '<dt><img src="blank.gif">'),
   send_thread(Object, Thread, Response),
   write(Response, '</dt>\r\n'), fail.
send_group_thread_list(_, _, _).

% send_thread(+Object, +Thread, +Stream)
:- private send_thread/3.
send_thread(Object, Thread, Response) :-
   current_thread(Thread), !,
   current_thread_flag(Thread, sys_thread_name, Name),
   write(Response, '<a href="stack.jsp?thread='),
   html_escape(Response, Name),
   write(Response, '"'),
   Object::html_target(Response, stack),
   write(Response, >),
   html_escape(Response, Name),
   write(Response, '</a>').
send_thread(_, Thread, Response) :-
   current_thread_flag(Thread, sys_thread_name, Name),
   html_escape(Response, Name).

/*************************************************************/
/* Stack Inspection                                          */
/*************************************************************/

/**
 * send_stack(M, A, O):
 * The predicate sends the stack element for parameter list
 * A to the output stream O from within the monitor object M.
 */
% send_stack(+Object, +Assoc, +Stream)
:- private send_stack/3.
send_stack(Object, Assoc, Response) :-
   http_parameter(Assoc, thread, Name),
   current_thread(Thread),
   current_thread_flag(Thread, sys_thread_name, Name), !,
   current_thread_flag(Thread, sys_top_frame, Frame),
   atom_list_concat(['Stack',Name], ', ', Atom),
   frame_begin(Response, Atom),
   write(Response, '<dl>'),
   send_call_stack(Object, Frame, Name, 0, Response),
   write(Response, '</dl>\r\n'),
   html_end(Response).
send_stack(_, _, Response) :-
   frame_begin(Response, 'Stack'),
   html_end(Response).

% send_call_stack(+Object, +Frame, +Atom, +Integer, +Stream)
:- private send_call_stack/5.
send_call_stack(_, null, _, _, _) :- !.
send_call_stack(Object, Frame, Name, Count, Response) :-
   write(Response, '<dt><a href="frame.jsp?thread='),
   html_escape(Response, Name),
   write(Response, '&index='),
   write(Response, Count),
   write(Response, '"'),
   Object::html_target(Response, frame),
   write(Response, >),
   frame_deref(Frame, Deref),
   frame_property(Deref, sys_call_goal(Goal)),
   functor(Goal, Functor, Arity),
   term_atom(Functor/Arity, Atom),
   html_escape(Response, Atom),
   write(Response, '</a></dt>\r\n'),
   frame_property(Frame, sys_parent_frame(Other)),
   Count2 is Count+1,
   send_call_stack(Object, Other, Name, Count2, Response).

% frame_deref(+Frame, -Frame)
:- private frame_deref/2.
frame_deref(Frame, Other) :-
   frame_property(Frame, sys_call_goal(trace_goal(_,Other))), !.
frame_deref(Frame, Frame).

/*************************************************************/
/* Frame Inspection                                          */
/*************************************************************/

/**
 * send_frame(A, O):
 * The predicate sends the frame for parameter list
 * A to the output stream O.
 */
% send_frame(+Assoc, +Stream)
:- private send_frame/2.
send_frame(Assoc, Response) :-
   http_parameter(Assoc, thread, Name),
   http_parameter(Assoc, index, CountStr),
   atom_codes(CountStr, List),
   number_codes(Count, List),
   current_thread(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   current_thread_flag(Thread, sys_top_frame, Frame),
   find_call_stack(Frame, Count, Other), !,
   atom_list_concat(['Bindings',Name,CountStr], ', ', Atom),
   frame_begin(Response, Atom),
   write(Response, '<dl>'),
   frame_deref(Other, Deref),
   frame_property(Deref, variable_names(Vars)),
   send_frame_vars(Vars, Response),
   write(Response, '</dl>\r\n'),
   html_end(Response).
send_frame(_, Response) :-
   frame_begin(Response, 'Bindings'),
   html_end(Response).

% send_frame_vars(+List, +Stream)
:- private send_frame_vars/2.
send_frame_vars([Var=Term|List], Response) :-
   write(Response, '<dt>'),
   sys_quoted_var(Var, Quote),
   html_escape(Response, Quote),
   write(Response, ' = '),
   term_atom(Term, Atom),
   html_escape(Response, Atom),
   write(Response, '</dt>\r\n'),
   send_frame_vars(List, Response).
send_frame_vars([], _).

% find_call_stack(+Frame, +Count, -Frame)
:- private find_call_stack/3.
find_call_stack(null, _, _) :- !, fail.
find_call_stack(Frame, 0, Frame) :- !.
find_call_stack(Frame, Count, Result) :-
   frame_property(Frame, sys_parent_frame(Other)),
   Count2 is Count-1,
   find_call_stack(Other, Count2, Result).
