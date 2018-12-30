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

/**
 * dispatch(O, S, A, R):
 * The predicate succeeds in dispatching the request for object
 * O, with spec S, with assoc A and with response R.
 */
:- public dispatch/4.
dispatch(_, '/index.html', _, Response) :- !,
   send_file(library(wire/index), Response).
dispatch(_, '/thread.jsp', _, Response) :- !,
   send_thread(Response).
dispatch(_, '/stack.jsp', Assoc, Response) :-
   http_parameter(Assoc, thread, Name), !,
   send_stack(Response, Name).
dispatch(_, '/stack.jsp', _, Response) :- !,
   send_stack(Response).
dispatch(_, '/frame.html', _, Response) :- !,
   send_file(library(wire/frame), Response).

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
   send_thread_list(Response),
   html_end(Response).

% send_thread_list(+Stream)
:- private send_thread_list/1.
send_thread_list(Response) :-
   write(Response, '<ul>\r\n'),
   current_thread(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   write(Response, '<li><a href="stack.jsp?thread='),
   html_escape(Response, Name),
   write(Response, '" target="stack">'),
   html_escape(Response, Name),
   write(Response, '</a></li>\r\n'), fail.
send_thread_list(Response) :-
   write(Response, '</ul>\r\n').

/*************************************************************/
/* Stack Inspection                                          */
/*************************************************************/

/**
 * send_stack(O):
 * The predicate sends empty stack elements to the output stream O.
 */
% send_stack(+Stream)
:- private send_stack/1.
send_stack(Response) :-
   html_begin(Response, 'Stack'),
   html_end(Response).

/**
 * send_stack(O, N):
 * The predicate sends the stack element of the thread N
 * to the output stream O.
 */
% send_stack(+Stream, +Atom)
:- private send_stack/2.
send_stack(Response, Name) :-
   atom_concat('Stack ', Name, Title),
   html_begin(Response, Title),
   html_end(Response).

