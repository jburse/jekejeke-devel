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
:- use_module(library(notebook/httpsrv)).
:- use_module(library(stream/console)).
:- use_module(library(runtime/distributed)).
:- use_module(library(notebook/websock)).

/**
 * start(P):
 * The predicate starts the monitor at port P:
 */
:- public start/1.
start(P) :-
   spawn((  server(wire/monitor, P), fail; true)).

/**
 * dispatch(O, P, R, S):
 * The predicate succeeds in dispatching the request for object
 * O, with path P, with request R and the session S.
 */
% dispatch(+Object, +Spec, +Request, +Session)
:- override dispatch/4.
:- public dispatch/4.
dispatch(_, '/images/closed.gif', _, Session) :- !,
   setup_call_cleanup(
      open(Session, write, Response, [type(binary)]),
      send_binary(library(wire/images/closed), Response),
      close(Response)).
dispatch(_, '/images/open.gif', _, Session) :- !,
   setup_call_cleanup(
      open(Session, write, Response, [type(binary)]),
      send_binary(library(wire/images/open), Response),
      close(Response)).
dispatch(_, '/images/blank.gif', _, Session) :- !,
   setup_call_cleanup(
      open(Session, write, Response, [type(binary)]),
      send_binary(library(wire/images/blank), Response),
      close(Response)).
dispatch(_, '/images/break.gif', _, Session) :- !,
   setup_call_cleanup(
      open(Session, write, Response, [type(binary)]),
      send_binary(library(wire/images/break), Response),
      close(Response)).
dispatch(_, '/index.html', _, Session) :- !,
   setup_call_cleanup(
      open(Session, write, Response),
      send_text(library(wire/pages/index), Response),
      close(Response)).
dispatch(_, Path, Request, Session) :-
   sub_atom(Path, 0, Pos, '/desktop/'), !,
   Pos2 is Pos-1,
   sub_atom(Path, Pos2, _, 0, Path2),
   wire/desktop::dispatch(Path2, Request, Session).
dispatch(_, Path, Request, Session) :-
   sub_atom(Path, 0, Pos, '/mobile/'), !,
   Pos2 is Pos-1,
   sub_atom(Path, Pos2, _, 0, Path2),
   wire/mobile::dispatch(Path2, Request, Session).
dispatch(Object, Spec, Request, Session) :-
   notebook/httpsrv:dispatch(Object, Spec, Request, Session).

/**
 * upgrade(O, P, R, S):
 * The predicate succeeds in upgrading the request for object
 * O, with path P, with request R and the session S.
 */
% upgrade(+Object, +Spec, +Request, +Session)
:- override upgrade/4.
:- public upgrade/4.
upgrade(_, _, Request, Session) :-
   setup_call_cleanup(
      open(Session, write, Output, [type(binary),buffer(0)]),
      response_upgrade(Request, Output),
      flush_output(Output)),
   open(Session, read, Input, [type(binary),buffer(0)]),
   endpoint(wire/pages/talkback, Input, Output).
