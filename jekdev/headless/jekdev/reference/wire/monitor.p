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
dispatch(_, Path, Params, Session) :-
   sub_atom(Path, 0, Pos, '/desktop/'), !,
   Pos2 is Pos-1,
   sub_atom(Path, Pos2, _, 0, Path2),
   wire/desktop::dispatch(Path2, Params, Session).
dispatch(_, Path, Params, Session) :-
   sub_atom(Path, 0, Pos, '/mobile/'), !,
   Pos2 is Pos-1,
   sub_atom(Path, Pos2, _, 0, Path2),
   wire/mobile::dispatch(Path2, Params, Session).

/***************************************************************/
/* HTTP Response Binary                                        */
/***************************************************************/

/**
 * send_binary(F, O):
 * The predicate sends the binary resource F to the output stream O.
 */
% send_binary(+File, +Stream)
:- private send_binary/2.
send_binary(File, Response) :-
   setup_call_cleanup(
      open_resource(File, Stream, [type(binary)]),
      (  response_binary(Response),
         send_blocks(Stream, Response)),
      close(Stream)).

% send_blocks(+Stream, +Stream)
:- private send_blocks/2.
send_blocks(Stream, Response) :-
   read_block(Stream, 1024, Block), !,
   write_block(Response, Block),
   send_blocks(Stream, Response).
send_blocks(_, _).
