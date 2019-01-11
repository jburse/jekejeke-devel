/**
 * This module provides a web socket extension.
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

:- package(library(jekdev/reference/notebook)).
:- use_package(foreign(jekdev/reference/notebook)).
:- use_package(foreign(java/io)).

:- module(websock, []).
:- use_module(library(runtime/distributed)).

/**
 * endpoint(O, S):
 * The predicate runs an end-point with object O and session S.
 */
% endpoint(+Object, +Input, +Output)
:- public endpoint/3.
endpoint(Object, Input, Output) :-
   spawn(runner(Object, Input, Output)).


% runner(+Object, +Input, +Output)
:- private runner/3.
runner(Object, Input, Output) :-
   Object::start(Output),
   runner2(Object, Input, Output).

% runner2(+Object, +Input, Output)
:- private runner2/3.
runner2(Object, Input, Output) :-
   websock_read(Input, OpCode, Payload), !,
   Object::handle(Output, OpCode, Payload),
   runner2(Object, Input, Output).
runner2(Object, _, Output) :-
   Object::stop(Output).

/**
 * send(S, O, P):
 * The predicate succeeds in sending op code O and payload
 * P to the session S.
 */
% send(+Output, +Integer, +Bytes)
:- public send/3.
send(Output, OpCode, Payload) :-
   websock_write(Output, OpCode, Payload),
   flush_output(Output).

/**
 * websock_read(S, O, P):
 * The predicate succeeds in O with the op code and in P
 * with the payload of reading from the input stream S.
 */
% websock_read(+Stream, -Integer, -Bytes)
:- public websock_read/3.
websock_read(Stream, OpCode, Payload) :-
   sys_websock_read(Stream, message(OpCode,Payload)).

% sys_websock_read(+Stream, -Term)
:- private sys_websock_read/2.
:- foreign(sys_websock_read/2, 'ForeignWebsock',
      sysWebsockRead('InputStream')).

/**
 * websock_write(S, O, P):
 * The predicate succeeds in writing the op code O and the
 * payload P to the output stream S.
 */
% websock_write(+Stream, +Integer, +Bytes)
:- public websock_write/3.
:- foreign(websock_write/3, 'ForeignWebsock',
      sysWebsockWrite('OutputStream',byte,{byte})).
