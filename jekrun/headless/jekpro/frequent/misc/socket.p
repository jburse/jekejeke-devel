/**
 * This module provides TCP/IP sockets. A socket provides a duplex binary
 * stream. The read and write stream can be obtained by the ordinary
 * ISO core standard open/[3,4] predicates. The open options also apply
 * to sockets so that a binary stream can be easily viewed as a
 * text stream in various encodings.
 *
 * Example:
 * ?- client_new('pot.ty', C), open(C, write, S),
 *    write_term(S, 'Hello World!'), nl(S), close(C).
 *
 * A server socket can be created with the predicates server_new/2. The
 * predicate server_accept/2 delivers a session socket. A client socket
 * can be created with the predicates client_new/3. Server, session
 * and client sockets can be closed with the ISO core standard
 * close/[1,2] predicates.
 *
 * The predicate websock_new/2 allows promoting a socket to a web
 * socket. The input and output streams will consume and generate
 * web socket frames, but can be used as ordinary ISO core standard
 * streams. During writing a final frame is generated when the
 * predicate flush_output/[1,2] is used.
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

:- package(library(jekpro/frequent/misc)).
:- use_package(foreign(jekpro/frequent/misc)).
:- use_package(foreign(matula/util/misc)).
:- use_package(foreign(java/net)).
:- use_package(foreign(java/io)).

:- module(socket, []).

/**
 * server_new(P, S):
 * The predicate succeeds in S with a new server socket for port P.
 */
:- public server_new/2.
:- foreign(server_new/2, 'ForeignSocket', sysServerNew(int)).

/**
 * server_port(S, P):
 * The predicate succeeds in P with the port of the server socket S.
 */
:- public server_port/2.
:- foreign(server_port/2, 'ForeignSocket', sysServerPort('ServerSocket')).

/**
 * server_accept(S, H):
 * The predicate succeeds in H with a new session socket from server socket S.
 */
:- public server_accept/2.
:- foreign(server_accept/2, 'ForeignSocket', sysServerAccept('ServerSocket')).

/**
 * client_new(H, P, C):
 * The predicate succeeds in C with a new client socket for host H and port P.
 */
:- public client_new/3.
:- foreign(client_new/3, 'ForeignSocket', sysClientNew('String',int)).

/**
 * websock_new(S, W):
 * The predicate succeeds in a web socket W for the socket S.
 */
:- public websock_new/2.
:- foreign_constructor(websock_new/2, 'Framed', new('Socket')).

