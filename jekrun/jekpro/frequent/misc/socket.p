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

/********************************************************************/
/* TCP/IP Sockets                                                   */
/********************************************************************/

/**
 * server_new(H, P, S):
 * The predicate succeeds in S with a new server socket for the host
 * H and the port P. A zero port number lets the operating system choose
 * a port number.
 */
:- public server_new/3.
:- foreign(server_new/3, 'ForeignSocket', sysServerNew('String', int)).

/**
 * server_port(S, P):
 * The predicate succeeds in P with the local port of the server socket S.
 */
:- public server_port/2.
:- foreign(server_port/2, 'ForeignSocket', sysServerPort('ServerSocket')).

/**
 * server_address(S, H):
 * The predicate succeeds in H with the inet address of the server socket S.
 */
:- public server_address/2.
:- foreign(server_address/2, 'ForeignSocket', sysServerAddress('ServerSocket')).

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
:- foreign(client_new/3, 'ForeignSocket', sysClientNew('String', int)).

/********************************************************************/
/* UDP/IP Sockets                                                   */
/********************************************************************/

/**
 * endpoint_new(H, P, S):
 * The predicate succeeds in S with a new datagram socket for the
 * host H and port P. A zero port number lets the operating system
 * choose a port number.
 */
:- public endpoint_new/3.
:- foreign(endpoint_new/3, 'ForeignSocket', sysEndpointNew('String', int)).

/**
 * endpoint_port(S, P):
 * The predicate succeeds in P with the local port of the datagram socket S.
 */
:- public endpoint_port/2.
:- foreign(endpoint_port/2, 'ForeignSocket', sysEndpointPort('DatagramSocket')).

/**
 * endpoint_address(S, H):
 * The predicate succeeds in H with the inet address of the datagram socket S.
 */
:- public endpoint_address/2.
:- foreign(endpoint_address/2, 'ForeignSocket', sysEndpointAddress('DatagramSocket')).

/**
 * endpoint_receive(S, B):
 * The predicate succeeds in B with the data received
 * from the datagram socket S.
 */
:- public endpoint_receive/2.
:- foreign(endpoint_receive/2, 'ForeignSocket',
      sysEndpointReceive('DatagramSocket')).

/**
 * endpoint_send(S, B, D, P):
 * The predicate succeeds in sending the data B to the destination
 * host D and destination port P on the datagram socket S.
 */
:- public endpoint_send/4.
:- foreign(endpoint_send/4, 'ForeignSocket',
      sysEndpointSend('DatagramSocket', {byte}, 'String', int)).