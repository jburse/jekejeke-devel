/**
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

:- package(library(jekdev/frequent/misc)).
:- use_package(foreign(matula/text/misc)).
:- use_package(foreign(java/net)).

:- module(filter, []).

/**
 * websock_new(S, W):
 * The predicate succeeds in a web socket W for the socket S.
 */
:- public websock_new/2.
:- foreign_constructor(websock_new/2, 'Framed', new('Socket')).
