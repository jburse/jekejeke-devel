/**
 * This module provides a couple of simple utilities to deal with the
 * access to internationalized domain names (IDNs). The predicate
 * make_authority/4 allows constructing and deconstructing user, host
 * and port. The predicate works bidirectional without loss of data.
 *
 * Example:
 * ?- make_authority('foo','λ.com',-1,X).
 * X = 'foo@λ.com'
 *
 * The predicate host_lookup/2 can be used to perform a forward or
 * reverse lookup of a host name. The predicate will fail if the
 * host name is not known. The predicate ping_host/1 can be used to
 * check the reachability of a host name. The Java internet libraries
 * do not automatically a name preparation. Neither do our Prolog
 * predicates presented so far.
 *
 * Example:
 * ?- uri_puny('http://zürich.ch/robots.txt', X).
 * X = 'http://xn--zrich-kva.ch/robots.txt'
 *
 * Name preparation is for example required for host names. Domain name
 * servers only work with ASCII represented host names and the
 * recommended encoding of Unicode towards ASCII for host names is
 * puny code. Such an encoding can be invoked by the predicate uri_puny/2
 * provided in this module.
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

:- package(library(jekpro/frequent/system)).
:- use_package(foreign(matula/util/system)).

:- module(domain, []).

/**
 * make_authority(U, H, P, A):
 * If U, H or P are variables, then the predicate succeeds when
 * U, H and P unify with the user, the host and the port of the
 * authority A. Otherwise the predicates succeeds when A unifies
 * with the constructed authority.
 */
% make_authority(+-Atom, +-Atom, +-Atom, -+Atom)
:- public make_authority/4.
make_authority(U, H, P, A) :- (var(U); var(H); var(P)), !,
   sys_authority_user(A, U),
   sys_authority_host(A, H),
   sys_authority_port(A, P).
make_authority(U, H, P, A) :-
   sys_authority_make(U, H, P, A).

:- private sys_authority_user/2.
:- foreign(sys_authority_user/2, 'ForeignDomain',
      sysAuthorityUser('String')).

:- private sys_authority_host/2.
:- foreign(sys_authority_host/2, 'ForeignDomain',
      sysAuthorityHost('String')).

:- private sys_authority_port/2.
:- foreign(sys_authority_port/2, 'ForeignDomain',
      sysAuthorityPort('String')).

:- private sys_authority_make/4.
:- foreign(sys_authority_make/4, 'ForeignDomain',
      sysAuthorityMake('String', 'String', int)).

/************************************************************/
/* Domain Lookup                                            */
/************************************************************/

/**
 * host_lookup(U, C):
 * If U is a variable then the predicate succeeds when U unifies with
 * reverse lookup of C. Otherwise the predicate succeeds when C unifies
 * with the forward lookup of U.
 */
% host_lookup(+-Atom, -+Atom)
:- public host_lookup/2.
host_lookup(X, Y) :- var(X), !, sys_domain_reverse(Y, X).
host_lookup(X, Y) :- sys_domain_forward(X, Y).

:- private sys_domain_forward/2.
:- foreign(sys_domain_forward/2, 'ForeignDomain',
      sysForwardLookup('String')).

:- private sys_domain_reverse/2.
:- foreign(sys_domain_reverse/2, 'ForeignDomain',
      sysReverseLookup('String')).

/************************************************************/
/* Ping Host                                                */
/************************************************************/

/**
 * ping_host(H):
 * The predicate succeeds when the host H is reachable.
 */
% ping_host(+Atom)
:- public ping_host/1.
:- foreign(ping_host/1, 'ForeignDomain',
      sysPingHost('String')).

/************************************************************/
/* Puny Code                                                */
/************************************************************/

/**
 * uri_puny(S, P):
 * If S is a variable then the predicate succeeds when S unifies with
 * the puny decode of P. Otherwise the predicate succeeds when P unifies
 * with the puny encode of S.
 */
% uri_puny(+-Atom, -+Atom)
:- public uri_puny/2.
uri_puny(X, Y) :- var(X), !, sys_uri_unpuny(Y, X).
uri_puny(X, Y) :- sys_uri_puny(X, Y).

:- private sys_uri_puny/2.
:- foreign(sys_uri_puny/2, 'ForeignDomain',
      sysUriPuny('String')).

:- private sys_uri_unpuny/2.
:- foreign(sys_uri_unpuny/2, 'ForeignDomain',
      sysUriUnpuny('String')).

/************************************************************/
/* SHA-1 Hash                                               */
/************************************************************/

/**
 * sha1_hash(B, H):
 * The predicate succeeds in H with the SHA-1 hash block
 * of the block B.
 */
% sha1_hash(+Bytes, -Bytes)
:- public sha1_hash/2.
:- foreign(sha1_hash/2, 'ForeignDomain', sysSHA1Hash({byte})).