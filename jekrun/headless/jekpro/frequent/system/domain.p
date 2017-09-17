/**
 * This module provides a couple of simple utilities to deal with the
 * access to internationalized domain names (IDNs). The predicate
 * make_domain/3 allows constructing and deconstructing user and
 * host. The predicate works bidirectional without loss of data.
 *
 * Example:
 * ?- make_domain('foo','λ.com',X).
 * X = 'foo@λ.com'
 *
 * The predicate host_lookup/2 can be used to perform a forward or
 * reverse lookup of a host name. The predicate will fail if the
 * host name is not known. The predicate ping_host/1 can be used
 * to check the reachability of a host name.
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- package(library(jekpro/frequent/system)).
:- use_package(foreign(matula/util/system)).

:- module(domain, []).

/**
 * make_domain(U, H, D):
 * If U and H are variables, then the predicate succeeds when
 * U and H unify with the user and the host of the domain D.
 * Otherwise the predicates succeeds when D unifies with the
 * constructed domain.
 */
% make_domain(+-Atom, +-Atom, -+Atom)
:- public make_domain/3.
make_domain(U, H, D) :-
   (  var(U)
   ;  var(H)), !,
   sys_domain_user(D, U),
   sys_domain_host(D, H).
make_domain(U, H, D) :-
   sys_domain_make(U, H, D).

:- private sys_domain_user/2.
:- foreign(sys_domain_user/2, 'ForeignDomain',
      sysDomainUser('String')).

:- private sys_domain_host/2.
:- foreign(sys_domain_host/2, 'ForeignDomain',
      sysDomainHost('String')).

:- private sys_domain_make/3.
:- foreign(sys_domain_make/3, 'ForeignDomain',
      sysDomainMake('String','String')).

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
host_lookup(X, Y) :-
   var(X), !,
   sys_domain_reverse(Y, X).
host_lookup(X, Y) :-
   sys_domain_forward(X, Y).

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
 * spec_puny(S, P):
 * If S is a variable then the predicate succeeds when S unifies with
 * the puny decode of P. Otherwise the predicate succeeds when P unifies
 * with the puny encode of S.
 */
% spec_puny(+-Atom, -+Atom)
:- public spec_puny/2.
spec_puny(X, Y) :-
   var(X), !,
   sys_spec_unpuny(Y, X).
spec_puny(X, Y) :-
   sys_spec_puny(X, Y).

:- private sys_spec_puny/2.
:- foreign(sys_spec_puny/2, 'ForeignDomain',
      sysSpecPuny('String')).

:- private sys_spec_unpuny/2.
:- foreign(sys_spec_unpuny/2, 'ForeignDomain',
      sysSpecUnpuny('String')).
