/**
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
