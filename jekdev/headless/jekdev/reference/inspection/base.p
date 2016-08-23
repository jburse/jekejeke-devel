/**
 * For debugging purpose it might be necessary to access sources
 * that are not accessible from the top-level. We provide predicates
 * to allow direct access.
 *
 * The directly accessible sources can be tested and enumerated
 * by the predicate current_base/1. The predicate sys_callable_slash/2
 * can be used to explicitly invoke to slash (/)/2 notation conversion
 * for a callable.
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

:- package(library(jekdev/reference/inspection)).
:- use_package(foreign(jekdev/reference/inspection)).

:- module(base, []).

/********************************************************************/
/* Predicates                                                       */
/********************************************************************/

/**
 * current_base(P):
 * The predicate succeeds for the directly accessible sources P.
 */
% current_base(-Indicator)
:- public current_base/1.
current_base(I) :-
   ground(I), !,
   sys_current_base_chk(I).
current_base(I) :-
   sys_current_base(L),
   sys_member(I, L).

:- private sys_current_base/1.
:- special(sys_current_base/1, 'SpecialBase', 0).

:- private sys_current_base_chk/1.
:- special(sys_current_base_chk/1, 'SpecialBase', 1).

/**
 * sys_callable_slash(S, T):
 * The predicate succeeds when S is a callable of the form
 * ‘s1....sn’(X1, .., Xm) and T is a slash notation atom of the
 * form s1/../sn(X1, .., Xm), for 1 ≤ n and 0 ≤ m.
 */
% sys_callable_slash(+-Callable, -+Term)
:- public sys_callable_slash/2.
:- special(sys_callable_slash/2, 'SpecialBase', 2).
