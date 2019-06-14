/**
 * Some helpers for the CLP(FD) library.
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

% :- package(library(ordered)).
:- package(library(jekmin/reference/finite)).
:- use_package(library(jekmin/reference/minimal)).

:- module(helper, []).
:- use_module(library(minimal/assume)).
:- use_module(library(experiment/trail)).
:- use_module(library(experiment/attr)).

/**********************************************************/
/* Post Extensions                                        */
/**********************************************************/

/**
 * sys_melt_const(S, C):
 * Unwrap the variable S and unify it with the constant C.
 */
:- public sys_melt_const/2.
sys_melt_const(S, C) :-
   sys_melt_var(S, C).

/**
 * sys_melt_join(S, T):
 * Unwrap the variables S and T and unify them.
 */
:- public sys_melt_join/2.
sys_melt_join(S, T) :-
   sys_melt_var(S, U),
   sys_melt_var(T, U).

/**
 * sys_melt_hook(X, H):
 * Unwrap the variable X and give the hook H to it.
 */
:- public sys_melt_hook/2.
sys_melt_hook(X, H) :-
   sys_melt_var(X, A),
   sys_ensure_hook(A, H).

% sys_ensure_hook(+Attr, +Closure)
:- private sys_ensure_hook/2.
:- meta_predicate sys_ensure_hook(?,2).
sys_ensure_hook(V, H) :-
   sys_clause_hook(V, H, _), !.
sys_ensure_hook(V, H) :-
   sys_compile_hook(V, H, K),
   depositz_ref(K).

/**
 * sys_fresh_var(X, B):
 * Give the variable X a serial number and wrap it into B.
 */
% sys_fresh_var(+Var, -Wrap)
sys_fresh_var(X, B) :-
   sys_ensure_serno(X),
   sys_freeze_var(X, B).
