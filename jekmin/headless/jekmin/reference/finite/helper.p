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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

% :- package(library(ordered)).
:- package(library(jekmin/reference/finite)).
:- use_package(library(jekmin/reference/minimal)).

:- module(helper, []).
:- use_module(library(minimal/assume)).
:- use_module(library(minimal/hypo)).
:- use_module(library(experiment/trail)).
:- use_module(library(experiment/attr)).

/**********************************************************/
/* Post Extensions                                        */
/**********************************************************/

% <= +Term
:- public hypo:(<=)/1.
:- multifile hypo:(<=)/1.
:- meta_predicate hypo:(<= -1).
:- discontiguous hypo:(<=)/1.
/**
 * sys_melt_join(S, T):
 * Unwrap the variables S and T and unify them.
 */
hypo:(<= sys_melt_join(S, T)) :- !,
   sys_melt_var(S, U),
   sys_melt_var(T, U).
/**
 * sys_melt_const(S, C):
 * Unwrap the variable S and unify it with the constant C.
 */
hypo:(<= sys_melt_const(S, C)) :- !,
   sys_melt_var(S, C).
/**
 * sys_melt_hook(X, H):
 * Unwrap the variable X and give the hook H to it.
 */
hypo:(<= sys_melt_hook(X, H)) :- !,
   sys_melt_var(X, A),
   sys_ensure_hook(A, H).

% hypo_abnormal(+Term)
:- public hypo:hypo_abnormal/1.
:- multifile hypo:hypo_abnormal/1.
:- discontiguous hypo:hypo_abnormal/1.
hypo:hypo_abnormal(sys_melt_join(_,_)).
hypo:hypo_abnormal(sys_melt_const(_,_)).
hypo:hypo_abnormal(sys_melt_hook(_,_)).

/**
 * sys_fresh_var(X, B):
 * Give the variable X a serial number and wrap it into B.
 */
% sys_fresh_var(+Var, -Wrap)
sys_fresh_var(X, B) :-
   sys_ensure_serno(X),
   sys_freeze_var(X, B).
