/**
 * By default the top-level shows the current unification equations.
 * An extension can show arbitrary constraints. It can do so by
 * defining further clauses for the multi-file predicates
 * sys_current_eq/2 and sys_unwrap_eq/3.
 *
 * The constraints that are related directly or indirectly to a term
 * can be retrieved by the predicate sys_term_eq_list/2. As a further
 * convenience the predicate call_residue/2 allows calling a goal and
 * retrieving the related constraints for each success.
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

:- module(residue, []).
:- use_module(library(advanced/sets)).
:- use_module(library(misc/residue)).

/***********************************************************/
/* Constraints Retrieval Hooks                             */
/***********************************************************/

/**
 * sys_current_eq(V, H):
 * The predicate succeeds for each equation H with variables
 * wrapped that listens on the variable V. Constraint solvers
 * should extend this multi-file predicate.
 */
% sys_current_eq(+Var, -Handle)
:- public sys_current_eq/2.
:- multifile sys_current_eq/2.
:- static sys_current_eq/2.

/**
 * sys_unwrap_eq(H, I, O):
 * The predicate converts equation H with variables wrapped into
 * equations I with variables unwrapped. The list uses the end O.
 * Constraint solvers should extend this multi-file predicate.
 */
% sys_unwrap_eq(+Handle, -Goals, +Goals)
:- public sys_unwrap_eq/3.
:- multifile sys_unwrap_eq/3.
:- static sys_unwrap_eq/3.

/***********************************************************/
/* Constraint Selection Algorithm                          */
/***********************************************************/

/**
 * sys_follow_vars(H, V, W, L, R):
 * Succeeds with the equations R with variables unwrapped related
 * to the variables H and extending the equations L, and the visited
 * variables V extending the visited variables W.
 */
% sys_follow_vars(+Vars, +Vars, -Vars, +Goals, -Goals)
:- private sys_follow_vars/5.
sys_follow_vars([U|H], V, W, L, R) :-
   sys_follow_vars(H, V, Z, L, M),
   sys_follow_vars2(U, Z, W, M, R).
sys_follow_vars([], V, V, L, L).

% sys_follow_vars2(+Var, +Vars, -Vars, +Goals, -Goals)
:- private sys_follow_vars2/5.
sys_follow_vars2(U, V, V, L, L) :-
   contains(U, V), !.
sys_follow_vars2(U, V, W, L, R) :-
   findall(K, sys_current_eq(U, K), J),
   sys_unwrap_eqs(J, K, []),
   sys_follow_eqs(K, [U|V], W, L, R).

% sys_unwrap_eqs(+Goals, -Goals, +Goals)
:- private sys_unwrap_eqs/3.
sys_unwrap_eqs([G|L], I, O) :-
   sys_unwrap_eq(G, I, H),
   sys_unwrap_eqs(L, H, O).
sys_unwrap_eqs([], L, L).

/**
 * sys_follow_eqs(H, V, W, L, R):
 * Succeeds with the equations R with variables unwrapped related
 * to the equations H and extending the equations L, and the visited
 * variables V extending the visited variables W.
 */
% sys_follow_eqs(+Goals, +Vars, -Vars, +Goals, -Goals)
:- private sys_follow_eqs/5.
sys_follow_eqs([G|H], V, W, L, R) :-
   sys_follow_eqs(H, V, Z, L, M),
   sys_follow_eqs2(G, Z, W, M, R).
sys_follow_eqs([], V, V, L, L).

% sys_follow_eqs2(Goal, +Vars, -Vars, +Goals, -Goals)
:- private sys_follow_eqs2/5.
sys_follow_eqs2(G, V, V, L, L) :-
   contains(G, L), !.
sys_follow_eqs2(G, V, W, L, R) :-
   safe_term_variables(G, U),
   sys_follow_vars(U, V, W, [G|L], R).

/***********************************************************/
/* Consraint Selection API                                 */
/***********************************************************/

/**
 * sys_term_eq_list(T, L):
 * The predicate unifies L with the list of constraints that
 * depend directly or indirectly on the variables of T.
 */
% sys_term_eq_list(+Term, -Goals)
:- public sys_term_eq_list/2.
sys_term_eq_list(T, L) :-
   safe_term_variables(T, H),
   sys_follow_vars(H, [], _, [], L).

/**
 * call_residue(G, L):
 * The predicate succeeds whenever the goal G succeeds. The predicate
 * unifies L with the list of constraints that depend directly or
 * indirectly on the variables of G.
 */
:- public call_residue/2.
:- meta_predicate call_residue(0,?).
call_residue(G, L) :-
   call(G),
   sys_term_eq_list(G, L).

/***********************************************************/
/* CAS Display Hook                                        */
/***********************************************************/

/**
 * printable(F, G):
 * The predicate succeeds in G with a custom form of F.
 */
% printable(+Term, -Term)
:- public printable/2.
printable(E, F) :-
   sys_printable_value(E, H), !,
   F = H.
printable(E, E).

/**
 * sys_printable_value(F, G):
 * The predicate succeeds in G with a custom form of F. The
 * predicate should be extended for custom forms.
 */
% sys_printable_value(+Term, -Term)
:- public sys_printable_value/2.
:- multifile sys_printable_value/2.
:- static sys_printable_value/2.
