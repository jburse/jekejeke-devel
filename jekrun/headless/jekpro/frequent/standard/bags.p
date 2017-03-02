/**
 * The following predicates do a grouping of the solutions and may
 * succeed more than once. This grouping is based on the determination
 * of the witnesses of a solution. The witness are the global variables
 * of T^G where T is the template and G is the goal.
 *
 * Examples:
 * ?- [user].
 * p(a,y).
 * p(a,x).
 * p(b,x).
 *
 * Yes
 * ?- bagof(X,p(X,Y),L).
 * Y = y, L = [a] ;
 * Y = x, L = [a, b]
 * ?- bagof(X,Y^p(X,Y),L).
 * L = [a, a, b]
 *
 * The predicate bagof/3 will neither do a sorting of the witnesses nor
 * of the resulting lists. The variation setof/3 will sort the resulting
 * lists and the variation sys_heapof/3 will sort the witnesses.
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

:- use_package(foreign(jekpro/frequent/standard)).

:- module(user, []).

/**
 * findall(T, G, L): [ISO 8.10.1]
 * The predicate first finds all the solutions to the goal G, whereby
 * collecting copies of the template T in a list. The predicate then
 * succeeds when L unifies with the list.
 */
% findall(+Goal, +Template, -List)
:- public findall/3.
:- meta_predicate findall(?,0,?).
:- special(findall/3, 'SpecialFind', 0).

/**********************************************************/
/* Bagof Predicates                                       */
/**********************************************************/

/**
 * bagof(T, X1^…^Xn^G, L): [ISO 8.10.2]
 * The predicate determines all the solutions to the matrix G, whereby
 * collecting copies of the template T sorted by the witnesses in a list.
 * The predicate then repeatedly succeeds by unifying the witnesses and when L
 * unifies with the corresponding list.
 */
% bagof(+Template, +QuantGoal, -List)
:- public bagof/3.
:- meta_predicate bagof(?,0,?).
bagof(T, G, L) :-
   sys_goal_globals(T^G, W),
   W \== [], !,
   sys_goal_kernel(G, B),
   findall(W-T, B, H),
   sys_key_variables(H, _),
   keysort(H, J),
   sys_run_values(J, W, L).
bagof(T, G, L) :-
   sys_goal_kernel(G, B),
   findall(T, B, L),
   L \== [].

/**
 * setof(T, X1^…^Xn^G, L): [ISO 8.10.2]
 * The predicate determines the same lists as the predicate bagof/3. But
 * before returning them the lists are sorted by means of the predicate sort/2.
 */
% setof(+Template, +QuantGoal, -List)
:- public setof/3.
:- meta_predicate setof(?,0,?).
setof(T, G, L) :-
   bagof(T, G, H),
   sort(H, L).

/**
 * sys_heapof(T, X1^…^Xn^G, L):
 * The predicate determines the same lists as the predicate bagof/3. But
 * the lists are grouped by the witnesses instead of sorted by the witnesses.
 */
% sys_heapof(+Template, +QuantGoal, -List)
:- public sys_heapof/3.
:- meta_predicate sys_heapof(?,0,?).
sys_heapof(T, G, L) :-
   sys_goal_globals(T^G, W),
   W \== [], !,
   sys_goal_kernel(G, B),
   findall(W-T, B, H),
   sys_key_variables(H, _),
   sys_keygroup(H, J),
   sys_run_values(J, W, L).
sys_heapof(T, G, L) :-
   sys_goal_kernel(G, B),
   findall(T, B, L),
   L \== [].

/**********************************************************/
/* Grouping Utilities                                     */
/**********************************************************/

% sys_key_variables(+Pairs, +Vars)
:- private sys_key_variables/2.
sys_key_variables([], _).
sys_key_variables([K-_|P], L) :-
   term_variables(K, L, _),
   sys_key_variables(P, L).

% sys_run_values(+Pairs, -Key, -Values)
:- private sys_run_values/3.
sys_run_values([K-V|P], J, M) :-
   sys_run_values_rest(P, K, L, Q),
   sys_run_values_more(Q, K, [V|L], J, M).

% sys_run_values_more(+Pairs, +Key, +Values, -Key, -Values)
:- private sys_run_values_more/5.
sys_run_values_more(_, J, M, J, M).
sys_run_values_more([K-V|P], _, _, J, M) :-
   sys_run_values_rest(P, K, L, Q),
   sys_run_values_more(Q, K, [V|L], J, M).

% sys_run_values_rest(+Pairs, +Key, -Values, -Pairs)
:- private sys_run_values_rest/4.
sys_run_values_rest([K-V|P], J, [V|L], Q) :-
   K == J, !,
   sys_run_values_rest(P, J, L, Q).
sys_run_values_rest(P, _, [], P).


/**********************************************************/
/* Copy Term                                              */
/**********************************************************/

/**
 * copy_term(X, Y): [ISO 8.5.4]
 * The predicate creates a copy of X and succeeds when the copy unifies with Y.
 */
% copy_term(+Term, -Term)
:- public copy_term/2.
:- special(copy_term/2, 'SpecialFind', 1).
