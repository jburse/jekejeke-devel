/**
 * Backtracking can be also triggered by so called pseudo Boolean
 * constraints. These are linear integer coefficient combinations of
 * Boolean variables. Our implementation does not convert pseudo
 * Boolean constraints into BDD but uses their own format. The
 * optimization predicate weighted_maximum/3 then uses this search method:
 *
 • * Branch and Bound Search
 *
 * The labelling variant random_labeling/1 uses a random order among
 * each variable. The maximization predicate uses random labeling in
 * its initial value and in its search. A pseudo Boolean constraint is
 * used in each step to improve the maximum. Since the weights can be
 * negative, the predicate is also suitable to solve minimization problems.
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

/**
 * Implementation note for pseudo boolean constraint solving. A
 * verify hook is used, since no new unification result from
 * forward checking only success or failure.
 *
 * - Each Boolean variable has an attribute of the form:
 *     pseudo:watch_ref(+Map)
 * where Map is a map from fresh and shared variables representing
 * a watcher to the variable weight. The Map gets updated
 * during model setup and during forward checking.
 *
 *- Each variable representing a watcher has an attribute of the form:
 *     pseudo:watch_root(+Interval, +Vars, +Comparator, +Value)
 * Where Interval is the estimated interval, where Vars are the
 * variables and where Comparator is the comparator and where
 * Value is the compared value.
 * The Maximum and the Value gets updated during forward checking.
 */

:- package(library(jekmin/reference/boole)).
:- module(pseudo, []).

:- use_module(library(experiment/maps)).
:- use_module(library(term/verify)).
:- use_module(library(basic/random)).

/**
 * random_labeling(L):
 * The predicate randomly labels the variables in L.
 */
% random_labeling(+List)
:- public random_labeling/1.
random_labeling(L) :- var(L),
   throw(error(instantiation_error, _)).
random_labeling([B|L]) :- var(B), !,
   random(2, C),
   expr_value(C, B),
   random_labeling(L).
random_labeling([_|L]) :- !,
   random_labeling(L).
random_labeling([]) :- !.
random_labeling(L) :-
   throw(error(type_error(list, L), _)).

/**
 * pseudo(R, L, C, K):
 * The predicate succeeds in a new pseudo Boolean constraint
 * with weights R, variables L, comparator C and value K.
 */
% pseudo(+List, +List, +Atom, +Number)
:- public pseudo/4.
pseudo(R, L, C, K) :-
   watch_add_vars(R, L, H, 0, V, 0-0, J),
   U is K-V,
   watch_trivial(H, J, L, C, U).

/**
 * weighted_maximum(W, L, O):
 * The predicate succeeds in O with the maximum of the weighted
 * sum from the values L and the weights W, and then succeeds
 * for all corresponding labelings of L.
 */
% weighted_maximum(+list, +List, -Number)
:- public weighted_maximum/3.
weighted_maximum(R, L, O) :-
   sat_find_start(R, L, K),
   watch_add_vars(R, L, H, 0, V, 0-0, J),
   sat_find_maximum(V, J, H, R, L, K, O),
   U is O-V,
   watch_trivial(H, J, L, >=, U),
   random_labeling(L).

/*****************************************************************/
/* Branch and Bound                                              */
/*****************************************************************/

% sat_find_start(+List, +List, -Number)
:- private sat_find_start/3.
sat_find_start(R, L, K) :-
   catch((random_labeling(L),
      sys_weighted_sum(R, L, 0, F),
      throw(sat_start_bound(F))),
      sat_start_bound(K),
      true).

% sat_find_maximum(+Number, +Interval, +Var, +List, +List, +Number, -Number)
:- private sat_find_maximum/7.
sat_find_maximum(V, J, H, R, L, K, O) :-
   catch((U is K-V,
      watch_trivial(H, J, L, >, U),
      random_labeling(L),
      sys_weighted_sum(R, L, 0, F),
      throw(sat_new_bound(F))),
      sat_new_bound(P),
      true), !,
   sat_find_maximum(V, J, H, R, L, P, O).
sat_find_maximum(_, _, _, _, _, K, K).

/**
 * sys_weighted_sum(R, L, S, T):
 * The predicate succeeds in T with the scalar product
 * of the weight R and the values L plus the number S.
 */
% sys_weighted_sum(+List, +List, +Number, -Number)
:- private sys_weighted_sum/4.
sys_weighted_sum([V|R], [B|L], S, T) :-
   H is S+B*V,
   sys_weighted_sum(R, L, H, T).
sys_weighted_sum([], [], S, S).

/*****************************************************************/
/* Pseudo Booleans                                               */
/*****************************************************************/

/**
 * watch_add_vars(R, L, H, S, T, P, Q):
 * The predicate succeeds in adding watchers to the fresh
 * and shared variable H, and returns in T the number S
 * plus the partial weighted sum, and returns in Q the
 * interval P plus the estimated interval.
 */
% watch_add_vars(+List, +List, +Var, +Number, -Number, +Interval, -Interval)
:- private watch_add_vars/7.
watch_add_vars(_, L, _, _, _, _, _) :- var(L),
   throw(error(instantiation_error, _)).
watch_add_vars([V|R], [B|L], H, S, T, P, Q) :- var(B),
   get_atts(B, pseudo, watch_ref(F)), !,
   map_include(V, H, F, G, P, J),
   put_atts(B, pseudo, watch_ref(G)),
   watch_add_vars(R, L, H, S, T, J, Q).
watch_add_vars([V|R], [B|L], H, S, T, P, Q) :- var(B), !,
   put_atts(B, pseudo, watch_ref([H-V])),
   interval_addition(P, V, J),
   watch_add_vars(R, L, H, S, T, J, Q).
watch_add_vars([V|R], [B|L], H, S, T, P, Q) :- !,
   expr_value(B),
   J is S+B*V,
   watch_add_vars(R, L, H, J, T, P, Q).
watch_add_vars([], [], _, S, S, P, P) :- !.
watch_add_vars(_, L, _, _, _, _, _) :-
   throw(error(type_error(list, L), _)).

% map_include(+Number, +Var, +Map, -Map, +Number, -Number)
:- private map_include/6.
map_include(V, H, F, G, S, T) :- eq_get(F, H, W), !,
   J is V+W,
   interval_subtract(S, W, K),
   interval_addition(K, J, T),
   eq_put(F, H, J, G).
map_include(V, H, F, [H-V|F], S, T) :-
   interval_addition(S, V, T).

% expr_value(+Boolean)
expr_value(0).
expr_value(1).

% expr_value(+Boolean, -Boolean)
:- private expr_value/2.
expr_value(X, X).
expr_value(X, Y) :- Y is 1-X.

/*****************************************************************/
/* Root Addition                                                 */
/*****************************************************************/

% watch_trivial(+Vars, +Interval, +List, +Comparator, +Number)
:- private watch_trivial/5.
watch_trivial(H, J, _, C, U) :-
   watch_success(C, J, U), !, del_atts(H, pseudo).
watch_trivial(_, J, _, C, U) :-
   watch_failure(C, J, U), !, fail.
watch_trivial(H, J, L, C, U) :-
   put_atts(H, pseudo, watch_root(J, L, C, U)).

% watch_success((+Comparator, +Interval, +Number)
:- private watch_success/3.
watch_success(>, F-_, U) :- !, F > U.
watch_success(>=, F-_, U) :- !, F >= U.
watch_success(<, _-T, U) :- !, T < U.
watch_success(=<, _-T, U) :- !, T =< U.
watch_success(=:=, F-T, U) :- !, T =< U, F >= U.
watch_success(=\=, _-T, U) :- T < U, !.
watch_success(=\=, F-_, U) :- !, F > U.
watch_success(C, _, _) :-
   throw(error(type_error(comparator, C), _)).

% watch_failure(+Comparator, +Interval, +Number)
:- private watch_failure/3.
watch_failure(>, _-T, U) :- !, T =< U.
watch_failure(>=, _-T, U) :- !, T < U.
watch_failure(<, F-_, U) :- !, F >= U.
watch_failure(=<, F-_, U) :- !, F > U.
watch_failure(=:=, _-T, U) :- T < U, !.
watch_failure(=:=, F-_, U) :- !, F > U.
watch_failure(=\=, F-T, U) :- !, T =< U, F >= U.
watch_failure(C, _, _) :-
   throw(error(type_error(comparator, C), _)).

/*****************************************************************/
/* Verify Hook                                                   */
/*****************************************************************/

/**
 * verify_attributes(A, W):
 * The predicate is called when a varable with attribute
 * term A got the value W assigned.
 */
% verify_attributes(+Attr, +Term)
:- public verify_attributes/2.
verify_attributes(watch_ref(F), W) :- var(W),
   get_atts(W, pseudo, watch_ref(G)), !,
   map_union(F, G, E),
   put_atts(W, pseudo, watch_ref(E)).
verify_attributes(watch_ref(F), W) :- var(W), !,
   put_atts(W, pseudo, watch_ref(F)).
verify_attributes(watch_ref(F), 0) :- !,
   watch_unify(F, 0).
verify_attributes(watch_ref(F), 1) :- !,
   watch_unify(F, 1).
verify_attributes(watch_ref(_, _), W) :-
   throw(error(type_error(sat_value, W), _)).

% watch_unify(+Map, +Boolean)
:- private watch_unify/2.
watch_unify([H-V|F], B) :-
   get_atts(H, pseudo, watch_root(J, Z, C, U)), !,
   W is U-B*V,
   interval_subtract(J, V, K),
   watch_trivial(H, K, Z, C, W),
   watch_unify(F, B).
watch_unify([_|F], B) :-
   watch_unify(F, B).
watch_unify([], _).

% map_union(+Map, +Map, -Map)
:- private map_union/3.
map_union([H-V|L], M, R) :- eq_get(M, H, W), !,
   S is V+W,
   watch_update(H, V, W, S),
   eq_put(M, H, S, N),
   map_union(L, N, R).
map_union([H-V|L], M, [H-V|R]) :-
   map_union(L, M, R).
map_union([], M, M).

% watch_update(+Var, +Number, +Number, +Number)
:- private watch_update/4.
watch_update(H, V, W, S) :-
   get_atts(H, pseudo, watch_root(J, Z, C, U)), !,
   interval_subtract(J, V, P),
   interval_subtract(P, W, Q),
   interval_addition(Q, S, K),
   watch_trivial(H, K, Z, C, U).
watch_update(_, _, _, _).

% interval_subtract(+Interval, +Number, -Interval)
:- private interval_subtract/3.
interval_subtract(F-T, V, U-T) :- V =< 0, !, U is F-V.
interval_subtract(F-T, V, F-U) :- U is T-V.

% interval_addition(+Interval, +Number, -Interval)
:- private interval_addition/3.
interval_addition(F-T, V, U-T) :- V =< 0, !, U is F+V.
interval_addition(F-T, V, F-U) :- U is T+V.

/*****************************************************************/
/* Portraying the Attributes                                     */
/*****************************************************************/

/**
 * portray_attributes(V, I, O):
 * The predicate is called when the goal list I of
 * the variable V is required. The list should end in O.
 */
% portray_attributes(+Variable, -List, +List)
:- public portray_attributes/3.
portray_attributes(A, S, S) :-
   get_atts(A, pseudo, watch_ref(_)).
portray_attributes(A, [pseudo(R, L, C, U)|S], S) :-
   get_atts(A, pseudo, watch_root(_, Z, C, U)),
   term_variables(Z, L), L \== [], !,
   watch_get_weights(L, A, R).
portray_attributes(A, S, S) :-
   get_atts(A, pseudo, watch_root(_, _, _, _)).

% watch_get_weights(+List, +Var, -List)
:- private watch_get_weights/3.
watch_get_weights([B|L], H, [V|R]) :-
   get_atts(B, pseudo, watch_ref(M)),
   eq_get(M, H, V),
   watch_get_weights(L, H, R).
watch_get_weights([], _, []).
