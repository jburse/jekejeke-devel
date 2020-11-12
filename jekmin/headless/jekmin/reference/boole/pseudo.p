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
map_include(V, H, F, G, S, T) :- get(F, H, W), !,
   J is V+W,
   interval_subtract(S, W, K),
   interval_addition(K, J, T),
   put(F, H, J, G).
map_include(V, H, F, [H-V|F], S, T) :-
   interval_addition(S, V, T).

% expr_value(+Boolean)
expr_value(0).
expr_value(1).

% expr_value_reverse(+Boolean)
expr_value_reverse(1).
expr_value_reverse(0).

/*****************************************************************/
/* Root Addition                                                 */
/*****************************************************************/

% watch_trivial(+Vars, +Interval, +List, +Comparator, +Number)
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
map_union([H-V|L], M, R) :- get(M, H, W), !,
   S is V+W,
   watch_update(H, V, W, S),
   put(M, H, S, N),
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
   get(M, H, V),
   watch_get_weights(L, H, R).
watch_get_weights([], _, []).
