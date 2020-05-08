/**
 * The aggregate predicates take a set of solutions and compute an
 * aggregate on it. The predi-cate aggregate_all/3 aggregates the
 * solution that is produced by findall/3. The predicate aggregate/3
 * respectively aggregate/4 aggregates the solutions that are
 * produced by bagof/3 respectively bagof/4.
 *
 * Examples:
 * ?- [user].
 * p(4,5).
 * p(1,2).
 * p(1,3).
 *
 * Yes
 * ?- aggregate((sum(X),count),p(Y,X),R).
 * Y = 1,
 * R = (5,2) ;
 * Y = 4,
 * R = (5,1)
 *
 * The implementation of aggregate_all/3 takes advantage of pivots
 * introduced in the module sequence. The other aggregate predicates
 * take advantage of a map from variant terms to pivots. The memory
 * usage is therefore proportional to the number of variant terms.
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

:- package(library(jekpro/frequent/advanced)).
:- use_package(foreign(jekpro/frequent/advanced)).
:- use_package(foreign(jekpro/reference/structure)).
:- use_package(foreign(matula/util/data)).
:- use_package(foreign(jekpro/tools/call)).
:- use_package(foreign(jekpro/tools/term)).

:- module(aggregate, []).
:- use_module(library(advanced/variant)).
:- use_module(library(advanced/pivot)).
:- use_module(library(advanced/revolve)).

/*************************************************************/
/* Non-Grouping Aggregate                                    */
/*************************************************************/

/**
 * aggregate_all(A, G, S):
 * aggregate_all(A, G, S, O):
 * The predicates aggregate the aggregate A for the solutions of G and
 * unifies the result with S. The quaternary predicate takes additional
 * sort options as argument. For a list of aggregates see the API
 * documentation. The sort options are the same as for compare/4 and
 * friends, for additions see the API documentation.
 */
% aggregate_all(+Aggregate, +Goal, -Value)
:- public aggregate_all/3.
:- meta_predicate aggregate_all(?, 0, ?).
aggregate_all(A, G, S) :-
   sys_pivot_new(P),
   (sys_aggregate_all(A, G, P, _), fail; true),
   sys_pivot_get_default(P, A, H),
   S = H.

% aggregate_all(+Aggregate, +Goal, -Value, +List)
:- public aggregate_all/4.
:- meta_predicate aggregate_all(?, 0, ?, ?).
aggregate_all(A, G, S, O) :-
   sys_pivot_new(P),
   sys_variant_comparator(O, C),
   (  sys_variant_eager(C)
   -> sys_aggregate_all(A, G, P, J),
      S = J
   ;  (sys_aggregate_all(A, G, P, _), fail; true),
      sys_pivot_get_default(P, A, H),
      S = H).

% sys_aggregate_all(+Aggregate, +Goal, +Pivot, -Value)
:- private sys_aggregate_all/4.
:- meta_predicate sys_aggregate_all(?, 0, ?, ?).
sys_aggregate_all(A, G, P, J) :-
   G,
   sys_pivot_get_default(P, A, H),
   sys_state_next(A, H, J),
   \+ sys_pivot_get(P, J),
   sys_pivot_set(P, J).

/*************************************************************/
/* Grouping Aggregate                                        */
/*************************************************************/

/**
 * aggregate(A, X1^…^Xn^G, S):
 * aggregate(A, X1^…^Xn^G, S, O):
 * The predicates aggregates the aggregate A for the solutions of G and
 * unifies the result with S. The result is sorted by the witnesses. The
 * quaternary predicate takes additional sort options as argument.
 */
% aggregate(+Aggregate, +QuantGoal, -Value)
:- public aggregate/3.
:- meta_predicate aggregate(?, 0, ?).
aggregate(A, Goal, S) :-
   sys_goal_globals(A^Goal, W),
   sys_revolve_new(R),
   (sys_aggregate(A, Goal, W, R, _), fail; true),
   sys_revolve_pair(R, W-Q),
   sys_pivot_get(Q, S).

% aggregate(+Aggregate, +QuantGoal, -Value, +List)
:- public aggregate/4.
:- meta_predicate aggregate(?, 0, ?, ?).
aggregate(A, Goal, S, O) :-
   sys_goal_globals(A^Goal, W),
   sys_variant_comparator(O, C),
   (  sys_variant_natural(C) -> sys_revolve_new(R)
   ;  sys_revolve_new(C, R)),
   (  sys_variant_eager(C)
   -> sys_aggregate(A, Goal, W, R, J),
      S = J
   ;  (sys_aggregate(A, Goal, W, R, _), fail; true),
      (  sys_variant_reverse(C) -> sys_revolve_pair(R, C, W-Q)
      ;  sys_revolve_pair(R, W-Q)),
      sys_pivot_get(Q, S)).

% sys_aggregate(+Aggregate, +Goal, +Vars, +Revolve, -Value)
:- meta_predicate sys_aggregate(?, 0, ?, ?, ?).
sys_aggregate(A, Goal, W, R, J) :-
   sys_goal_kernel(Goal, B),
   B,
   sys_revolve_lookup(R, W, P),
   sys_pivot_get_default(P, A, H),
   sys_state_next(A, H, J),
   \+ sys_pivot_get(P, J),
   sys_pivot_set(P, J).

/*************************************************************/
/* Aggregate State                                           */
/*************************************************************/

/**
 * sys_pivot_get_default(P, A, H):
 * The predicate succeeds in H with the value of the pivot P
 * or the initial state for the aggregate A.
 */
% sys_pivot_get_default(+Pivot, +Aggregate, -Value)
:- private sys_pivot_get_default/3.
sys_pivot_get_default(P, _, H) :-
   sys_pivot_get(P, H), !.
sys_pivot_get_default(_, A, H) :-
   sys_state_init(A, H).

/**
 * sys_state_init(A, H):
 * The predicate succeeds in H with the initial state for the aggregate A.
 */
% sys_state_init(+Aggregate, -Value)
:- private sys_state_init/2.
sys_state_init(X, _) :- var(X),
   throw(error(instantiation_error, _)).
sys_state_init(count, 0).
sys_state_init(sum(_), 0).
sys_state_init(mul(_), 1).
sys_state_init(min(_), sup).
sys_state_init(max(_), inf).
sys_state_init((A, B), (S, T)) :-
   sys_state_init(A, S),
   sys_state_init(B, T).
sys_state_init(nil, nil).
sys_state_init(first(_, _), sup).
sys_state_init(last(_, _), inf).
sys_state_init(reduce(I, _, _), I).

/**
 * sys_state_next(A, H, J):
 * The predicate succeeds in J with the next state for the
 * aggregate A after the state H.
 */
% sys_state_next(+Aggregate, +Value, -Value)
:- private sys_state_next/3.
sys_state_next(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
sys_state_next(count, S, T) :- T is S+1.
sys_state_next(sum(X), S, T) :- T is S+X.
sys_state_next(mul(X), S, T) :- T is S*X.
sys_state_next(min(X), sup, X) :- !.
sys_state_next(min(X), S, T) :- T is min(S, X).
sys_state_next(max(X), inf, X) :- !.
sys_state_next(max(X), S, T) :- T is max(S, X).
sys_state_next((S, T), (A, B), (U, V)) :-
   sys_state_next(S, A, U),
   sys_state_next(T, B, V).
sys_state_next(nil, nil, nil).
sys_state_next(first(_, X), sup, X) :- !.
sys_state_next(first(C, X), S, X) :- call(C, X, S), !.
sys_state_next(first(_, _), S, S).
sys_state_next(last(_, X), inf, X) :- !.
sys_state_next(last(C, X), S, X) :- call(C, S, X), !.
sys_state_next(last(_, _), S, S).
sys_state_next(reduce(_, A, X), S, Y) :- call(A, S, X, Y).
