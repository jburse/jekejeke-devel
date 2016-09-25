/**
 * The aggregate predicates take a set of solutions and compute an
 * aggregate on it. The predicate aggregate_all/3 aggregates the solution
 * that is produced by findall/3. The predicate aggregate/3 respectively
 * sys_collect/3 aggregates the solutions that are produced by bagof/3
 * respectively sys_heapof/3. The current implementation is not yet optimal,
 * since the solution lists are always materialized.
 *
 * Examples:
 * ?- [user].
 * p(4,5).
 * p(1,2).
 * p(1,3).
 *
 * Yes
 * ?- aggregate((sum(X),count),p(Y,X),R).
 * Y = 4,
 * R = (5,1) ;
 * Y = 1,
 * R = (5,2)
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

:- package(library(jekpro/frequent/advanced)).

:- module(aggregate, []).
:- use_module(library(experiment/abstract)).
:- use_module(library(basic/lists)).

/**
 * aggregate_all(A, G, R):
 * The predicate finds all the solutions to the goal G. It then
 * computes the aggregate A from the solutions. The predicate then
 * succeeds when R unifies with the result. The following aggregates
 * are recognized:
 *
 *   count:		The result is the number of solutions.
 *   sum(X):	The result is the sum of the X values.
 *   min(X):	The result is the minimum of the X values.
 *   max(X):	The result is the maximum of the X values.
 *   bag(X):	The result is the list of the X values.
 *   set(X):	The result is the list of the sorted X values.
 *   nodup(X):	The result is the list of the distinct X values.
 *   (X,Y):		The result is the aggregate X paired by the aggregate Y.
 */
% aggregate_all(+Aggregate, +Goal, -Value)
:- public aggregate_all/3.
:- meta_predicate aggregate_all(?,0,?).
aggregate_all(A, G, R) :-
   sys_make_accu(A, T, C),
   findall(T, G, L),
   call(C, L, R).

/**
 * aggregate_all(A, T, X1^...^Xn^G, R):
 * The predicate first filters the matrix by the template
 * T before computing the aggregate A.
 */
% aggregate_all(+Aggregate, +Template, +Goal, -Value)
:- public aggregate_all/4.
:- meta_predicate aggregate_all(?,?,0,?).
aggregate_all(A, T, G, R) :-
   findall(T, G, H),
   sort(H, L),
   aggregate_all(A, member(T, L), R).

/**
 * aggregate(A, X1^...^Xn^G, R):
 * The predicate determines all the solutions to the matrix G,
 * whereby computing the aggregate A of the solutions grouped
 * by the witnesses. The predicate then repeatly succeeds by
 * unifying the witnesses and when R unifies with the result.
 */
% aggregate(+Aggregate, +QuantGoal, -Result)
:- public aggregate/3.
:- meta_predicate aggregate(?,0,?).
aggregate(A, G, R) :-
   sys_make_accu(A, T, C),
   bagof(T, G, L),
   call(C, L, R).

/**
 * aggregate(A, T, X1^...^Xn^G, R):
 * The predicate first filters the matrix by the template
 * T before computing the aggregate A for each witness.
 */
% aggregate(+Aggregate, +Template, +Goal, -Value)
:- public aggregate/4.
:- meta_predicate aggregate(?,?,0,?).
aggregate(A, T, G, R) :-
   setof(T, G, L),
   aggregate_all(A, member(T, L), R).

/**
 * sys_collect(A, X1^...^Xn^G, R):
 * The predicate determines the same results as the predicate
 * aggregate/3. But the results are grouped by the witnesses
 * instead of sorted by the witnesses.
 */
% sys_collect(+Aggregate, +QuantGoal, -Result)
:- public sys_collect/3.
:- meta_predicate sys_collect(?,0,?).
sys_collect(A, G, R) :-
   sys_make_accu(A, T, C),
   sys_heapof(T, G, L),
   call(C, L, R).

/**
 * sys_collect(A, T, X1^...^Xn^G, R):
 * The predicate first filters the matrix by the template
 * T before computing the aggregate A for each witness.
 */
% sys_collect(+Aggregate, +Template, +Goal, -Value)
:- public sys_collect/4.
:- meta_predicate sys_collect(?,?,0,?).
sys_collect(A, T, G, R) :-
   sys_heapof(T, G, H),
   sys_distinct(H, L),
   aggregate_all(A, member(T, L), R).

/***************************************************************/
/* The Aggregation Closure                                     */
/***************************************************************/

% sys_make_accu(+Aggregate, -Template, -Closure)
:- meta_predicate sys_make_accu(?,?,2).
sys_make_accu(X, _, _) :-
   var(X),
   throw(error(instantiation_error,_)).
sys_make_accu(count, hit, length) :- !.
sys_make_accu(sum(X), X, sys_accu_sum) :- !.
sys_make_accu(min(X), X, sys_accu_min) :- !.
sys_make_accu(max(X), X, sys_accu_max) :- !.
sys_make_accu(bag(X), X, sys_accu_null) :- !.
sys_make_accu(set(X), X, sort) :- !.
sys_make_accu(nodup(X), X, sys_distinct) :- !.
sys_make_accu((X,Y), (Z,T), sys_accu_comma(A, B)) :- !,
   sys_make_accu(X, Z, A),
   sys_make_accu(Y, T, B).
sys_make_accu(X, _, _) :-
   callable(X), !,
   functor(X, F, N),
   throw(error(domain_error(aggregate,F/N),_)).
sys_make_accu(X, _, _) :-
   throw(error(type_error(callable,X),_)).

% sys_accu_sum(+List, -Value)
:- private sys_accu_sum/2.
sys_accu_sum([], 0).
sys_accu_sum([X|Y], Z) :-
   sys_accu_sum(Y, H),
   Z is H+X.

% sys_accu_min(+List, -Value)
:- private sys_accu_min/2.
sys_accu_min([X|Y], Z) :-
   sys_accu_min2(Y, X, Z).

% sys_accu_min2(+List, +Value, -Value)
:- private sys_accu_min2/3.
sys_accu_min2([], X, X).
sys_accu_min2([Y|T], X, Z) :-
   sys_accu_min2(T, Y, H),
   Z is min(H,X).

% sys_accu_max(+List, -Value)
:- private sys_accu_max/2.
sys_accu_max([X|Y], Z) :-
   sys_accu_max2(Y, X, Z).

% sys_accu_max2(+List, +Value, -Value)
:- private sys_accu_max2/3.
sys_accu_max2([], X, X).
sys_accu_max2([Y|T], X, Z) :-
   sys_accu_max2(T, Y, H),
   Z is max(H,X).

% sys_accu_null(+List, -Value)
:- private sys_accu_null/2.
sys_accu_null(X, X).

% sys_accu_comma(+Closure, +Closure, +List, -Value)
:- private sys_accu_comma/4.
:- meta_predicate sys_accu_comma(2,2,?,?).
sys_accu_comma(A, B, L, (Z,T)) :-
   sys_list_first(L, X),
   call(A, X, Z),
   sys_list_second(L, Y),
   call(B, Y, T).

% sys_list_first(+List, -List)
:- private sys_list_first/2.
sys_list_first([], []).
sys_list_first([(X,_)|Z], [X|S]) :-
   sys_list_first(Z, S).

% sys_list_second(+List, -List)
:- private sys_list_second/2.
sys_list_second([], []).
sys_list_second([(_,Y)|Z], [Y|T]) :-
   sys_list_second(Z, T).

/**
 * foreach(G, F):
 * Calls the conjunction of those instances of F where G succeeds. Variables
 * occuring in F and not occuring in G are shared across the conjunction
 * and across the context.
 */
% foreach(+Generator, +Goal)
:- public foreach/2.
:- meta_predicate foreach(0,0).
foreach(G, F) :-
   sys_goal_kernel(F, B),
   sys_goal_globals(G^F, W),
   findall(W-B, G, H),
   sys_join_keys(H, W),
   sys_call_values(H).

% sys_join_keys(+Pairs, +Key)
:- private sys_join_keys/2.
sys_join_keys([], _).
sys_join_keys([K-_|L], K) :-
   sys_join_keys(L, K).

% sys_call_values(+Pairs)
:- private sys_call_values/1.
sys_call_values([]).
sys_call_values([_-V|L]) :-
   call(V),
   sys_call_values(L).
