/**
 * The aggregate predicates take a set of solutions and compute an
 * aggregate on it. The predicate aggregate_all/3 aggregates the
 * solution that is produced by findall/3. The predicate aggregate/3
 * respectively sys_collect/3 aggregates the solutions that are
 * produced by bagof/3 respectively sys_heapof/3.
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
:- use_package(foreign(matula/util/data)).
:- use_package(foreign(jekpro/tools/call)).
:- use_package(foreign(jekpro/tools/term)).

:- module(aggregate, []).
:- use_module(library(advanced/sequence)).
:- use_module(library(basic/lists)).

/**
 * aggregate_all(A, G, S):
 * The predicates aggregates the aggregate A for the solutions of G and
 * unifies the result with S. The following aggregates are recognized:
 *
 *   count:		    The result is the number of solutions.
 *   sum(X):	    The result is the sum of the X values.
 *   mul(X):	    The result is the product of the X values.
 *   min(X):	    The result is the minimum of the X values.
 *   max(X):	    The result is the maximum of the X values.
 *   (X,Y):		    The result is the aggregate X paired by the aggregate Y.
 *   nil:           The result is always nil.
 *   first(C,X):    The result is the C first of the X values.
 *   last(C,X):     The result is the C last of the X values.
 *   reduce(I,A,X): The result is the I and A reduct of the X values.
 */
% aggregate_all(+Aggregate, +Goal, -Value)
:- public aggregate_all/3.
:- meta_predicate aggregate_all(?, 0, ?).
aggregate_all(A, G, S) :-
   pivot_new(P),
   aggregate_all2(A, G, P),
   pivot_get_default(P, A, H),
   S = H.

% aggregate_all2(+Aggregate, +Goal, +Pivot)
:- private aggregate_all2/3.
:- meta_predicate aggregate_all2(?, 0, ?).
aggregate_all2(A, G, P) :- G,
   pivot_get_default(P, A, H),
   next_state(A, H, J),
   pivot_set(P, J), fail.
aggregate_all2(_, _, _).

/**
 * aggregate(A, X1^…^Xn^G, S):
 * The predicates aggregates the aggregate A for the solutions of G and
 * unifies the result with S. The result is sorted by the witnesses.
 */
% aggregate(+Aggregate, +QuantGoal, -Value)
:- public aggregate/3.
:- meta_predicate aggregate(?, 0, ?).
aggregate(A, G, S) :-
   sys_goal_globals(A^G, W),
   sys_revolve_tree(A, G, W, P),
   sys_revolve_list(W, P, S).

% aggregate2(+Vars, +Aggregate, +Goal, +Revolve)
:- private aggregate2/4.
:- meta_predicate aggregate2(?, ?, 0, ?).
aggregate2(W, A, G, R) :- G,
   revolve_lookup(R, W, P),
   pivot_get_default(P, A, H),
   next_state(A, H, J),
   pivot_set(P, J), fail.
aggregate2(_, _, _, _).

/**
 * sys_collect(A, X1^…^Xn^G, S):
 * The predicates aggregates the aggregate A for the solutions of G and
 * unifies the result with S. The result is grouped by the witnesses.
 */
% sys_collect(+Aggregate, +QuantGoal, -Value)
:- public sys_collect/3.
:- meta_predicate sys_collect(?, 0, ?).
sys_collect(A, G, S) :-
   sys_goal_globals(A^G, W),
   sys_revolve_hash(A, G, W, P),
   sys_revolve_list(W, P, S).

/*************************************************************/
/* Revolve Helper                                            */
/*************************************************************/

% sys_revolve_tree(+Aggregate, +QuantGoal, +List, -Ref)
:- meta_predicate sys_revolve_tree(?, 0, ?, ?).
sys_revolve_tree(A, G, [], P) :- !,
   sys_goal_kernel(G, B),
   pivot_new(P),
   aggregate_all2(A, B, P).
sys_revolve_tree(A, G, W, R) :-
   sys_goal_kernel(G, B),
   variant_comparator(C),
   revolve_new(C, R),
   aggregate2(W, A, B, R).

% sys_revolve_hash(+Aggregate, +QuantGoal, +List, -Ref)
:- meta_predicate sys_revolve_hash(?, 0, ?, ?).
sys_revolve_hash(A, G, [], P) :- !,
   sys_goal_kernel(G, B),
   pivot_new(P),
   aggregate_all2(A, B, P).
sys_revolve_hash(A, G, W, R) :-
   sys_goal_kernel(G, B),
   revolve_new(R),
   aggregate2(W, A, B, R).

% sys_revolve_list(+List, +Ref, -Value)
sys_revolve_list([], P, S) :- !,
   pivot_get(P, S).
sys_revolve_list(W, R, S) :-
   revolve_pair(R, W-Q),
   pivot_get(Q, S).

/*************************************************************/
/* Aggregate State                                           */
/*************************************************************/

/**
 * pivot_get_default(P, A, H):
 * The predicate succeeds in H with the value of the pivot P
 * or the initial state for the aggregate A.
 */
% pivot_get_default(+Pivot, +Aggregate, -Value)
:- private pivot_get_default/3.
pivot_get_default(P, _, H) :-
   pivot_get(P, H), !.
pivot_get_default(_, A, H) :-
   init_state(A, H).

/**
 * init_state(A, H):
 * The predicate succeeds in H with the initial state for the aggregate A.
 */
% init_state(+Aggregate, -Value)
:- private init_state/2.
init_state(X, _) :-
   var(X),
   throw(error(instantiation_error, _)).
init_state(count, 0).
init_state(sum(_), 0).
init_state(mul(_), 1).
init_state(min(_), sup).
init_state(max(_), inf).
init_state((  A, B), (  S, T)) :-
   init_state(A, S),
   init_state(B, T).
init_state(nil, nil).
init_state(first(_, _), sup).
init_state(last(_, _), inf).
init_state(reduce(I, _, _), I).

/**
 * next_state(A, H, J):
 * The predicate succeeds in J with the next state for the
 * aggregate A after the state H.
 */
% next_state(+Aggregate, +Value, -Value)
:- private next_state/3.
next_state(X, _, _) :-
   var(X),
   throw(error(instantiation_error, _)).
next_state(count, S, T) :-
   T is S+1.
next_state(sum(X), S, T) :-
   T is S+X.
next_state(mul(X), S, T) :-
   T is S*X.
next_state(min(X), sup, X) :- !.
next_state(min(X), S, T) :-
   T is min(S, X).
next_state(max(X), inf, X) :- !.
next_state(max(X), S, T) :-
   T is max(S, X).
next_state((  S, T), (  A, B), (  U, V)) :-
   next_state(S, A, U),
   next_state(T, B, V).
next_state(nil, nil, nil).
next_state(first(_, X), sup, X) :- !.
next_state(first(C, X), S, X) :-
   call(C, X, S), !.
next_state(first(_, _), S, S).
next_state(last(_, X), inf, X) :- !.
next_state(last(C, X), S, X) :-
   call(C, S, X), !.
next_state(last(_, _), S, S).
next_state(reduce(_, A, X), S, Y) :-
   call(A, S, X, Y).

/*************************************************************/
/* Revolve Datatype                                          */
/*************************************************************/

/**
 * revolve_new(R):
 * Thre predicate succeeds in R with a new revolve.
 */
% revolve_new(-Revolve)
:- private revolve_new/1.
:- foreign_constructor(revolve_new/1, 'MapHashLink', new).

/**
 * revolve_new(C, R):
 * The predicate succeeds in R with a new revolve for the comparator C.
 */
% revolve_new(+Comparator, -Revolve)
:- private revolve_new/2.
:- foreign_constructor(revolve_new/2, 'MapTree', new(java/util/'Comparator')).

/**
 * revolve_lookup(R, K, P):
 * The predicate succeeds in P with the old or new pivot
 * for a copy of the key K in the revolve R.
 */
% revolve_lookup(+Revolve, +Term, -Pivot)
:- private revolve_lookup/3.
:- foreign(revolve_lookup/3, 'ForeignAggregate', 
      sysRevolveLookup('Interpreter', 'AbstractMap', 'Object')).

/**
 * revolve_pair(R, U):
 * The predicate succeeds in U with the key value pairs of the revolve R.
 */
% revolve_pair(+Revolve, +Pair)
:- private revolve_pair/2.
:- foreign(revolve_pair/2, 'ForeignAggregate', 
      sysRevolvePair('CallOut', 'AbstractMap')).

/**
 * variant_comparator(C):
 * The predicate succeeds in C with the variant comparator.
 */
% variant_comparator(-Comparator)
:- private variant_comparator/1.
:- foreign(variant_comparator/1, 'ForeignAggregate', sysVariantComparator).


