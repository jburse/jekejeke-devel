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

:- package(library(jekpro/frequent/advanced)).

:- module(aggregate, []).
:- use_module(library(advanced/micro)).
:- use_module(library(basic/lists)).

/**
 * aggregate_all(A, G, S):
 * The predicates aggregates the aggregate A for the solutions of G and
 * unifies the result with S. The following aggregates are recognized:
 *
 *   count:		The result is the number of solutions.
 *   sum(X):	The result is the sum of the X values.
 *   mul(X):	The result is the product of the X values.
 *   min(X):	The result is the minimum of the X values.
 *   max(X):	The result is the maximum of the X values.
 *   (X,Y):		The result is the aggregate X paired by the aggregate Y.
 */
% aggregate_all(+Aggregate, +Goal, -Value)
:- public aggregate_all/3.
:- meta_predicate aggregate_all(?,0,?).
aggregate_all(A, G, S) :-
   init_state(A, I),
   pivot_new(P),
   pivot_set(P, I),
   aggregate_all2(A, G, P),
   pivot_get(P, S).

% aggregate_all2(+Aggregate, +Goal, +Pivot)
:- private aggregate_all2/3.
:- meta_predicate aggregate_all2(?,0,?).
aggregate_all2(A, G, P) :- G,
   pivot_get(P, H),
   next_state(H, A, J),
   pivot_set(P, J), fail.
aggregate_all2(_, _, _).

/**
 * aggregate(A, X1^…^Xn^G, S):
 * The predicates aggregates the aggregate A for the solutions of G and
 * unifies the result with S. The result is grouped by the witnesses.
 */
% aggregate(+Aggregate, +QuantGoal, -Value)
:- public aggregate/3.
:- meta_predicate aggregate(?,0,?).
aggregate(A, G, S) :-
   sys_goal_globals(A^G, W),
   sys_goal_kernel(G, B),
   revolve_new(R),
   aggregate2(W, A, B, R),
   revolve_pair(R, W-Q),
   pivot_get(Q, S).

% aggregate(+Vars, +Aggregate, +Goal, +Revolve)
:- private aggregate2/4.
:- meta_predicate aggregate2(?,?,0,?).
aggregate2(W, A, B, R) :- B,
   revolve_lookup(R, W, P),
   pivot_get_default(P, A, H),
   next_state(H, A, J),
   pivot_set(P, J), fail.
aggregate2(_, _, _, _).

% pivot_get_default(+Pivot, +Aggregate, -Value)
:- private pivot_get_default/3.
pivot_get_default(P, _, H) :-
   pivot_get(P, H), !.
pivot_get_default(_, A, H) :-
   init_state(A, H).

% init_state(+Aggregate, -Value)
:- private init_state/2.
init_state(X, _) :-
   var(X),
   throw(error(instantiation_error,_)).
init_state(count, 0) :- !.
init_state(sum(_), 0) :- !.
init_state(mul(_), 1) :- !.
init_state(min(_), null) :- !.
init_state(max(_), null) :- !.
init_state((A,B), (S,T)) :- !,
   init_state(A, S),
   init_state(B, T).
init_state(X, _) :-
   callable(X), !,
   functor(X, F, N),
   throw(error(domain_error(aggregate,F/N),_)).
init_state(X, _) :-
   throw(error(type_error(callable,X),_)).

% next_state(+Value, +Aggregate, -Value)
:- private next_state/3.
next_state(S, count, T) :-
   T is S+1.
next_state(S, sum(X), T) :-
   T is S+X.
next_state(S, mul(X), T) :-
   T is S*X.
next_state(S, min(X), T) :-
   S = null
-> T = X
;  T is min(S,X).
next_state(S, max(X), T) :-
   S = null
-> T = X
;  T is max(S,X).
next_state((S,T), (A,B), (U,V)) :-
   next_state(S, A, U),
   next_state(T, B, V).
