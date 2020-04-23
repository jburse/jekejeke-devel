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
:- use_module(library(advanced/sequence)).
:- use_module(library(basic/lists)).

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
   pivot_new(P),
   (sys_aggregate_all(A, G, P, _), fail; true),
   pivot_get_default(P, A, H),
   S = H.

% aggregate_all(+Aggregate, +Goal, -Value, +List)
:- public aggregate_all/4.
:- meta_predicate aggregate_all(?, 0, ?, ?).
aggregate_all(A, G, S, O) :-
   pivot_new(P),
   sys_variant_comparator(O, C),
   (  variant_eager(C)
   -> sys_aggregate_all(A, G, P, J),
      S = J
   ;  (sys_aggregate_all(A, G, P, _), fail; true),
      pivot_get_default(P, A, H),
      S = H).

% sys_aggregate_all(+Aggregate, +Goal, +Pivot, -Value)
:- private sys_aggregate_all/4.
:- meta_predicate sys_aggregate_all(?, 0, ?, ?).
sys_aggregate_all(A, G, P, J) :-
   G,
   pivot_get_default(P, A, H),
   next_state(A, H, J),
   \+ pivot_get(P, J),
   pivot_set(P, J).

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
   sys_revolve_make(W, P),
   (sys_revolve_run(A, Goal, W, P, _), fail; true),
   sys_revolve_list(W, P, S).

% aggregate(+Aggregate, +QuantGoal, -Value, +List)
:- public aggregate/4.
:- meta_predicate aggregate(?, 0, ?, ?).
aggregate(A, Goal, S, O) :-
   sys_goal_globals(A^Goal, W),
   sys_variant_comparator(O, C),
   (  variant_natural(C) -> sys_revolve_make(W, P)
   ;  sys_revolve_make(W, P, C)),
   (  variant_eager(C)
   -> sys_revolve_run(A, Goal, W, P, J),
      S = J
   ;  (sys_revolve_run(A, Goal, W, P, _), fail; true),
      (  variant_reverse(C) -> sys_revolve_list(W, P, S, C)
      ;  sys_revolve_list(W, P, S))).

% sys_aggregate(+Vars, +Aggregate, +Goal, +Revolve, -Value)
:- private sys_aggregate/5.
:- meta_predicate sys_aggregate(?, ?, 0, ?, ?).
sys_aggregate(W, A, G, R, J) :-
   G,
   sys_revolve_lookup(R, W, P),
   pivot_get_default(P, A, H),
   next_state(A, H, J),
   \+ pivot_get(P, J),
   pivot_set(P, J).

% sys_revolve_run(+Aggregate, +Goal, +List, +Ref, -Value)
:- meta_predicate sys_revolve_run(?, 0, ?, ?, ?).
sys_revolve_run(A, Goal, [], P, J) :- !,
   sys_goal_kernel(Goal, B),
   sys_aggregate_all(A, B, P, J).
sys_revolve_run(A, Goal, W, R, J) :-
   sys_goal_kernel(Goal, B),
   sys_aggregate(W, A, B, R, J).

/*************************************************************/
/* Revolve Helper                                            */
/*************************************************************/

% sys_revolve_make(+List, -Ref)
sys_revolve_make([], P) :- !,
   pivot_new(P).
sys_revolve_make(_, R) :-
   sys_revolve_new(R).

% sys_revolve_make(+List, -Ref, +Comparator)
sys_revolve_make([], P, _) :- !,
   pivot_new(P).
sys_revolve_make(_, R, C) :-
   sys_revolve_new(C, R).

% sys_revolve_list(+List, +Ref, -Value)
sys_revolve_list([], P, S) :- !,
   pivot_get(P, S).
sys_revolve_list(W, R, S) :-
   sys_revolve_pair(R, W-Q),
   pivot_get(Q, S).

% sys_revolve_list(+List, +Ref, -Value, +Comparator)
sys_revolve_list([], P, S, _) :- !,
   pivot_get(P, S).
sys_revolve_list(W, R, S, C) :-
   sys_revolve_pair(R, C, W-Q),
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
init_state(X, _) :- var(X),
   throw(error(instantiation_error, _)).
init_state(count, 0).
init_state(sum(_), 0).
init_state(mul(_), 1).
init_state(min(_), sup).
init_state(max(_), inf).
init_state((A, B), (S, T)) :-
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
next_state(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
next_state(count, S, T) :- T is S+1.
next_state(sum(X), S, T) :- T is S+X.
next_state(mul(X), S, T) :- T is S*X.
next_state(min(X), sup, X) :- !.
next_state(min(X), S, T) :- T is min(S, X).
next_state(max(X), inf, X) :- !.
next_state(max(X), S, T) :- T is max(S, X).
next_state((S, T), (A, B), (U, V)) :-
   next_state(S, A, U),
   next_state(T, B, V).
next_state(nil, nil, nil).
next_state(first(_, X), sup, X) :- !.
next_state(first(C, X), S, X) :- call(C, X, S), !.
next_state(first(_, _), S, S).
next_state(last(_, X), inf, X) :- !.
next_state(last(C, X), S, X) :- call(C, S, X), !.
next_state(last(_, _), S, S).
next_state(reduce(_, A, X), S, Y) :- call(A, S, X, Y).

/*************************************************************/
/* Revolve Datatype                                          */
/*************************************************************/

/**
 * variant_eager(C):
 * The predicate succeeds if the variant comparator is eager.
 */
% variant_eager(+Comparator)
:- foreign(variant_eager/1, 'ForeignAggregate',
      sysVariantEager('AbstractLexical')).

/**
 * variant_reverse(C):
 * The predicate succeeds if the variant comparator is reverse.
 */
% variant_reverse(+Comparator)
:- foreign(variant_reverse/1, 'ForeignAggregate',
      sysVariantReverse('AbstractLexical')).

/**
 * variant_natural(C):
 * The predicate succeeds if the variant comparator is natural.
 */
% variant_natural(+Comparator)
:- foreign(variant_natural/1, 'ForeignAggregate',
      sysVariantNatural('AbstractLexical')).
