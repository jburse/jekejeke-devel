/**
 * This module enhance aggregates by memorization. The table/1 directive
 * has two effects. First of all a tabled predicate call is materialized
 * into a table by the given aggregate. This means for example that
 * duplicates are removed. Second, the materialized table is memorized
 * so that recurrent calls do not re-evaluate the tabled predicate.
 *
 * Example:
 * :- table concat/3.
 * concat([], X, X).
 * concat([X|Y], Z, [X|T]) :- concat(Y, Z, T).
 * ?- concat(X, Y, [1,2,3]).
 * X = [],
 * Y = [1,2,3] ;
 * X = [1],
 * Y = [2,3]
 *
 * The table/1 directive accepts both a predicate indicators and a callable.
 * If a predicate indicator is specified the given aggregate will be the
 * empty aggregate nil/0. If a callable is specified the arguments of the
 * callable specify the given aggregate. Multiple aggregate specifications
 * will be automatically combined by the aggregate pairing operator (',')/2.
 *
 * Example:
 * :- table path(_,_,min).
 * path(X, X, 0).
 * path(X, Y, N) :-  edge(X, Z), path(Z, Y, M), N is M+1.
 * ?- path(a, e, X).
 * X = 2
 * ?- path(a, e, 2).
 * Yes
 * ?- path(a, e, 1).
 * No
 *
 * The memorization stores the variant keys from the tabled predicate
 * calls. Recursive tabled predicate calls are allowed and when completed
 * extend the memorization store. The memorization store can be queried
 * by the predicate current_table/2. Variant keys are not checked whether
 * they subsume, so that different modes result in new variant keys.
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

:- module(tabling, []).
:- use_module(library(advanced/sequence)).
:- use_module(library(advanced/aggregate)).
:- use_module(library(basic/lists)).

:- public prefix(table).
:- op(1150, fx, table).

/**
 * table P, ..:
 * The predicate sets the predicate P to tabled. The predicate can be
 * specified via a predicate indicator or a callable. The following
 * aggregates are recognized:
 *
 *   _:         The argument is not aggregated.
 *   sum:       The result is the sum of the argument.
 *   mul:       The result is the product of the argument.
 *   min:       The result is the minimum of the argument.
 *   max:       The result is the maximum of the argument.
 */
% public +Indicators
:- public (table)/1.
table [P|Q] :- !,
   sys_table(P),
   table(Q).
table P,Q :- !,
   sys_table(P),
   table(Q).
table [] :- !.
table P :-
   sys_table(P).

% sys_table(+IndicatorOrCallable)
:- private sys_table/1.
sys_table(X) :-
   var(X),
   throw(error(instantiation_error,_)).
sys_table(F/N) :- !,
   sys_table_declare(F, N),
   length(L, N),
   H =.. [F|L],
   sys_table_wrapper(H, nil, nil).
sys_table(C) :-
   functor(C, F, N),
   sys_table_declare(F, N),
   length(L, N),
   C =.. [_|R],
   sys_table_aggregate(R, L, A, S),
   H =.. [F|L],
   sys_table_wrapper(H, A, S).

% sys_table_aggregate(+List, +List, -Aggregate, -Value)
:- private sys_table_aggregate/4.
sys_table_aggregate([], [], nil, nil).
sys_table_aggregate([X|L], [Y|R], (A,P), (S,Q)) :-
   sys_table_spec(X, Y, A, S), !,
   sys_table_aggregate(L, R, P, Q).
sys_table_aggregate([_|L], [_|R], P, Q) :-
   sys_table_aggregate(L, R, P, Q).

% sys_table_spec(+Spec, +Var, -Aggregate, -Value)
:- private sys_table_spec/4.
sys_table_spec(X, _, _, _) :-
   var(X), !, fail.
sys_table_spec(sum, X, sum(X), X).
sys_table_spec(mul, X, mul(X), X).
sys_table_spec(min, X, min(X), X).
sys_table_spec(max, X, max(X), X).

/**
 * current_table(V, R):
 * The predicate succeeds in V with the current variant keys
 * and in R with the current materialized table keys.
 */
% current_table(-Callable, -Ref)
:- public current_table/2.
current_table(V, R) :-
   var(V), !,
   sys_tabled(F, N),
   sys_table_test(F, N, H),
   Test =.. [H,P,R], Test,
   pivot_get(P, V).
current_table(V, R) :-
   functor(V, F, N),
   sys_tabled(F, N),
   sys_table_test(F, N, H),
   Test =.. [H,P,R], Test,
   pivot_get(P, V).

% sys_table_declare(+Atom, +Integer)
:- private sys_table_declare/2.
sys_table_declare(F, N) :-
   assertz(sys_tabled(F, N)),
   sys_table_test(F, N, M),
   thread_local(M/2).

% sys_tabled(-Atom, -Integer)
:- private sys_tabled/2.
:- dynamic sys_tabled/2.

/*************************************************************/
/* Tabling Helper                                            */
/*************************************************************/

% sys_table_wrapper(+Head, +Aggregate, +Value)
:- private sys_table_wrapper/3.
sys_table_wrapper(H, A, S) :-
   functor(H, F, N),
   sys_table_test(F, N, M),
   T =.. [M,P,R],
   sys_table_head(H, G),
   assertz((H :-
              sys_goal_globals(A^G, W),
              pivot_new(P),
              pivot_set(P, H),
              (  T -> true
              ;  sys_table_make(A, G, W, R),
                 assertz(T)),
              sys_table_list(W, R, S))).

% sys_table_test(+Atom, -Integer, -Atom)
:- private sys_table_test/3.
sys_table_test(F, N, H) :-
   atom_number(U, N),
   atom_split(G, '_', [F,U,m]),
   sys_replace_site(H, F, G).

% sys_table_make(+Aggregate, +QuantGoal, +List, -Ref)
:- private sys_table_make/4.
:- meta_predicate sys_table_make(?,0,?,?).
sys_table_make(A, G, [], P) :- !,
   sys_goal_kernel(G, B),
   pivot_new(P),
   aggregate_all2(A, B, P).
sys_table_make(A, G, W, R) :-
   sys_goal_kernel(G, B),
   revolve_new(R),
   aggregate2(W, A, B, R).

% sys_table_list(+List, +Ref, -Value)
:- private sys_table_list/3.
sys_table_list([], P, S) :- !,
   pivot_get(P, S).
sys_table_list(W, R, S) :-
   revolve_pair(R, W-Q),
   pivot_get(Q, S).

/**********************************************************/
/* Term Rewriting                                         */
/**********************************************************/

% sys_table_head(+Callable, -Callable)
:- private sys_table_head/2.
sys_table_head(A, B) :-
   A =.. [F|L],
   length(L, N),
   sys_tabled(F, N),
   atom_concat(F, '_f', G),
   sys_replace_site(H, F, G),
   B =.. [H|L].

% user:term_expansion(+Term, -Term)
:- public user:term_expansion/2.
:- multifile user:term_expansion/2.
:- meta_predicate user:term_expansion(-1,-1).
user:term_expansion(A, _) :-
   var(A), !, fail.
user:term_expansion((A :- _), _) :-
   var(A), !, fail.
user:term_expansion((A :- B), (C :- B)) :-
   sys_table_head(A, C), !.
user:term_expansion(A, B) :-
   sys_table_head(A, B), !.
