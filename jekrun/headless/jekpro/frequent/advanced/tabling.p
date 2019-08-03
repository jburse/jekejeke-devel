/**
 * This module enhances aggregates by memorization. The table/1 directive
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
:- use_module(library(experiment/ref)).

:- public prefix(table).
:- op(1150, fx, table).

:- public prefix(sys_sorter).
:- op(1150, fx, sys_sorter).

/**
 * table P, ..:
 * The predicate sets the predicate P to tabled. The predicate can be
 * specified via a predicate indicator or a callable. The result is
 * grouped by the witnesses. The following aggregates are recognized:
 *
 *   _:            The argument is not aggregated.
 *   sum:          The result is the sum of the argument.
 *   mul:          The result is the product of the argument.
 *   min:          The result is the minimum of the argument.
 *   max:          The result is the maximum of the argument.
 *   first(C):     The result is the C first of the argument.
 *   last(C):      The result is the C last of the argument.
 *   reduce(I,A):  The result is the I and A reduct of the argument.
 */
% table(+Indicators)
:- public (table)/1.
table [P|Q] :- !, sys_table(P, hash), table(Q).
table P, Q :- !, sys_table(P, hash), table(Q).
table [] :- !.
table P :- sys_table(P, hash).

/**
 * sys_sorter P, ..:
 * The predicate sets the predicate P to tabled. The predicate can be
 * specified via a predicate indicator or a callable. The result is
 * sorted by the witnesses.
 */
% sys_sorter(+Indicators)
:- public (sys_sorter)/1.
sys_sorter [P|Q] :- !, sys_table(P, tree), sys_sorter(Q).
sys_sorter P, Q :- !, sys_table(P, tree), sys_sorter(Q).
sys_sorter [] :- !.
sys_sorter P :- sys_table(P, tree).

% sys_table(+IndicatorOrCallable, +Atom)
:- private sys_table/2.
sys_table(I, O) :- sys_is_indicator(I), !,
   sys_table_declare(I),
   sys_make_indicator(F, N, I),
   length(L, N),
   sys_table_wrapper(F, L, L, nil, nil, O).
sys_table(C, O) :-
   sys_callable(C),
   sys_functor(C, F, N),
   sys_make_indicator(F, N, I),
   sys_table_declare(I),
   length(L, N),
   C =.. [_|R],
   sys_table_aggregate(R, L, T, A, S),
   sys_table_wrapper(F, T, L, A, S, O).

/**********************************************************/
/* Aggregate Helper                                       */
/**********************************************************/

% sys_table_aggregate(+List, +List, -List, -Aggregate, -Value)
:- private sys_table_aggregate/5.
sys_table_aggregate([], [], [], nil, nil).
sys_table_aggregate([X|L], [Y|R], [S|T], (A, P), (S, Q)) :-
   sys_table_spec(X, Y, A), !,
   sys_table_aggregate(L, R, T, P, Q).
sys_table_aggregate([_|L], [Y|R], [Y|T], P, Q) :-
   sys_table_aggregate(L, R, T, P, Q).

% sys_table_spec(+Spec, +Var, -Aggregate)
:- private sys_table_spec/3.
sys_table_spec(X, _, _) :- var(X), !, fail.
sys_table_spec(sum, X, sum(X)).
sys_table_spec(mul, X, mul(X)).
sys_table_spec(min, X, min(X)).
sys_table_spec(max, X, max(X)).
sys_table_spec(first(C), X, first(C, X)).
sys_table_spec(last(C), X, last(C, X)).
sys_table_spec(reduce(I, A), X, reduce(I, A, X)).

/**********************************************************/
/* Wrapper Helper                                         */
/**********************************************************/

% sys_table_declare(+Indicator)
:- private sys_table_declare/1.
sys_table_declare(I) :-
   static(I),
   set_predicate_property(I, sys_tabled),
   sys_make_indicator(F, N, I),
   sys_table_test(F, N, M),
   sys_make_indicator(M, 2, J),
   (predicate_property(I, visible(public)) -> public(J);
    predicate_property(I, visible(private)) -> private(J);
    true),
   (predicate_property(I, multifile) -> multifile(J);
    true),
   thread_local(J).

% sys_table_wrapper(+Atom, +Term, +Goal, +Aggregate, +Value, +Atom)
:- private sys_table_wrapper/6.
sys_table_wrapper(F, T, L, A, S, O) :-
   length(T, N),
   sys_univ(Head, [F|T]),
   sys_table_aux(F, G),
   sys_univ(Goal, [G|L]),
   sys_table_test(F, N, M),
   sys_univ(Test, [M, P, R]),
   sys_table_revolve(O, A, Goal, W, R, Q),
   Key =.. [''|T],
   Descr =.. [''|L],
   sys_make_indicator(F, N, I),
   Body = (sys_goal_globals(A^Descr, W),
           pivot_new(P),
           pivot_set(P, Key),
           (Test -> true;
            Q,
            assertz(Test)),
           sys_revolve_list(W, R, S)),
   (predicate_property(I, multifile) ->
       compilable_ref((Head :- !, Body), K);
    compilable_ref((Head :- Body), K)),
   recordz_ref(K),
   sys_make_indicator(G, N, J),
   (predicate_property(I, visible(public)) -> public(J);
    predicate_property(I, visible(private)) -> private(J);
    true),
   (predicate_property(I, multifile) -> multifile(J);
    true),
   static(J).

% sys_table_revolve(+Atom, +Aggregate, +Goal, +List, +Ref, -Goal)
:- private sys_table_revolve/6.
sys_table_revolve(hash, A, Goal, W, R,
   sys_revolve_hash(A, Goal, W, R)).
sys_table_revolve(tree, A, Goal, W, R,
   sys_revolve_tree(A, Goal, W, R)).

/**********************************************************/
/* Table Inspection                                       */
/**********************************************************/

/**
 * current_table(V, R):
 * The predicate succeeds in V with the current variant keys
 * and in R with the current materialized table keys.
 */
% current_table(-Callable, -Ref)
:- public current_table/2.
current_table(V, R) :-
   sys_callable(V), !,
   sys_functor(V, F, N),
   sys_make_indicator(F, N, I),
   predicate_property(I, sys_tabled),
   sys_table_test(F, N, H),
   sys_univ(Test, [H, P, R]),
   Test,
   pivot_get(P, Key),
   Key =.. [_|L],
   sys_univ(V, [F|L]).
current_table(V, R) :-
   predicate_property(I, sys_tabled),
   sys_make_indicator(F, N, I),
   sys_table_test(F, N, H),
   sys_univ(Test, [H, P, R]),
   Test,
   pivot_get(P, Key),
   Key =.. [_|L],
   sys_univ(V, [F|L]).

% sys_table_test(+Atom, -Integer, -Atom)
:- private sys_table_test/3.
sys_table_test(K, N, J) :- K = M:F, !,
   sys_table_test(F, N, I),
   sys_replace_site(J, K, M:I).
sys_table_test(F, N, H) :-
   atom_number(U, N),
   atom_split(G, '_', [F, U, m]),
   sys_replace_site(H, F, G).

/**********************************************************/
/* Term Rewriting                                         */
/**********************************************************/

% sys_table_head(+Callable, -Callable)
:- private sys_table_head/2.
sys_table_head(G, N) :-
   sys_callable(G),
   sys_functor(G, J, A),
   sys_make_indicator(J, A, I),
   sys_provable_property_chk(I, sys_tabled/0, [sys_tabled]),
   sys_univ(G, [K|L]),
   sys_table_aux(K, U),
   sys_univ(N, [U|L]).

% sys_table_aux(+Atom, -Atom)
:- private sys_table_aux/2.
sys_table_aux(K, J) :- K = M:F, !,
   sys_table_aux(F, I),
   sys_replace_site(J, K, M:I).
sys_table_aux(F, H) :-
   atom_concat(F, '_a', G),
   sys_replace_site(H, F, G).

% user:term_expansion(+Term, -Term)
:- public user:term_expansion/2.
:- multifile user:term_expansion/2.
:- meta_predicate user:term_expansion(-1, -1).
user:term_expansion(A, _) :- var(A), !, fail.
user:term_expansion((A :- _), _) :- var(A), !, fail.
user:term_expansion((A :- B), (C :- B)) :- sys_table_head(A, C), !.
user:term_expansion(A, B) :- sys_table_head(A, B), !.
