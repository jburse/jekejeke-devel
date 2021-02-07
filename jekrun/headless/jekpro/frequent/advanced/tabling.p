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
 * they subsume, so that specializations result in new variant keys.
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
:- use_package(foreign(jekpro/frequent/advanced)).
:- use_package(foreign(jekpro/reference/structure)).
:- use_package(foreign(matula/util/data)).
:- use_package(foreign(jekpro/tools/call)).
:- use_module(library(advanced/variant)).
:- use_module(library(advanced/pivot)).
:- use_module(library(advanced/revolve)).
:- use_module(library(advanced/aggregate)).
:- use_module(library(basic/lists)).
:- use_module(library(experiment/ref)).
:- use_module(library(misc/lock)).
:- use_module(library(runtime/quali)).
:- use_module(library(advanced/abstract)).

:- public prefix(table).
:- op(1150, fx, table).

:- public infix(as).
:- op(700, xfx, as).

/**
 * table P, ..:
 * table (P, ..) as O:
 * The predicate sets the predicate P to tabled. The predicate can be
 * specified via a predicate indicator or a callable. The result is
 * grouped by the witnesses. The as/2 construct can be used to specify
 * sort options. For a list of tabling functions when a callable is
 * specified see the API documentation. The sort options are the same
 * as for aggregate/4 and friends, for additions see the API documentation.
 */
% table(+IndicatorOrCallable)
:- public (table)/1.
table P :-
   sys_variant_comparator([], C),
   sys_table_dire(P, C).

% sys_table_dire(+IndicatorOrCallable, +Comparator)
:- private sys_table_dire/2.
sys_table_dire(P as O, _) :- !,
   sys_variant_comparator(O, C), sys_table_dire(P, C).
sys_table_dire([P|Q], C) :- !,
   sys_table_def(P, C), sys_table_dire(Q, C).
sys_table_dire((P, Q), C) :- !,
   sys_table_def(P, C), sys_table_dire(Q, C).
sys_table_dire([], _) :- !.
sys_table_dire(P, C) :- sys_table_def(P, C).

% sys_table_def(+IndicatorOrCallable, +Comparator)
:- private sys_table_def/2.
sys_table_def(I, C) :- sys_indicator(I), !,
   sys_make_indicator(F, N, I),
   sys_table_declare(F, N, C),
   length(L, N),
   sys_table_wrapper(F, L, L, nil, nil, C),
   sys_table_mode(F, N).
sys_table_def(M, C) :-
   callable(M),
   functor(M, F, N),
   sys_table_declare(F, N, C),
   length(L, N),
   M =.. [_|R],
   sys_table_aggregate(R, L, T, A, S),
   sys_table_wrapper(F, T, L, A, S, C),
   sys_table_mode(F, N).

/*****************************************************************/
/* Predicate Properties                                          */
/*****************************************************************/

/**
 * sys_table_declare(F, N, C):
 * The predicate sets the predicate properties of the tabling cache
 * and if needed the tabling lock, based on the tabled predicate.
 */
% sys_table_declare(+Atom, +Integer, +Comparator)
:- private sys_table_declare/3.
sys_table_declare(F, N, C) :-
   sys_variant_shared(C), !,
   sys_make_indicator(F, N, I),
   sys_table_cache(F, N, M),
   sys_make_indicator(M, 2, J),
   sys_table_props(I, J),
   (sys_variant_dynamic(C) -> dynamic(J); group_local(J)),
   set_predicate_property(J, automatic),
   sys_table_lock(F, N, L),
   sys_make_indicator(L, 2, K),
   sys_table_props(I, K),
   (sys_variant_dynamic(C) -> dynamic(K); group_local(K)),
   set_predicate_property(K, automatic),
   static(I),
   set_predicate_property(I, sys_tabled),
   set_predicate_property(I, automatic).
sys_table_declare(F, N, _) :-
   sys_make_indicator(F, N, I),
   sys_table_cache(F, N, M),
   sys_make_indicator(M, 2, J),
   sys_table_props(I, J),
   thread_local(J),
   set_predicate_property(J, automatic),
   static(I),
   set_predicate_property(I, sys_tabled),
   set_predicate_property(I, automatic).

/**
 * sys_table_mode(F, N):
 * The predicate sets the predicate properties of the aux predicate
 * based on the tabled predicate.
 */
% sys_table_mode(+Atom, +Integer)
:- private sys_table_mode/2.
sys_table_mode(F, N) :-
   sys_make_indicator(F, N, I),
   sys_table_aux(F, G),
   sys_make_indicator(G, N, J),
   sys_table_props(I, J),
   static(J).

/**
 * sys_table_more(F, N):
 * The predicate sets the predicate properties of the help predicate
 * based on the tabled predicate.
 */
% sys_table_more(+Atom, +Integer)
:- private sys_table_more/2.
sys_table_more(F, N) :-
   sys_make_indicator(F, N, I),
   sys_table_help(F, G),
   M is N+2,
   sys_make_indicator(G, M, J),
   sys_table_props(I, J),
   static(J).

/**
 * sys_table_props(I, J):
 * The predicate set the visibility of the predicate J to the
 * visibility of the predicate I.
 */
% sys_table_props(+Indicator, +Indicator)
:- private sys_table_props/2.
sys_table_props(I, J) :-
   (  predicate_property(I, visible(public)) -> public(J)
   ;  predicate_property(I, visible(private)) -> private(J)
   ;  true),
   (  predicate_property(I, multifile) -> multifile(J)
   ;  true),
   sys_make_indicator(F, _, I),
   callable_property(F, sys_context(C)),
   (  predicate_property(I, override(C)) -> override(J)
   ;  true),
   sys_notrace(J).

/*****************************************************************/
/* Aggregate Helper                                              */
/*****************************************************************/

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

/*****************************************************************/
/* Wrapper Helper                                                */
/*****************************************************************/

% sys_table_wrapper(+Atom, +Term, +Goal, +Aggregate, +Value, +Comparator)
:- private sys_table_wrapper/6.
sys_table_wrapper(F, T, L, A, S, C) :-
   sys_variant_shared(C), !,
   length(T, N),
   sys_table_lock(F, N, V),
   Find =.. [V, P, E],
   sys_make_indicator(V, 2, J),
   sys_table_call(F, T, L, A, S, C, Call, P, W),
   sys_table_help(F, G),
   SubHead =.. [G, P, W|T],
   sys_make_indicator(F, N, I),
   (  predicate_property(I, multifile)
   -> sys_compilez((SubHead :- !, Call))
   ;  sys_compilez((SubHead :- Call))),
   sys_table_more(F, N),
   Descr =.. [''|L],
   Key =.. [''|T],
   sys_table_cache(F, N, M),
   Test =.. [M, P, R],
   sys_table_list(C, W, R, S, List),
   Body = (sys_goal_globals(A^Descr, W),
      sys_variant_key(P),
      sys_pivot_set(P, Key),
      (  Test -> List
      ;  sys_find_lock(Find, J, E),
         with_lock(E, SubHead))),
   Head =.. [F|T],
   (  predicate_property(I, multifile)
   -> sys_compilez((Head :- !, Body))
   ;  sys_compilez((Head :- Body))).
sys_table_wrapper(F, T, L, A, S, C) :-
   length(T, N),
   sys_table_call(F, T, L, A, S, C, Call, P, W),
   sys_make_indicator(F, N, I),
   Descr =.. [''|L],
   Key =.. [''|T],
   Body = (sys_goal_globals(A^Descr, W),
      sys_variant_key(P),
      sys_pivot_set(P, Key),
      Call),
   Head =.. [F|T],
   (  predicate_property(I, multifile)
   -> sys_compilez((Head :- !, Body))
   ;  sys_compilez((Head :- Body))).

/**
 * sys_variant_shared(C):
 * The predicate succeeds if the variant comparator is shared.
 */
:- private sys_variant_shared/1.
sys_variant_shared(C) :-
   sys_variant_dynamic(C), !.
sys_variant_shared(C) :-
   sys_variant_group_local(C).

% sys_table_call(+Atom, +Term, +Goal, +Aggregate, +Value, +Comparator, -Goal, -Var, -Var)
:- private sys_table_call/9.
sys_table_call(F, T, L, A, S, C, Call, P, W) :-
   sys_variant_eager(C), !,
   length(T, N),
   sys_table_aux(F, G),
   Goal =.. [G|L],
   sys_table_cache(F, N, M),
   Test =.. [M, P, R],
   sys_table_new(C, R, New),
   sys_table_list(C, W, R, S, List),
   Call = (  Test -> List
      ;  New,
         sys_call_info((sys_aggregate(A, Goal, W, R, J), S = J), Res),
         (  Res == det -> assertz(Test)
         ;  Res == fail -> assertz(Test), fail
         ;  true)).
sys_table_call(F, T, L, A, S, C, Call, P, W) :-
   length(T, N),
   sys_table_aux(F, G),
   Goal =.. [G|L],
   sys_table_cache(F, N, M),
   Test =.. [M, P, R],
   sys_table_new(C, R, New),
   sys_table_list(C, W, R, S, List),
   Call = (  Test -> List
      ;  New,
         (sys_aggregate(A, Goal, W, R, _), fail; true),
         assertz(Test),
         List).

/**
 * sys_table_new(C, W, R, G):
 * The predicate succeeds in G with a code snippet to create
 * a pivot or revolve R for the witness W.
 */
% sys_table_new(+Comparator, +Ref, -Goal)
:- private sys_table_new/3.
sys_table_new(C, R, G) :-
   sys_variant_natural(C), !,
   G = sys_revolve_new(R).
sys_table_new(C, R, G) :-
   sys_variant_comparator(L, C),
   G = (sys_variant_comparator(L, D),
      sys_revolve_new(D, R)).

/**
 * sys_table_list(C, W, R, S, G):
 * The predicate succeeds in G with a code snippet to retrieve
 * the pivot or revolve R into the witness W and result S.
 */
% sys_table_list(+Comparator, +List, +Ref, +Value, -Goal)
:- private sys_table_list/5.
sys_table_list(C, W, R, S, G) :-
   sys_variant_reverse(C), !,
   G = (sys_variant_comparator([reverse(true)], D),
      sys_revolve_pair(R, D, W-Q),
      sys_pivot_get(Q, S)).
sys_table_list(_, W, R, S, G) :-
   G = (sys_revolve_pair(R, W-Q),
      sys_pivot_get(Q, S)).

/*****************************************************************/
/* Eager Evaluation                                              */
/*****************************************************************/

% sys_call_info(+Goal, -Atom)
:- private sys_call_info/2.
:- meta_predicate sys_call_info(0, ?).
sys_call_info(G, Res) :-
   current_prolog_flag(sys_choices, X),
   G,
   current_prolog_flag(sys_choices, Y),
   (X == Y, !, Res = det; Res = true).
sys_call_info(_, Res) :-
   Res = fail.

% sys_find_lock(+Clause, +Indicator, +Var)
:- private sys_find_lock/3.
sys_find_lock(Find, _, _) :-
   Find, !.
sys_find_lock(Find, J, R) :-
   predicate_property(J, sys_readwrite_lock(U)),
   get_write(U, V),
   with_lock(V, sys_find_lock(Find, R)).

% sys_find_lock(+Clause, +Indicator)
:- private sys_find_lock/2.
sys_find_lock(Find, _) :-
   Find, !.
sys_find_lock(Find, R) :-
   mutex_new(R),
   assertz(Find).

/*****************************************************************/
/* Table Inspection & Modification                               */
/*****************************************************************/

/**
 * current_table(V, E):
 * The predicate succeeds in V with the cached variant keys
 * and in E with the cached pivot or revolve.
 */
% current_table(-Callable, -Ref)
:- public current_table/2.
% :- meta_predicate current_table(-1,?).
current_table(V, E) :-
   sys_current_table(V, _, E).

/**
 * retract_table(V):
 * The predicate succeeds with and removes the cached
 * variant keys that match V.
 */
% retract_table(-Callable)
:- public retract_table/1.
% :- meta_predicate retract_table(-1).
retract_table(V) :-
   sys_current_table(V, R, _),
   erase_ref(R).

/**
 * retractall_table(V):
 * The predicate succeeds and removes all the cached
 * variant keys that match V.
 */
% retractall_table(+Callable)
:- public retractall_table/1.
% :- meta_predicate retractall_table(-1).
retractall_table(V) :-
   sys_current_table(V, R, _),
   erase_ref(R),
   fail.
retractall_table(_).

% sys_current_table(-Callable, -Ref, -Ref)
:- private sys_current_table/3.
% :- meta_predicate sys_current_table(-1,?,?).
sys_current_table(V, R, E) :-
   callable(V), !,
   functor(V, F, N),
   sys_make_indicator(F, N, I),
   predicate_property(I, sys_tabled),
   sys_current_table(V, F, N, R, E).
sys_current_table(V, R, E) :-
   predicate_property(I, sys_tabled),
   sys_make_indicator(F, N, I),
   sys_current_table(V, F, N, R, E).

% sys_current_table(-Callable, +Atom, +Integer, -Ref, -Ref)
:- private sys_current_table/5.
sys_current_table(V, F, N, R, E) :-
   sys_table_cache(F, N, M),
   Test =.. [M, P, E],
   clause_ref(Test, true, R),
   sys_pivot_get(P, Key),
   Key =.. [_|L],
   V =.. [F|L].
sys_current_table(V, F, N, R, E) :-
   sys_table_lock(F, N, M),
   Find =.. [M, P, E],
   clause_ref(Find, true, R),
   sys_pivot_get(P, Key),
   Key =.. [_|L],
   V =.. [F|L].

/**
 * sys_table_cache(F, A, N):
 * The predicate succeeds in N with the name of the tabling
 * cache for the predicate indicator F/N.
 */
% sys_table_cache(+Atom, -Integer, -Atom)
:- private sys_table_cache/3.
sys_table_cache(K, N, J) :- K = M:F, !,
   sys_table_cache(F, N, I),
   sys_replace_site(J, K, M:I).
sys_table_cache(F, N, H) :-
   atom_number(U, N),
   atom_split(G, '_', [F, U, m]),
   sys_replace_site(H, F, G).

/**
 * sys_table_lock(F, A, N):
 * The predicate succeeds in N with the name of the tabling
 * lock for the predicate indicator F/N.
 */
% sys_table_lock(+Atom, -Integer, -Atom)
:- private sys_table_lock/3.
sys_table_lock(K, N, J) :- K = M:F, !,
   sys_table_lock(F, N, I),
   sys_replace_site(J, K, M:I).
sys_table_lock(F, N, H) :-
   atom_number(U, N),
   atom_split(G, '_', [F, U, s]),
   sys_replace_site(H, F, G).

/*****************************************************************/
/* Term Rewriting                                                */
/*****************************************************************/

% sys_table_head(+Callable, -Callable)
:- private sys_table_head/2.
sys_table_head(G, N) :-
   callable(G),
   functor(G, J, A),
   sys_make_indicator(J, A, I),
   sys_provable_property_chk(I, sys_tabled/0, [sys_tabled]),
   G =.. [K|L],
   sys_table_aux(K, U),
   N =.. [U|L].

/**
 * sys_table_aux(F, N):
 * The predicate succeeds in N with the name of the tabling
 * aux for the predicate name F.
 */
% sys_table_aux(+Atom, -Atom)
:- private sys_table_aux/2.
sys_table_aux(K, J) :- K = M:F, !,
   sys_table_aux(F, I),
   sys_replace_site(J, K, M:I).
sys_table_aux(F, H) :-
   atom_concat(F, '_a', G),
   sys_replace_site(H, F, G).

/**
 * sys_table_help(F, N):
 * The predicate succeeds in N with the name of the tabling
 * help for the predicate name F.
 */
% sys_table_help(+Atom, -Atom)
:- private sys_table_help/2.
sys_table_help(K, J) :- K = M:F, !,
   sys_table_help(F, I),
   sys_replace_site(J, K, M:I).
sys_table_help(F, H) :-
   atom_concat(F, '_h', G),
   sys_replace_site(H, F, G).

% user:term_expansion(+Term, -Term)
:- public user:term_expansion/2.
:- multifile user:term_expansion/2.
:- meta_predicate user:term_expansion(-1, -1).
user:term_expansion(A, _) :- var(A), !, fail.
user:term_expansion(A, B) :- sys_table_head(A, B), !.
