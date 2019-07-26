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

:- module(tabling, []).
:- use_module(library(advanced/sequence)).
:- use_module(library(advanced/aggregate)).
:- use_module(library(basic/lists)).

:- public prefix(table).
:- op(1150, fx, table).

/**
 * table P, ..:
 * The predicate stes the predicate P to table.
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

% sys_table(+Indicator)
:- private sys_table/1.
sys_table(X) :-
   var(X),
   throw(error(instantiation_error,_)).
sys_table(F/N) :-
   assertz(sys_tabled(F, N)),
   atom_concat(F, '_m', G),
   sys_replace_site(H, F, G),
   thread_local(H/2),
   Test =.. [H,P,R],
   length(L, N),
   Head =.. [F|L],
   sys_table_head(Head, Call),
   assertz((Head :-
              sys_goal_globals(nil^Call, W),
              pivot_new(P),
              pivot_set(P, Head),
              (  Test -> true
              ;  sys_table_make(nil, Call, W, R),
                 assertz(Test)),
              sys_table_list(W, R, nil))).

% sys_tabled(-Name, -Integer)
:- dynamic sys_tabled/2.
:- multifile sys_tabled/2.

/*************************************************************/
/* Tablling Helper                                           */
/*************************************************************/

% sys_table_make(+Aggregate, +QuantGoal, +List, -Ref)
:- private sys_table_make/4.
:- meta_predicate sys_table_make(?,0,?,?).
sys_table_make(A, G, [], P) :- !,
   sys_goal_kernel(G, B),
   pivot_new(P),
   aggregate_all2(A, B, P).
sys_table_make(A, G, W, R) :-
   sys_goal_kernel(G, B),
%   variant_comparator(C),
%   revolve_new(C, R),
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



