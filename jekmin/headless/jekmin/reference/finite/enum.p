/**
 * The constant values in the domain of a variable can be enumerated
 * via the predicate indo-main/1. This predicate is capable of
 * enumerating finite and infinite domains. For infinite domains it
 * is also possible to enumerate domains that are open ended on both
 * sides, resulting in an alternating enumeration towards inf and sup:
 *
 * Examples:
 * ?- X in 10..15, indomain(X).
 * X = 10 ;
 * ...
 * X = 15
 * ?- indomain(X).
 * X = 0 ;
 * X = -1 ;
 * ...
 *
 * As a convenience the finite domain solver provides a couple
 * of solving techniques. We provide the following solving techniques
 * along domain ranges when there is an attempt to label multiple
 * variables at once. The predicate for this search is label/1:
 *
 * * Brute Infinite Search
 * * Heuristic Finite Search
 *
 * Infinite domains are filtered out first and then cantor paired. For
 * finite domains we have im-plemented a search strategy, which
 * prefers those variables with a smaller cardinality of the domain
 * first. In certain cases this can reduce the search space. Further
 * notions of consistency and search are discussed in [2].
 *
 * Examples:
 * ?- [X,Y] ins 0..9, 3*X+5*Y #= 11, label([X,Y]).
 * X = 2,
 * Y = 1 ;
 * No
 * ?- 3*X+5*Y #= 11, label([X,Y]).
 * X = 2,
 * Y = 1 ;
 * X = -3,
 * Y = 4 ;
 * ...
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

% :- package(library(ordered)).
:- package(library(jekmin/reference/finite)).

:- module(enum, []).
:- use_module(library(experiment/trail)).
:- use_module(library(minimal/hypo)).
:- use_module(library(advanced/arith)).
:- use_module(library(basic/lists)).
:- use_module(intset).
:- use_module(linform).

% sys_in(+Wrap, +Set, +Bound)
% sys_in(X,S,_) = X in S
:- multifile intset:sys_in/3.
:- thread_local intset:sys_in/3.
% :- multifile intset:sys_in/4.

/***************************************************/
/* Value Enumeration                               */
/***************************************************/

/**
 * indomain(V):
 * The predicate succeeds for every constant I that is in the
 * domain of the variable V. The domain of the variable can be
 * finite or infinite. A missing domain is interpreted as the
 * full domain.
 */
% indomain(-Integer)
:- public indomain/1.
indomain(X) :-
   var(X), !,
   sys_freeze_var(X, B),
   sys_retire_set(B, S),
   sys_mem_set(S, X).
indomain(X) :-
   integer(X), !.
indomain(X) :-
   throw(error(type_error(integer,X),_)).

/**
 * sys_retire_set(B, S):
 * The predicate succeeds for the domain S of the variable 
 * reference B. The predicate also removes the domain 
 * range constraint of the variable reference B.
 */
% sys_retire_set(+Wrap, -Set)
:- private sys_retire_set/2.
sys_retire_set(B, S) :-
   retire(sys_in(B, S, _)), !.
sys_retire_set(_, [...]).

/**********************************************************/
/* Variable Labeling                                      */
/**********************************************************/

/**
 * label([V1, .., Vn]):
 * The predicate post all the assignments of constants I1, .., In
 * to the variables V1, .., Vn from their domains. Infinite domains
 * are filtered out and cantor paired. Then smaller domains are
 * enumerated first.
 */
% SWI-Prolog like naming.
% label(+List)
:- public label/1.
label(L) :-
   sys_sel_infinite(L, D),
   D \== [], !,
   sys_abs_bound(D, M),
   sys_abs_sum(D, H),
   H #= M,
   indomain(M),
   sys_label_finite(L).
label(L) :-
   sys_label_finite(L).

% sys_label_finite(+List)
:- private sys_label_finite/1.
sys_label_finite(L) :-
   sys_good_pick(L, X, _, T), !,
   indomain(X),
   sys_label_finite(T).
sys_label_finite(_).

/**
 * sys_good_pick(L, Y, N, R):
 * The predicate succeeds with the best variable Y and its
 * set cardinality N from the list L of variables and returning
 * in R the remaining variabels.
 */
% sys_good_pick(+List, -Var, -Integer, -List)
% Fails if all variables are instantiated
% Doesn't do a validation of the list
:- private sys_good_pick/4.
sys_good_pick([X|L], Y, N, R) :-
   var(X), !,
   sys_freeze_var(X, B),
   sys_get_set(B, S),
   sys_card_set(S, M),
   sys_better_pick(X, M, L, Y, N, R).
sys_good_pick([_|L], Y, N, R) :-
   sys_good_pick(L, Y, N, R).

/**
 * sys_get_set(B, S):
 * The predicate succeeds for the domain S of the variable
 * reference B.
 */
% sys_get_set(+Wrap, -Set)
:- private sys_get_set/2.
sys_get_set(B, S) :-
   sys_in(B, S, _), !.
sys_get_set(_, [...]).

% sys_better_pick(+Var, +Integer, +List, -Var, -Integer, -List)
:- private sys_better_pick/6.
sys_better_pick(X, N, L, U, O, R) :-
   sys_good_pick(L, Y, M, Z), !,
   sys_best_pick(X, N, Y, M, Z, U, O, R).
sys_better_pick(X, N, _, X, N, []).

% sys_best_pick(+Var, +Integer, +Var, +Integer, +List, -Var, -Integer, -List)
:- private sys_best_pick/8.
sys_best_pick(X, N, Y, M, L, X, N, [Y|L]) :-
   (N,X) @< (M,Y), !.
sys_best_pick(X, _, Y, M, L, Y, M, [X|L]).

/**********************************************************/
/* Cantor Pairing                                         */
/**********************************************************/

% sys_sel_infinite(+List, -List)
% Does a validation of the list
:- private sys_sel_infinite/2.
sys_sel_infinite(V, _) :-
   var(V),
   throw(error(instantiation_error,_)).
sys_sel_infinite([X|L], C) :-
   var(X), !,
   sys_freeze_var(X, B),
   sys_get_set(B, S),
   sys_card_set(S, M),
   sys_sel_infinite(L, D),
   (  M \== ...
   -> C = D
   ;  C = [X|D]).
sys_sel_infinite([X|L], D) :-
   integer(X), !,
   sys_sel_infinite(L, D).
sys_sel_infinite([X|_], _) :-
   throw(error(type_error(integer,X),_)).
sys_sel_infinite([], []) :- !.
sys_sel_infinite(X, _) :-
   throw(error(type_error(list,X),_)).

% sys_abs_sum(+List, -Integer)
:- private sys_abs_sum/2.
sys_abs_sum([X,Y|Z], abs(X)+R) :- !,
   sys_abs_sum([Y|Z], R).
sys_abs_sum([X], abs(X)).

% sys_abs_bound(+List, +Integer))
:- private sys_abs_bound/2.
sys_abs_bound([X|Y], N) :- !,
   -N #=< X,
   X #=< N,
   sys_abs_bound(Y, N).
sys_abs_bound([], _).

/**********************************************************/
/* Set Enumeration                                        */
/**********************************************************/

% sys_mem_set(+Set, -Integer)
:- private sys_mem_set/2.
sys_mem_set([..A,B...], C) :- !,
   above(0, Y),
   (  C is B+Y
   ;  C is A-Y).
sys_mem_set([..A,U|L], C) :- !,
   sys_mem_set2([..A|L], U, C).
sys_mem_set([U|L], C) :-
   sys_mem_set2(L, U, C).

% sys_mem_set2(+Set, +Range, -Integer)
:- private sys_mem_set2/3.
sys_mem_set2(_, U, C) :-
   sys_mem_range(U, C).
sys_mem_set2([..A,B...], _, C) :- !,
   above(0, Y),
   (  C is B+Y
   ;  C is A-Y).
sys_mem_set2([..A,U|L], _, C) :- !,
   sys_mem_set2([..A|L], U, C).
sys_mem_set2([U|L], _, C) :-
   sys_mem_set2(L, U, C).

% sys_mem_range(+Range, -Integer)
:- private sys_mem_range/2.
sys_mem_range(..., C) :- !,
   above(0, Y),
   (  C is Y
   ;  C is -1-Y).
sys_mem_range(..A, C) :- !,
   B is -A,
   above(B, Y),
   C is -Y.
sys_mem_range(A..., C) :- !,
   above(A, C).
sys_mem_range(A..B, C) :- !,
   between(A, B, C).
sys_mem_range(A, A).

/**********************************************************/
/* Set Cardinality                                        */
/**********************************************************/

% sys_card_set(+Set, -Integer)
:- private sys_card_set/2.
sys_card_set([], 0).
sys_card_set([U|Y], C) :-
   sys_card_range(U, A),
   sys_card_set(Y, B),
   sys_card_add(A, B, C).

% sys_card_add(+Integer, +Integer, -Integer)
:- private sys_card_add/3.
sys_card_add(..., _, ...) :- !.
sys_card_add(_, ..., ...) :- !.
sys_card_add(A, B, C) :-
   C is A+B.

% sys_card_range(+Range, -Integer)
:- private sys_card_range/2.
sys_card_range(..., ...) :- !.
sys_card_range(.._, ...) :- !.
sys_card_range(_..., ...) :- !.
sys_card_range(A..B, C) :- !,
   C is B-A+1.
sys_card_range(_, 1).
