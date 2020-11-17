/**
 * This module provides ordered sets. The ordered sets are
 * represented by lists [x1, .., xn]. The lists must be ordered
 * and duplicate free. If this precondition is violated the
 * behaviour of the predicates is undefined.
 *
 * Examples:
 * ?- ord_union([2,3,4],[1,2,4,5],X).
 * X = [1,2,3,4,5]
 * ?- ord_union([1,2,4,5],[2,3,4],X).
 * X = [1,2,3,4,5]
 *
 * The realization uses a membership check based on (==)/2 and
 * lexical ordering based on (@<)/2. As a result the predicates
 * are safe to be used with non-ground terms. On the other hand,
 * since this comparison is not arithmetical, 1 and 1.0 are for
 * example considered different.
 *
 * An unordered set can be converted into an ordered set by
 * using the ISO predicate sort/2. Also there is no need for
 * predicate permutation/2 here, since equality of ordered sets
 * can be tested via the ISO predicate ==/2, provided the elements
 * are sufficiently normalized.
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

:- module(ordsets, []).

/*******************************************************************/
/* Element Operations                                              */
/*******************************************************************/

/**
 * ord_contains(O, E):
 * The predicate succeeds when the set O contains the element E.
 */
% ord_contains(+OrdSet, +Elem)
:- public ord_contains/2.
ord_contains([X|_], Y) :- X == Y, !.
ord_contains([X|Y], Z) :- X @< Z, !,
   ord_contains(Y, Z).

/**
 * ord_delete(O1, E, O2):
 * The predicate succeeds when O2 unifies with the subtract of O1 by [E].
 */
% ord_delete(+OrdSet, +Elem, -OrdSet)
:- public ord_delete/3.
ord_delete([X|Y], Z, Y) :- X == Z, !.
ord_delete([X|Y], Z, [X|R]) :- X @< Z, !,
   ord_delete(Y, Z, R).
ord_delete(X, _, X).

/**
 * ord_add(O1, E, O2):
 * The predicate succeeds when O2 unifies with the union of [E] and O1.
 */
% ord_add(+OrdSet, +Elem, -OrdSet)
:- public ord_add/3.
ord_add([X|Y], Z, [X|Y]) :- X == Z, !.
ord_add([X|Y], Z, [X|R]) :- X @< Z, !,
   ord_add(Y, Z, R).
ord_add(X, Y, [Y|X]).

/*******************************************************************/
/* Set Operations                                               */
/*******************************************************************/

/**
 * ord_subtract(O1, O2, O3):
 * The predicate succeeds when O3 unifies with the subtract of O1 by O2.
 */
% ord_subtract(+OrdSet, +OrdSet, -OrdSet)
:- public ord_subtract/3.
ord_subtract([X|Y], [Z|T], R) :- X == Z, !,
   ord_subtract(Y, T, R).
ord_subtract([X|Y], [Z|T], [X|R]) :- X @< Z, !,
   ord_subtract(Y, [Z|T], R).
ord_subtract([X|Y], [_|T], R) :-
   ord_subtract([X|Y], T, R).
ord_subtract([], _, []) :- !.
ord_subtract(X, [], X).

/**
 * ord_intersection(O1, O2, O3):
 * The predicate succeeds when O3 unifies with the intersection of O1 and O2.
 */
% ord_intersection(+OrdSet, +OrdSet, -OrdSet)
:- public ord_intersection/3.
ord_intersection([X|Y], [Z|T], [X|R]) :- X == Z, !,
   ord_intersection(Y, T, R).
ord_intersection([X|Y], [Z|T], R) :- X @< Z, !,
   ord_intersection(Y, [Z|T], R).
ord_intersection([X|Y], [_|T], R) :-
   ord_intersection([X|Y], T, R).
ord_intersection([], _, []) :- !.
ord_intersection(_, [], []).

/**
 * ord_union(O1, O2, O3):
 * The predicate succeeds when O3 unifies with the union of O1 and O2.
 */
% ord_union(+OrdSet, +OrdSet, -OrdSet)
:- public ord_union/3.
ord_union([X|Y], [Z|T], [X|R]) :- X == Z, !,
   ord_union(Y, T, R).
ord_union([X|Y], [Z|T], [X|R]) :- X @< Z, !,
   ord_union(Y, [Z|T], R).
ord_union([X|Y], [Z|T], [Z|R]) :-
   ord_union([X|Y], T, R).
ord_union([], X, X) :- !.
ord_union(X, [], X).

/*******************************************************************/
/* Test Operations                                                 */
/*******************************************************************/

/**
 * ord_subset(O1, O2):
 * The predicate succeeds when O1 is a subset of O2.
 */
% ord_subset(+OrdSet, +OrdSet)
:- public ord_subset/2.
ord_subset([X|Y], [Z|T]) :- X == Z, !,
   ord_subset(Y, T).
ord_subset([X|_], [Z|_]) :- X @< Z, !, fail.
ord_subset([X|Y], [_|Z]) :-
   ord_subset([X|Y], Z).
ord_subset([], _).

/**
 * ord_disjoint(O1, O2):
 * The predicate succeeds when O1 is disjoint to O2.
 */
% ord_disjoint(+OrdSet, +OrdSet)
:- public ord_disjoint/2.
ord_disjoint([X|_], [Y|_]) :- X == Y, !, fail.
ord_disjoint([X|Y], [Z|T]) :- X @< Z, !,
   ord_disjoint(Y, [Z|T]).
ord_disjoint([X|Y], [_|Z]) :-
   ord_disjoint([X|Y], Z).
ord_disjoint([], _) :- !.
ord_disjoint(_, []).
