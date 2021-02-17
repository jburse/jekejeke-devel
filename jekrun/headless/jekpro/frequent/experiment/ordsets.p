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

:- package(library(jekpro/frequent/experiment)).

:- module(ordsets, []).

/*******************************************************************/
/* Element Operations                                              */
/*******************************************************************/

/**
 * ord_contains(O, E):
 * The predicate succeeds when the set O contains the element E.
 */
% ord_contains(+OrdSet, +Term)
:- public ord_contains/2.
ord_contains(X, _) :- var(X),
   throw(error(instantiation_error, _)).
ord_contains([X|_], Y) :- X == Y, !.
ord_contains([X|Y], Z) :- X @< Z, !,
   ord_contains(Y, Z).
ord_contains([_|_], _) :- !, fail.
ord_contains([], _) :- !, fail.
ord_contains(X, _) :-
   throw(error(type_error(list, X), _)).

/**
 * ord_delete(O, E, P):
 * The predicate succeeds when P unifies with the subtract of O by [E].
 */
% ord_delete(+OrdSet, +Term, -OrdSet)
:- public ord_delete/3.
ord_delete(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
ord_delete([X|Y], Z, R) :- X == Z, !,
   R = Y.
ord_delete([X|Y], Z, R) :- X @< Z, !,
   R = [X|T],
   ord_delete(Y, Z, T).
ord_delete(X, _, R) :- ord_check(X), !,
   R = X.
ord_delete(X, _, _) :-
   throw(error(type_error(list, X), _)).

/**
 * ord_add(O, E, P):
 * The predicate succeeds when P unifies with the union of [E] and O.
 */
% ord_add(+OrdSet, +Term, -OrdSet)
:- public ord_add/3.
ord_add(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
ord_add([X|Y], Z, R) :- X == Z, !,
   R = [X|Y].
ord_add([X|Y], Z, R) :- X @< Z, !,
   R = [X|T],
   ord_add(Y, Z, T).
ord_add(X, Y, R) :- ord_check(X), !,
   R = [Y|X].
ord_add(X, _, _) :-
   throw(error(type_error(list, X), _)).

/*******************************************************************/
/* Set Operations                                               */
/*******************************************************************/

/**
 * ord_subtract(O, P, Q):
 * The predicate succeeds when Q unifies with the subtract of O by P.
 */
% ord_subtract(+OrdSet, +OrdSet, -OrdSet)
:- public ord_subtract/3.
ord_subtract(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
ord_subtract(_, X, _) :- var(X),
   throw(error(instantiation_error, _)).
ord_subtract([X|Y], [Z|T], R) :- X == Z, !,
   ord_subtract(Y, T, R).
ord_subtract([X|Y], [Z|T], H) :- X @< Z, !,
   H = [X|R],
   ord_subtract(Y, [Z|T], R).
ord_subtract([X|Y], [_|T], R) :- !,
   ord_subtract([X|Y], T, R).
ord_subtract([], X, R) :- ord_check(X), !,
   R = [].
ord_subtract(X, [], R) :- ord_check(X), !,
   R = X.
ord_subtract(X, Y, _) :- ord_check(X),
   throw(error(type_error(list, Y), _)).
ord_subtract(X, _, _) :-
   throw(error(type_error(list, X), _)).

/**
 * ord_intersection(O, P, Q):
 * The predicate succeeds when Q unifies with the intersection of O and P.
 */
% ord_intersection(+OrdSet, +OrdSet, -OrdSet)
:- public ord_intersection/3.
ord_intersection(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
ord_intersection(_, X, _) :- var(X),
   throw(error(instantiation_error, _)).
ord_intersection([X|Y], [Z|T], H) :- X == Z, !,
   H = [X|R],
   ord_intersection(Y, T, R).
ord_intersection([X|Y], [Z|T], R) :- X @< Z, !,
   ord_intersection(Y, [Z|T], R).
ord_intersection([X|Y], [_|T], R) :- !,
   ord_intersection([X|Y], T, R).
ord_intersection([], X, R) :- ord_check(X), !,
   R = [].
ord_intersection(X, [], R) :- ord_check(X), !,
   R = [].
ord_intersection(X, Y, _) :- ord_check(X),
   throw(error(type_error(list, Y), _)).
ord_intersection(X, _, _) :-
   throw(error(type_error(list, X), _)).

/**
 * ord_union(O, P, Q):
 * The predicate succeeds when Q unifies with the union of O and P.
 */
% ord_union(+OrdSet, +OrdSet, -OrdSet)
:- public ord_union/3.
ord_union(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
ord_union(_, X, _) :- var(X),
   throw(error(instantiation_error, _)).
ord_union([X|Y], [Z|T], H) :- X == Z, !,
   H = [X|R],
   ord_union(Y, T, R).
ord_union([X|Y], [Z|T], H) :- X @< Z, !,
   H = [X|R],
   ord_union(Y, [Z|T], R).
ord_union([X|Y], [Z|T], H) :- !,
   H = [Z|R],
   ord_union([X|Y], T, R).
ord_union([], X, R) :- ord_check(X), !,
   R = X.
ord_union(X, [], R) :- ord_check(X), !,
   R = X.
ord_union(X, Y, _) :- ord_check(X),
   throw(error(type_error(list, Y), _)).
ord_union(X, _, _) :-
   throw(error(type_error(list, X), _)).

/**
 * ord_symdiff(O, P, Q):
 * The predicate succeeds when R unifies with the symmetric subtract of S and T.
 */
% ord_symdiff(+OrdSet, +OrdSet, -OrdSet)
:- public ord_symdiff/3.
ord_symdiff(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
ord_symdiff(_, X, _) :- var(X),
   throw(error(instantiation_error, _)).
ord_symdiff([X|Y], [Z|T], R) :- X == Z, !,
   ord_symdiff(Y, T, R).
ord_symdiff([X|Y], [Z|T], H) :- X @< Z, !,
   H = [X|R],
   ord_symdiff(Y, [Z|T], R).
ord_symdiff([X|Y], [Z|T], H) :- !,
   H = [Z|R],
   ord_symdiff([X|Y], T, R).
ord_symdiff([], X, R) :- ord_check(X), !,
   R = X.
ord_symdiff(X, [], R) :- ord_check(X), !,
   R = X.
ord_symdiff(X, Y, _) :- ord_check(X),
   throw(error(type_error(list, Y), _)).
ord_symdiff(X, _, _) :-
   throw(error(type_error(list, X), _)).

/*******************************************************************/
/* Set Tests                                                       */
/*******************************************************************/

/**
 * ord_subset(O, P):
 * The predicate succeeds when O is a subset of P.
 */
% ord_subset(+OrdSet, +OrdSet)
:- public ord_subset/2.
ord_subset(X, _) :- var(X),
   throw(error(instantiation_error, _)).
ord_subset(_, X) :- var(X),
   throw(error(instantiation_error, _)).
ord_subset([X|Y], [Z|T]) :- X == Z, !,
   ord_subset(Y, T).
ord_subset([X|_], [Z|_]) :- X @< Z, !, fail.
ord_subset([X|Y], [_|Z]) :- !,
   ord_subset([X|Y], Z).
ord_subset([], X) :- ord_check(X), !.
ord_subset(X, []) :- ord_check(X), !, fail.
ord_subset(X, Y) :- ord_check(X),
   throw(error(type_error(list, Y), _)).
ord_subset(X, _) :-
   throw(error(type_error(list, X), _)).

/**
 * ord_disjoint(O, P):
 * The predicate succeeds when O is disjoint to P.
 */
% ord_disjoint(+OrdSet, +OrdSet)
:- public ord_disjoint/2.
ord_disjoint(X, _) :- var(X),
   throw(error(instantiation_error, _)).
ord_disjoint(_, X) :- var(X),
   throw(error(instantiation_error, _)).
ord_disjoint([X|_], [Y|_]) :- X == Y, !, fail.
ord_disjoint([X|Y], [Z|T]) :- X @< Z, !,
   ord_disjoint(Y, [Z|T]).
ord_disjoint([X|Y], [_|Z]) :- !,
   ord_disjoint([X|Y], Z).
ord_disjoint([], X) :- ord_check(X), !.
ord_disjoint(X, []) :- ord_check(X), !.
ord_disjoint(X, Y) :- ord_check(X),
   throw(error(type_error(list, Y), _)).
ord_disjoint(X, _) :-
   throw(error(type_error(list, X), _)).

/*******************************************************************/
/* Helper                                                          */
/*******************************************************************/

/**
 * ord_check(O):
 * The predicate succeeds if O is either nil or a cons cell.
 */
:- private ord_check/1.
ord_check([_|_]).
ord_check([]).
