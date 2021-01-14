/**
 * This module provides unordered sets. The unordered sets are
 * represented by lists [x1, .., xn]. The lists need not to be
 * ordered or duplicate free. But the provided operations do not
 * necessarily preserve duplicates:
 *
 * Examples:
 * ?- eq_union([2,3,4], [1,2,4,5], X).
 * X = [3,1,2,4,5]
 * ?- eq_union([1,2,4,5], [2,3,4], X).
 * X = [1,5,2,3,4]
 *
 * The realization uses a membership check based on (==)/2. As a
 * result the predicates are safe to be used with non-ground terms.
 * On the other hand, since this comparison is not arithmetical,
 * 1 and 1.0 are for example considered different.
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

:- module(sets, []).
:- use_module(library(basic/lists)).

/*******************************************************************/
/* Element Operations                                              */
/*******************************************************************/

/**
 * eq_contains(S, E):
 * The predicate succeeds when the set S contains the element E.
 */
% eq_contains(+Set, +Elem)
:- public eq_contains/2.
eq_contains(X, _) :- var(X),
   throw(error(instantiation_error, _)).
eq_contains([X|_], Y) :- X == Y, !.
eq_contains([_|X], Y) :- !,
   eq_contains(X, Y).
eq_contains([], _) :- !, fail.
eq_contains(X, _) :-
   throw(error(type_error(list, X), _)).

/**
 * eq_delete(S, E, T):
 * The predicate succeeds when T unifies with the subtract of S by [E].
 */
% eq_delete(+Set, +Elem, -Set)
:- public eq_delete/3.
eq_delete(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
eq_delete([X|Y], Z, T) :- X == Z, !,
   eq_delete(Y, Z, T).
eq_delete([X|Y], Z, R) :- !,
   R = [X|T],
   eq_delete(Y, Z, T).
eq_delete([], _, R) :- !,
   R = [].
eq_delete(X, _, _) :-
   throw(error(type_error(list, X), _)).

/**
 * eq_add(S, E, T):
 * The predicate succeeds when T unifies with the union of [E] and S.
 */
% eq_add(+Set, +Elem, -Set)
:- public eq_add/3.
eq_add(X, Y, X) :-
   eq_contains(X, Y), !.
eq_add(X, Y, [Y|X]).

/*******************************************************************/
/* Set Operations                                               */
/*******************************************************************/

/**
 * eq_subtract(S, T, R):
 * The predicate succeeds when R unifies with the subtract of S by T.
 */
% eq_subtract(+Set, +Set, -Set)
:- public eq_subtract/3.
eq_subtract(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
eq_subtract([X|Y], Z, T) :-
   eq_contains(Z, X), !,
   eq_subtract(Y, Z, T).
eq_subtract([X|Y], Z, R) :- !,
   R = [X|T],
   eq_subtract(Y, Z, T).
eq_subtract([], _, R) :- !,
   R = [].
eq_subtract(X, _, _) :-
   throw(error(type_error(list, X), _)).

/**
 * eq_intersection(S, T, R):
 * The predicate succeeds when R unifies with the intersection of S and T.
 */
% eq_intersection(+Set, +Set, -Set)
:- public eq_intersection/3.
eq_intersection(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
eq_intersection([X|Y], Z, R) :-
   eq_contains(Z, X), !,
   R = [X|T],
   eq_intersection(Y, Z, T).
eq_intersection([_|X], Y, Z) :- !,
   eq_intersection(X, Y, Z).
eq_intersection([], _, R) :- !,
   R = [].
eq_intersection(X, _, _) :-
   throw(error(type_error(list, X), _)).

/**
 * eq_union(S, T, R):
 * The predicate succeeds when R unifies with the union of S and T.
 */
% eq_union(+Set, +Set, -Set)
:- public eq_union/3.
eq_union(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
eq_union([X|Y], Z, T) :-
   eq_contains(Z, X), !,
   eq_union(Y, Z, T).
eq_union([X|Y], Z, R) :- !,
   R = [X|T],
   eq_union(Y, Z, T).
eq_union([], X, R) :- !,
   R = X.
eq_union(X, _, _) :-
   throw(error(type_error(list, X), _)).

/**
 * eq_symdiff(S, T, R):
 * The predicate succeeds when R unifies with the symmetric subtract of S and T.
 */
% eq_symdiff(+Set, +Set, -Set)
:- public eq_symdiff/3.
eq_symdiff(X, Y, Z) :-
   eq_subtract(X, Y, H),
   eq_subtract(Y, X, J),
   append(H, J, Z).

/*******************************************************************/
/* Set Tests                                                       */
/*******************************************************************/

/**
 * eq_subset(S, T):
 * The predicate succeeds when S is a subset of T.
 */
% eq_subset(+Set, +Set)
:- public eq_subset/2.
eq_subset(X, _) :- var(X),
   throw(error(instantiation_error, _)).
eq_subset([X|Y], Z) :-
   eq_contains(Z, X), !,
   eq_subset(Y, Z).
eq_subset([_|_], _) :- !, fail.
eq_subset([], _) :- !.
eq_subset(X, _) :-
   throw(error(type_error(list, X), _)).

/**
 * eq_disjoint(S, T):
 * The predicate succeeds when S is disjoint to T.
 */
% eq_disjoint(+Set, +Set)
:- public eq_disjoint/2.
eq_disjoint(X, _) :- var(X),
   throw(error(instantiation_error, _)).
eq_disjoint([X|_], Z) :-
   eq_contains(Z, X), !, fail.
eq_disjoint([_|Y], Z) :- !,
   eq_disjoint(Y, Z).
eq_disjoint([], _) :- !.
eq_disjoint(X, _) :-
   throw(error(type_error(list, X), _)).

/**
 * eq_equal(S, T):
 * The predicate succeeds when S is equal to T.
 */
% eq_equal(+Set, +Set)
:- public eq_equal/2.
eq_equal(X, Y) :-
   eq_subset(X, Y),
   eq_subset(Y, X).
