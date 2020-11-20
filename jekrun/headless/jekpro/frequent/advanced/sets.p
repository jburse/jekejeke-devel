/**
 * This module provides unordered sets. The unordered sets are
 * represented by lists [x1, .., xn]. The lists need not to be
 * ordered or duplicate free. But the provided operations do not
 * necessarily preserve duplicates:
 *
 * Examples:
 * ?- union([2,3,4], [1,2,4,5], X).
 * X = [3,1,2,4,5]
 * ?- union([1,2,4,5], [2,3,4], X).
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

:- package(library(jekpro/frequent/advanced)).

:- module(sets, []).
:- use_module(library(basic/lists)).

/*******************************************************************/
/* Element Operations                                              */
/*******************************************************************/

/**
 * contains(S, E):
 * The predicate succeeds when the set S contains the element E.
 */
% contains(+Set, +Elem)
:- public contains/2.
contains(X, _) :- var(X),
   throw(error(instantiation_error, _)).
contains([X|_], Y) :- X == Y, !.
contains([_|X], Y) :- !,
   contains(X, Y).
contains([], _) :- !, fail.
contains(X, _) :-
   throw(error(type_error(list, X), _)).

/**
 * delete(S, E, T):
 * The predicate succeeds when T unifies with the subtract of S by [E].
 */
% delete(+Set, +Elem, -Set)
:- public delete/3.
delete(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
delete([X|Y], Z, T) :- X == Z, !,
   delete(Y, Z, T).
delete([X|Y], Z, R) :- !,
   R = [X|T],
   delete(Y, Z, T).
delete([], _, R) :- !,
   R = [].
delete(X, _, _) :-
   throw(error(type_error(list, X), _)).

/**
 * add(S, E, T):
 * The predicate succeeds when T unifies with the union of [E] and S.
 */
% add(+Set, +Elem, -Set)
:- public add/3.
add(X, Y, X) :-
   contains(X, Y), !.
add(X, Y, [Y|X]).

/*******************************************************************/
/* Set Operations                                               */
/*******************************************************************/

/**
 * subtract(S, T, R):
 * The predicate succeeds when R unifies with the subtract of S by T.
 */
% subtract(+Set, +Set, -Set)
:- public subtract/3.
subtract(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
subtract([X|Y], Z, T) :-
   contains(Z, X), !,
   subtract(Y, Z, T).
subtract([X|Y], Z, R) :- !,
   R = [X|T],
   subtract(Y, Z, T).
subtract([], _, R) :- !,
   R = [].
subtract(X, _, _) :-
   throw(error(type_error(list, X), _)).

/**
 * intersection(S, T, R):
 * The predicate succeeds when R unifies with the intersection of S and T.
 */
% intersection(+Set, +Set, -Set)
:- public intersection/3.
intersection(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
intersection([X|Y], Z, R) :-
   contains(Z, X), !,
   R = [X|T],
   intersection(Y, Z, T).
intersection([_|X], Y, Z) :- !,
   intersection(X, Y, Z).
intersection([], _, R) :- !,
   R = [].
intersection(X, _, _) :-
   throw(error(type_error(list, X), _)).

/**
 * union(S, T, R):
 * The predicate succeeds when R unifies with the union of S and T.
 */
% union(+Set, +Set, -Set)
:- public union/3.
union(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
union([X|Y], Z, T) :-
   contains(Z, X), !,
   union(Y, Z, T).
union([X|Y], Z, R) :- !,
   R = [X|T],
   union(Y, Z, T).
union([], X, R) :- !,
   R = X.
union(X, _, _) :-
   throw(error(type_error(list, X), _)).

/**
 * symdiff(S, T, R):
 * The predicate succeeds when R unifies with the symmetric subtract of S and T.
 */
% symdiff(+Set, +Set, -Set)
:- public symdiff/3.
symdiff(X, Y, Z) :-
   subtract(X, Y, H),
   subtract(Y, X, J),
   append(H, J, Z).

/*******************************************************************/
/* Test Operations                                                 */
/*******************************************************************/

/**
 * subset(S, T):
 * The predicate succeeds when S is a subset of T.
 */
% subset(+Set, +Set)
:- public subset/2.
subset(X, _) :- var(X),
   throw(error(instantiation_error, _)).
subset([X|Y], Z) :- !,
   contains(Z, X),
   subset(Y, Z).
subset([], _) :- !.
subset(X, _) :-
   throw(error(type_error(list, X), _)).

/**
 * disjoint(S, T):
 * The predicate succeeds when S is disjoint to T.
 */
% disjoint(+Set, +Set)
:- public disjoint/2.
disjoint(X, _) :- var(X),
   throw(error(instantiation_error, _)).
disjoint([X|_], Z) :-
   contains(Z, X), !, fail.
disjoint([_|Y], Z) :- !,
   disjoint(Y, Z).
disjoint([], _) :- !.
disjoint(X, _) :-
   throw(error(type_error(list, X), _)).

/**
 * equal(S, T):
 * The predicate succeeds when S is equal to T.
 */
% equal(+Set, +Set)
:- public equal/2.
equal(X, Y) :-
   subset(X, Y),
   subset(Y, X).
