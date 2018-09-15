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

/**
 * contains(E, S):
 * The predicate succeeds when the set S contains the element E.
 */
% contains(+Elem, +Set)
:- public contains/2.
contains(X, [Y|_]) :-
   X == Y, !.
contains(X, [_|Y]) :-
   contains(X, Y).

/**
 * remove(E, S, T):
 * The predicate succeeds when the set S contains the element E
 * and T is the set without the element.
 */
% remove(+Elem, +Set, -Set)
:- public remove/3.
remove(X, [Y|Z], Z) :-
   X == Y, !.
remove(X, [Y|Z], [Y|T]) :-
   remove(X, Z, T).

/**
 * difference(S1, S2, S3):
 * The predicate succeeds when S3 unifies with the difference of S1 by S2.
 */
% difference(+Set, +Set, -Set)
:- public difference/3.
difference([X|Y], Z, T) :-
   contains(X, Z), !,
   difference(Y, Z, T).
difference([X|Y], Z, [X|T]) :-
   difference(Y, Z, T).
difference([], _, []).

/**
 * intersection(S1, S2, S3):
 * The predicate succeeds when S3 unifies with the intersection of S1 and S2.
 */
% intersection(+Set, +Set, -Set)
:- public intersection/3.
intersection([X|Y], Z, [X|T]) :-
   contains(X, Z), !,
   intersection(Y, Z, T).
intersection([_|X], Y, Z) :-
   intersection(X, Y, Z).
intersection([], _, []).

/**
 * union(S1, S2, S3):
 * The predicate succeeds when S3 unifies with the union of S1 and S2.
 */
% union(+Set, +Set, -Set)
:- public union/3.
union([X|Y], Z, T) :-
   contains(X, Z), !,
   union(Y, Z, T).
union([X|Y], Z, [X|T]) :-
   union(Y, Z, T).
union([], X, X).

/**
 * subset(S1, S2):
 * The predicate succeeds when S1 is a subset of S2.
 */
% subset(+Set, +Set)
:- public subset/2.
subset([X|Y], Z) :-
   contains(X, Z),
   subset(Y, Z).
subset([], _).

/**
 * permutation(S1, S2):
 * The predicate succeeds when S1 is a permutation of S2.
 */
% permutation(+Set, +Set)
:- public permutation/2.
permutation([X|Y], L) :-
   remove(X, L, R),
   permutation(Y, R).
permutation([], []).
