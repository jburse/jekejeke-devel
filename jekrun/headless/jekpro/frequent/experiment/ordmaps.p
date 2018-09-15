/**
 * This module provides ordered maps. The ordered maps are
 * represented by lists of key value pairs [k1-v1, .., kn-vn].
 * The lists need to be key ordered and key duplicate free.
 * If this precondition is violated the behaviour of the
 * predicates is undefined.
 *
 * Examples:
 * ?- ord_get([1-a,2-b], 2, X).
 * X = b
 * ?- ord_put([1-a,2-b], 2, c, X).
 * X = [1-a,2-c]
 *
 * The realization uses a membership check based on (==)/2 and
 * lexical ordering based on (@<)/2. As a result the predicates
 * are safe to be used with non-ground terms. On the other hand,
 * since this comparison is not arithmetical, 1 and 1.0 are for
 * example considered different.
 *
 * An unordered map can be converted into an ordered map by
 * using the ISO predicate keysort/2. Also there is no need for
 * predicate permutation/2 here, since equality of ordered maps
 * can be tested via the ISO predicate ==/2, provided the keys
 * and values are sufficiently normalized.
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

:- module(ordmaps, []).

/**
 * ord_get(M, K, V):
 * The predicate succeeds for the value V associated with K
 * in the ordered map M.
 */
:- public ord_get/3.
ord_get([K-_|_], J, _) :-
   J @< K, !, fail.
ord_get([K-V|_], J, U) :-
   J == K, !,
   U = V.
ord_get([_|M], J, U) :-
   ord_get(M, J, U).

/**
 * ord_put(M, K, V, N):
 * The predicate succeeds for an ordered map N where the value V is
 * associated with the key K and the other key values are associated
 * as in the ordered map M.
 */
:- public ord_put/4.
ord_put([K-V|M], J, U, R) :-
   J @< K, !,
   R = [J-U,K-V|M].
ord_put([K-_|M], J, U, R) :-
   J == K, !,
   R = [K-U|M].
ord_put([A|M], J, U, [A|N]) :-
   ord_put(M, J, U, N).
ord_put([], J, U, [J-U]).

/**
 * ord_remove(M, K, N):
 * The predicate succeeds for an ordered map N where the key K has
 * no value and the other key values are associated as in the
 * ordered map M.
 */
:- public ord_remove/3.
ord_remove([K-V|M], J, R) :-
   J @< K, !,
   R = [K-V|M].
ord_remove([K-_|M], J, R) :-
   J == K, !,
   R = M.
ord_remove([A|M], J, [A|N]) :-
   ord_remove(M, J, N).
ord_remove([], _, []).
