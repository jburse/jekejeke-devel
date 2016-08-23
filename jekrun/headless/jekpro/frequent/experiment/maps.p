/**
 * This module provides unordered maps. The unordered maps are
 * represented by lists of key value pairs [k1-v1, .., kn-vn]. The
 * lists need not be key ordered or key duplicate free. During
 * operations for duplicates the first key wins:
 *
 * Examples:
 * ?- get([2-a,1-b], 1, X).
 * X = b
 * ?- put([2-a,1-b], 1, c, X).
 * X = [2-a,1-c]
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- package(library(jekpro/frequent/experiment)).

:- module(maps, []).

/**
 * get(M, K, V):
 * The predicate succeeds for the value V associated with the
 * key K in the map M.
 */
:- public get/3.
get([K-V|_], J, U) :-
   J == K, !,
   U = V.
get([_|M], J, U) :-
   get(M, J, U).

/**
 * put(M, K, V, N):
 * The predicate succeeds for a map N where the value V is associated
 * with the key K and the other key values are associated as in
 * the map M.
 */
:- public put/4.
put([K-_|M], J, U, R) :-
   J == K, !,
   R = [K-U|M].
put([A|M], J, U, [A|N]) :-
   put(M, J, U, N).
put([], J, U, [J-U]).

/**
 * remove(M, K, N):
 * The predicate succeeds for a map N where the key K has no value
 * and the other key values are associated as in the map M.
 */
:- public remove/3.
remove([K-_|M], J, R) :-
   J == K, !,
   R = M.
remove([A|M], J, [A|N]) :-
   remove(M, J, N).
remove([], _, []).


