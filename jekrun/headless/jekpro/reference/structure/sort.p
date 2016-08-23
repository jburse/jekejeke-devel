/**
 * The predicate sys_distinct/1 will remove duplicates from a list using
 * a hash set in the current implementation, thus relying only on equality
 * among the elements. On the other hand the predicate sort/2 will sort a
 * list by using a tree in the current implementation and also requires
 * comparison among the elements.
 *
 * Example:
 * ?- sys_distinct([2,1,3,1], X).
 * X = [2,1,3]
 * ?- sort([2,1,3,1], X).
 * X = [1,2,3]
 *
 * The predicate sys_keygroup/2 will key group a list using a hash
 * table in the current imple-mentation, thus relying only on equality
 * among the keys. On the other hand the predicate keysort/2 will key
 * sort a list by using a tree in the current implementation, thus also
 * requiring comparison among the keys.
 *
 * The hash code that is the basis for the grouping predicates can be
 * queried by the predicate term_hash/2. The hash code is recursively
 * computed along the structure of the given term. For atomic values
 * the hash code is the same that is also used in clause indexing.
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

:- use_package(foreign(jekpro/reference/structure)).

:- module(user, []).

/**
 * sort(L, R): [TC2 8.4.3]
 * The predicate sorts the list L and unifies the result with R.
 */
% sort(+List, -List)
:- public sort/2.
:- special(sort/2, 'SpecialSort', 0).

/**
 * sys_distinct(L, R):
 * The predicate sorts the list L and unifies the result with R.
 */
% sys_distinct(+List, -List)
:- public sys_distinct/2.
:- special(sys_distinct/2, 'SpecialSort', 1).

/**
 * keysort(L, R): [TC2 8.4.4]
 * The predicate key-sorts the pair list L and unifies the result with R.
 */
% keysort(+Pairs, -Pairs)
:- public keysort/2.
:- special(keysort/2, 'SpecialSort', 2).

/**
 * sys_keygroup(L, R):
 * The predicate key-groups the pair list L and unifies the result with R.
 */
% sys_keygroup(+Pairs, -Pairs)
:- public sys_keygroup/2.
:- special(sys_keygroup/2, 'SpecialSort', 3).

/**
 * term_hash(T, H):
 * The predicate succeeds when H unifies with the hash of T. The hash will
 * be in the range from -2147483648 to 2147483647. The term need not be ground.
 */
% term_hash(+Term, -Integer)
:- public term_hash/2.
:- special(term_hash/2, 'SpecialSort', 4).
