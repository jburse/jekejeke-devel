/**
 * The predicate sort/2 will sort a list by using a set data structure
 * and a comparison among elements. The default implementation use a
 * tree data structure and ISO Prolog lexical compari-son for its
 * sorting. The predicate sort/3 allows specifying other comparisons
 * or a hash data structure. The hash data structure will preserve
 * the input order.
 *
 * Examples:
 * ?- sort([2,1,3,1], X).
 * X = [1, 2, 3]
 * ?- sort([2,1,3,1], X, [type(hash)]).
 * X = [2, 1, 3]
 *
 * The predicate keysort/2 will key sort an association list by using a
 * map data structure and a comparison among keys. For duplicate keys,
 * the values will retain their input order. Again, the predicate
 * keysort/3 allows specifying further sort options. The hash data
 * structure will further preserve the input order of the keys.
 *
 * Examples:
 * ?- hash_code(f, R).
 * R = 102
 * ?- sys_hash_code(f(X), 1, R).
 * R = 102
 *
 * The hash code that is the basis for the hash data type can be
 * queried by the predicates hash_code/2. The hash code is recursively
 * computed along the structure of the given term. Reference data
 * types can implement their own Java hashCode() method. The predicates
 * sys_ground/2 and sys_hash_code/3 provide limited depth versions.
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

:- use_package(foreign(jekpro/frequent/standard)).

:- module(user, []).

/**
 * sort(L, R): [TC2 8.4.3]
 * sort(L, R, O):
 * The predicate sorts the list L and unifies the result with R.
 * The ternary predicate takes additional sort options as argument.
 */
% sort(+List, -List)
:- public sort/2.
:- special(sort/2, 'SpecialSort', 0).

% sort(+List, -List, +List)
:- public sort/3.
:- special(sort/3, 'SpecialSort', 1).

/**
 * keysort(L, R): [TC2 8.4.4]
 * keysort(L, R, O):
 * The predicate key-sorts the pair list L and unifies the result with R.
 * The ternary predicate takes additional sort options as argument.
 */
% keysort(+Assoc, -Assoc)
:- public keysort/2.
:- special(keysort/2, 'SpecialSort', 2).

% keysort(+Assoc, -Assoc, +List)
:- public keysort/3.
:- special(keysort/3, 'SpecialSort', 3).

/**
 * hash_code(T, H):
 * The predicate succeeds when H unifies with the hash code of T.
 * The term T need not be ground. The hash will be in the range
 * from -2147483648 to 2147483647.
 */
% hash_code(+Term, -Integer)
:- public hash_code/2.
:- special(hash_code/2, 'SpecialSort', 4).

/**
 * sys_ground(T, D):
 * The predicate succeeds when T is ground up to depth D.
 */
:- public sys_ground/2.
:- special(sys_ground/2, 'SpecialSort', 5).

/**
 * sys_hash_code(T, D, H):
 * The predicate succeeds in H with hash code of T up to depth D.
 * The term T need not be ground up to depth D. The hash will be
 * in the range from -2147483648 to 2147483647.
 */
:- public sys_hash_code/3.
:- special(sys_hash_code/3, 'SpecialSort', 6).
