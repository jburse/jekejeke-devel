/**
 * The predicate sys_distinct/1 will remove duplicates from a list
 * using a hash set in the current implementation, thus relying only
 * on equality among the elements. On the other hand the predicate
 * sort/2 will sort a list by using a tree in the current implementation
 * and also requires comparison among the elements.
 *
 * Examples:
 * ?- sys_distinct([2,1,3,1], X).
 * X = [2,1,3]
 * ?- sort([2,1,3,1], X).
 * X = [1,2,3]
 *
 * The predicate sys_keygroup/2 will key group a list using a hash
 * table in the current implementation, thus relying only on equality
 * among the keys. On the other hand the predicate keysort/2 will key
 * sort a list by using a tree in the current implementation, thus also
 * requiring comparison among the keys.
 *
 * Examples:
 * ?- hash_code(f, R).
 * R = 102
 * ?- term_hash(f(X), 1, 1000, R).
 * R = 102
 *
 * The hash code that is the basis for the removal and grouping predicates
 * can be queried by the predicates hash_code/2. The hash code is
 * recursively computed along the structure of the given term. The
 * hash code that forms the basis of our clause indexing can be
 * queried by the predicates term_hash/[2,4].
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
 * The predicate distincts the list L and unifies the result with R.
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
 * hash_code(T, H):
 * The predicate succeeds when H unifies with the hash code of T. The term
 * T need not be ground. The hash will be in the range from -2147483648
 * to 2147483647.
 */
% hash_code(+Term, -Integer)
:- public hash_code/2.
:- special(hash_code/2, 'SpecialSort', 4).

/**
 * term_hash(T, H):
 * term_hash(T, D, R, H):
 * The predicate succeeds when T is ground and when H unifies with the
 * hash code of T. The predicate also succeeds when T is non-ground, the H
 * argument is then simply ignored. The hash will be in the range from
 * -2147483648 to 2147483647. The quinary perdicate allows specifying a
 * depth D and a modulus R. A negative depth D is interpreted as infinity.
 */
% term_hash(+Term, -Integer)
:- public term_hash/2.
term_hash(T, H) :-
   ground(T), !,
   hash_code(T, H).
term_hash(_, _).

% term_hash(+Term, +Integer, +Integer, -Integer)
:- public term_hash/4.
term_hash(T, D, R, H) :-
   sys_term_ground(T, D), !,
   sys_term_hash(T, D, J),
   H is J mod R.
term_hash(_, _, _, _).

:- private sys_term_ground/2.
:- special(sys_term_ground/2, 'SpecialSort', 5).

:- public sys_term_hash/3.
:- special(sys_term_hash/3, 'SpecialSort', 6).
