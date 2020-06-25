/**
 * This module provides additional number predicates. The predicates
 * between/3 and above/2 allow enumerating numbers in a given range
 * by the unit step. For both predicates the type of the result is
 * the type of the lower bound. The predicate above/2 doesn't have
 * an upper bound and will return numbers forever.
 *
 * Examples:
 * ?- between(1, 3, X).
 * X = 1 ;
 * X = 2 ;
 * X = 3
 * ?- between(1, 3, 4).
 * No
 *
 * The predicates plus/3 and succ/2 allow solving primitive numeric
 * addition equations. These predicates will not enumerate solutions,
 * but they will work in different modes. The predicate plus/3
 * requires at least two instantiated arguments and the predicate
 * succ/2 requires at least one instantiated argument.
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

:- package(library(jekpro/frequent/standard)).
:- use_package(foreign(jekpro/frequent/standard)).

:- module(arith, []).

/**
 * between(L, H, X):
 * The predicate succeeds in unit steps for every
 * number X between the two numbers L and H.
 */
% between(+Integer, +Integer, -Integer)
:- public between/3.
between(L, H, X) :- var(X), !, sys_between(L, H, X).
between(L, H, X) :- L =< X, X =< H.

:- private sys_between/3.
:- special(sys_between/3, 'SpecialArith', 0).

/**
 * above(L, X):
 * The predicate succeeds in unit steps for every
 * number X above the number L.
 */
% above(+Integer, -Integer)
:- public above/2.
above(L, X) :- var(X), !, sys_above(L, X).
above(L, X) :- L =< X.

:- private sys_above/2.
:- special(sys_above/2, 'SpecialArith', 1).

/**
 * plus(A, B, C):
 * The predicate succeeds for numbers A, B and C such that
 * A+B equals C. At least two arguments have to be instantiated.
 */
% plus(+Number, +Number, -Number)
:- public plus/3.
plus(A, B, C) :- var(A), !, A is C-B.
plus(A, B, C) :- var(B), !, B is C-A.
plus(A, B, C) :- C is A+B.

/**
 * succ(A, B):
 * The predicate succeeds for numbers A and B such that
 * A+1 equals B. At least one arguments has to be instantiated.
 */
% succ(+Number, -Number)
:- public succ/2.
succ(A, B) :- var(A), !, A is B-1.
succ(A, B) :- B is A+1.
