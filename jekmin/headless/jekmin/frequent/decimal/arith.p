/**
 * This module provides predicates for arithmetic of decimals.
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

:- package(library(jekmin/frequent/decimal)).
:- use_package(foreign(jekmin/frequent/decimal)).

:- module(arith, []).

/**
 * new_context(P, C):
 * Predicate succeeds in C with a new math context of precision P.
 */
:- public new_context/2.
:- special(new_context/2, 'SpecialArith', 0).

/**
 * mp_decimal(X, P, Z):
 * Predicate succeeds in Z with X converted to decimal with context P.
 */
:- public mp_decimal/3.
:- special(mp_decimal/3, 'SpecialArith', 1).

/**
 * mp_add(X, Y, P, Z):
 * The predicate succeeds in Z with the addition of two number
 * X and Y with context P.
 */
:- public mp_add/4.
:- special(mp_add/4, 'SpecialArith', 2).

/**
 * mp_sub(X, Y, P, Z):
 * The predicate succeeds in Z with the number X subracted by the
 * number Y with context P.
 */
:- public mp_sub/4.
:- special(mp_sub/4, 'SpecialArith', 3).

/**
 * mp_mul(X, Y, P, Z):
 * The predicate succeeds in Z with the multiplication of two numbers
 * X and Y with context P.
 */
:- public mp_mul/4.
:- special(mp_mul/4, 'SpecialArith', 4).

/**
 * mp_slash(X, Y, P, Z):
 * The predicate succeeds in Z with the number X divided by the
 * number Y with context P.
 */
:- public mp_slash/4.
:- special(mp_slash/4, 'SpecialArith', 5).

/**
 * mp_int_pow(X, N, P, Z):
 * The predicate succeeds in Z with the number X raised to the
 * power of the integer N with context P.
 */
:- public mp_int_pow/4.
:- special(mp_int_pow/4, 'SpecialArith', 6).
