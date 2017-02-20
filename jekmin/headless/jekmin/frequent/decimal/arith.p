/**
 * The default decimal number operations are unlimited precision. We
 * provide here additional predicates mp_decimal/3, mp_add/3, mp_sub/3,
 * mp_mul/3, mp_slash/3 and mp_int_pow/3 that provide limited precision
 * basic arithmetic operations. The predicate mp_decimal/3 reduces the
 * argument to the requested precision by using the proposed rounding.
 * It can do this for integer, float and decimal numbers.
 *
 * Examples:
 * ?- X is mp_decimal(1<<10, new_context(3)).
 * X = 0d1.02E+3
 * ?- X is mp_decimal(pi, new_context(3)).
 * X = 0d3.14
 * ?- X is mp_decimal(0d2.7183, new_context(3)).
 * X = 0d2.72
 *
 * The arithmetic predicates mp_add/3, mp_sub/3, mp_mul/3, mp_slash/3 and
 * mp_int_pow/3 proceed in that they first round the given arguments to
 * the requested precision. Then they compute the arithmetic operation
 * up to the requested precision. The predicate mp_int_pow/3 internally
 * computes with additional precision. Except for the argument and result
 * rounding, they don’t introduce additional errors in the computation.
 *
 * Examples:
 * ?- X is mp_decimal(0d2.7183*0d2.7183, new_context(3)).
 * X = 0d7.39
 * ?- X is mp_mul(0d2.7183, 0d2.7183, new_context(3)).
 * X = 0d7.40
 *
 * The arithmetic predicates fall back to the ordinary operations if none
 * of the arguments are decimals and they might thus also produce integer
 * and float results. There is one exception to this rule for the predicate
 * mp_slash/3 which always produces a decimal.
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
 * mp_decimal(X, P, Z):
 * Predicate succeeds in Z with X converted to decimal with context P.
 */
:- public mp_decimal/3.
:- special(mp_decimal/3, 'SpecialArith', 0).

/**
 * mp_add(X, Y, P, Z):
 * The predicate succeeds in Z with the addition of two number
 * X and Y with context P.
 */
:- public mp_add/4.
:- special(mp_add/4, 'SpecialArith', 1).

/**
 * mp_sub(X, Y, P, Z):
 * The predicate succeeds in Z with the number X subtracted by the
 * number Y with context P.
 */
:- public mp_sub/4.
:- special(mp_sub/4, 'SpecialArith', 2).

/**
 * mp_mul(X, Y, P, Z):
 * The predicate succeeds in Z with the multiplication of two numbers
 * X and Y with context P.
 */
:- public mp_mul/4.
:- special(mp_mul/4, 'SpecialArith', 3).

/**
 * mp_slash(X, Y, P, Z):
 * The predicate succeeds in Z with the number X divided by the
 * number Y with context P.
 */
:- public mp_slash/4.
:- special(mp_slash/4, 'SpecialArith', 4).

/**
 * mp_int_pow(X, N, P, Z):
 * The predicate succeeds in Z with the number X rose to the
 * power of the integer N with context P.
 */
:- public mp_int_pow/4.
:- special(mp_int_pow/4, 'SpecialArith', 5).
