/**
 * We provide a couple of additional elementary operations. The ulp
 * operation is defined for integer, float and decimal. It returns a
 * result of the same type as its argument. The gcd operation is
 * currently only defined for integers. It returns a result that
 * is an integer.
 *
 * ulp: integer -> integer
 * ulp: float -> float
 * ulp: decimal -> decimal
 * gcd: integer x integer -> integer
 *
 * The ulp operation makes use of the ulp() function of the Java Math
 * library. The gcd operation implements a binary gcd algorithm for
 * 32-bit integers and otherwise delegates to the Java BigInteger
 * gcd operation implementation.
 *
 * Examples:
 * ulp(0)          --> 1
 * ulp(0.0)        --> 4.9E-324
 * ulp(0d0.00)     --> 0d0.01
 * gcd(36,24)      --> 12
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

:- package(library(jekmin/reference/mics)).
:- use_package(foreign(jekmin/reference/misc)).

:- module(elem, []).

/**
 * ulp(X, Y):
 * Predicate succeeds in Y with the unit of least precision of
 * the number X.
 */
:- public ulp/2.
:- special(ulp/2, 'SupplementElem', 0).

/**
 * scale(X, S):
 * Predicate succeeds in S with the scale of the decimal number X.
 */
:- public scale/2.
:- special(scale/2, 'SupplementElem', 1).

/**
 * precision(X, P):
 * Predicate succeeds in P with the precision of the decimal number X.
 */
:- public precision/2.
:- special(precision/2, 'SupplementElem', 2).

/**
 * unscaled_value(X, P):
 * Predicate succeeds in P with the unscale value of the decimal number X.
 */
:- public unscaled_value/2.
:- special(unscaled_value/2, 'SupplementElem', 3).

/**
 * gcd(X, Y, Z):
 * Predicate succeeds in Z with the greatest common divisor of the
 * integer X and the integer Y.
 */
:- public gcd/3.
:- special(gcd/3, 'SupplementElem', 4).

/**
 * decimal(X, P, Z):
 * Predicate succeeds in Z with X converted to decimal with precision P.
 */
:- public decimal/3.
:- special(decimal/3, 'SupplementElem', 5).

/**
 * add(X, Y, P, Z):
 * The predicate succeeds in Z with the addition of two number
 * X and Y with some precision P.
 */
:- public add/4.
:- special(add/4, 'SupplementElem', 6).

/**
 * sub(X, Y, P, Z):
 * The predicate succeeds in Z with the number X subracted by the
 * number Y with some precision P.
 */
:- public sub/4.
:- special(sub/4, 'SupplementElem', 7).

/**
 * mul(X, Y, P, Z):
 * The predicate succeeds in Z with the multiplication of two numbers
 * X and Y with some precision P.
 */
:- public mul/4.
:- special(mul/4, 'SupplementElem', 8).

/**
 * div(X, Y, P, Z):
 * The predicate succeeds in Z with the number X divided by the
 * number Y with some precision P.
 */
:- public div/4.
:- special(div/4, 'SupplementElem', 9).

/**
 * int_pow(X, N, P, Z):
 * The predicate succeeds in Z with the number X raised to the
 * power of the integer N with some precision P.
 */
:- public int_pow/4.
:- special(int_pow/4, 'SupplementElem', 10).

