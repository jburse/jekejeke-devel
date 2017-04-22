/**
 * This module provides access to decimal numbers. Among the ordinary
 * access to decimal numbers that is available both for unlimited and
 * limited precision decimal numbers, we also find access that is
 * specific to limited precision decimal numbers. A decimal number is
 * represented as follows according to the Java BigDecimal class:
 *
 * D = V * U * 10^(-S)
 * V : The sign, -1, 0 or 1
 * U : The unscaled value from the Java BigInteger class.
 * S : The scale from the Java int class.
 *
 * For limited precision decimal numbers the order of magnitude of the
 * unscaled value is determined as well. This is called the precision of
 * the decimal number and it is independent of the scale of the decimal
 * number. During limited precision arithmetic the precision is limited
 * by a requested precision from a math context:
 *
 * 10^(P-1) <= U < 10^P
 * P : The precision
 *
 * The evaluable functions scale/2, unscaled_value/2 and new_decimal/3 allow
 * accessing and creating an unlimited or limited decimal number. The
 * evaluable functions precision/2 and requested/2 allow accessing the
 * current precision of a decimal number and the requested precision
 * through a math context.
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

:- module(scale, []).

/**
 * scale(X, S):
 * Predicate succeeds in S with the scale of the decimal number X.
 */
:- public scale/2.
:- special(scale/2, 'SupplementScale', 0).

/**
 * unscaled_value(X, U):
 * Predicate succeeds in U with the unscaled value of the decimal number X.
 */
:- public unscaled_value/2.
:- special(unscaled_value/2, 'SupplementScale', 1).

/**
 * new_decimal(U, S, D):
 * The predicate succeeds in D with a decimal of unscaled value U and scale S.
 */
:- public new_decimal/3.
:- special(new_decimal/3, 'SupplementScale', 2).

/**
 * precision(X, P):
 * Predicate succeeds in P with the precision of the decimal number X.
 */
:- public precision/2.
:- special(precision/2, 'SupplementScale', 3).

/**
 * requested(C, P):
 * Predicate succeeds in P with requested precision by the math context C.
 */
:- public requested/2.
:- special(requested/2, 'SupplementScale', 4).

/**
 * new_context(P, C):
 * Predicate succeeds in C with a new math context of requested precision P.
 */
:- public new_context/2.
:- special(new_context/2, 'SupplementScale', 5).
