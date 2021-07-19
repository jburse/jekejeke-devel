/**
 * The Jekejeke Prolog interpreter accepts numbers and internally
 * represents them as integers, floats or decimals. The implementation
 * represents floats and decimals as pairs of a mantissa and a scale.
 * For floats the scale is binary, for decimals the scale is decimal:
 *
 * float = mantissa * 2^scale
 * decimal = mantissa * 10^scale
 *
 * Examples:
 * 1.0 = 100.0E-2
 * 0d0.01 = 0d1E-2
 * 0d123.4 = 0d1.234E2
 * 0d1.0 \= 0d100E-2
 *
 * If an integral value is between -2^31 and 2^31-1 then the Java Integer
 * is used instead of the Java BigInteger. Similarly if a decimal value has
 * scale 0 and a mantissa between -2^63 and 2^63-1 then the Java Long is
 * used instead of the Java BigDecimal. The decimal scale is restricted
 * to 31 bits.
 *
 * The float mantissa is bounded and approximated to the precision. The
 * small respectively large float binary scale is restricted to 8 bits
 * respectively 11 bits. A negative zero is mapped to a positive zero.
 * A NaN or infinity, irrespective of its sign, is considered outside
 * of the domain and leads to an error.
 *
 * Example:
 * ?- [user].
 * factorial(0, 1).
 * factorial(X, Y) :- X>0, H is X-1, factorial(H, J), Y is X*J.
 * ^D
 * ?- X is factorial(4).
 * X = 24
 *
 * The predicate is/2 can be used to evaluate a term consisting of
 * evaluable functions and number constants. Thanks to bridging an
 * evaluable function can also be defined by ordinary Prolog clauses
 * for the corresponding predicate. The evaluable function will only
 * deliver the first result of the corresponding predicate.
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

:- use_package(foreign(jekpro/reference/arithmetic)).

:- module(user, []).

:- public infix(is).
:- op(700, xfx, is).

/**
 * X is Y: [ISO 8.6.1]
 * The predicate succeeds in X with the evaluation of Y.
 */
% -Number is +Expr
:- public is/2.
:- set_predicate_property(is/2, meta_predicate([?, 1])).
:- callable_property(here, sys_context(C)),
   set_predicate_property(is/2, sys_meta_predicate(C)).
:- special(is/2, 'SpecialEval', 0).
