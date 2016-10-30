/**
 * When the arguments of the binary elementary operations do
 * not have the same types then widening is applied to them
 * before performing the operation. Widening is done towards
 * the bigger domain of the two arguments. Widening from integer
 * to float might fail with an excep-tion, since the unbounded
 * integers have a greater range than floats. The ordering of the
 * domains is as follows:
 *
 * integer < float32 < float < decimal
 *
 * The signature of the available binary and unary elementary
 * operations is listed here:
 *
 * +, -, *, ^: integer x integer -> integer
 * /: integer x integer -> float
 * +, -, *, /: float x float -> float
 * +, -, *, /: decimal x decimal -> decimal
 * -, +, abs, sign: integer -> integer
 * -, +, abs, sign, float: float -> float
 * -, +, abs, sign, decimal: decimal -> decimal
 * float: integer -> float
 * float: decimal -> float
 * decimal: integer -> decimal
 * decimal: float -> decimal
 *
 * Examples:
 * abs(-1)			    --> 1
 * abs(-1.0)			--> 1.0
 * abs(-0d1.00)		    --> 0d1.00
 * decimal(0.1)		    --> 0d0.1000000000 0000000555 1115123125
 * 					        7827021181 5834045410 15625
 * 9 + 1				--> 10
 * 0.99 + 0.01			--> 1.0
 * 0d0.990 + 0d0.01		--> 0d1.000
 * 5 * 2				--> 10
 * 5.0 * 2.0			--> 10.0
 * 0d5.0 * 0d2.0		--> 0d10.00
 * 5 / 2				--> 2.5
 * 5.0 / 2.0			--> 2.5
 * 0d5.00 / 2			--> 0d2.50
 * 3 ^ 27			    --> 7625597484987
 *
 * The unary operation float is approximate for integer and decimal
 * arguments and returns always floats. The unary operation decimal is
 * exact for integer and float arguments and returns always decimals.
 * The binary decimal operations return the preferred scale as defined
 * in the java class BigDecimal. They thus differ from the usual
 * float operations.
 *
 * Example:
 * ?- abs(-1, X).
 * X = 1
 * ?- abs(- 1, X).
 * Error: Argument should be a number, found - 1.
 * 	     abs/2
 *
 * Thanks to tunnelling an evaluable function can also be invoked
 * by calling the corresponding predicate. When invoking the predicate
 * the arguments are not evaluated, only type checked. The result of
 * the evaluable function is returned in the last argument of
 * the predicate. 
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

:- use_package(foreign(jekpro/reference/arithmetic)).

:- module(user, []).

:- public infix(+).
:- op(500, yfx, +).

:- public infix(-).
:- op(500, yfx, -).

:- public infix(*).
:- op(400, yfx, *).

% already defined in special
% :- public infix(/).
% :- op(400, yfx, /).

:- public prefix(+).
:- op(200, fy, +).

:- public prefix(-).
:- op(200, fy, -).

:- public infix(^).
:- op(200, xfy, ^).
:- set_oper_property(infix(^), nspl).
:- set_oper_property(infix(^), nspr).

/**
 * - X: [ISO 9.1.7]
 * If X is a number then returns the sign inversion of X.
 */
% - : integer -> integer
% - : float -> float
% - : decimal -> decimal
:- public (-)/2.
:- special((-)/2, 'EvaluableElem', 0).

/**
 * + X: [TC2 9.1.3]
 * If X is a number then returns X unchanged.
 */
% + : integer -> integer
% + : float -> float
% + : decimal -> decimal
:- public (+)/2.
:- special((+)/2, 'EvaluableElem', 1).

/**
 * abs(X): [ISO 9.1.7]
 * If X is a number then returns the absolute value of X.
 */
% abs : integer -> integer
% abs : float -> float
% abs : decimal -> decimal
:- public abs/2.
:- special(abs/2, 'EvaluableElem', 2).

/**
 * sign(X): [ISO 9.1.4]
 * If X is a number then returns the sign of X.
 */
% sign : integer -> integer
% sign : float -> float
% sign : decimal -> decimal
:- public sign/2.
:- special(sign/2, 'EvaluableElem', 3).

/**
 * float(X): [ISO 9.17]
 * If X is a number then returns the conversion of X to a float.
 */
% float : integer -> float
% float : float -> float
% float : decimal -> float
:- public float/2.
:- special(float/2, 'EvaluableElem', 4).

/**
 * decimal(X):
 * If X is a number then returns the conversion of X to a decimal.
 */
% decimal : integer -> decimal
% decimal : float -> decimal
% decimal : decimal -> decimal
:- public decimal/2.
:- special(decimal/2, 'EvaluableElem', 5).

/**
 * float32(X):
 * If X is a number then returns the conversion of X to a float32.
 */
% float32 : integer -> float
% float32 : float -> float
% float32 : decimal -> float
:- public float32/2.
:- special(float32/2, 'EvaluableElem', 6).

/**
 * X + Y: [ISO 9.1.7]
 * If X and Y are both numbers then the function returns the addition of X and Y.
 */
% + : integer x integer -> integer
% + : float x float -> float
% + : decimal x decimal -> decimal
:- public (+)/3.
:- special((+)/3, 'EvaluableElem', 7).

/**
 * X - Y: [ISO 9.1.7]
 * If X and Y are both numbers then the function returns the subtraction of X by Y.
 */
% - : integer x integer -> integer
% - : float x float -> float
% - : decimal x decimal -> decimal
:- public (-)/3.
:- special((-)/3, 'EvaluableElem', 8).

/**
 * X * Y: [ISO 9.1.7]
 * If X and Y are both numbers then the function returns the multiplication of X and Y.
 */
% * : integer x integer -> integer
% * : float x float -> float
% * : decimal x decimal -> decimal
:- public * /3.
:- special(* /3, 'EvaluableElem', 9).

/**
 * X / Y: [ISO 9.1.7]
 * If X and Y are both numbers then the function returns the scaled division of X by Y.
 */
% * : integer x integer -> float
% * : float x float -> float
% * : decimal x decimal -> decimal
:- public / /3.
:- special(/ /3, 'EvaluableElem', 10).

/**
 * X ^ Y: [TC2 9.3.10]
 * If X and Y are both integers then the function returns X raised to the power of Y.
 */
% ^ : integer x integer -> integer
:- public ^ /3.
:- special(^ /3, 'EvaluableElem', 11).