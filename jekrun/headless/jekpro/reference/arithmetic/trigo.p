/**
 * The trigonometric operations are defined for integers, floats
 * and decimals. If the argument is an integer it is widened to a
 * float before computing the operation. On the other hand if the
 * argument is a decimal it is narrowed to a float before computing
 * the operation. Widening from decimal to float might fail with
 * an exception, since the unbounded decimals have a greater
 * range than floats.
 *
 * If the argument of a trigonometric operation is outside of its
 * domain then an undefined evaluation error is thrown. Further
 * when the mathematical result exceeds the range of the float
 * then a float overflow evaluation error is thrown. The throwing
 * of an exception is preferred over returning a float with the
 * meaning of not a number (NaN), negative infinite (-Inf) or
 * positive infinite (+Inf).
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

:- use_package(foreign(jekpro/frequent/basic)).

:- module(user, []).

:- public infix(**).
:- op(200, xfx, **).

/**
 * sin(X): [ISO 9.3.2]
 * Returns the float representation of the sine of X, the
 * argument must be given in radians.
 */
:- public sin/2.
:- foreign(sin/2, 'Math', sin(double)).

/**
 * cos(X): [ISO 9.3.3]
 * Returns the float representation of the cosine of X, the
 * argument must be given in radians.
 */
:- public cos/2.
:- foreign(cos/2, 'Math', cos(double)).

/**
 * tan(X): [TC2 9.3.14]
 * Returns the float representation of the tangent of X, the
 * argument must be given in radians.
 */
:- public tan/2.
:- foreign(tan/2, 'Math', tan(double)).

/**
 * asin(X): [TC2 9.3.11]
 * Returns the float representation of the arcus sine of X, the
 * result is in radians.
 */
:- public asin/2.
:- foreign(asin/2, 'Math', asin(double)).

/**
 * acos(X): [TC2 9.3.12]
 * Returns the float representation of the arcus cosine of X, the
 * result is in radians.
 */
:- public acos/2.
:- foreign(acos/2, 'Math', acos(double)).

/**
 * atan(X): [ISO 9.3.4]
 * Returns the float representation of the arcus tangent of X, the
 * result is in radians.
 */
:- public atan/2.
:- foreign(atan/2, 'Math', atan(double)).

/**
 * X ** Y: [ISO 9.3.1]
 * Returns the float representation of X raised to the
 * power of Y.
 */
:- public ** /3.
:- foreign(** /3, 'Math', pow(double, double)).

/**
 * exp(X): [ISO 9.3.5]
 * Returns the float representation of Euler’s number e
 * raised to the power of X.
 */
:- public exp/2.
:- foreign(exp/2, 'Math', exp(double)).

/**
 * log(X): [ISO 9.3.6]
 * Returns the float representation of the natural
 * logarithm of X.
 */
:- public log/2.
:- foreign(log/2, 'Math', log(double)).

/**
 * sqrt(X): [ISO 9.3.7]
 * Returns the float representation of the square root of X.
 */
:- public sqrt/2.
:- foreign(sqrt/2, 'Math', sqrt(double)).

/**
 * pi: [TC2 9.3.15]
 * Returns the float representation of π.
 */
:- public pi/1.
% :- foreign_getter(pi/1, 'Math', 'PI').
:- foreign(pi/1, 'ForeignHyper', pi).

/**
 * e: [N208 9.7.2]
 * Returns the float representation of Euler’s number e.
 */
:- public e/1.
% :- foreign_getter(e/1, 'Math', 'E').
:- foreign(e/1, 'ForeignHyper', e).

/**
 * atan2(X,Y): [TC2 9.3.13]
 * Returns the float representation of the arc tangent of X and Y, the
 * result is in radians.
 */
:- public atan2/3.
atan2(X, Y, _) :- X =:= 0, Y =:= 0,
   throw(error(evaluation_error(undefined), _)).
atan2(X, Y, Z) :-
   Z is sys_atan2(X, Y).

:- private sys_atan2/3.
:- foreign(sys_atan2/3, 'Math', atan2(double, double)).

/**
 * epsilon: [N208 9.7.3]
 * Returns the ulp of 64-bit one.
 */
% epsilon32(-Number)
:- public epsilon/1.
% :- foreign_getter(epsilon/1, 'SpecialCompare', 'EPSILON').
:- foreign(epsilon/1, 'ForeignHyper', epsilon).

/**
 * epsilon32:
 * Returns the ulp of 32-bit one.
 */
% epsilon32(-Number)
:- public epsilon32/1.
% :- foreign_getter(epsilon32/1, 'SpecialCompare', 'EPSILON32').
:- foreign(epsilon32/1, 'ForeignHyper', epsilon32).
