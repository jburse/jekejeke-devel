/**
 * We supply a number of binary and unary operations that deal
 * with rounding. We find different rounding modes and we also
 * find different forms of division. The result type of the evaluable
 * function integer/1 is always an integer. On the other hand the
 * result type of the evaluable functions truncate/1, floor/1,
 * ceiling/1 and round/1 is the same as the argument type:
 *
 * integer:	        integer towards zero
 * truncate:	    towards zero
 * floor:			towards lower infinity
 * ceiling:			towards upper infinity
 * round:			towards nearest neighbour or then away from zero
 *
 * Examples:
 * floor(-3)          --> -3
 * floor(-3.14)       --> -4
 * floor(-0d3.1415)   --> -4
 *
 * The division is based on a hypothetical /F operation. This operation
 * is approximate for float arguments and exact for integer and decimal
 * arguments. The result type of the evaluable functions (//)/2 and
 * (div)/2 is always an integer. On the other hand the result type of
 * the evaluable functions (rem)/2 and (mod)/2 is the same as the
 * argument types:
 *
 * X // Y = truncate_I(X /_R Y).
 * X div Y = floor_I(X /_R Y)).
 * X rem Y = X – (X // Y) * Y.
 * X mod Y = X – (X div Y) * Y.
 *
 * Examples:
 * 5 // 2             --> 2
 * 5.0 // 2.0         --> 2
 * 0d5.00 // 2        --> 2
 * (-5) // 2          --> -2
 * (-5) div 2         --> -3
 * 5 rem 2            --> 1
 * 5.0 rem 2.0        --> 1.0
 * 0d5.00 rem 2       --> 0d1.00
 * (-5) rem 2         --> -1
 * (-5) mod 2         --> 1
 *
 * If the arguments of the binary operations have different types the
 * same widening as already defined for the basic operations is applied.
 * This means the widening is done towards the big-ger domain of the
 * two arguments.
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

:- public infix(//).
:- op(400, yfx, //).

:- public infix(rem).
:- op(400, yfx, rem).

:- public infix(div).
:- op(400, yfx, div).

:- public infix(mod).
:- op(400, yfx, mod).

/**
 * integer(X):
 * If X is a number the returns the integer of X.
 */
% integer(+Number, -Integer)
:- public integer/2.
:- special(integer/2, 'EvaluableRound', 0).

/**
 * truncate(X): [ISO 9.1.7]
 * If X is a number then returns the rounding of X towards zero.
 */
% truncate(+Integer, -Integer)
% truncate(+Float32, -Float32)
% truncate(+Float, -Float)
% truncate(+Decimal, -Decimal)
:- public truncate/2.
:- special(truncate/2, 'EvaluableRound', 1).

/**
 * floor(X): [ISO 9.1.7]
 * If X is a number then returns the rounding of X towards
 * negative infinity.
 */
% floor(+Integer, -Integer)
% floor(+Float32, -Float32)
% floor(+Float, -Float)
% floor(+Decimal, -Decimal)
:- public floor/2.
:- special(floor/2, 'EvaluableRound', 2).

/**
 * ceiling(X): [ISO 9.1.7]
 * If X is a number then returns the rounding of X towards
 * positive infinity.
 */
% ceiling integer -> integer
% ceiling(+Float32, -Float32)
% ceiling(+Float, -Float)
% ceiling(+Decimal, -Decimal)
:- public ceiling/2.
:- special(ceiling/2, 'EvaluableRound', 3).

/**
 * round(X): [ISO 9.1.7]
 * If X is a number then returns the rounding of X towards the nearest
 * integer. If the absolute fraction of the number is 0.5 then returns
 * the rounding away from zero.
 */
% round(+Integer, -Integer)
% round(+Float32, -Float32)
% round(+Float, -Float)
% round(+Decimal, -Decimal)
:- public round/2.
:- special(round/2, 'EvaluableRound', 4).

/**
 * X // Y: [ISO 9.1.7]
 * If X and Y are both numbers then the function returns
 * the truncated division of X by Y.
 */
% //(+Number, +Number, -Integer)
:- public // /3.
:- special(// /3, 'EvaluableRound', 5).

/**
 * X rem Y: [ISO 9.1.7]
 * If X and Y are both numbers then the function returns
 * the remainder of X by Y.
 */
% rem(+Integer, +Integer, -Decimal)
% rem(+Float32, +Float32, -Float32)
% rem(+Float, +Float, -Float)
% rem(+Decimal, +Decimal, -Decimal)
:- public rem/3.
:- special(rem/3, 'EvaluableRound', 6).

/**
 * X div Y: [TC2 9.1.3]
 * If X and Y are both numbers then the function returns
 * the floored division of X by Y.
 */
% div(+Number, +Number, -Integer)
:- public div/3.
:- special(div/3, 'EvaluableRound', 7).

/**
 * X mod Y: [ISO 9.1.7]
 * If X and Y are both numbers then the function returns
 * the modulus of X by Y.
 */
% mod(+Integer, +Integer, -Decimal)
% mod(+Float32, +Float32, -Float32)
% mod(+Float, +Float, -Float)
% mod(+Decimal, +Decimal, -Decimal)
:- public mod/3.
:- special(mod/3, 'EvaluableRound', 8).
