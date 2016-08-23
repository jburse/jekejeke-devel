/**
 * We supply a number of binary and unary operations that deal
 * with round. We find different rounding modes and different
 * forms of division. The definitions are as follows where /F is
 * a hypothetical exact division. The result is always an integer:
 *
 * truncate:	towards zero
 * floor:		towards lower infinity
 * ceiling:		towards upper infinity
 * round:		towards nearest neighbour or then away from zero
 * X // Y = truncate(X /F Y)
 * X div Y = floor(X /F Y).
 * X rem Y = X – (X // Y) * Y
 * X mod Y = X – (X div Y) * Y
 *
 * Examples:
 * floor(-3)		--> -3
 * floor(-3.14)	--> -4
 * floor(-0d3.1415)	--> -4
 * 5 // 2		--> 2
 * 5.0 // 2.0		--> 2
 * 0d5.00 // 2		--> 2
 * (-5) // 2		--> -2
 * (-5) div 2		--> -3
 * 5 rem 2		--> 1
 * 5.0 rem 2.0		--> 1.0
 * 0d5.00 rem 2	--> 0d1.00
 * (-5) rem 2		--> -1
 * (-5) mod 2		--> 1
 *
 * Each integer division corresponds to a remainder function which is
 * defined again for integer, float and decimal arguments. The result
 * has the same type as the type of arguments. The same widening as
 * already defined for the basic operations applies as well.
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

:- public infix(//).
:- op(400, yfx, //).

:- public infix(div).
:- op(400, yfx, div).

:- public infix(mod).
:- op(400, yfx, mod).

:- public infix(rem).
:- op(400, yfx, rem).

/**
 * truncate(X): [ISO 9.1.7]
 * If X is a number then returns the rounding of X towards zero.
 */
% truncate: integer -> integer
% truncate: float -> integer
% truncate: decimal -> integer
:- public truncate/2.
:- special(truncate/2, 'EvaluableRound', 0).

/**
 * floor(X): [ISO 9.1.7]
 * If X is a number then returns the rounding of X towards
 * negative infinity.
 */
% floor: integer -> integer
% floor: float -> integer
% floor: decimal -> integer
:- public floor/2.
:- special(floor/2, 'EvaluableRound', 1).

/**
 * ceiling(X): [ISO 9.1.7]
 * If X is a number then returns the rounding of X towards
 * positive infinity.
 */
% ceiling integer -> integer
% ceiling: float -> integer
% ceiling: decimal -> integer
:- public ceiling/2.
:- special(ceiling/2, 'EvaluableRound', 2).

/**
 * round(X): [ISO 9.1.7]
 * If X is a number then returns the rounding of X towards the nearest
 * integer. If the absolute fraction of the number is 0.5 then returns
 * the rounding away from zero.
 */
% round: integer -> integer
% round: float -> integer
% round: decimal -> integer
:- public round/2.
:- special(round/2, 'EvaluableRound', 3).

/**
 * X // Y: [ISO 9.1.7]
 * If X and Y are both numbers then the function returns
 * the truncated division of X by Y.
 */
% // : integer x integer -> integer
% // : float x float -> integer
% // : decimal x decimal -> integer
:- public // /3.
:- special(// /3, 'EvaluableRound', 4).

/**
 * X rem Y: [ISO 9.1.7]
 * If X and Y are both numbers then the function returns
 * the remainder of X by Y.
 */
% rem : integer x integer -> integer
% rem : float x float -> float
% rem : decimal x decimal -> decimal
:- public rem/3.
:- special(rem/3, 'EvaluableRound', 5).

/**
 * X div Y: [TC2 9.1.3]
 * If X and Y are both numbers then the function returns
 * the floored division of X by Y.
 */
% div : integer x integer -> integer
% div : float x float -> integer
% div : decimal x decimal -> integer
:- public div/3.
:- special(div/3, 'EvaluableRound', 6).

/**
 * X mod Y: [ISO 9.1.7]
 * If X and Y are both numbers then the function returns
 * the modulus of X by Y.
 */
% mod : integer x integer -> integer
% mod : float x float -> float
% mod : decimal x decimal -> decimal
:- public mod/3.
:- special(mod/3, 'EvaluableRound', 7).
