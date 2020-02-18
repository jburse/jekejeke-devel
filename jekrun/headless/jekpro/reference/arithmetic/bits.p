/**
 * We provide bitwise operations defined on integers. They are not
 * defined for floats or decimals. The bitwise operations work for
 * negative and positive integers including zero. They also work for
 * unbounded integers. The bit pattern that corresponds to an integer
 * can be viewed as infinitely extending to the left, for positive
 * integers and zero by 0’s and for negative integers by 1’s. The
 * bitwise operations then work on the corresponding binary digits:
 *
 * Examples:
 * 			-11	=	  …10101
 * 	 			  2	=	    …010
 * 	(-11) \/ 2	-->	 -9	=	  …10111
 * 	(-11) /\ 2	-->	  0	=	      …0
 * 	(-11) >> 2	-->	 -3	=	    …101
 * 	(-11) << 2	-->	-44	=	…1010100
 * 	\ (-11)	-->	 10	=	  …01010
 *
 * The displacements n in the shift operations are restricted to
 * the range -2147483648 =< n =< 2147483647. When the displacement
 * is outside this range an exception is thrown. A negative
 * displacement shifts in the opposite direction.
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

:- public infix(/\).
:- op(500, yfx, /\).

:- public infix(\/).
:- op(500, yfx, \/).

:- public infix(xor).
:- op(400, yfx, xor).

:- public infix(<<).
:- op(400, yfx, <<).

:- public infix(>>).
:- op(400, yfx, >>).

:- public prefix(\).
:- op(200, fy, \).

/**
 * \ X: [ISO 9.4.5]
 * If X is an integer then returns the bitwise complement of X.
 */
% \(+Integer, -Integer)
:- public (\)/2.
:- special((\)/2, 'EvaluableBits', 0).

/**
 * X /\ Y: [ISO 9.4.3]
 * If X and Y are both integers then the function returns the bitwise X and Y.
 */
% /\(+Integer, +Integer, -Integer)
:- public /\ /3.
:- special(/\ /3, 'EvaluableBits', 1).

/**
 * X \/ Y: [ISO 9.4.4]
 * If X and Y are both integers then the function returns the bitwise X or Y.
 */
% \/(+Integer, +Integer, -Integer)
:- public \/ /3.
:- special(\/ /3, 'EvaluableBits', 2).

/**
 * X xor Y: [TC2 9.4.6]
 * If X and Y are both integers then the function returns the
 * bitwise exclusive X or Y.
 */
% xor(+Integer, +Integer, -Integer)
:- public xor/3.
:- special(xor/3, 'EvaluableBits', 3).

/**
 * X << Y: [ISO 9.4.2]
 * If X and Y are both integers then the function returns X
 * shifted by Y places left.
 */
% <<(+Integer, +Integer, -Integer)
:- public << /3.
:- special(<< /3, 'EvaluableBits', 4).

/**
 * X >> Y: [ISO 9.4.1]
 * If X and Y are both integers then the function returns X
 * shifted by Y places right.
 */
% >>(+Integer, +Integer, -Integer)
:- public >> /3.
:- special(>> /3, 'EvaluableBits', 5).

/**
 * gcd(X, Y):
 * If X and Y are both integers then the function returns the greatest
 * common divisor of X and Y.
 */
% gcd(+Integer, +Integer, -Integer)
:- public gcd/3.
:- special(gcd/3, 'EvaluableBits', 6).

/**
 * lcm(X, Y):
 * If X and Y are both integers then the function returns the least
 * common multiple of X and Y.
 */
% lcm(+Integer, +Integer, -Integer)
:- public lcm/3.
lcm(_, 0, R) :- !, =(R, 0).
lcm(0, _, R) :- !, =(R, 0).
lcm(X, Y, R) :- R is X//gcd(X, Y)*Y.

/**
 * msb(X):
 * If X is an integer then the function returns the most significant bit.
 */
:- public msb/2.
:- special(msb/2, 'EvaluableBits', 7).
