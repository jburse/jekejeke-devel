/**
 * We provide a couple of additional elementary operations. The ulp
 * operation is defined for integer, float and decimal. It returns a
 * result of the same type as its argument. The gcd operation is
 * currently only defined for integers. It returns a result that
 * is an integer.
 *
 * ulp: integer -> integer             isqrt: integer -> integer
 * ulp: float -> float                 sqrtrem: integer -> integer^2
 * ulp: decimal -> decimal             divmod : number^2 -> number^2
 * gcd: integer^2 -> integer
 * lcm: integer^2 -> integer
 *
 * The ulp operation makes use of the ulp() function of the Java Math
 * library. The gcd operation implements a binary gcd algorithm for
 * 32-bit integers and otherwise delegates to the Java BigInteger
 * gcd operation implementation.
 *
 * Examples:
 * ulp(0)          --> 1               isqrt(7)        --> 2
 * ulp(0.0)        --> 4.9E-324        sqrtrem(7)      --> (2,3)
 * ulp(0d0.00)     --> 0d0.01          divmod(12,-7)   --> (-1,5)
 * gcd(36,24)      --> 12
 * lcm(36,24)      --> 72
 *
 * The evaluable function isqrt/1 and the predicate sqrtrem/3 use
 * the fast Zimmerman method of finding an integer root. The predicate
 * divmod/4 returns both the quotient and remainder of a division.
 * This is faster than invoking the ISO core standard evaluable
 * functions (//)/2 and (rem)/2 separately.
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

:- package(library(jekmin/reference/misc)).
:- use_package(foreign(jekmin/reference/misc)).

:- module(elem, []).
:- use_module(bits).

/**
 * ulp(X, Y):
 * The predicate succeeds in Y with the unit of least precision of
 * the number X.
 */
:- public ulp/2.
:- special(ulp/2, 'SupplementElem', 0).

/**
 * gcd(X, Y, Z):
 * The predicate succeeds in Z with the greatest common divisor of the
 * integer X and the integer Y.
 */
:- public gcd/3.
:- special(gcd/3, 'SupplementElem', 1).

/**
 * lcm(X, Y, Z):
 * The predicate succeeds in Z with the least common multiple of the
 * integer X and the integer Y.
 */
:- public lcm/3.
lcm(_, 0, R) :- !,
   R = 0.
lcm(0, _, R) :- !,
   R = 0.
lcm(X, Y, R) :-
   R is X//gcd(X,Y)*Y.

/**
 * isqrt(X, Y):
 * The predicate succeeds in Y with the integer square root of X.
 */
% isqrt(+Integer, -Integer)
:- public isqrt/2.
isqrt(X, _) :-
   X < 0,
   throw(error(evaluation_error(undefined),_)).
isqrt(X, Y) :-
   zimmerman(X, Y, _).

/**
 * sqrtrem(X, Y, Z):
 * The predicte suceeds in Y with integer square root of X
 * and in Z with the corresponding remainder.
 */
% sqrtrem(+Integer, -Integer, -Integer)
:- public sqrtrem/3.
sqrtrem(X, _, _) :-
   X < 0,
   throw(error(evaluation_error(undefined),_)).
sqrtrem(X, Y, Z) :-
   zimmerman(X, Y, Z).

:- private zimmerman/3.
zimmerman(0, X, Y) :- !,
   X = 0,
   Y = 0.
zimmerman(1, X, Y) :- !,
   X = 1,
   Y = 0.
zimmerman(2, X, Y) :- !,
   X = 1,
   Y = 1.
zimmerman(3, X, Y) :- !,
   X = 1,
   Y = 2.
zimmerman(N, X, Y) :-
   K is (bitlength(N)+1)//4,
   N1 is N>>(2*K),
   N2 is N/\(1<<(2*K)-1),
   zimmerman(N1, X2, Y2),
   X3 is X2<<K,
   Y3 is Y2<<(2*K)+N2,
   newton(X3, Y3, X, Y).

:- private newton/4.
newton(X, Y, X3, Y3) :-
   Q is Y//X>>1,
   Q =\= 0, !,
   X2 is X+Q,
   Y2 is Y-2*X*Q-Q^2,
   newton(X2, Y2, X3, Y3).
newton(X, Y, X3, Y3) :-
   schoolboy(X, Y, X3, Y3).

:- private schoolboy/4.
schoolboy(X3, Y3, X, Y) :-
   Y3 >= 0, !,
   X = X3,
   Y = Y3.
schoolboy(X3, Y3, X, Y) :-
   X is X3-1,
   Y is Y3+2*X3-1.

/**
 * iroot(X, Y, Z):
 * The predicate succeeds in Z with the Y-th root of X.
 */
% iroot(+Integer, +Integer, -Integer)
:- public iroot/3.
iroot(X, _, _) :-
   X < 0,
   throw(error(evaluation_error(undefined),_)).
iroot(_, Y, _) :-
   Y =< 0,
   throw(error(evaluation_error(undefined),_)).
iroot(0, _, Z) :- !,
   Z = 0.
iroot(X, 1, Z) :- !,
   Z = X.
iroot(X, Y, Z) :-
   Lo is 1<<((bitlength(X)-1)//Y),
   Hi is Lo*2,
   iroot(Lo, Hi, X, Y, Z).

% iroot(+Integer, +Integer, +Integer, +Integer, -Integer)
:- private iroot/5.
iroot(Lo, Hi, X, Y, Z) :-
   Lo+1 < Hi, !,
   M is (Lo+Hi)//2,
   S is M^Y,
   iroot(S, Lo, Hi, M, X, Y, Z).
iroot(Lo, _, _, _, Lo).

% iroot(+Integer, +Integer, +Integer, +Integer, +Integer, +Integer, -Integer)
:- private iroot/7.
iroot(S, Lo, _, M, X, Y, Z) :-
   S > X, !,
   iroot(Lo, M, X, Y, Z).
iroot(S, _, Hi, M, X, Y, Z) :-
   S < X, !,
   iroot(M, Hi, X, Y, Z).
iroot(_, _, _, Z, _, _, Z).

/**
 * divmod(X, Y, Z, T):
 * The predicate succeeds in Z with the division of X by Y,
 * and in T with the modulo of X by Y.
 */
:- public divmod/4.
:- special(divmod/4, 'SpecialElem', 0).
