/**
 * We provide a couple of additional elementary operations. The ulp
 * operation is defined for integer, float and decimal. It returns a
 * result of the same type as its argument. The gcd operation is
 * currently only defined for integers. It returns a result that
 * is an integer.
 *
 * ulp: integer -> integer             isqrt: integer -> integer
 * ulp: float -> float                 sqrtrem: integer -> integer^2
 * ulp: decimal -> decimal             iroot: integer^2 -> integer
 * gcd: integer^2 -> integer           rootrem: integer^2 -> integer^2
 * lcm: integer^2 -> integer           divmod : number^2 -> number^2
 *
 * The ulp operation makes use of the ulp() function of the Java Math
 * library. The gcd operation implements a binary gcd algorithm for
 * 32-bit integers and otherwise delegates to the Java BigInteger
 * gcd operation implementation.
 *
 * Examples:
 * ulp(0)          --> 1               isqrt(7)        --> 2
 * ulp(0.0)        --> 4.9E-324        sqrtrem(7)      --> (2,3)
 * ulp(0d0.00)     --> 0d0.01          iroot(77,5)     --> 2
 * gcd(36,24)      --> 12              rootrem(77,5)   --> (2, 45)
 * lcm(36,24)      --> 72              divmod(12,-7)   --> (-1,5)
 *
 * The evaluable functions isqrt/1 and iroot/2 as well as the predicates
 * sqrtrem/3 and rootrem/4 use the fast Hacker method of finding an
 * integer root. The predicate divmod/4 returns both the quotient
 * and remainder of a division. This is faster than invoking the ISO
 * core standard eval-uable functions (//)/2 and (rem)/2 separately.
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
   hacker(X, Y, _).

/**
 * sqrtrem(X, Y, Z):
 * The predicate succeeds in Y with integer square root of X
 * and in Z with the corresponding remainder.
 */
% sqrtrem(+Integer, -Integer, -Integer)
:- public sqrtrem/3.
sqrtrem(X, _, _) :-
   X < 0,
   throw(error(evaluation_error(undefined),_)).
sqrtrem(X, Y, Z) :-
   hacker(X, Y, Z).

% hacker(+Integer, -Integer, -Integer)
:- private hacker/3.
hacker(0, X, Y) :- !,
   X = 0,
   Y = 0.
hacker(N, X2, Y2) :-
   J is bitlength(N)//2,
   I is max(J-52,0),
   U is N>>(2*I),
   V is integer(sqrt(U)),
   X is V<<I,
   Y is N-V^2<<(2*I),
   newton(X, Y, X2, Y2).

% newton(+Integer, +Integer, -Integer, -Integer)
:- private newton/4.
newton(X, Y, X3, Y3) :-
   Q is Y//(2*X),
   Q =\= 0, !,
   X2 is X+Q,
   Y2 is Y-(2*X+Q)*Q,
   newton(X2, Y2, X3, Y3).
newton(X, Y, X2, Y2) :-
   Y < 0, !,
   X2 is X-1,
   Y2 is Y+2*X-1.
newton(X, Y, X, Y).

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
iroot(X, 1, Z) :- !,
   Z = X.
iroot(X, 2, Z) :- !,
   hacker(X, Z, _).
iroot(X, Y, Z) :-
   hacker(X, Y, Z, _).

/**
 * rootrem(X, Y, Z, T):
 * The predicate succeeds in Z with the Y-th root of X
 * and in T with the corresponding remainder.
 */
:- public rootrem/4.
rootrem(X, _, _, _) :-
   X < 0,
   throw(error(evaluation_error(undefined),_)).
rootrem(_, Y, _, _) :-
   Y =< 0,
   throw(error(evaluation_error(undefined),_)).
rootrem(X, 1, Z, T) :- !,
   Z = X,
   T = 0.
rootrem(X, 2, Z, T) :- !,
   hacker(X, Z, T).
rootrem(X, Y, Z, T) :-
   hacker(X, Y, Z, T).

% hacker(+Integer, +Integer, -Integer, -Integer)
:- private hacker/4.
hacker(0, _, X, Y) :- !,
   X = 0,
   Y = 0.
hacker(N, M, X2, Y2) :-
   J is (bitlength(N)-1+M//2)//M,
   I is max(J-52,0),
   U is N>>(M*I),
   V is integer(U**(1/M)),
   X is V<<I,
   F is V^M<<(M*I),
   Y is N-F,
   newton(X, Y, F, M, X2, Y2).

% newton(+Integer, +Integer, +Integer, +Integer, -Integer, -Integer)
:- private newton/6.
newton(X, Y, F, M, X3, Y3) :-
   Q is X*Y//(M*F),
   Q =\= 0, !,
   X2 is X+Q,
   F2 is X2^M,
   Y2 is Y+(F-F2),
   newton(X2, Y2, F2, M, X3, Y3).
newton(X, Y, F, M, X2, Y2) :-
   Y < 0, !,
   X2 is X-1,
   F2 is X2^M,
   Y2 is Y+(F-F2).
newton(X, Y, _, _, X, Y).

/**
 * divmod(X, Y, Z, T):
 * The predicate succeeds in Z with the division of X by Y,
 * and in T with the modulo of X by Y.
 */
:- public divmod/4.
:- special(divmod/4, 'SpecialElem', 0).
