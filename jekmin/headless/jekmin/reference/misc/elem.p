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
 * isqrt: integer -> integer
 *
 * The ulp operation makes use of the ulp() function of the Java Math
 * library. The gcd operation implements a binary gcd algorithm for
 * 32-bit integers and otherwise delegates to the Java BigInteger
 * gcd operation implementation. The isqrt evaluable function uses a
 * logarithmic bisection method.
 *
 * Examples:
 * ulp(0)          --> 1
 * ulp(0.0)        --> 4.9E-324
 * ulp(0d0.00)     --> 0d0.01
 * gcd(36,24)      --> 12
 * isqrt(7)        --> 8
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
:- public isqrt/2.
isqrt(X, _) :-
   X < 0,
   throw(error(evaluation_error(undefined),_)).
isqrt(0, R) :- !,
   R = 0.
isqrt(X, Y) :-
   Lo is 1<<((bitlength(X)-1)//2),
   Hi is Lo*2,
   sys_bisect(Lo, Hi, X, Y).

% sys_bisect(+Integer, +Integer, +Integer, -Integer)
:- private sys_bisect/4.
sys_bisect(Lo, Hi, X, Y) :-
   Lo+1 < Hi, !,
   M is (Lo+Hi)//2,
   S is M*M,
   sys_bisect(S, Lo, Hi, M, X, Y).
sys_bisect(Lo, _, _, Lo).

% sys_bisect(+Integer, +Integer, +Integer, +Integer, +Integer, -Integer)
:- private sys_bisect/6.
sys_bisect(S, Lo, _, M, X, Y) :-
   S > X, !,
   sys_bisect(Lo, M, X, Y).
sys_bisect(S, _, Hi, M, X, Y) :-
   S < X, !,
   sys_bisect(M, Hi, X, Y).
sys_bisect(_, _, _, Y, _, Y).

/**
 * isqrt(X, Y, Z):
 * The predicate succeeds in Z with the integer square root of X/Y.
 */
:- public isqrt/3.
isqrt(X, _, _) :-
   X < 0,
   throw(error(evaluation_error(undefined),_)).
isqrt(X, Y, R) :-
   X < Y, !,
   R = 0.
isqrt(X, Y, Z) :-
   Lo is 1<<((bitlength(X)-1-bitlength(Y-1))//2),
   Hi is Lo*4,
   sys_bisect(Lo, Hi, X, Y, Z).

% sys_bisect(+Integer, +Integer, +Integer, +Integer, -Integer)
:- private sys_bisect/5.
sys_bisect(Lo, Hi, X, Y, Z) :-
   Lo+1 < Hi, !,
   M is (Lo+Hi)//2,
   S is Y*M*M,
   sys_bisect(S, Lo, Hi, M, X, Y, Z).
sys_bisect(Lo, _, _, _, Lo).

% sys_bisect(+Integer, +Integer, +Integer, +Integer, +Integer, +Integer, -Integer)
:- private sys_bisect/7.
sys_bisect(S, Lo, _, M, X, Y, Z) :-
   S > X, !,
   sys_bisect(Lo, M, X, Y, Z).
sys_bisect(S, _, Hi, M, X, Y, Z) :-
   S < X, !,
   sys_bisect(M, Hi, X, Y, Z).
sys_bisect(_, _, _, Z, _, _, Z).
