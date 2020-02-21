/**
 * This module provides rational number approximations. In contrast to
 * rational/1 the number of bits in the result is minimized so that the
 * target is still reached. The evaluable functions rationalize/1 and
 * rationalize32/1 find rational numbers wih fewer bits that approximate
 * 64-bit respectively 32-bit floating point numbers.
 *
 * Examples:
 * ?- X is rationalize(8*(pi-3)).
 * X = 121642183#107387442
 * ?- X is rationalize32(8*(pi-3)).
 * X = 4591#4053
 *
 * The current realization uses an integer algorithm based on Euclid's
 * and it does also use rational arithmetic. Since our rational arithmetic
 * is still slow and since the algorithm doesn't use floating points
 * internally, it is relatively slow compared to other implementations.
 * Again this might change in the future.
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

:- package(library(jekpro/frequent/advanced)).

:- module(approx, []).
:- reexport(library(arithmetic/ratio)).

/**
 * rationalize(X):
 * If X is a number then the function returns an approximate
 * rational number. The target precision is epsilon/2.
 */
% rationalize(+Number, -Rational)
:- public rationalize/2.
rationalize(X, S) :-
   R is rational(X),
   rational(R, V, W),
   P is R-1#9007199254740992,
   Q is R+1#9007199254740992,
   rat_iter(V#W, 1#0, 0#1, P, Q, S).

/**
 * rationalize32(X):
 * If X is a number then the function returns an approximate
 * rational number. The target precision is epsilon32/2.
 */
% rationalize32(+Number, -Rational)
:- public rationalize32/2.
rationalize32(X, S) :-
   R is rational(X),
   rational(R, V, W),
   P is R-1#16777216,
   Q is R+1#16777216,
   rat_iter(V#W, 1#0, 0#1, P, Q, S).

/**
 * rat_iter(R, L, H, P, Q, S):
 * The predicate succeeds in S with the first continued fraction development
 * of R that is inside the interval (P,Q). Euclids algoritm is used and the
 * interval (L,H) is used to develop the stern-brocot branch.
 */
:- private rat_iter/6.
rat_iter(_, X, _, Y, Z, X) :-
   X \= 1#0,
   Y < X, X < Z, !.
rat_iter(_#0, X, _, _, _, X) :- !.
rat_iter(V#W, M#N, P#Q, Y, Z, X) :-
   user:divmod(V, W, D, U),
   user: *(D, M, H),
   user: +(H, P, A),
   user: *(D, N, J),
   user: +(J, Q, B),
   rat_iter(W#U, A#B, M#N, Y, Z, X).
