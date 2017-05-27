/**
 * This module provides symbolic substitution. The substitution
 * operator subst/3 takes an original reduced expression, a replacement
 * variable and a replacement expression. It will re-execute the
 * constructors in the original reduced expression and replace every
 * occurrence of the replacement variable by the replacement expression.
 *
 * Example:
 * ?- X is A^2-B*A+B^2, Y is subst(X,A,2).
 * X is A^2-A*B+B^2,
 * Y is 4-2*B+B^2
 *
 * Since the constructors of the original expression are re-executed
 * the substitution operator might cause new partial evaluations or
 * simplifications. At the moment we provide substitution only for
 * elements, substitution for vectors and matrices has not yet been
 * implemented. Accordingly we do not yet support some special functions.
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

:- package(library(jekmin/frequent/leibniz)).
:- use_package(library(jekmin/frequent/groebner)).
:- use_package(library(jekmin/frequent/gauss)).

:- module(deriv, []).

:- use_module(../groebner/polynom).
:- use_module(../groebner/generic).

/*********************************************************************/
/* Substitution                                                      */
/*********************************************************************/

/**
 * subst(P, X, R, Q):
 * The predicate succeeds in Q with the substitution P[X/R].
 */
% ordered:subst(+Ordered, +Variable, +Internal, -Ordered)
:- public ordered:subst/4.
ordered:subst(X, _, _, X).

/**
 * subst(P, X, R, Q):
 * The predicate succeeds in Q with the substitution P[X/R].
 */
% variable:subst(+Variable, +Variable, +Internal, -Internal)
:- public variable:subst/4.
variable:subst(X, X, R, S) :- !,
   S = R.
variable:subst(X, _, _, X).

/**
 * subst(P, X, R, Q):
 * The predicate succeeds in Q with the substitution P[X/R].
 */
% polynom:subst(+Polynom, +Variable, +Internal, -Internal)
:- public polynom:subst/4.
polynom:subst(polynom(A,[N-B|L]), X, R, S) :-
   A @> X, !,
   H is subst(B,X,R),
   sys_coeff_subst(L, N, H, A, X, R, S).
polynom:subst(polynom(X,[N-A|L]), X, R, S) :- !,
   sys_poly_subst(L, N, A, R, S).
polynom:subst(X, _, _, X).

% sys_poly_subst(+Map, +Integer, +Internal, +Internal, -Internal)
:- private sys_poly_subst/5.
sys_poly_subst([M-B|L], N, A, R, S) :-
   user: -(N, M, K),
   H is R^K*A+B,
   sys_poly_subst(L, M, H, R, S).
sys_poly_subst([], N, A, R, S) :-
   S is R^N*A.

% sys_coeff_subst(+Map, +Integer, +Internal, +Variable, +Variable, +Internal, -Internal)
:- private sys_coeff_subst/7.
sys_coeff_subst([M-B|L], N, C, A, X, R, S) :-
   user: -(N, M, K),
   H is A^K*C+subst(B,X,R),
   sys_coeff_subst(L, M, H, A, X, R, S).
sys_coeff_subst([], N, C, A, _, _, S) :-
   S is A^N*C.

/**
 * subst(P, X, R, Q):
 * The predicate succeeds in Q with the substitution P[X/R].
 */
% fraction:subst(+Polynom, +Variable, +Internal, -Internal)
:- public fraction:subst/4.
fraction:subst(fraction(A,B), X, R, S) :-
   S is subst(A,X,R)/subst(B,X,R).
