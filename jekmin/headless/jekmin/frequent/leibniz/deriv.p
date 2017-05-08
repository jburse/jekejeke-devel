/**
 * This module provides symbolic differentiation. The differentiation
 * operator deriv/2 takes an original reduced expression and a
 * varying variable. It will re-execute the constructors in the
 * original reduced expression by their differential counterpart
 * constructors and treat variables different from the varying
 * variable as constant.
 *
 * Example:
 * ?- X is A^2-B*A+B^2, Y is deriv(X,A).
 * X is A^2-A*B+B^2,
 * Y is 2*A-B
 *
 * Since the differential counterpart constructors of the original
 * expression are re-executed the differentiation operator might
 * cause new partial evaluations or simplifications. At the moment
 * we provide differentiation only for elements, differentiation for
 * vectors and matrices has not yet been implemented. Accordingly
 * we do not yet support some special functions.
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

:- module(deriv, []).

:- use_module('../groebner/polynom').
:- use_module('../groebner/generic').

/*********************************************************************/
/* Integer                                                           */
/*********************************************************************/

/**
 * deriv(P, X, Q):
 * The predicate succeeds in Q with the derivation dP/dX.
 */
% integer:deriv(+Integer, +Variable, -Internal)
:- public integer:deriv/3.
integer:deriv(_, _, 0).

/*********************************************************************/
/* Rational                                                          */
/*********************************************************************/

/**
 * deriv(P, X, Q):
 * The predicate succeeds in Q with the derivation dP/dX.
 */
% rational:deriv(+Rational, +Variable, -Internal)
:- public rational:deriv/3.
rational:deriv(_, _, 0).

/*********************************************************************/
/* Variable                                                          */
/*********************************************************************/

/**
 * deriv(P, X, Q):
 * The predicate succeeds in Q with the derivation dP/dX.
 */
% variable:deriv(+Variable, +Variable, -Internal)
:- public variable:deriv/3.
variable:deriv(X, X, R) :- !,
   R = 1.
variable:deriv(X, _, X).

/*********************************************************************/
/* Polynom                                                           */
/*********************************************************************/

/**
 * deriv(P, X, Q):
 * The predicate succeeds in Q with the derivation dP/dX.
 */
% polynom:deriv(+Polynom, +Variable, -Internal)
:- public polynom:deriv/3.
polynom:deriv(polynom(A,B), X, R) :-
   A @> X, !,
   sys_coeff_deriv(B, X, H),
   sys_make_poly(A, H, R).
polynom:deriv(polynom(X,B), X, R) :- !,
   sys_poly_deriv(B, H),
   sys_make_poly(X, H, R).
polynom:deriv(_, _, 0).

% sys_poly_deriv(+Map, -Map)
:- private sys_poly_deriv/2.
sys_poly_deriv([0-_|L], R) :- !,
   sys_poly_deriv(L, R).
sys_poly_deriv([N-A|L], [M-B|R]) :-
   user: -(N, 1, M),
   B is N*A,
   sys_poly_deriv(L, R).
sys_poly_deriv([], []).

% sys_coeff_deriv(+Map, +Variable, -Map)
:- private sys_coeff_deriv/3.
sys_coeff_deriv([N-A|L], X, R) :-
   B is deriv(A,X),
   sys_coeff_deriv(L, X, H),
   sys_make_coeff(H, N, B, R).
sys_coeff_deriv([], _, []).

/*********************************************************************/
/* Fraction                                                          */
/*********************************************************************/

/**
 * deriv(P, X, Q):
 * The predicate succeeds in Q with the derivation dP/dX.
 */
% fraction:deriv(+Fraction, +Variable, -Internal)
:- public fraction:deriv/3.
fraction:deriv(fraction(A,B), X, R) :-
   R is (deriv(A,X)-deriv(B,X)*fraction(A,B))/B.

/*********************************************************************/
/* Radical                                                           */
/*********************************************************************/

/**
 * deriv(P, X, Q):
 * The predicate succeeds in Q with the derivation dP/dX.
 */
% radical:deriv(+Rational, +Variable, -Internal)
:- public radical:deriv/3.
radical:deriv(_, _, 0).
