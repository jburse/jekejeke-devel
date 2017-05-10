/**
 * This module provides ring elements. The module realizes a base class
 * for the classes repre-sented by the module ordered from this module
 * and the module variable and the module polynom from the
 3 package groebner.
 *
 * Examples:
 * ?- quorem(X^2-1,X-1,Q,R).
 * Q is 1+X,
 * R is 0
 * ?- quorem(X^2-2*Y^2,X-Y,Q,R).
 * Q is 2*X+2*Y,
 * R is -X^2
 *
 * Common factors in polynomial fractions are determined and cancelled
 * in the module fraction from the package groebner. The realized Gröbner
 * Basis algorithm uses multivariate polynomial division which we expose
 * here by the predicate quorem/4.
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

:- package(library(jekmin/frequent/gauss)).

:- module(ring, []).
:- reexport(element).

:- use_module(../groebner/generic).
:- use_module(../groebner/polynom).

:- use_module(library(experiment/trail)).
:- use_module(library(basic/lists)).

/*********************************************************************/
/* Facade                                                            */
/*********************************************************************/

/**
 * quorem(A, B, Q, R):
 * The predicate succeeds with quotient Q and remainder R of A divided by B.
 */
:- public quorem/4.
quorem(A, B, Q, R) :-
   X is A,
   Y is B,
   sys_poly_send(X, gen_div, [Y,Q,R]).

/**
 * gen_div(A, B, Q, R):
 * The predicate succeeds with quotient Q and remainder R of A divided by B.
 */
% gen_div(+Ring, +Internal, -Internal, -Internal)
:- public gen_div/4.
gen_div(A, B, Q, R) :-
   sys_poly_div(A, B, Q, R).

/**
 * reduced(A, R, F):
 * The predicate succeeds with reduced R and factor F of A.
 */
:- public reduced/3.
reduced(A, R, F) :-
   X is A,
   sys_poly_send(X, gen_red, [R,F]).

/**
 * gen_red(A, R, F):
 * The predicate succeeds with reduced R and factor F of A.
 */
% gen_red(+Ring, -Internal, -Ordered)
:- public gen_red/3.
gen_red(A, R, F) :-
   sys_poly_reduced(A, R, F).

/*********************************************************************/
/* Division                                                          */
/*********************************************************************/

/**
 * sys_poly_div(F, G, K, M):
 * The predicate succeeds in K and M, with the separation of
 * F along G such that F = K*G+M.
 */
% sys_poly_div(+Internal, +Internal, -Internal, -Internal)
:- public sys_poly_div/4.
sys_poly_div(F, G, K, M) :-
   sys_poly_head(G, H),
   sys_poly_comb(F, H, I, N),
   I \== 0, !,
   J is N-I*(G-H),
   sys_poly_div(J, G, L, M),
   K is I+L.
sys_poly_div(F, _, 0, F).

/**
 * sys_poly_head(P, M):
 * The predicate succeeds in M with the greatest monomial of P.
 */
% sys_poly_head(+Internal, -Monomial)
:- public sys_poly_head/2.
sys_poly_head(X, R) :-
   sys_freezer(X), !,
   R = polynom(X,[1-1]).
sys_poly_head(polynom(A,[N-B|_]), R) :- !,
   sys_poly_head(B, C),
   R = polynom(A,[N-C]).
sys_poly_head(X, X).

/**
 * sys_poly_factor(P, F):
 * The predicate succeeds in F with the factor of P.
 */
% sys_poly_factor(+Internal, -Ordered)
:- public sys_poly_factor/2.
sys_poly_factor(X, R) :-
   sys_freezer(X), !,
   R = 1.
sys_poly_factor(polynom(_,[_-B|_]), R) :- !,
   sys_poly_factor(B, R).
sys_poly_factor(X, X).

/*********************************************************************/
/* Polynomial Combing                                                */
/*********************************************************************/

/**
 * sys_poly_comb(P, M, K, N):
 * The predicate succeeds in K and N, with the separation of
 * P along M such that P = K*M+N.
 */
% sys_poly_comb(+Internal, +Monomial, -Internal, -Internal)
:- public sys_poly_comb/4.
sys_poly_comb(A, polynom(C,D), K, M) :-
   sys_freezer(A),
   A @> C, !,
   sys_coeff_comb([1-1], polynom(C,D), R, S),
   sys_make_poly(A, R, K),
   sys_make_poly(A, S, M).
sys_poly_comb(A, polynom(A,[N-C]), K, M) :-
   sys_freezer(A), !,
   sys_same_comb([1-1], N, C, R, S),
   sys_make_poly(A, R, K),
   sys_make_poly(A, S, M).
sys_poly_comb(polynom(A,B), polynom(C,D), K, M) :-
   A @> C, !,
   sys_coeff_comb(B, polynom(C,D), R, S),
   sys_make_poly(A, R, K),
   sys_make_poly(A, S, M).
sys_poly_comb(polynom(A,B), polynom(A,[N-C]), K, M) :- !,
   sys_same_comb(B, N, C, R, S),
   sys_make_poly(A, R, K),
   sys_make_poly(A, S, M).
sys_poly_comb(X, polynom(_,_), K, M) :- !,
   K = 0,
   M = X.
sys_poly_comb(X, Y, K, 0) :-
   K is X/Y.

% sys_same_comb(+List, +Integer, +Monomial, -List, -List) :-
:- private sys_same_comb/5.
sys_same_comb([N-A|L], J, G, P, Q) :-
   user:(N >= J), !,
   sys_poly_comb(A, G, K, M),
   sys_same_comb(L, J, G, R, S),
   user: -(N, J, I),
   sys_make_coeff(R, I, K, P),
   sys_make_coeff(S, N, M, Q).
sys_same_comb(L, _, _, [], L).

% sys_coeff_comb(+List, +Monomial, -List, -List)
:- private sys_coeff_comb/4.
sys_coeff_comb([N-A|L], G, P, Q) :-
   sys_poly_comb(A, G, K, M),
   sys_coeff_comb(L, G, R, S),
   sys_make_coeff(R, N, K, P),
   sys_make_coeff(S, N, M, Q).
sys_coeff_comb([], _, [], []).

/*********************************************************************/
/* Reduction                                                         */
/*********************************************************************/

% sys_poly_reduced(+Ring, -Internal, -Ordered)
:- public sys_poly_reduced/3.
sys_poly_reduced(A, R, F) :-
   sys_poly_factor(A, K),
   K \== 1,
   K \== -1, !,
   L is 1/K,
   B is L*A,
   sys_poly_reduced2(B, K, R, F).
sys_poly_reduced(A, R, F) :-
   sys_poly_reduced2(A, 1, R, F).

% sys_poly_reduced2(+Internal, +Ordered, -Internal, -Ordered)
:- public sys_poly_reduced2/4.
sys_poly_reduced2(R, F, T, H) :-
   sys_poly_sign(R, S),
   S \== 1, !,
   T is -R,
   H is -F.
sys_poly_reduced2(R, F, R, F).

% sys_poly_sign(+Internal -Integer)
:- public sys_poly_sign/2.
sys_poly_sign(X, R) :-
   integer(X), !,
   user:sign(X, R).
sys_poly_sign(rational(A,_), R) :- !,
   user:sign(A, R).
sys_poly_sign(radical(0,[_-S|_]), R) :- !,
   user:sign(S, R).
sys_poly_sign(radical(A,_), R) :- !,
   sys_poly_sign(A, R).
sys_poly_sign(X, R) :-
   sys_freezer(X), !,
   R = 1.
sys_poly_sign(polynom(_,L), R) :-
   last(L, _-B),
   sys_poly_sign(B, R).
