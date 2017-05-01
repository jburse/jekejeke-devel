/**
 * This module provides symbolic series development. The substitution
 * and the differential operator are the building blocks for the
 * development of series. The operator taylor/3 can be used to develop
 * a Taylor series. It takes an original reduced expression, a varying
 * variable and the number of desired summands.
 *
 * Examples:
 * ?- X is 1/(1+A), Y is taylor(X,A,5).
 * X is 1/(1+A),
 * Y is 1-A+A^2-A^3+A^4-A^5
 * ?- X is 1/(1+A), Y is laurent(X,A,5).
 * X is 1/(1+A),
 * Y is (1-A+A^2-A^3+A^4)/A^5
 *
 * By default the Taylor series is developed at the point zero (0),
 * known as Maclaurin series. There is a variant operator taylor/4
 * with a further argument for the point where the series should be
 * developed. The operator laurent/[3,4] produce a Laurent series.
 * Error handling is rudimentary. Cancellation does not yet generate
 * non-zero side conditions.
 *
 * We do not yet support some special functions. The series operators
 * are realized without any limes operator. Limes calculation is
 * implicit in our polynomial division since common roots are cancelled.
 * We do not yet provide some computation for the remainder term,
 * the convergence radius of the infinite series or a symbolic form
 * for the infinite series.
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
:- use_package(library(jekmin/frequent/gauss)).

:- module(series, []).

:- use_module('../groebner/generic').

/**
 * taylor(P, X, N, Q):
 * taylor(P, X, N, R, Q):
 * The predicate succeeds in Q with the Taylor series
 * of P along the variable X for N summands. The quinary
 * predicate allows specifying the point R.
 */
% element:taylor(+Element, +Variable, +Integer, -Internal)
:- public element:taylor/4.
element:taylor(P, X, N, R) :-
   sys_maclaurin_horner(P, X, 0, N, R).

% sys_maclaurin_horner(+Internal, +Variable, +Integer, +Integer, -Internal)
:- private sys_maclaurin_horner/5.
sys_maclaurin_horner(P, X, N, N, R) :- !,
   R is subst(P,X,0).
sys_maclaurin_horner(P, X, N, M, R) :-
   Q is deriv(P,X),
   user: +(N, 1, K),
   sys_maclaurin_horner(Q, X, K, M, H),
   R is X*H/K+subst(P,X,0).

% element:taylor(+Element, +Variable, +Integer, +Internal, -Internal)
:- public element:taylor/5.
element:taylor(P, X, N, R, S) :-
   sys_taylor_horner(P, X, 0, N, R, S).

% sys_taylor_horner(+Internal, +Variable, +Integer, +Integer, -Internal)
:- private sys_taylor_horner/6.
sys_taylor_horner(P, X, N, N, R, S) :- !,
   S is subst(P,X,R).
sys_taylor_horner(P, X, N, M, R, S) :-
   Q is deriv(P,X),
   user: +(N, 1, K),
   sys_taylor_horner(Q, X, K, M, R, H),
   S is (X-R)*H/K+subst(P,X,R).

/**
 * laurent(P, X, N, Q):
 * laurent(P, X, N, R, Q):
 * The predicate succeeds in Q with the Laurent series
 * of P along the variable X for N summands. The quinary
 * predicate allows specifying the point R.
 */
% element:laurent(+Element, +Variable, +Integer, -Internal)
:- public element:laurent/4.
element:laurent(P, X, N, R) :-
   H is subst(P,X,1/Y),
   J is taylor(H,Y,N),
   R is subst(J,Y,1/X).

% element:laurent(+Element, +Variable, +Integer, +Internal, -Internal)
:- public element:laurent/5.
element:laurent(P, X, N, R, S) :-
   H is subst(P,X,1/Y),
   J is taylor(H,Y,N,R),
   S is subst(J,Y,1/X).
