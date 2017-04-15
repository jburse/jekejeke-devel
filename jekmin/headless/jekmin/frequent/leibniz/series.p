/**
 * Series expansion.
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
 * The predicate succeeds in Q with the maclaurin series
 * of P along the variable X for N summands.
 */
% element:taylor(+Internal, +Variable, +Integer, -Internal)
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

/**
 * taylor(P, X, N, R, Q):
 * The predicate succeeds in Q with the taylor series
 * of P along the variable X for N summands at point R.
 */
% element:taylor(+Internal, +Variable, +Integer, +Internal, -Internal)
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
 * The predicate suceeds in Q with the laurent series
 * of P along the variable X for N summands.
 */
% element:laurent(+Internal, +Variable, +Integer, -Internal)
:- public element:laurent/4.
element:laurent(P, X, N, R) :-
   H is subst(P,X,1/Y),
   J is taylor(H,Y,N),
   R is subst(J,Y,1/X).

/**
 * laurent(P, X, N, R, Q):
 * The predicate suceeds in Q with the laurent series
 * of P along the variable X for N summands at point R.
 */
% element:laurent(+Internal, +Variable, +Integer, +Internal, -Internal)
:- public element:laurent/5.
element:laurent(P, X, N, R, S) :-
   H is subst(P,X,1/Y),
   J is taylor(H,Y,N,R),
   S is subst(J,Y,1/X).
