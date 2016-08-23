/**
 * This module provides additional integer predicates. The predicates
 * between/3 and above/2 allow enumerating integers. The predicate
 * plus/3 provides a multi-directional addition.
 *
 * Examples:
 * ?- between(1, 3, X).
 * X = 1 ;
 * X = 2 ;
 * X = 3
 * ?- between(1, 3, 4).
 * No
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

:- package(library(jekpro/frequent/advanced)).

:- module(arith, []).

/**
 * between(L, H, X):
 * The predicate succeeds for every integer X between the two integers L and H.
 */
% between(+Integer, +Integer, -Integer)
:- public between/3.
between(L, _, _) :-
   var(L),
   throw(error(instantiation_error,_)).
between(L, _, _) :-
   \+ integer(L),
   throw(error(type_error(integer,L),_)).
between(_, H, _) :-
   var(H),
   throw(error(instantiation_error,_)).
between(_, H, _) :-
   \+ integer(H),
   throw(error(type_error(integer,H),_)).
between(L, H, X) :-
   var(X), !,
   L =< H,
   between2(L, H, X).
between(L, H, X) :-
   integer(X), !,
   L =< X, X =< H.
between(_, _, X) :-
   throw(error(type_error(integer,X),_)).

% between2(+Integer, +Integer, -Integer)
:- private between2/3.
between2(L, H, X) :-
   (  L = H -> !; true),
   X = L.
between2(L, H, X) :-
   Y is L + 1,
   between2(Y, H, X).

/**
 * above(L, X):
 * The predicate succeeds for every integer X above the integer L.
 */
% above(+Integer, -Integer)
:- public above/2.
above(L, _) :-
   var(L),
   throw(error(instantiation_error,_)).
above(L, _) :-
   \+ integer(L),
   throw(error(type_error(integer,L),_)).
above(L, X) :-
   var(X), !,
   above2(L, X).
above(L, X) :-
   integer(X), !, L =< X.
above(_, X) :-
   throw(error(type_error(integer,X),_)).

:- private above2/2.
above2(L, X) :-
   X = L.
above2(L, X) :-
   Y is L + 1,
   above2(Y, X).

/**
 * plus(A, B, C):
 * The predicate succeeds for numbers A, B and C such that A+B equals C.
 * At least two arguments have to be instantiated.
 */
% plus(+Number, +Number, -Number)
:- public plus/3.
plus(A, B, C) :-
   var(C), !,
   C is A + B.
plus(A, B, C) :-
   var(A), !,
   A is C - B.
plus(A, B, C) :-
   B is C - A.
