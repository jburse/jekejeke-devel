/**
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
 * If X is a number then the function returns an approximate rational number.
 */
:- public rationalize/2.
rationalize(X, Y) :-
   R is rational(X),
   rat_find(R, 0, 1, Y).

:- private rat_find/4.
rat_find(R, L, H, Y) :-
   rat_mediant(L, H, M),
   rat_find(R, L, H, M, Y).

:- private rat_find/5.
rat_find(R, _, _, M, Y) :-
   abs(R-M) < 1#4503599627370496, !,
   Y = M.
rat_find(R, _, H, M, Y) :-
   M < R, !,
   rat_find(R, M, H, Y).
rat_find(R, L, _, M, Y) :-
   rat_find(R, L, M, Y).

:- private rat_mediant/3.
rat_mediant(A#B, C#D, E#F) :- !,
   E is A+C,
   F is B+D.
rat_mediant(A, B#C, E#F) :- !,
   E is A+B,
   F is 1+C.
rat_mediant(A#B, C, E#F) :- !,
   E is A+C,
   F is B+1.
rat_mediant(A, B, E#2) :-
   E is A+B.
