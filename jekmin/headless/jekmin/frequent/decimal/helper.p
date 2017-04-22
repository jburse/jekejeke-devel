/**
 * Some helpers.
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

:- package(library(jekmin/frequent/decimal)).
:- use_package(foreign(jekmin/frequent/decimal)).

:- module(helper, []).
:- use_module(library(decimal/scale)).
:- use_module(library(decimal/arith)).
:- use_module(library(misc/bits)).

/****************************************************************/
/* Precision Helper                                             */
/****************************************************************/

/**
 * dec_log10(X, N):
 * The predicate succeeds in N for the negated log10 of X.
 */
% dec_log10(+Decimal, -Float)
:- public dec_log10/2.
dec_log10(X, N) :-
   U is unscaled_value(X),
   S is scale(X),
   N is S-int_log10(U).

% int_log10(+Integer, -Float)
:- private int_log10/2.
int_log10(X, N) :-
   L is bitlength(X)-1,
   L > 52, !,
   K is L-52,
   N is (log(X>>K)+K*log2)/log10.
int_log10(X, N) :-
   N is log(X)/log10.

/**
 * log2(Y):
 * Predicate succeeds in Y with the float log(2).
 */
:- private log2/1.
:- special(log2/1, 'SpecialHelper', 0).

/**
 * log10(Y):
 * Predicate succeeds in Y with the float log(2).
 */
:- private log10/1.
:- special(log10/1, 'SpecialHelper', 1).

/****************************************************************/
/* Decomposition Helper                                         */
/****************************************************************/

/**
 * dec_decomp(X, E, M):
 * The predicate succeeds in E and M with the 10 exponent and mantissa of X.
 */
% dec_decomp(+Decimal, -Integer, -Decimal)
:- public dec_decomp/3.
dec_decomp(X, E, M) :-
   S is scale(X),
   P is precision(X)-1,
   E is P-S,
   E \== 0, !,
   U is unscaled_value(X),
   M is new_decimal(U,P).
dec_decomp(X, 0, X).

/**
 * bin_decomp(X, P, E, M):
 * The predicate succeeds in E and M with the 2 exponent and mantissa of X
 * with precision P.
 */
% bin_decomp(+Decimal, +Context, -Integer, -Decimal)
:- public bin_decomp/4.
bin_decomp(X, P, E, M) :-
   E is bitlength(integer(X))-1,
   E \== 0, !,
   K is 1<<E,
   mp_slash(X, K, P, M).
bin_decomp(X, _, 0, X).
