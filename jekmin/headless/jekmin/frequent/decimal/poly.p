/**
 * This module provides arithmetic multi precision functions. The arithmetic
 * expressions decimal/3, +/4, -/4, * /4, //4 and ^/4 delegate to the
 * module arith. The result of these expressions is the unlimited operation
 * rounded to the requested precision:
 *
 * Examples:
 * ?- X is mp((1001/1000)^99, 30).
 * X = 0d1.10401168603473323514396127584
 *
 * ?- X is mp(sqrt(2), 30).
 * X = 0d1.41421356237309504880168872422
 *
 * The expression sqrt/3 is implemented by a range reduction and a Taylor
 * series. The current implementation estimates the number of terms
 * needed for the Taylor series and then applies a Horner schema. Speed
 * and accuracy are not yet optimized.
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

:- package(library(jekmin/frequent/decimal)).

:- module(poly, []).
:- use_module(library(decimal/scale)).
:- use_module(library(decimal/arith)).
:- use_module(library(decimal/helper)).
:- use_module(multi).

/****************************************************************/
/* Sqrt                                                         */
/****************************************************************/

/**
 * mp_sqrt(X, P, Y):
 * The predicate succeeds in Y with sqrt(X) for precision P.
 */
% mp_sqrt(+Decimal, +Context, -Decimal)
:- private mp_sqrt/3.
mp_sqrt(X, _, Y) :- X =:= 0d0, !,
   Y = 0d0.
mp_sqrt(X, _, _) :- X < 0d0,
   throw(error(evaluation_error(undefined), _)).
mp_sqrt(X, P, Y) :-
   dec_decomp(X, D, M),
   bin_decomp(M, P, B, N),
   mp_sqrt(B, D, N, P, Y).

% mp_sqrt(+Integer, +Integer, +Decimal, +Context, -Decimal)
:- private mp_sqrt/5.
mp_sqrt(B, D, X, P, Y) :- X =:= 0d1, !,
   bin_dec_sqrt(B, D, P, Y).
mp_sqrt(B, D, X, P, Y) :- X > 0d1.4, !,
   mp_math(0d2/X, P, M),
   L is B+1,
   mp_math(M-0d1, P, Z),
   K is integer(ceiling(requested(P)/dec_log10(Z))),
   init_sqrt(K, Z, P, U),
   mp_sqrt(U, Z, P, H),
   bin_dec_sqrt(L, D, P, J),
   mp_math(J/H, P, Y).
mp_sqrt(B, D, X, P, Y) :-
   mp_math(X-0d1, P, Z),
   K is integer(ceiling(requested(P)/dec_log10(Z))),
   init_sqrt(K, Z, P, U),
   mp_sqrt(U, Z, P, H),
   bin_dec_sqrt(B, D, P, J),
   mp_math(J*H, P, Y).

% mp_sqrt(+Part, +Decimal, +Context, -Decimal)
:- private mp_sqrt/4.
mp_sqrt((0, S), _, _, S) :- !.
mp_sqrt(U, X, P, Y) :-
   next_sqrt(U, X, P, V),
   mp_sqrt(V, X, P, Y).

% init_sqrt(+Integer, +Decimal, +Context, -Part)
:- private init_sqrt/4.
init_sqrt(0, _, _, (0, 0d1)) :- !.
init_sqrt(K, _, P, (K, S)) :-
   mp_math((3-2*K)/(2*K), P, S).

% next_sqrt(+Part, +Decimal, +Context, -Part)
:- private next_sqrt/4.
next_sqrt((1, T), X, P, (0, S)) :- !,
   mp_math(T*X+0d1, P, S).
next_sqrt((L, T), X, P, (K, S)) :-
   K is L-1,
   mp_math((T*X+0d1)*(3-2*K)/(2*K), P, S).

/**
 * bin_dec_sqrt(B, D, P, X):
 * The predicate succeeds in X with sqrt(2)^B*sqrt(10)^D.
 */
% bin_dec_sqrt(+Integer, +Integer, +Context, -Decimal)
:- private bin_dec_sqrt/4.
bin_dec_sqrt(0, 0, _, X) :- !,
   X = 0d1.
bin_dec_sqrt(B, D, P, X) :-
   mp_math(sqrt2^B*sqrt10^D, P, X).

/****************************************************************/
/* Hooks                                                        */
/****************************************************************/

% mp_math(+Expression, +Context, -Decimal)
:- meta_predicate multi:mp_math(#(1), ?, ?).
:- multifile multi:mp_math/3.
:- public multi:mp_math/3.
multi:mp_math(V, _, _) :- var(V),
   throw(error(instantiation_error, _)).

/* conversion */
multi:mp_math(decimal(X), P, R) :- !,
   mp_math(X, P, H),
   mp_decimal(H, P, R).

/* arithmetic */
multi:mp_math(X+Y, P, R) :- !,
   mp_math(X, P, H),
   mp_math(Y, P, J),
   mp_add(H, J, P, R).
multi:mp_math(X-Y, P, R) :- !,
   mp_math(X, P, H),
   mp_math(Y, P, J),
   mp_sub(H, J, P, R).
multi:mp_math(X*Y, P, R) :- !,
   mp_math(X, P, H),
   mp_math(Y, P, J),
   mp_mul(H, J, P, R).
multi:mp_math(X/Y, P, R) :- !,
   mp_math(X, P, H),
   mp_math(Y, P, J),
   mp_slash(H, J, P, R).
multi:mp_math(X^Y, P, R) :- !,
   mp_math(X, P, H),
   mp_math(Y, P, J),
   mp_int_pow(H, J, P, R).

/* special */
multi:mp_math(sqrt(X), P, R) :- !,
   mp_math(decimal(X), P, H),
   mp_sqrt(H, P, R).

/* constants */
multi:mp_math(sqrt10, P, R) :- !,
   mp_memo(sqrt10, sqrt(5/4)^7*sqrt(32/25)^3, P, R).
multi:mp_math(sqrt2, P, R) :- !,
   mp_memo(sqrt2, sqrt(5/4)^2*sqrt(32/25), P, R).

/**
 * mp_abnormal(E):
 * The predicate succeeds when the expression E has a non-default
 * handling. This predicate is multi file and can be thus
 * extended.
 */
:- multifile multi:mp_abnormal/1.
:- public multi:mp_abnormal/1.
multi:mp_abnormal(decimal(_)).
multi:mp_abnormal(_+_).
multi:mp_abnormal(_-_).
multi:mp_abnormal(_*_).
multi:mp_abnormal(_/_).
multi:mp_abnormal(_^_).
multi:mp_abnormal(sqrt(_)).
multi:mp_abnormal(sqrt10).
multi:mp_abnormal(sqrt2).
