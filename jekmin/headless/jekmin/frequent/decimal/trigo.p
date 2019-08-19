/**
 * This module provides trigonometric multi precision functions. The
 * expressions sin/3, cos/3, atan/3, exp/3 and log/3 are implemented
 * by a range reduction and a Maclaurin respective Taylor series. The
 * current implementation estimates the number of terms needed for
 * the Maclaurin respective Taylor series and then applies a Horner schema.
 *
 * Examples:
 * ?- X is mp((1001/1000)**99, 30).
 * X = 0d1.10401168603473323514396127584
 *
 * ?- X is mp(4*acos(sqrt(1/2)), 30).
 * X = 0d3.14159265358979323846264338330
 *
 * The expression atan2/4 is bootstrapped from the expression atan/3.
 * Further the expressions asin/3 and acos/3 are then bootstrapped from
 * the expression atan2/4. On the other hand the expression ** /4 is
 * bootstrapped from the expressions exp/3 and log/3. Speed and accuracy
 * of the series and the bootstrapping is not yet optimized.
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

:- module(trigo, []).
:- use_module(library(decimal/scale)).
:- use_module(library(decimal/helper)).
:- use_module(multi).

/****************************************************************/
/* Sinus                                                        */
/****************************************************************/

/**
 * mp_sin(X, P, Y):
 * The predicate succeeds in Y with the sin of X for precision P.
 */
% mp_sin(+Decimal, +Context, -Decimal)
:- private mp_sin/3.
mp_sin(X, _, Y) :- X =:= 0d0, !,
   Y = 0d0.
mp_sin(X, P, Y) :- X < 0d0, !,
   mp_math(-sin(-X), P, Y).
mp_sin(X, P, Y) :- X > 0d0.40, !,
   mod_range(X, K, P, H),
   mp_sin_case(K, H, P, Y).
mp_sin(X, P, Y) :-
   K is integer(ceiling(requested(P)/(2*dec_log10(X)))),
   init_sin(K, X, P, U),
   mp_sin(U, X, P, Y).

% mp_sin_case(+Integer, +Decimal, +Context, -Decimal)
:- private mp_sin_case/4.
mp_sin_case(0, X, P, Y) :-
   mp_math(sin(X), P, Y).
mp_sin_case(1, X, P, Y) :-
   mp_math(sqrt1div2*(cos(X)+sin(X)), P, Y).
mp_sin_case(2, X, P, Y) :-
   mp_math(cos(X), P, Y).
mp_sin_case(3, X, P, Y) :-
   mp_math(sqrt1div2*(cos(X)-sin(X)), P, Y).
mp_sin_case(4, X, P, Y) :-
   mp_math(-sin(X), P, Y).
mp_sin_case(5, X, P, Y) :-
   mp_math(sqrt1div2*(-cos(X)-sin(X)), P, Y).
mp_sin_case(6, X, P, Y) :-
   mp_math(-cos(X), P, Y).
mp_sin_case(7, X, P, Y) :-
   mp_math(sqrt1div2*(-cos(X)+sin(X)), P, Y).

% mp_sin(+Part, +Decimal, +Context, -Decimal)
:- private mp_sin/4.
mp_sin((0, S), _, _, S) :- !.
mp_sin(U, X, P, Y) :-
   next_sin(U, X, P, V),
   mp_sin(V, X, P, Y).

% init_sin(+Integer, +Decimal, +Context, -Part)
:- private init_sin/4.
init_sin(K, X, P, (K, S)) :-
   (K mod 2 =:= 0 -> V = X; V = -X),
   mp_math(V/(2*K+1), P, S).

% next_sin(+Part, +Decimal, +Context, -Part)
:- private next_sin/4.
next_sin((L, T), X, P, (K, S)) :-
   K is L-1,
   (K mod 2 =:= 0 -> V = X; V = -X),
   mp_math((T*X*X/(2*K+2)+V)/(2*K+1), P, S).

% mod_range(+Decimal, -Integer, +Context, -Decimal)
:- private mod_range/4.
mod_range(X, K, P, Y) :-
   mp_math((X+pi*0d0.125)div(pi*0d0.25), P, I),
   K is I mod 8,
   mp_math(X-I*(pi*0d0.25), P, Y).

/****************************************************************/
/* Cosinus                                                      */
/****************************************************************/

/**
 * mp_cos(X, P, Y):
 * The predicate succeeds in Y with the cos of X for precision P.
 */
% mp_cos(+Decimal, +Context, -Decimal)
:- private mp_cos/3.
mp_cos(X, _, Y) :- X =:= 0d0, !,
   Y = 0d1.
mp_cos(X, P, Y) :- X < 0d0, !,
   mp_math(cos(-X), P, Y).
mp_cos(X, P, Y) :- X > 0d0.40, !,
   mod_range(X, K, P, H),
   mp_cos_case(K, H, P, Y).
mp_cos(X, P, Y) :-
   K is integer(ceiling(requested(P)/(2*dec_log10(X)))),
   init_cos(K, X, P, U),
   mp_cos(U, X, P, Y).

% mp_cos_case(+Integer, +Decimal, +Context, -Decimal)
:- private mp_cos_case/4.
mp_cos_case(0, X, P, Y) :-
   mp_math(cos(X), P, Y).
mp_cos_case(1, X, P, Y) :-
   mp_math(sqrt1div2*(cos(X)-sin(X)), P, Y).
mp_cos_case(2, X, P, Y) :-
   mp_math(-sin(X), P, Y).
mp_cos_case(3, X, P, Y) :-
   mp_math(sqrt1div2*(-cos(X)-sin(X)), P, Y).
mp_cos_case(4, X, P, Y) :-
   mp_math(-cos(X), P, Y).
mp_cos_case(5, X, P, Y) :-
   mp_math(sqrt1div2*(-cos(X)+sin(X)), P, Y).
mp_cos_case(6, X, P, Y) :-
   mp_math(sin(X), P, Y).
mp_cos_case(7, X, P, Y) :-
   mp_math(sqrt1div2*(cos(X)+sin(X)), P, Y).

% mp_cos(+Part, +Decimal, +Context, -Decimal)
:- private mp_cos/4.
mp_cos((0, S), _, _, S) :- !.
mp_cos(U, X, P, Y) :-
   next_cos(U, X, P, V),
   mp_cos(V, X, P, Y).

% init_cos(+Integer, +Decimal, +Context, -Part)
:- private init_cos/4.
init_cos(K, _, P, (K, S)) :-
   (K mod 2 =:= 0 -> V = 0d1; V = -0d1),
   mp_math(V/max(2*K, 1), P, S).

% next_cos(+Part, +Decimal, +Context, -Part)
:- private next_cos/4.
next_cos((L, T), X, P, (K, S)) :-
   K is L-1,
   (K mod 2 =:= 0 -> V = 0d1; V = -0d1),
   mp_math((T*X*X/(2*K+1)+V)/max(2*K, 1), P, S).

/****************************************************************/
/* Arcus Tangent                                                */
/****************************************************************/

/**
 * mp_atan(X, P, Y):
 * The predicate succeeds in Y with the atan of X for precision P.
 */
% mp_atan(+Decimal, +Context, -Decimal)
:- private mp_atan/3.
mp_atan(X, _, Y) :- X =:= 0d0, !,
   Y = 0d0.
mp_atan(X, P, Y) :- X < 0d0, !,
   mp_math(-atan(-X), P, Y).
mp_atan(X, P, Y) :- X > 0d1, !,
   mp_math(pi*0d0.5-atan(0d1/X), P, Y).
mp_atan(X, P, Y) :- X > 0d0.5, !,
   mp_math(pi*0d0.25-atan((0d1-X)/(0d1+X)), P, Y).
mp_atan(X, P, Y) :-
   K is integer(ceiling(requested(P)/(2*dec_log10(X)))),
   init_atan(K, X, P, U),
   mp_atan(U, X, P, Y).

% mp_atan(+Part, +Decimal, +Context, -Decimal)
:- private mp_atan/4.
mp_atan((0, S), _, _, S) :- !.
mp_atan(U, X, P, Y) :-
   next_atan(U, X, P, V),
   mp_atan(V, X, P, Y).

% init_atan(+Integer, +Decimal, +Context, -Part)
:- private init_atan/4.
init_atan(K, X, P, (K, S)) :-
   (K mod 2 =:= 0 -> V = X; V = -X),
   mp_math(V/(2*K+1), P, S).

% next_atan(+Part, +Decimal, +Context, -Part)
:- private next_atan/4.
next_atan((L, T), X, P, (K, S)) :-
   K is L-1,
   (K mod 2 =:= 0 -> V = X; V = -X),
   mp_math(T*X*X+V/(2*K+1), P, S).

/****************************************************************/
/* Exponential                                                  */
/****************************************************************/

/**
 * mp_exp(X, P, Y):
 * The predicate succeeds in Y with the exp of X for precision P.
 */
% mp_exp(+Decimal, +Context, -Decimal)
:- private mp_exp/3.
mp_exp(X, _, Y) :- X =:= 0d0, !,
   Y = 0d1.
mp_exp(X, P, Y) :- X < 0d0, !,
   mp_math(0d1/exp(-X), P, Y).
mp_exp(X, P, Y) :- X >= 0d1, !,
   K is integer(X),
   mp_math(exp(X-K)*e^K, P, Y).
mp_exp(X, P, Y) :- X > 0d0.5, !,
   mp_math(exp(X/0d2)^2, P, Y).
mp_exp(X, P, Y) :-
   K is integer(ceiling(requested(P)/dec_log10(X))),
   init_exp(K, X, P, U),
   mp_exp(U, X, P, Y).

% mp_exp(+Part, +Decimal, +Context, -Decimal)
:- private mp_exp/4.
mp_exp((0, S), _, _, S) :- !.
mp_exp(U, X, P, Y) :-
   next_exp(U, X, P, V),
   mp_exp(V, X, P, Y).

% init_exp(+Integer, +Decimal, +Context, -Part)
:- private init_exp/4.
init_exp(K, _, P, (K, S)) :-
   mp_math(0d1/max(K, 1), P, S).

% next_exp(+Part, +Decimal, +Context, -Part)
:- private next_exp/4.
next_exp((L, T), X, P, (K, S)) :-
   K is L-1,
   mp_math((T*X+0d1)/max(K, 1), P, S).

/****************************************************************/
/* Logarithm                                                    */
/****************************************************************/

/**
 * mp_log(X, P, Y):
 * The predicate succeeds in Y with the log of X for precision P.
 */
% mp_log(+Decimal, +Context, -Decimal)
:- private mp_log/3.
mp_log(X, _, _) :- X =:= 0d0,
   throw(error(evaluation_error(float_underflow), _)).
mp_log(X, _, _) :- X < 0d0,
   throw(error(evaluation_error(undefined), _)).
mp_log(X, P, Y) :-
   dec_decomp(X, D, M),
   bin_decomp(M, P, B, N),
   mp_log(B, D, N, P, Y).

% mp_log(+Integer, +Integer, +Decimal, +Context, -Decimal)
:- private mp_log/5.
mp_log(B, D, X, P, Y) :- X =:= 0d1, !,
   bin_dec_log(B, D, P, Y).
mp_log(B, D, X, P, Y) :- X > 0d1.4, !,
   mp_math(0d2/X, P, M),
   L is B+1,
   mp_math(M-0d1, P, Z),
   K is integer(ceiling(requested(P)/dec_log10(Z))),
   init_log(K, Z, P, U),
   mp_log(U, Z, P, H),
   bin_dec_log(L, D, P, J),
   mp_math(J-H, P, Y).
mp_log(B, D, X, P, Y) :-
   mp_math(X-0d1, P, Z),
   K is integer(ceiling(requested(P)/dec_log10(Z))),
   init_log(K, Z, P, U),
   mp_log(U, Z, P, H),
   bin_dec_log(B, D, P, J),
   mp_math(J+H, P, Y).

% mp_log(+Part, +Decimal, +Context, -Decimal)
:- private mp_log/4.
mp_log((0, S), _, _, S) :- !.
mp_log(U, X, P, Y) :-
   next_log(U, X, P, V),
   mp_log(V, X, P, Y).

% init_log(+Integer, +Decimal, +Context, -Part)
:- private init_log/4.
init_log(K, X, P, (K, S)) :-
   (K mod 2 =:= 0 -> V = X; V = -X),
   mp_math(V/(K+1), P, S).

% next_log(+Part, +Decimal, +Context, -Part)
:- private next_log/4.
next_log((L, T), X, P, (K, S)) :-
   K is L-1,
   (K mod 2 =:= 0 -> V = X; V = -X),
   mp_math(T*X+V/(K+1), P, S).

/**
 * bin_dec_log(B, D, P, X):
 * The predicate succeeds in X with B*log(2)+D*log(10).
 */
% bin_dec_log(+Integer, +Integer, +Context, -Decimal)
:- private bin_dec_log/4.
bin_dec_log(0, 0, _, X) :- !,
   X = 0d0.
bin_dec_log(B, D, P, X) :-
   mp_math(B*log2+D*log10, P, X).

/****************************************************************/
/* Arcus Tangent 2                                              */
/****************************************************************/

/**
 * mp_atan2(Y, X, P, Z):
 * The predicate succeeds in Z with the atan2 of Y and X for precision P.
 */
% mp_atan2(+Decimal, +Context, +Decimal, -Decimal)
:- private mp_atan2/4.
mp_atan2(Y, X, P, Z) :- X < 0d0, Y < 0d0, !,
   mp_math(-pi+atan2(-Y, -X), P, Z).
mp_atan2(Y, X, P, Z) :- X < 0d0, !,
   mp_math(pi-atan2(Y, -X), P, Z).
mp_atan2(Y, X, P, Z) :- Y < 0d0, !,
   mp_math(-atan2(-Y, X), P, Z).
mp_atan2(Y, X, P, Z) :- X < Y, !,
   mp_math(pi*0d0.5-atan(X/Y), P, Z).
mp_atan2(Y, X, P, Z) :-
   mp_math(atan(Y/X), P, Z).

/****************************************************************/
/* Hooks                                                        */
/****************************************************************/

% mp_math(+Expression, +Context, -Decimal)
:- meta_predicate multi:mp_math(#(1), ?, ?).
:- multifile multi:mp_math/3.
:- public multi:mp_math/3.
multi:mp_math(V, _, _) :- var(V),
   throw(error(instantiation_error, _)).

/* special sin, etc.. */
multi:mp_math(sin(X), P, R) :- !,
   mp_math(decimal(X), P, H),
   mp_sin(H, P, R).
multi:mp_math(cos(X), P, R) :- !,
   mp_math(decimal(X), P, H),
   mp_cos(H, P, R).
multi:mp_math(tan(X), P, R) :- !,
   mp_math(sin(X)/cos(X), P, R).
multi:mp_math(atan(X), P, R) :- !,
   mp_math(decimal(X), P, H),
   mp_atan(H, P, R).

/* special exp, etc.. */
multi:mp_math(exp(X), P, R) :- !,
   mp_math(decimal(X), P, H),
   mp_exp(H, P, R).
multi:mp_math(log(X), P, R) :- !,
   mp_math(decimal(X), P, H),
   mp_log(H, P, R).
multi:mp_math(X**Y, P, R) :- !,
   mp_math(exp(log(X)*Y), P, R).

/* special atan2, etc.. */
multi:mp_math(atan2(X, Y), P, R) :- !,
   mp_math(decimal(X), P, H),
   mp_math(decimal(Y), P, J),
   mp_atan2(H, J, P, R).
multi:mp_math(asin(X), P, R) :- !,
   mp_math(atan2(X, sqrt(1-X^2)), P, R).
multi:mp_math(acos(X), P, R) :- !,
   mp_math(atan2(sqrt(1-X^2), X), P, R).

/* constants */
multi:mp_math(pi, P, R) :- !,
   mp_memo(pi, 20*atan(1/7)+8*atan(3/79), P, R).
multi:mp_math(e, P, R) :- !,
   mp_memo(e, exp(1/2)^2, P, R).
multi:mp_math(log10, P, R) :- !,
   mp_memo(log10, 7*log(5/4)+3*log(32/25), P, R).
multi:mp_math(log2, P, R) :- !,
   mp_memo(log2, 2*log(5/4)+log(32/25), P, R).
multi:mp_math(sqrt1div2, P, R) :- !,
   mp_memo(sqrt1div2, sqrt(1/2), P, R).

/**
 * mp_abnormal(E):
 * The predicate succeeds when the expression E has a non-default
 * handling. This predicate is multi file and can be thus
 * extended.
 */
:- multifile multi:mp_abnormal/1.
:- public multi:mp_abnormal/1.
multi:mp_abnormal(sin(_)).
multi:mp_abnormal(cos(_)).
multi:mp_abnormal(tan(_)).
multi:mp_abnormal(atan(_)).
multi:mp_abnormal(atan2(_, _)).
multi:mp_abnormal(asin(_)).
multi:mp_abnormal(acos(_)).
multi:mp_abnormal(exp(_)).
multi:mp_abnormal(log(_)).
multi:mp_abnormal(_**_).
multi:mp_abnormal(pi).
multi:mp_abnormal(e).
multi:mp_abnormal(log10).
multi:mp_abnormal(log2).
multi:mp_abnormal(sqrt1div2).
