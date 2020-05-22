/**
 * This module provides rational number approximations. In contrast to
 * rational/1 the number of bits in the result is minimized so that the
 * target is still reached. The evaluable functions rationalize/1 and
 * rationalize32/1 find rational numbers wih fewer bits that approximate
 * 64-bit respectively 32-bit floating point numbers.
 *
 * Examples:
 * ?- X is rationalize(8*(pi-3)).
 * X = 121642183#107387442
 * ?- X is rationalize32(8*(pi-3)).
 * X = 4591#4053
 *
 * The current realization uses an integer algorithm based on Euclid's
 * and it does also use rational arithmetic. Since our rational arithmetic
 * is still slow and since the algorithm doesn't use floating points
 * internally, it is relatively slow compared to other implementations.
 * Again this might change in the future.
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

/**
 * See also:
 * Continued Fractions on the Stern-Brocot Tree
 * https://www.cut-the-knot.org/blue/ContinuedFractions.shtml
 */

:- package(library(jekpro/frequent/standard)).
:- use_package(foreign(jekpro/frequent/standard)).

:- module(approx, []).
:- reexport(library(arithmetic/ratio)).

/**
 * rational(X):
 * If X is a number then the function returns the corresponding rational number.
 */
% rational(+Number, -Rational)
:- public rational/2.
rational(A#B, R) :- !,
   R = A#B.
rational(F, R) :-
   sys_float_mantissa(F, M),
   sys_float_exponent(F, E),
   sys_float_radix(F, B),
   R is M*B^E.

/**
 * rationalize(X):
 * If X is a number then the function returns an approximate
 * rational number. The target precision is epsilon/2.
 */
% rationalize(+Number, -Rational)
:- public rationalize/2.
rationalize(X, S) :- X < 0, !,
   F is -float(X),
   rat_rationalize(F, H),
   S is -H.
rationalize(X, S) :-
   F is float(X),
   rat_rationalize(F, S).

% rat_rationalize(+Float, -Rational)
:- private rat_rationalize/2.
rat_rationalize(F, S) :-
   R is rational(F),
   rational(R, V, W),
   rat_iter(V#W, 1#0, 0#1, F, A#B),
   rational(S, A, B).

% rat_iter(+Rational, +Rational, +Rational, +Float, -Rational)
:- private rat_iter/5.
rat_iter(_, X, _, Y, X) :-
   X \= 1#0,
   float(X) =:= Y, !.
rat_iter(_#0, X, _, _, X) :- !.
rat_iter(V#W, M#N, P#Q, Y, X) :-
   user:divmod(V, W, D, U),
   user: *(D, M, H),
   user: +(H, P, A),
   user: *(D, N, J),
   user: +(J, Q, B),
   rat_iter(W#U, A#B, M#N, Y, X).

/**
 * rationalize32(X):
 * If X is a number then the function returns an approximate
 * rational number. The target precision is epsilon32/2.
 */
% rationalize32(+Number, -Rational)
:- public rationalize32/2.
rationalize32(X, S) :- X < 0, !,
   F is -float32(X),
   rat_rationalize32(F, H),
   S is -H.
rationalize32(X, S) :-
   F is float32(X),
   rat_rationalize32(F, S).

% rat_rationalize32(+Float, -Rational)
:- private rat_rationalize32/2.
rat_rationalize32(F, S) :-
   R is rational(F),
   rational(R, V, W),
   rat_iter32(V#W, 1#0, 0#1, F, A#B),
   rational(S, A, B).

% rat_iter32(+Rational, +Rational, +Rational, +Float, -Rational)
:- private rat_iter32/5.
rat_iter32(_, X, _, Y, X) :-
   X \= 1#0,
   float32(X) =:= Y, !.
rat_iter32(_#0, X, _, _, X) :- !.
rat_iter32(V#W, M#N, P#Q, Y, X) :-
   user:divmod(V, W, D, U),
   user: *(D, M, H),
   user: +(H, P, A),
   user: *(D, N, J),
   user: +(J, Q, B),
   rat_iter32(W#U, A#B, M#N, Y, X).

/***************************************************************/
/* Helper                                                      */
/***************************************************************/

/**
 * sys_float_mantissa(F, M): [ISO 7.1.3]
 * The predicate succeeds in M with the mantissa of F.
 */
:- public sys_float_mantissa/2.
:- foreign(sys_float_mantissa/2, 'ForeignApprox', sysFloatMantissa('Number')).

/**
 * sys_float_exponent(F, E): [ISO 7.1.3]
 * The predicate succeeds in E with the exponent of F.
 */
:- public sys_float_exponent/2.
:- foreign(sys_float_exponent/2, 'ForeignApprox', sysFloatExponent('Number')).

/**
 * sys_float_radix(F, B): [ISO 7.1.3]
 * The predicate succeeds in B with the radix of F.
 */
:- public sys_float_radix/2.
:- foreign(sys_float_radix/2, 'ForeignApprox', sysFloatRadix('Number')).

/**********************************************************/
/* Number Compare                                         */
/**********************************************************/

/**
 * number_compare(C, X, Y):
 * The predicate succeeds when C unifies with the result of
 * numerical comparing the term X to the term Y. The result is
 * one of the following atoms <, = or >.
 */
% number_compare(-Atom, +Term, +Term)
:- public number_compare/3.
:- override number_compare/3.
number_compare(C, X, Y) :-
   sys_type(X, S),
   sys_type(X, T),
   user:sys_number_test(D, S, T),
   (  D == =
   -> (  S == 0 -> compare(C, X, Y)
      ;  S == 1 -> sys_number_test(C, X, Y)
      ;  S == 2 -> compare(C, X, Y)
      ;  functor(X, F, A),
         functor(Y, G, B),
         user:sys_number_test(E, A, B),
         (  E == =
         -> compare(H, F, G),
            (  H == =
            -> X =.. L,
               Y =.. R,
               sys_number_compare_list(L, R, C)
            ;  C = H)
         ;  C = E))
   ;  C = D).

/**
 * sys_type(X, T):
 * The predicate succeeds in T with the type of X.
 */
% sys_type(+Term, -Integer)
:- private sys_type/2.
sys_type(X, T) :- var(X), !, T = 0.
sys_type(X, T) :- number(X), !, T = 1.
sys_type(X, T) :- atomic(X), !, T = 2.
sys_type(_#_, T) :- !, T = 1.
sys_type(_, 3).

/**
 * sys_number_compare_list(L, R, C):
 * The predicate succeeds when C unifies with the result of
 * numerical comparing the list X to the list Y. The result is
 * one of the following atoms <, = or >.
 */
:- private sys_number_compare_list/3.
sys_number_compare_list([], [], =).
sys_number_compare_list([X|L], [Y|R], C) :-
   number_compare(D, X, Y),
   (  D == =
   -> sys_number_compare_list(L, R, C)
   ;  C = D).

/**
 * sys_number_test(C, X, Y):
 * The predicate succeeds when C unifies with the result of
 * numerical testing the number X to the number Y. The result is
 * one of the following atoms <, = or >.
 */
% sys_number_test(-Atom, +Rational, +Rational)
:- override sys_number_test/3.
sys_number_test(R, A#B, C#D) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user:sys_number_test(R, H, J).
sys_number_test(R, A#B, C) :- !,
   user: *(B, C, J),
   user:sys_number_test(R, A, J).
sys_number_test(R, A, B#C) :- !,
   user: *(A, C, H),
   user:sys_number_test(R, H, B).
sys_number_test(R, A, B) :-
   user:sys_number_test(R, A, B).