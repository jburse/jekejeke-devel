/**
 * The module enables rational number arithmetic. Importing the module
 * overrides the number evaluable functions and the number predicates.
 * It also provides a few new unique evaluable functions and predicates
 * for rational numbers. The arithmetic operation (rdiv)/2 creates a
 * rational number and normalizes it into a compound with functor #/2:
 *
 * Examples:
 * ?- X is 4 rdiv 6.
 * X = 2#3
 * ?- X is (- 12) rdiv 6.
 * X = -2
 *
 * Rational numbers with unit denominator are normalized into integer
 * numbers. Rational numbers can be tested by the predicate rational/1.
 * They can be decomposed into numerator and denominator via the
 * predicate rational/3. The evaluable functions numerator/1 and
 * denominator/1 do the same individually.
 *
 * Examples:
 * ?- rational(2#3, X, Y).
 * X = 2,
 * Y = 3
 * ?- rational(-2, X, Y).
 * X = -2,
 * Y = 1
 *
 * The overridden evaluable functions and predicates are realized
 * in Prolog and use multi-argument indexing to dispatch into rational
 * number or ordinary number routines. We currently measure a ca. 4-fold
 * overhead for ordinary numbers, but this might improve in the future
 * releases of the Jekejeke Prolog runtime library.
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

:- package(library(jekpro/reference/arithmetic)).

:- module(ratio, []).

:- public infix(rdiv).
:- op(400, yfx, rdiv).

:- public infix(#).
:- op(100, xfx, #).

/**
 * X rdiv Y:
 * If X and Y are both numbers then the function returns the rational division of X by Y.
 */
:- public rdiv/3.
rdiv(A#B, D#C, R) :- !,
   user: *(A, C, P),
   user: *(B, D, Q),
   rat_norm(P, Q, R).
rdiv(A#B, D, R) :- !,
   user: *(B, D, Q),
   rat_norm(A, Q, R).
rdiv(A, D#C, R) :- !,
   user: *(A, C, P),
   rat_norm(P, D, R).
rdiv(A, D, R) :-
   rat_norm(A, D, R).

/**
 * #(X, Y):
 * If X and Y are both numbers then the function returns the rational number X/Y.
 */
:- public # /3.
#(X, Y, X#Y).

/**
 * rational(X):
 * The predicate succeeds when X is a rational number.
 */
:- public rational/1.
rational(_#_) :- !.
rational(X) :- integer(X).

/**
 * rational(R, N, D):
 * The predicate succeeds in N with the numerator and in D with
 * the denominator of the rational number R.
 */
:- public rational/3.
rational(X, A, B) :- var(X), !,
   rat_make(A, B, X).
rational(X#Y, A, B) :- !,
   A = X,
   B = Y.
rational(X, A, B) :- integer(X), !,
   A = X,
   B = 1.
rational(X, _, _) :-
   throw(error(type_error(rational, X), _)).

/**
 * numerator(X):
 * If X is a rational number then the function returns the numerator.
 */
:- public numerator/2.
numerator(X#_, A) :- !,
   A = X.
numerator(X, A) :- integer(X), !,
   A = X.
numerator(X, _) :-
   throw(error(type_error(rational, X), _)).

/**
 * denominator(X):
 * If X is a rational number then the function returns the denominator.
 */
:- public denominator/2.
denominator(_#X, A) :- !,
   A = X.
denominator(X, A) :- integer(X), !,
   A = 1.
denominator(X, _) :-
   throw(error(type_error(rational, X), _)).

/***************************************************************/
/* elem.p                                                      */
/***************************************************************/

:- public (-)/2.
:- override (-)/2.
-(A#B, R) :- !,
   user: -(A, C),
   R = C#B.
-(A, B) :-
   user: -(A, B).

:- public (+)/2.
:- override (+)/2.
+(A#B, R) :- !,
   R = A#B.
+(A, B) :-
   user: +(A, B).

:- public abs/2.
:- override abs/2.
abs(A#B, R) :- !,
   user:abs(A, C),
   R = C#B.
abs(A, B) :-
   user:abs(A, B).

:- public sign/2.
:- override sign/2.
sign(A#_, C) :- !,
   user:sign(A, C).
sign(A, B) :-
   user:sign(A, B).

:- public float/2.
:- override float/2.
float(A#B, C) :- !,
   user: /(A, B, C).
float(A, B) :-
   user:float(A, B).

:- public decimal/2.
:- override decimal/2.
decimal(A#B, C) :- !,
   user: /(A, B, H),
   user:decimal(H, C).
decimal(A, B) :-
   user:decimal(A, B).

:- public float32/2.
:- override float32/2.
float32(A#B, C) :- !,
   user: /(A, B, H),
   user:float32(H, C).
float32(A, B) :-
   user:float32(A, B).

:- public (+)/3.
:- override (+)/3.
+(A#B, C#D, R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user: +(H, J, P),
   user: *(B, D, Q),
   rat_norm(P, Q, R).
+(A#B, C, R) :- !,
   user: *(B, C, H),
   user: +(A, H, P),
   rat_norm(P, B, R).
+(A, C#D, R) :- !,
   user: *(A, D, H),
   user: +(H, C, P),
   rat_norm(P, D, R).
+(A, B, C) :-
   user: +(A, B, C).

:- public (-)/3.
:- override (-)/3.
-(A#B, C#D, R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user: -(H, J, P),
   user: *(B, D, Q),
   rat_norm(P, Q, R).
-(A#B, C, R) :- !,
   user: *(B, C, H),
   user: -(A, H, P),
   rat_norm(P, B, R).
-(A, C#D, R) :- !,
   user: *(A, D, H),
   user: -(H, C, P),
   rat_norm(P, D, R).
-(A, B, C) :-
   user: -(A, B, C).

:- public * /3.
:- override * /3.
*(A#B, C#D, R) :- !,
   user: *(A, C, P),
   user: *(B, D, Q),
   rat_norm(P, Q, R).
*(A#B, C, R) :- !,
   user: *(A, C, P),
   rat_norm(P, B, R).
*(A, C#D, R) :- !,
   user: *(A, C, P),
   rat_norm(P, D, R).
*(A, B, C) :-
   user: *(A, B, C).

:- public / /3.
:- override / /3.
/(A#B, D#C, R) :- !,
   user: *(A, C, P),
   user: *(B, D, Q),
   user: /(P, Q, R).
/(A#B, D, R) :- !,
   user: *(B, D, Q),
   user: /(A, Q, R).
/(A, D#C, R) :- !,
   user: *(A, C, P),
   user: /(P, D, R).
/(A, B, C) :-
   user: /(A, B, C).

:- public ^ /3.
:- override ^ /3.
^(X, C, R) :- user: <(C, 0), !,
   user: -(C, H),
   ^(X, H, J),
   rational(J, P, Q),
   rat_make(Q, P, R).
^(A#B, C, R) :- !,
   user: ^(A, C, P),
   user: ^(B, C, Q),
   rat_make(P, Q, R).
^(A, B, C) :-
   user: ^(A, B, C).

:- private rat_norm/3.
rat_norm(A, D, R) :-
   user:gcd(A, D, H),
   user: //(A, H, P),
   user: //(D, H, Q),
   rat_make(P, Q, R).

:- private rat_make/3.
rat_make(_, 0, _) :-
   throw(error(evaluation_error(zero_divisor), _)).
rat_make(P, -1, C) :- !,
   user: -(P, C).
rat_make(P, 1, C) :- !,
   C = P.
rat_make(P, Q, C) :- user: <(Q, 0), !,
   user: -(P, R),
   user: -(Q, S),
   C = R#S.
rat_make(P, Q, P#Q).

/***************************************************************/
/* compare.p                                                   */
/***************************************************************/

:- public =:= /2.
:- override =:= /2.
:- meta_predicate =:=(1, 1).
=:=(A, B) :-
   H is A,
   J is B,
   rat_equal(H, J).

:- public =\= /2.
:- override =\= /2.
:- meta_predicate =\=(1, 1).
=\=(A, B) :-
   H is A,
   J is B,
   \+ rat_equal(H, J).

:- public < /2.
:- override < /2.
:- meta_predicate <(1, 1).
<(A, B) :-
   H is A,
   J is B,
   rat_less(H, J).

:- public =< /2.
:- override =< /2.
:- meta_predicate =<(1, 1).
=<(A, B) :-
   H is A,
   J is B,
   \+ rat_less(J, H).

:- public > /2.
:- override > /2.
:- meta_predicate >(1, 1).
>(A, B) :-
   H is A,
   J is B,
   rat_less(J, H).

:- public >= /2.
:- override >= /2.
:- meta_predicate >=(1, 1).
>=(A, B) :-
   H is A,
   J is B,
   \+ rat_less(H, J).

:- public min/3.
:- override min/3.
min(A#B, C#D, R) :- !,
   (rat_less(A#B, C#D) -> R = A#B; R = C#D).
min(A#B, C, R) :- !,
   (rat_less(A#B, C) -> R = A#B; R = C).
min(A, B#C, R) :- !,
   (rat_less(A, B#C) -> R = A; R = B#C).
min(A, B, C) :-
   user:min(A, B, C).

:- public max/3.
:- override max/3.
max(A#B, C#D, R) :- !,
   (rat_less(A#B, C#D) -> R = C#D; R = A#B).
max(A#B, C, R) :- !,
   (rat_less(A#B, C) -> R = C; R = A#B).
max(A, B#C, R) :- !,
   (rat_less(A, B#C) -> R = B#C; R = A).
max(A, B, C) :-
   user:max(A, B, C).

:- private rat_equal/2.
rat_equal(A#B, C#D) :- !,
   user: =:=(A, C),
   user: =:=(B, D).
rat_equal(_#_, _) :- !,
   fail.
rat_equal(_, _#_) :- !,
   fail.
rat_equal(A, B) :-
   user: =:=(A, B).

:- private rat_less/2.
rat_less(A#B, C#D) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user: <(H, J).
rat_less(A#B, C) :- !,
   user: *(B, C, J),
   user: <(A, J).
rat_less(A, B#C) :- !,
   user: *(A, C, H),
   user: <(H, B).
rat_less(A, B) :-
   user: <(A, B).

/***************************************************************/
/* bits.p                                                      */
/***************************************************************/

:- public gcd/3.
:- override gcd/3.
gcd(A#B, C#D, R) :- !,
   user:gcd(A, C, P),
   user:lcm(B, D, Q),
   R = P#Q.
gcd(A#B, C, R) :- !,
   user:gcd(A, C, P),
   R = P#B.
gcd(A, B#C, R) :- !,
   user:gcd(A, B, P),
   R = P#C.
gcd(A, B, C) :-
   user:gcd(A, B, C).

:- public lcm/3.
:- override lcm/3.
lcm(A#B, C#D, R) :- !,
   user:lcm(A, C, P),
   user:gcd(B, D, Q),
   rat_make(P, Q, R).
lcm(A#_, C, R) :- !,
   user:lcm(A, C, P),
   R = P.
lcm(A, B#_, R) :- !,
   user:lcm(A, B, P),
   R = P.
lcm(A, B, C) :-
   user:lcm(A, B, C).

/***************************************************************/
/* round.p                                                     */
/***************************************************************/

:- public integer/2.
:- override integer/2.
integer(A#B, C) :- !,
   user: //(A, B, C).
integer(A, B) :-
   user:integer(A, B).

:- public truncate/2.
:- override truncate/2.
truncate(A#B, C) :- !,
   user: //(A, B, C).
truncate(A, B) :-
   user:truncate(A, B).

:- public floor/2.
:- override floor/2.
floor(A#B, C) :- !,
   user:div(A, B, C).
floor(A, B) :-
   user:floor(A, B).

:- public ceiling/2.
:- override ceiling/2.
ceiling(A#B, C) :- !,
   user: -(A, H),
   user:div(H, B, J),
   user: -(J, C).
ceiling(A, B) :-
   user:ceiling(A, B).

:- public round/2.
:- override round/2.
round(A#B, C) :- user: <(A, 0), !,
   user: >>(B, 1, H),
   user: -(A, H, J),
   user: //(J, B, C).
round(A#B, C) :- !,
   user: >>(B, 1, H),
   user: +(A, H, J),
   user: //(J, B, C).
round(A, B) :-
   user:round(A, B).

:- public // /3.
:- override // /3.
//(A#B, C#D, R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user: //(H, J, R).
//(A#B, C, R) :- !,
   user: *(B, C, J),
   user: //(A, J, R).
//(A, B#C, R) :- !,
   user: *(A, C, H),
   user: //(H, B, R).
//(A, B, C) :-
   user: //(A, B, C).

:- public rem/3.
:- override rem/3.
rem(A#B, C#D, R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user:rem(H, J, K),
   user: *(B, D, L),
   rat_norm(K, L, R).
rem(A#B, C, R) :- !,
   user: *(B, C, J),
   user:rem(A, J, K),
   rat_norm(K, B, R).
rem(A, B#C, R) :- !,
   user: *(A, C, H),
   user:rem(H, B, K),
   rat_norm(K, C, R).
rem(A, B, C) :-
   user:rem(A, B, C).

:- public div/3.
:- override div/3.
div(A#B, C#D, R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user:div(H, J, R).
div(A#B, C, R) :- !,
   user: *(B, C, J),
   user:div(A, J, R).
div(A, B#C, R) :- !,
   user: *(A, C, H),
   user:div(H, B, R).
div(A, B, C) :-
   user:div(A, B, C).

:- public mod/3.
:- override mod/3.
mod(A#B, C#D, R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user:mod(H, J, K),
   user: *(B, D, L),
   rat_norm(K, L, R).
mod(A#B, C, R) :- !,
   user: *(B, C, J),
   user:mod(A, J, K),
   rat_norm(K, B, R).
mod(A, B#C, R) :- !,
   user: *(A, C, H),
   user:mod(H, B, K),
   rat_norm(K, C, R).
mod(A, B, C) :-
   user:mod(A, B, C).

:- public divmod/4.
:- override divmod/4.
divmod(A#B, C#D, R, S) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user:divmod(H, J, R, K),
   user: *(B, D, L),
   rat_norm(K, L, S).
divmod(A#B, C, R, S) :- !,
   user: *(B, C, J),
   user:divmod(A, J, R, K),
   rat_norm(K, B, S).
divmod(A, B#C, R, S) :- !,
   user: *(A, C, H),
   user:divmod(H, B, R, K),
   rat_norm(K, C, S).
divmod(A, B, C, D) :-
   user:divmod(A, B, C, D).
