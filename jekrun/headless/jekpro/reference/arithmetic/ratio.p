/**
 * The module enables rational number arithmetic. Importing the module
 * overrides the number evaluable functions and the number predicates.
 * It also provides a few new unique evaluable functions and predicates
 * for rational numbers. The arithmetic operation (rdiv)/2 creates a
 * rational number and normalizes it into a compound rat/2:
 *
 * ?- X is 4 rdiv 6.
 * X = rat(2, 3)
 *
 * Rational numbers with unit denominator are normalized into ordinary
 * numbers. The overridden evaluable functions and predicates are realized
 * in Prolog and use multi-argument indexing to dispatch into rational
 * number or ordinary number routines. We currently measure a ca. 4-fold
 * overhead for ordinary numbers, but this might improve in the future.
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
:- use_package(foreign(jekpro/reference/arithmetic)).

:- module(ratio, []).

:- public infix(rdiv).
:- op(400, yfx, rdiv).

/**
 * rational(X):
 * The predicate succeeds when X is a proper rational number.
 */
:- public rational/1.
rational(rat(_, _)).

/**
 * X rdiv Y:
 * If X and Y are both numbers then the function returns the rational division of X by Y.
 */
:- public rdiv/3.
rdiv(rat(A, B), rat(D, C), R) :- !,
   user: *(A, C, P),
   user: *(B, D, Q),
   rat_make(P, Q, R).
rdiv(rat(A, B), D, R) :- !,
   user: *(B, D, Q),
   rat_make(A, Q, R).
rdiv(A, rat(D, C), R) :- !,
   user: *(A, C, P),
   rat_make(P, D, R).
rdiv(A, D, R) :-
   rat_make(A, D, R).

/**
 * rat(X, Y):
 * If X and Y are both numbers then the function returns the rational number X/Y.
 */
:- public rat/3.
rat(X, Y, rat(X, Y)).

/**
 * rational(X):
 * If X is a number then the function returns the corresponding rational number.
 */
:- public rational/2.
rational(rat(A, B), R) :- !,
   R = rat(A, B).
rational(X, R) :-
   fp_mantissa(X, M),
   fp_exponent(X, E),
   fp_radix(X, B),
   (  user: <(E, 0)
   -> user: -(E, F),
      user: ^(B, F, H),
      rat_make(M, H, R)
   ;  user: ^(B, E, H),
      user: *(M, H, R)).

/***************************************************************/
/* elem.p                                                      */
/***************************************************************/

:- public (-)/2.
:- override (-)/2.
-(rat(A, B), R) :- !,
   user: -(A, C),
   R = rat(C, B).
-(A, B) :-
   user: -(A, B).

:- public (+)/2.
:- override (+)/2.
+(rat(A, B), R) :- !,
   R = rat(A, B).
+(A, B) :-
   user: +(A, B).

:- public abs/2.
:- override abs/2.
abs(rat(A, B), R) :- !,
   user:abs(A, C),
   R = rat(C, B).
abs(A, B) :-
   user:abs(A, B).

:- public sign/2.
:- override sign/2.
sign(rat(A, _), C) :- !,
   user:sign(A, C).
sign(A, B) :-
   user:sign(A, B).

:- public float/2.
:- override float/2.
float(rat(A, B), C) :- !,
   user: /(A, B, C).
float(A, B) :-
   user:float(A, B).

:- public float32/2.
:- override float32/2.
float32(rat(A, B), C) :- !,
   user: /(A, B, H),
   user:float32(H, C).
float32(A, B) :-
   user:float32(A, B).

:- public (+)/3.
:- override (+)/3.
+(rat(A, B), rat(C, D), R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user: +(H, J, P),
   user: *(B, D, Q),
   rat_make(P, Q, R).
+(rat(A, B), C, R) :- !,
   user: *(B, C, H),
   user: +(A, H, P),
   rat_make(P, B, R).
+(A, rat(C, D), R) :- !,
   user: *(A, D, H),
   user: +(H, C, P),
   rat_make(P, D, R).
+(A, B, C) :-
   user: +(A, B, C).

:- public (-)/3.
:- override (-)/3.
-(rat(A, B), rat(C, D), R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user: -(H, J, P),
   user: *(B, D, Q),
   rat_make(P, Q, R).
-(rat(A, B), C, R) :- !,
   user: *(B, C, H),
   user: -(A, H, P),
   rat_make(P, B, R).
-(A, rat(C, D), R) :- !,
   user: *(A, D, H),
   user: -(H, C, P),
   rat_make(P, D, R).
-(A, B, C) :-
   user: -(A, B, C).

:- public * /3.
:- override * /3.
*(rat(A, B), rat(C, D), R) :- !,
   user: *(A, C, P),
   user: *(B, D, Q),
   rat_make(P, Q, R).
*(rat(A, B), C, R) :- !,
   user: *(A, C, P),
   rat_make(P, B, R).
*(A, rat(C, D), R) :- !,
   user: *(A, C, P),
   rat_make(P, D, R).
*(A, B, C) :-
   user: *(A, B, C).

:- public / /3.
:- override / /3.
/(rat(A, B), rat(D, C), R) :- !,
   user: *(A, C, P),
   user: *(B, D, Q),
   user: /(P, Q, R).
/(rat(A, B), D, R) :- !,
   user: *(B, D, Q),
   user: /(A, Q, R).
/(A, rat(D, C), R) :- !,
   user: *(A, C, P),
   user: /(P, D, R).
/(A, B, C) :-
   user: /(A, B, C).

:- public ^ /3.
:- override ^ /3.
^(rat(A, B), C, R) :- user: <(C, 0), !,
   user: -(C, D),
   user: ^(A, D, P),
   user: ^(B, D, Q),
   rat_norm(Q, P, R).
^(rat(A, B), C, R) :- !,
   user: ^(A, C, P),
   user: ^(B, C, Q),
   rat_norm(P, Q, R).
^(A, B, C) :-
   user: ^(A, B, C).

:- private rat_make/3.
rat_make(A, D, R) :-
   user:gcd(A, D, H),
   user: //(A, H, P),
   user: //(D, H, Q),
   rat_norm(P, Q, R).

:- private rat_norm/3.
rat_norm(_, 0, _) :-
   throw(error(evaluation_error(zero_divisor), _)).
rat_norm(P, -1, C) :- !,
   user: -(P, C).
rat_norm(P, 1, C) :- !,
   C = P.
rat_norm(P, Q, C) :- user: <(Q, 0), !,
   user: -(P, R),
   user: -(Q, S),
   C = rat(R, S).
rat_norm(P, Q, rat(P, Q)).

/***************************************************************/
/* compare.p                                                   */
/***************************************************************/

:- public =:= /2.
:- override =:= /2.
:- meta_predicate =:=(#(1), #(1)).
=:=(A, B) :-
   H is A,
   J is B,
   rat_equal(H, J).

:- public =\= /2.
:- override =\= /2.
:- meta_predicate =\=(#(1), #(1)).
=\=(A, B) :-
   H is A,
   J is B,
   \+ rat_equal(H, J).

:- public < /2.
:- override < /2.
:- meta_predicate <(#(1), #(1)).
<(A, B) :-
   H is A,
   J is B,
   rat_less(H, J).

:- public =< /2.
:- override =< /2.
:- meta_predicate =<(#(1), #(1)).
=<(A, B) :-
   H is A,
   J is B,
   \+ rat_less(J, H).

:- public > /2.
:- override > /2.
:- meta_predicate >(#(1), #(1)).
>(A, B) :-
   H is A,
   J is B,
   rat_less(J, H).

:- public >= /2.
:- override >= /2.
:- meta_predicate >=(#(1), #(1)).
>=(A, B) :-
   H is A,
   J is B,
   \+ rat_less(H, J).

:- public min/3.
:- override min/3.
min(rat(A, B), rat(C, D), R) :- !,
   (rat_less(rat(A, B), rat(C, D)) -> R = rat(A, B); R = rat(C, D)).
min(rat(A, B), C, R) :- !,
   (rat_less(rat(A, B), C) -> R = rat(A, B); R = C).
min(A, rat(B, C), R) :- !,
   (rat_less(A, rat(B, C)) -> R = A; R = rat(B, C)).
min(A, B, C) :-
   user:min(A, B, C).

:- public max/3.
:- override max/3.
max(rat(A, B), rat(C, D), R) :- !,
   (rat_less(rat(A, B), rat(C, D)) -> R = rat(C, D); R = rat(A, B)).
max(rat(A, B), C, R) :- !,
   (rat_less(rat(A, B), C) -> R = C; R = rat(A, B)).
max(A, rat(B, C), R) :- !,
   (rat_less(A, rat(B, C)) -> R = rat(B, C); R = A).
max(A, B, C) :-
   user:max(A, B, C).

:- private rat_equal/2.
rat_equal(rat(A, B), rat(C, D)) :- !,
   user: =:=(A, C),
   user: =:=(B, D).
rat_equal(rat(_, _), _) :- !,
   fail.
rat_equal(_, rat(_, _)) :- !,
   fail.
rat_equal(A, B) :-
   user: =:=(A, B).

:- private rat_less/2.
rat_less(rat(A, B), rat(C, D)) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user: <(H, J).
rat_less(rat(A, B), C) :- !,
   user: *(B, C, J),
   user: <(A, J).
rat_less(A, rat(B, C)) :- !,
   user: *(A, C, H),
   user: <(H, B).
rat_less(A, B) :-
   user: <(A, B).

/***************************************************************/
/* bits.p                                                      */
/***************************************************************/

:- public gcd/3.
:- override gcd/3.
gcd(rat(A, B), rat(C, D), R) :-
   user:gcd(A, C, P),
   user:lcm(B, D, Q),
   R = rat(P, Q).
gcd(rat(A, B), C, R) :-
   user:gcd(A, C, P),
   R = rat(P, B).
gcd(A, rat(B, C), R) :-
   user:gcd(A, B, P),
   R = rat(P, C).
gcd(A, B, C) :-
   user:gcd(A, B, C).

:- public lcm/3.
:- override lcm/3.
lcm(rat(A, B), rat(C, D), R) :-
   user:lcm(A, C, P),
   user:gcd(B, D, Q),
   R = rat(P, Q).
lcm(rat(A, B), C, R) :-
   user:lcm(A, C, P),
   R = rat(P, B).
lcm(A, rat(B, C), R) :-
   user:lcm(A, B, P),
   R = rat(P, C).
lcm(A, B, C) :-
   user:lcm(A, B, C).

/***************************************************************/
/* round.p                                                     */
/***************************************************************/

:- public integer/2.
:- override integer/2.
integer(rat(A, B), C) :- !,
   user: //(A, B, C).
integer(C, D) :-
   user:integer(C, D).

:- public // /3.
:- override // /3.
//(rat(A, B), rat(C, D), R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user: //(H, J, R).
//(rat(A, B), C, R) :- !,
   user: *(B, C, J),
   user: //(A, J, R).
//(A, rat(B, C), R) :- !,
   user: *(A, C, H),
   user: //(H, B, R).
//(A, B, C) :-
   user: //(A, B, C).

:- public rem/3.
:- override rem/3.
rem(rat(A, B), rat(C, D), R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user:rem(H, J, K),
   user: *(B, D, L),
   rat_make(K, L, R).
rem(rat(A, B), C, R) :- !,
   user: *(B, C, J),
   user:rem(A, J, K),
   rat_make(K, B, R).
rem(A, rat(B, C), R) :- !,
   user: *(A, C, H),
   user:rem(H, B, K),
   rat_make(K, C, R).
rem(A, B, C) :-
   user:rem(A, B, C).

:- public div/3.
:- override div/3.
div(rat(A, B), rat(C, D), R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user:div(H, J, R).
div(rat(A, B), C, R) :- !,
   user: *(B, C, J),
   user:div(A, J, R).
div(A, rat(B, C), R) :- !,
   user: *(A, C, H),
   user:div(H, B, R).
div(A, B, C) :-
   user:div(A, B, C).

:- public mod/3.
:- override mod/3.
mod(rat(A, B), rat(C, D), R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user:mod(H, J, K),
   user: *(B, D, L),
   rat_make(K, L, R).
mod(rat(A, B), C, R) :- !,
   user: *(B, C, J),
   user:mod(A, J, K),
   rat_make(K, B, R).
mod(A, rat(B, C), R) :- !,
   user: *(A, C, H),
   user:mod(H, B, K),
   rat_make(K, C, R).
mod(A, B, C) :-
   user:mod(A, B, C).

/***************************************************************/
/* Helper                                                      */
/***************************************************************/

/**
 * fp_exponent(X): [ISO 7.1.3]
 * If X is a number then the function returns its exponent.
 */
:- private fp_exponent/2.
:- special(fp_exponent/2, 'EvaluableCompare', 2).

/**
 * fp_mantissa(X): [ISO 7.1.3]
 * If X is a number then the function returns its mantissa.
 */
:- private fp_mantissa/2.
:- special(fp_mantissa/2, 'EvaluableCompare', 3).

/**
 * fp_radix(X): [ISO 7.1.3]
 * If X is a number then the function returns its radix.
 */
:- private fp_radix/2.
:- special(fp_radix/2, 'EvaluableCompare', 4).
