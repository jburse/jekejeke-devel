/**
 * This module provides rational constants. The module is responsible
 * for the reduction rules that perform partial evaluation. The result
 * can be also an integer. In case that some extra arguments is not rational,
 * the rules delegate to the polynom and fraction methods since a
 * rational can be easily also viewed as a polynom or fraction.
 *
 * Examples:
 * ?- X is 2/3*(3/2).
 * X = 1
 * ?- X is 1/2*(Y+2).
 * X is 1+1/2*Y
 *
 * The reduction rules are just predicates inside the rational
 * module with a Python first argument for the method receiver. We
 * provide reduction rules for basic arithmetic. Special functions
 * are currently not supported. Equality is also realized by the same
 * mechanism. Error handling is rudimentary.
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

:- package(library(jekmin/frequent/groebner)).
:- use_package(library(jekpro/frequent/misc)).
:- use_package(library(jekmin/reference/misc)).

:- module(rational, []).
:- reexport('../gauss/ordered').

:- use_module(generic).
:- use_module(fraction).

:- use_module(library(experiment/attr)).
:- use_module(library(experiment/trail)).

/*********************************************************************/
/* Arithmetic                                                        */
/*********************************************************************/

/**
 * -(P, Q):
 * The predicate succeeds in Q with the P negated.
 */
% -(+Rational, -Rational)
:- override (-)/2.
:- public (-)/2.
rational(A,B) - rational(C,B) :-
   user:A - C.

/**
 * +(P, Q, R):
 * The predicate succeeds in R with the sum of P and Q.
 */
% +(+Rational, +Internal, -Internal)
:- override (+)/3.
:- public (+)/3.
+(rational(A,B), Y, R) :-
   integer(Y), !,
   rational: +(rational(A,B), rational(Y,1), R).
+(rational(A,B), Y, R) :-
   sys_freezer(Y), !,
   polynom: +(polynom(Y,[0-rational(A,B)]), polynom(Y,[1-1]), R).
+(rational(A,B), rational(C,D), R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user: +(H, J, K),
   user: *(B, D, L),
   make_rational(K, L, R).
+(rational(A,B), polynom(C,D), R) :- !,
   polynom: +(polynom(C,[0-rational(A,B)]), polynom(C,D), R).
+(rational(A,B), fraction(C,D), R) :-
   fraction: +(fraction(rational(A,B),1), fraction(C,D), R).

/**
 * -(P, Q, R):
 * The predicate succeeds in R with P subtracted by Q.
 */
% -(+Rational, +Internal, -Internal)
:- override (-)/3.
:- public (-)/3.
-(rational(A,B), Y, R) :-
   integer(Y), !,
   rational: -(rational(A,B), rational(Y,1), R).
-(rational(A,B), Y, R) :-
   sys_freezer(Y), !,
   polynom: -(polynom(Y,[0-rational(A,B)]), polynom(Y,[1-1]), R).
-(rational(A,B), rational(C,D), R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user: -(H, J, K),
   user: *(B, D, L),
   make_rational(K, L, R).
-(rational(A,B), polynom(C,D), R) :- !,
   polynom: -(polynom(C,[0-rational(A,B)]), polynom(C,D), R).
-(rational(A,B), fraction(C,D), R) :-
   fraction: -(fraction(rational(A,B),1), fraction(C,D), R).

/**
 * *(P, Q, R):
 * The predicate succeeds in R with the product of P and Q.
 */
% *(+Rational, +Internal, -Internal)
:- override * /3.
:- public * /3.
*(rational(A,B), Y, R) :-
   integer(Y), !,
   rational: *(rational(A,B), rational(Y,1), R).
*(rational(A,B), Y, R) :-
   sys_freezer(Y), !,
   polynom: *(polynom(Y,[0-rational(A,B)]), polynom(Y,[1-1]), R).
*(rational(A,B), rational(C,D), R) :- !,
   user: *(A, C, H),
   user: *(B, D, J),
   make_rational(H, J, R).
*(rational(A,B), polynom(C,D), R) :- !,
   polynom: *(polynom(C,[0-rational(A,B)]), polynom(C,D), R).
*(rational(A,B), fraction(C,D), R) :-
   fraction: *(fraction(rational(A,B),1), fraction(C,D), R).

/**
 * /(P, Q, R):
 * The predicate succeeds in R with P divided by Q.
 */
% /(+Rational, +Internal, -Internal)
:- override / /3.
:- public / /3.
/(rational(A,B), Y, R) :-
   integer(Y), !,
   rational: /(rational(A,B), rational(Y,1), R).
/(rational(A,B), Y, R) :-
   sys_freezer(Y), !,
   make_fraction(rational(A,B), Y, R).
/(rational(A,B), rational(C,D), R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   make_rational(H, J, R).
/(rational(A,B), polynom(C,D), R) :- !,
   make_fraction(rational(A,B), polynom(C,D), R).
/(rational(A,B), fraction(C,D), R) :-
   fraction: /(fraction(rational(A,B),1), fraction(C,D), R).

/**
 * ^(P, Q, R):
 * The predicate succeeds in R with P raised by Q.
 */
% ^(+Rational, +Integer, -Internal)
:- override ^ /3.
:- public ^ /3.
^(rational(A,B), Y, R) :-
   user:(Y < 0), !,
   user:Y - Z,
   user: ^(A, Z, H),
   user: ^(B, Z, J),
   new_rational(J, H, R).
^(_, 0, R) :- !,
   R = 1.
^(rational(A,B), Y, rational(H,J)) :-
   user: ^(A, Y, H),
   user: ^(B, Y, J).

/*********************************************************************/
/* Rounding                                                          */
/*********************************************************************/

/**
 * integer(P, Q):
 * The predicate succeeds in Q with the integer of P.
 */
% integer(+Rational, -Integer)
:- override integer/2.
:- public integer/2.
integer(rational(A,B), X) :-
   user: //(A, B, X).

/**
 * floor(P, Q):
 * The predicate succeeds in Q with the floor of P.
 */
% floor(+Rational, -Integer)
:- override floor/2.
:- public floor/2.
floor(rational(A,B), X) :-
   user:div(A, B, X).

/**
 * ceiling(P, Q):
 * The predicate succeeds in Q with the ceiling of P.
 */
% ceiling(+Rational, -Integer)
:- override ceiling/2.
:- public ceiling/2.
ceiling(rational(A,B), X) :-
   user: -(B, 1, H),
   user: +(A, H, J),
   user:div(J, B, X).

/*********************************************************************/
/* Equalty & Comparison                                              */
/*********************************************************************/

/**
 * gen_eq(X, Y):
 * The predicate succeeds when X equals Y.
 */
:- override gen_eq/2.
:- public gen_eq/2.
gen_eq(_, X) :-
   integer(X), !, fail.
gen_eq(rational(A,B), rational(C,D)) :- !,
   user:(A =:= C),
   user:(B =:= D).
gen_eq(_, _) :-
   throw(error(evaluation_error(ordered),_)).

/**
 * gen_ls(X, Y):
 * The predicate succeeds when X is less than Y.
 */
:- override gen_ls/2.
:- public gen_ls/2.
gen_ls(rational(A,B), X) :-
   integer(X), !,
   user: *(B, X, H),
   user:(A < H).
gen_ls(rational(A,B), rational(C,D)) :- !,
   user: *(D, A, H),
   user: *(B, C, J),
   user:(H < J).
gen_ls(_, _) :-
   throw(error(evaluation_error(ordered),_)).

/*********************************************************************/
/* Arithmetic Helper                                                 */
/*********************************************************************/

% make_rational(+Integer, +Integer, -Internal)
make_rational(_, 0, _) :-
   throw(error(evaluation_error(zero_divisor),_)).
make_rational(0, _, R) :- !,
   R = 0.
make_rational(A, B, C) :-
   elem:gcd(A, B, H),
   user: //(A, H, J),
   user: //(B, H, K),
   new_rational(J, K, C).

% new_rational(+Integer, +Integer, -Internal)
new_rational(A, -1, B) :- !,
   user:A - B.
new_rational(A, 1, R) :- !,
   R = A.
new_rational(A, B, rational(C,D)) :-
   user:(B < 0), !,
   user:A - C,
   user:B - D.
new_rational(A, B, rational(A,B)).

/*********************************************************************/
/* CAS Display Hook                                                  */
/*********************************************************************/

/**
 * sys_printable_value(F, G):
 * The predicate succeeds in G with a custom form of F. The
 * predicate should be extended for custom forms.
 */
% sys_printable_value(+Term, -Term)
:- public residue:sys_printable_value/2.
:- multifile residue:sys_printable_value/2.
residue:sys_printable_value(X, _) :-
   var(X), !, fail.
residue:sys_printable_value(rational(A,B), X) :-
   user: //(A, B, H),
   user:(H =\= 0), !,
   user: *(B, H, J),
   user: -(A, J, R),
   sys_make_integer(H, R, B, X).
residue:sys_printable_value(rational(A,B), X) :-
   user:(A < 0), !,
   user:A - C,
   X = -C/B.
residue:sys_printable_value(rational(A,B), X) :- !,
   X = A/B.

% sys_make_integer(+Integer, +Integer, +Integer, -External)
:- private sys_make_integer/4.
sys_make_integer(H, R, B, X) :-
   user:(H < 0), !,
   user:H - K,
   user:R - S,
   X = -K-S/B.
sys_make_integer(H, R, B, X) :-
   X = H+R/B.

/*********************************************************************/
/* Generic Hook                                                      */
/*********************************************************************/

/**
 * X is E:
 * The predicate succeeds in evaluating E by using polymorphism.
 */
% is(-Internal, +Expr)
:- override generic:is/2.
:- multifile generic:is/2.
:- public generic:is/2.
:- meta_predicate generic:(?is#(1)).
generic:(X is E) :-
   var(E), !,
   sys_ensure_serno(E),
   sys_freeze_var(E, X).
generic:(X is rational(A,B)) :- !,
   X = rational(A,B).

:- multifile generic:is_abnormal/1.
:- public generic:is_abnormal/1.
generic:is_abnormal(rational(_,_)).

/*********************************************************************/
/* User Evaluation                                                   */
/*********************************************************************/

% rational(+Integer, +Integer, -Float)
:- public user:rational/3.
user:rational(A, B, D) :-
   user: /(A, B, D).
