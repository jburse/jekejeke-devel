/**
 * This module provides radical constants. The module is responsible
 * for the reduction rules that perform partial evaluation. The result
 * can be also an integer or a rational. In case that some extra arguments
 * is a radical, the rules delegate to the polynom and fraction methods
 * since a rational can be easily also viewed as a polynom or fraction.
 * The reciprocal of a radical constant is calculated with the help
 * of a Swinnerton-Dyer polynomial.
 *
 * Examples:
 * ?- X is 1/(sqrt(3)+sqrt(2)).
 * X is -sqrt(2)+sqrt(3)
 * ?- X is (2-A^2)/(sqrt(6)+sqrt(3)*A).
 * X is sqrt(2/3)-sqrt(1/3)*A
 *
 * Because reciprocal is available the Gröbner Bases algorithm works
 * with radical coefficients. The reduction rules are just predicates
 * inside the radical module with a Python first argument for the method
 * receiver. We provide reduction rules for basic arithmetic. Radical
 * constants are modelled as arbitrary long square root sums but nesting
 * of radicals is not yet supported. Special functions are currently not
 * supported. Error handling is rudimentary.
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

:- package(library(jekmin/frequent/leibniz)).
:- use_package(library(jekmin/frequent/groebner)).
:- use_package(library(jekpro/frequent/misc)).
:- use_package(library(jekmin/reference/misc)).

:- module(radical, []).
:- reexport(../gauss/ordered).

:- use_module(../groebner/generic).
:- use_module(../groebner/rational).
:- use_module(../groebner/fraction).

:- use_module(library(misc/residue)).
:- use_module(library(basic/lists)).

:- use_module(library(experiment/attr)).
:- use_module(library(experiment/trail)).

/*********************************************************************/
/* Arithmetic                                                        */
/*********************************************************************/

/**
 * -(P, Q):
 * The predicate succeeds in Q with the P negated.
 */
% -(+Radical, -Radical)
:- override (-)/2.
:- public (-)/2.
-(radical(A, B), radical(C, D)) :-
   C is -A,
   sys_radical_neg(B, D).

/**
 * +(P, Q, R):
 * The predicate succeeds in R with the sum of P and Q.
 */
% +(+Radical, +Internal, -Internal)
:- override (+)/3.
:- public (+)/3.
+(X, Y, R) :- integer(Y), !,
   radical: +(X, radical(Y, []), R).
+(X, rational(C, D), R) :- !,
   radical: +(X, radical(rational(C, D), []), R).
+(radical(A, B), radical(C, D), R) :- !,
   H is A+C,
   sys_radical_add(B, D, J),
   sys_new_radical(H, J, R).
+(X, Y, R) :- sys_freezer(Y), !,
   polynom: +(polynom(Y, [0-X]), polynom(Y, [1-1]), R).
+(X, polynom(C, D), R) :- !,
   polynom: +(polynom(C, [0-X]), polynom(C, D), R).
+(X, fraction(C, D), R) :-
   fraction: +(fraction(X, 1), fraction(C, D), R).

/**
 * -(P, Q, R):
 * The predicate succeeds in R with P subtracted by Q.
 */
% -(+Radical, +Internal, -Internal)
:- override (-)/3.
:- public (-)/3.
-(X, Y, R) :- integer(Y), !,
   radical: -(X, radical(Y, []), R).
-(X, rational(C, D), R) :- !,
   radical: -(X, radical(rational(C, D), []), R).
-(radical(A, B), radical(C, D), R) :- !,
   H is A-C,
   sys_radical_sub(B, D, J),
   sys_new_radical(H, J, R).
-(X, Y, R) :- sys_freezer(Y), !,
   polynom: -(polynom(Y, [0-X]), polynom(Y, [1-1]), R).
-(X, polynom(C, D), R) :- !,
   polynom: -(polynom(C, [0-X]), polynom(C, D), R).
-(X, fraction(C, D), R) :-
   fraction: -(fraction(X, 1), fraction(C, D), R).

/**
 * *(P, Q, R):
 * The predicate succeeds in R with the product of P and Q.
 */
% *(+Radical, +Internal, -Internal)
:- override * /3.
:- public * /3.
*(X, Y, R) :- integer(Y), !,
   radical: *(X, radical(Y, []), R).
*(X, rational(C, D), R) :- !,
   radical: *(X, radical(rational(C, D), []), R).
*(radical(A, B), radical(C, D), R) :- !,
   sys_radical_lift(A, D, K),
   sys_radical_lift(C, B, L),
   sys_radical_add(L, K, M),
   sys_radical_mul(B, D, N, V),
   sys_radical_add(M, N, U),
   H is A*C+V,
   sys_new_radical(H, U, R).
*(X, Y, R) :- sys_freezer(Y), !,
   polynom: *(polynom(Y, [0-X]), polynom(Y, [1-1]), R).
*(X, polynom(C, D), R) :- !,
   polynom: *(polynom(C, [0-X]), polynom(C, D), R).
*(X, fraction(C, D), R) :-
   fraction: *(fraction(X, 1), fraction(C, D), R).

/**
 * /(P, Q, R):
 * The predicate succeeds in R with P divided by Q.
 */
% /(+Integer, +Internal, -Internal)
:- override / /3.
:- public / /3.
/(X, Y, R) :- integer(Y), !,
   R is X*(1/Y).
/(X, rational(C, D), R) :- !,
   R is X*(1/rational(C, D)).
/(X, radical(0, [A-S]), R) :- !,
   B is 1/A,
   R is X*radical(0, [B-S]).
/(X, radical(C, D), Y) :- !,
   sys_radical_triage(radical(C, D), P, Q),
   Y is X*(P-Q)/(P^2-Q^2).
/(X, Y, R) :- sys_freezer(Y), !,
   new_fraction(X, Y, R).
/(X, polynom(C, D), R) :- !,
   new_fraction(X, polynom(C, D), R).
/(X, fraction(C, D), R) :-
   fraction: /(fraction(X, 1), fraction(C, D), R).

/**
 * ^(P, Q, R):
 * The predicate succeeds in R with P raised by Q.
 */
% ^(+Radical, +Integer, -Internal)
:- override ^ /3.
:- public ^ /3.
^(P, Y, R) :- user:(Y < 0), !,
   user: -(Y, Z),
   R is (1/P)^Z.
^(_, 0, R) :- !,
   R = 1.
^(P, 1, R) :- !,
   R = P.
^(radical(0, [A-_]), 2, R) :- !,
   R = A.
^(X, 2, R) :- !,
   sys_radical_split(X, P, Q),
   R is P^2+2*P*Q+Q^2.
^(P, N, R) :- user:mod(N, 2, 1), !,
   user: -(N, 1, M),
   R is P^M*P.
^(P, N, R) :- user: //(N, 2, M),
   H is P^M,
   R is H^2.

% sys_radical_split(+Radical, -Internal, -Internal)
:- private sys_radical_split/3.
sys_radical_split(radical(A, B), P, Q) :-
   sys_sqrt_split(B, U, V),
   sys_new_radical(A, U, P),
   sys_new_radical(0, V, Q).

% sys_sqrt_split(+Map, -Map, -Map)
:- private sys_sqrt_split/3.
sys_sqrt_split([X, Y|L], [Y|P], [X|Q]) :- !,
   sys_sqrt_split(L, P, Q).
sys_sqrt_split(L, [], L).

/*********************************************************************/
/* Radicals                                                          */
/*********************************************************************/

/**
 * sqrt(P, Q):
 * The predicate succeeds in Q with the square root of P.
 */
% sqrt(+Radical, -Radical)
:- override sqrt/2.
:- public sqrt/2.
sqrt(X, _) :- X < 0,
   throw(error(evaluation_error(undefined), _)).
sqrt(X, R) :-
   make_radical(X, R).

% make_radical(+Radical, -Radical)
:- public make_radical/2.
make_radical(X, R) :- integer(X),
   elem:sqrtrem(X, H, J),
   user:(J =:= 0), !,
   R = H.
make_radical(rational(A, B), R) :-
   make_radical(A, H), integer(H),
   make_radical(B, J), integer(J), !,
   R = rational(H, J).
make_radical(radical(0, [A-S]), R) :- !,
   R = radical(0, [radical(0, [A-S])-1]).
make_radical(radical(A, B), Y) :-
   sys_radical_triage(radical(A, B), P, Q),
   D is P^2-Q^2,
   D >= 0,
   sys_radical_level(D, V),
   make_radical(D, H),
   sys_radical_level(H, W),
   user:(W =< V),
   sys_radical_midlevel(radical(A, B), N),
   sys_radical_base(P, N, Z),
   S is (P+H)/2,
   sys_radical_base(S, N, O),
   user:(O =< Z), !,
   make_radical(S, J),
   T is (P-H)/2,
   make_radical(T, K),
   Y is J+sign(Q)*K.
make_radical(X, R) :-
   R = radical(0, [X-1]).

/*********************************************************************/
/* Arithmetic Helper                                                 */
/*********************************************************************/

% sys_radical_neg(+Map, -Map)
:- private sys_radical_neg/2.
sys_radical_neg([A-S|L], [A-T|R]) :-
   user: -(S, T),
   sys_radical_neg(L, R).
sys_radical_neg([], []).

% sys_radical_add(+Map, +Map, -Map)
:- private sys_radical_add/3.
sys_radical_add([A-S|L], H, R) :-
   sys_radical_add2(A, S, H, J), !,
   sys_radical_add(L, J, R).
sys_radical_add([A-S|L], H, U) :-
   sys_radical_add(L, H, R),
   sys_radical_insert(R, A, S, U).
sys_radical_add([], A, A).

% sys_radical_add2(+Internal, +Integer, +Map, -Map)
:- private sys_radical_add2/4.
sys_radical_add2(A, S, [B-T|L], U) :-
   sys_test_lindep(A, B, H), !,
   user: *(T, S, V),
   K is A+2*V*H+B,
   (  K \== 0
   -> (V \== 1 -> W is T*sign(B-A); W = T),
      sys_radical_insert(L, K, W, U)
   ;  U = L).
sys_radical_add2(A, S, [B-T|L], U) :-
   sys_radical_add2(A, S, L, R),
   sys_radical_insert(R, B, T, U).

% sys_radical_sub(+Map, +Map, -Map)
:- private sys_radical_sub/3.
sys_radical_sub([A-S|L], H, R) :-
   sys_radical_sub2(A, S, H, J), !,
   sys_radical_sub(L, J, R).
sys_radical_sub([A-S|L], H, U) :-
   sys_radical_sub(L, H, R),
   sys_radical_insert(R, A, S, U).
sys_radical_sub([], A, B) :-
   sys_radical_neg(A, B).

% sys_radical_sub2(+Internal, +Integer, +Map, -Map)
:- private sys_radical_sub2/4.
sys_radical_sub2(A, S, [B-T|L], U) :-
   sys_test_lindep(A, B, H), !,
   user: *(T, S, V),
   K is A-2*V*H+B,
   (  K \== 0
   -> (V \== -1 -> W is T*sign(B-A); W = T),
      sys_radical_insert(L, K, W, U)
   ;  U = L).
sys_radical_sub2(A, S, [B-T|L], U) :-
   sys_radical_sub2(A, S, L, R),
   sys_radical_insert(R, B, T, U).

% sys_radical_mul(+Map, +Map, -Map, -Internal)
:- private sys_radical_mul/4.
sys_radical_mul([B-T|L], R, S, V) :-
   sys_radical_scale(R, B, T, H, C),
   sys_radical_mul(L, R, J, D),
   sys_radical_add(H, J, S),
   V is C+D.
sys_radical_mul([], _, [], 0).

% sys_radical_scale(+Map, +Internal, +Integer, -Map, -Internal)
:- private sys_radical_scale/5.
sys_radical_scale([B-T|L], A, S, U, V) :-
   D is A*B,
   make_radical(D, H),
   J is S*T*H,
   sys_radical_scale(L, A, S, R, C),
   sys_radical_plus(J, R, C, U, V).
sys_radical_scale([], _, _, [], 0).

% sys_radical_plus(+Internal, +Map, +Internal, -Map, -Internal)
:- private sys_radical_plus/5.
sys_radical_plus(X, L, B, U, V) :- integer(X), !,
   U = L,
   V is X+B.
sys_radical_plus(rational(C, D), L, B, U, V) :- !,
   U = L,
   V is rational(C, D)+B.
sys_radical_plus(radical(C, D), L, B, U, V) :-
   sys_radical_add(D, L, U),
   V is C+B.

/*********************************************************************/
/* Builders                                                         */
/*********************************************************************/

% sys_radical_insert(+Map, +Internal, +Integer, -Map)
:- private sys_radical_insert/4.
sys_radical_insert([B-T|L], A, S, R) :-
   sys_radical_level(A, P),
   sys_radical_level(B, Q),
   (user:(P =:= Q) -> A < B; user:(P > Q)), !,
   R = [A-S, B-T|L].
sys_radical_insert([B-T|L], A, S, [B-T|R]) :-
   sys_radical_insert(L, A, S, R).
sys_radical_insert([], A, S, [A-S]).

% sys_radical_lift(+Rational, +Map, -Map)
:- private sys_radical_lift/3.
sys_radical_lift(0, _, R) :- !,
   R = [].
sys_radical_lift(1, L, R) :- !,
   R = L.
sys_radical_lift(-1, L, R) :- !,
   sys_radical_neg(L, R).
sys_radical_lift(X, L, R) :-
   H is X^2,
   J is sign(X),
   sys_radical_up(L, H, J, R).

% sys_radical_up(+Map, +Rational, +Integer, -Map)
:- private sys_radical_up/4.
sys_radical_up([B-T|L], A, S, [H-J|R]) :-
   H is A*B,
   user: *(S, T, J),
   sys_radical_up(L, A, S, R).
sys_radical_up([], _, _, []).

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
residue:sys_printable_value(X, _) :- var(X), !, fail.
residue:sys_printable_value(radical(0, [A- -1|L]), R) :- !,
   printable(A, H),
   sys_radical_addition(L, -sqrt(H), R).
residue:sys_printable_value(radical(0, [A-1|L]), R) :- !,
   printable(A, H),
   sys_radical_addition(L, sqrt(H), R).
residue:sys_printable_value(radical(A, L), R) :- !,
   printable(A, H),
   sys_radical_addition(L, H, R).

% sys_radical_addition(+List, +External, -External)
:- private sys_radical_addition/3.
sys_radical_addition([A- -1|L], K, R) :- !,
   printable(A, H),
   sys_radical_addition(L, K-sqrt(H), R).
sys_radical_addition([A-1|L], K, R) :-
   printable(A, H),
   sys_radical_addition(L, K+sqrt(H), R).
sys_radical_addition([], A, A).

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
:- meta_predicate generic:is(?, #(1)).
generic:(X is E) :- var(E), !,
   sys_ensure_serno(E),
   sys_freeze_var(E, X).
generic:(X is radical(A, B)) :- !,
   X = radical(A, B).

:- multifile generic:is_abnormal/1.
:- public generic:is_abnormal/1.
generic:is_abnormal(radical(_, _)).
