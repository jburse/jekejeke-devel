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

:- use_module(library(experiment/trail)).
:- use_module(library(misc/residue)).
:- use_module(library(basic/lists)).

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
radical(A,B) - radical(C,D) :-
   C is -A,
   sys_radical_neg(B, D).

/**
 * +(P, Q, R):
 * The predicate succeeds in R with the sum of P and Q.
 */
% +(+Radical, +Internal, -Internal)
:- override (+)/3.
:- public (+)/3.
+(X, Y, R) :-
   integer(Y), !,
   radical: +(X, radical(Y,[]), R).
+(X, rational(C,D), R) :- !,
   radical: +(X, radical(rational(C,D),[]), R).
+(radical(A,B), radical(C,D), R) :- !,
   H is A+C,
   sys_radical_add(B, D, J),
   sys_new_radical(H, J, R).
+(X, Y, R) :-
   sys_freezer(Y), !,
   polynom: +(polynom(Y,[0-X]), polynom(Y,[1-1]), R).
+(X, polynom(C,D), R) :- !,
   polynom: +(polynom(C,[0-X]), polynom(C,D), R).
+(X, fraction(C,D), R) :-
   fraction: +(fraction(X,1), fraction(C,D), R).

/**
 * -(P, Q, R):
 * The predicate succeeds in R with P subtracted by Q.
 */
% -(+Radical, +Internal, -Internal)
:- override (-)/3.
:- public (-)/3.
-(X, Y, R) :-
   integer(Y), !,
   radical: -(X, radical(Y,[]), R).
-(X, rational(C,D), R) :- !,
   radical: -(X, radical(rational(C,D),[]), R).
-(radical(A,B), radical(C,D), R) :- !,
   H is A-C,
   sys_radical_sub(B, D, J),
   sys_new_radical(H, J, R).
-(X, Y, R) :-
   sys_freezer(Y), !,
   polynom: -(polynom(Y,[0-X]), polynom(Y,[1-1]), R).
-(X, polynom(C,D), R) :- !,
   polynom: -(polynom(C,[0-X]), polynom(C,D), R).
-(X, fraction(C,D), R) :-
   fraction: -(fraction(X,1), fraction(C,D), R).

/**
 * *(P, Q, R):
 * The predicate succeeds in R with the product of P and Q.
 */
% *(+Radical, +Internal, -Internal)
:- override * /3.
:- public * /3.
*(X, Y, R) :-
   integer(Y), !,
   radical: *(X, radical(Y,[]), R).
*(X, rational(C,D), R) :- !,
   radical: *(X, radical(rational(C,D),[]), R).
*(radical(A,B), radical(C,D), R) :- !,
   sys_radical_lift(A, D, K),
   sys_radical_lift(C, B, L),
   sys_radical_add(L, K, M),
   sys_radical_mul(B, D, N, V),
   sys_radical_add(M, N, U),
   H is A*C+V,
   sys_new_radical(H, U, R).
*(X, Y, R) :-
   sys_freezer(Y), !,
   polynom: *(polynom(Y,[0-X]), polynom(Y,[1-1]), R).
*(X, polynom(C,D), R) :- !,
   polynom: *(polynom(C,[0-X]), polynom(C,D), R).
*(X, fraction(C,D), R) :-
   fraction: *(fraction(X,1), fraction(C,D), R).

/**
 * /(P, Q, R):
 * The predicate succeeds in R with P divided by Q.
 */
% /(+Integer, +Internal, -Internal)
:- override / /3.
:- public / /3.
/(X, Y, R) :-
   integer(Y), !,
   R is X*(1/Y).
/(X, rational(C,D), R) :- !,
   R is X*(1/rational(C,D)).
/(X, radical(C,D), R) :- !,
   sys_swinnerton_dyer(radical(C,D), S),
   R is X*S/(radical(C,D)*S).
/(X, Y, R) :-
   sys_freezer(Y), !,
   new_fraction(X, Y, R).
/(X, polynom(C,D), R) :- !,
   new_fraction(X, polynom(C,D), R).
/(X, fraction(C,D), R) :-
   fraction: /(fraction(X,1), fraction(C,D), R).

/**
 * ^(P, Q, R):
 * The predicate succeeds in R with P raised by Q.
 */
% ^(+Radical, +Integer, -Internal)
:- override ^ /3.
:- public ^ /3.
^(P, Y, R) :-
   user:(Y < 0), !,
   user:Y - Z,
   R is 1/P^Z.
^(_, 0, R) :- !,
   R = 1.
^(P, 1, R) :- !,
   R = P.
^(P, 2, R) :- !,
   R is P*P.
^(P, N, R) :-
   user:mod(N, 2, 1), !,
   user: -(N, 1, M),
   R is P^M*P.
^(P, N, R) :-
   user: //(N, 2, M),
   H is P^M,
   R is H*H.

/*********************************************************************/
/* Radicals                                                          */
/*********************************************************************/

% make_radical(+Ordered, -Internal)
:- public make_radical/2.
make_radical(X, R) :-
   has_sqrt(X, Y), !,
   R = Y.
make_radical(X, radical(0,[X-1])).

% has_sqrt(+Ordered, -Ordered)
:- private has_sqrt/2.
has_sqrt(X, Y) :-
   integer(X), !,
   elem:isqrt(X, Y),
   user: *(Y, Y, H),
   user:(X =:= H).
has_sqrt(rational(A,B), R) :-
   has_sqrt(A, X),
   has_sqrt(B, Y),
   make_rational(X, Y, R).

/*********************************************************************/
/* Swinnerton-Dyer Polynomial                                        */
/*********************************************************************/

% sys_swinnerton_dyer(+Radical, -Internal)
:- private sys_swinnerton_dyer/2.
sys_swinnerton_dyer(radical(A,B), R) :-
   sys_swinnerton_dyer2(B, A, [], 1, R).

% sys_swinnerton_dyer2(+Map, +Internal, +Map, +Integer, -Internal)
:- private sys_swinnerton_dyer2/5.
sys_swinnerton_dyer2([A-S,C|L], B, U, 1, R) :- !,
   user:S - T,
   sys_swinnerton_dyer2([C|L], B, [A-T|U], 0, P),
   sys_swinnerton_dyer2([C|L], B, [A-S|U], 1, Q),
   R is P*Q.
sys_swinnerton_dyer2([A-S,C|L], B, U, 0, R) :- !,
   user:S - T,
   sys_swinnerton_dyer2([C|L], B, [A-T|U], 0, P),
   sys_swinnerton_dyer2([C|L], B, [A-S|U], 0, Q),
   R is P*Q.
sys_swinnerton_dyer2([A-S], B, L, 1, R) :- !,
   user:S - T,
   reverse([A-T|L], H),
   R = radical(B,H).
sys_swinnerton_dyer2([A-S], B, L, 0, R) :-
   user:S - T,
   reverse([A-T|L], H),
   reverse([A-S|L], J),
   R is radical(B,H)*radical(B,J).

/*********************************************************************/
/* Arithmetic Helper                                                 */
/*********************************************************************/

% sys_radical_neg(+Map, -Map)
:- public sys_radical_neg/2.
sys_radical_neg([A-S|L], [A-T|R]) :-
   user:S - T,
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
   H is A/B,
   has_sqrt(H, R), !,
   J is T+S*R,
   sys_radical_sqrt(J, B, L, U).
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
   H is A/B,
   has_sqrt(H, R), !,
   J is T-S*R,
   sys_radical_sqrt(J, B, L, U).
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
   sys_radical_scale(L, A, S, R, C),
   H is A*B,
   user: *(S, T, J),
   sys_radical_scale2(H, J, R, C, U, V).
sys_radical_scale([], _, _, [], 0).

% sys_radical_scale2(+Internal, +Integer, +Map, +Internal, -Map, -Internal)
:- private sys_radical_scale2/6.
sys_radical_scale2(A, S, L, B, U, V) :-
   has_sqrt(A, R), !,
   U = L,
   V is B+S*R.
sys_radical_scale2(A, S, L, B, [A-S|L], B).

/*********************************************************************/
/* Builders                                                         */
/*********************************************************************/

% sys_new_radical(+Internal, +Map, -Internal)
:- private sys_new_radical/3.
sys_new_radical(A, [], R) :- !,
   R = A.
sys_new_radical(A, L, radical(A,L)).

% sys_radical_sqrt(+Internal, +Internal, +Map, -Map)
:- private sys_radical_sqrt/4.
sys_radical_sqrt(0, _, L, R) :- !,
   R = L.
sys_radical_sqrt(J, B, L, R) :-
   S is sign(J),
   A is J*J*B,
   sys_radical_insert(L, A, S, R).

% sys_radical_insert(+Map, +Internal, +Integer, -Map)
:- private sys_radical_insert/4.
sys_radical_insert([B-T|L], A, S, R) :-
   A < B, !,
   R = [A-S,B-T|L].
sys_radical_insert([B-T|L], A, S, [B-T|R]) :-
   sys_radical_insert(L, A, S, R).
sys_radical_insert([], A, S, [A-S]).

% sys_radical_lift(+Internal, +Map, -Map)
:- private sys_radical_lift/3.
sys_radical_lift(0, _, R) :- !,
   R = [].
sys_radical_lift(X, L, R) :-
   A is X*X,
   S is sign(X),
   sys_radical_up(L, A, S, R).

% sys_radical_up(+Map, +Internal, +Integer, -Map)
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
residue:sys_printable_value(X, _) :-
   var(X), !, fail.
residue:sys_printable_value(radical(0,[A- -1|L]), R) :- !,
   printable(A, H),
   sys_radical_printable(L, -sqrt(H), R).
residue:sys_printable_value(radical(0,[A-1|L]), R) :- !,
   printable(A, H),
   sys_radical_printable(L, sqrt(H), R).
residue:sys_printable_value(radical(A,L), R) :- !,
   printable(A, H),
   sys_radical_printable(L, H, R).

% sys_radical_printable(+List, +External, -External)
:- private sys_radical_printable/3.
sys_radical_printable([A- -1|L], J, R) :- !,
   printable(A, H),
   sys_radical_printable(L, J-sqrt(H), R).
sys_radical_printable([A-1|L], J, R) :-
   printable(A, H),
   sys_radical_printable(L, J+sqrt(H), R).
sys_radical_printable([], A, A).

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
generic:(X is radical(A,B)) :- !,
   X = radical(A,B).

:- multifile generic:is_abnormal/1.
:- public generic:is_abnormal/1.
generic:is_abnormal(radical(_,_)).
