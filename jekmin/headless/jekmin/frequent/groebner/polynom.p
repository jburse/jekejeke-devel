/**
 * This module provides symbolic polynomials. The module is responsible
 * for the reduction rules that perform simplification. The result
 * can be also an integer, rational or fraction. In case that some
 * extra arguments is a fraction, the rules delegate to the fraction
 * methods since a polynom can be easily also viewed as a fraction.
 * In case some extra argument is an integer or rational, this argument
 * is viewed as a polynom.
 *
 * Examples:
 * ?- X is (1+A)^2.
 * X is 1+2*A+A^2
 * ?- X is (1+A)/(1+A)^2.
 * X is 1/(1+A)
 *
 * The reduction rules are just predicates inside the polynom module
 * with a Python first argument for the method receiver. We provide
 * reduction rules for basic arithmetic. Special functions are
 * currently not supported. Error handling is rudimentary.
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

:- package(library(jekmin/frequent/groebner)).
:- use_package(library(jekmin/frequent/leibniz)).
:- use_package(library(jekpro/frequent/misc)).

:- module(polynom, []).
:- reexport(../gauss/ring).

:- use_module(generic).
:- use_module(fraction).

:- use_module(library(experiment/attr)).
:- use_module(library(experiment/trail)).
:- use_module(library(misc/residue)).

/*********************************************************************/
/* Arithmetic                                                        */
/*********************************************************************/

/**
 * -(P, Q):
 * The predicate succeeds in Q with the P negated.
 */
% -(+Polynom, -Internal)
:- override (-)/2.
:- public (-)/2.
-(polynom(A, B), R) :-
   sys_poly_neg(B, H),
   sys_make_poly(H, A, R).

/**
 * +(P, Q, R):
 * The predicate succeeds in R with the sum of P and Q.
 */
% +(+Polynom, +Internal, -Internal)
:- override (+)/3.
:- public (+)/3.
+(polynom(A, B), Y, R) :- integer(Y), !,
   sys_make_coeff([], 0, Y, L),
   polynom: +(polynom(A, B), polynom(A, L), R).
+(polynom(A, B), rational(C, D), R) :- !,
   polynom: +(polynom(A, B), polynom(A, [0-rational(C, D)]), R).
+(polynom(A, B), radical(C, D), R) :- !,
   polynom: +(polynom(A, B), polynom(A, [0-radical(C, D)]), R).
+(X, Y, R) :- sys_freezer(Y), !,
   polynom: +(X, polynom(Y, [1-1]), R).
+(polynom(A, B), polynom(C, D), R) :- A @> C, !,
   sys_poly_add(B, [0-polynom(C, D)], H),
   sys_make_poly(H, A, R).
+(polynom(A, B), polynom(A, D), R) :- !,
   sys_poly_add(B, D, H),
   sys_make_poly(H, A, R).
+(X, polynom(A, B), R) :- !,
   sys_poly_add([0-X], B, H),
   sys_make_poly(H, A, R).
+(X, fraction(C, D), R) :-
   fraction: +(fraction(X, 1), fraction(C, D), R).

/**
 * -(P, Q, R):
 * The predicate succeeds in R with P subtracted by Q.
 */
% -(+Polynom, +Internal, -Internal)
:- override (-)/3.
:- public (-)/3.
-(polynom(A, B), Y, R) :- integer(Y), !,
   sys_make_coeff([], 0, Y, L),
   polynom: -(polynom(A, B), polynom(A, L), R).
-(polynom(A, B), rational(C, D), R) :- !,
   polynom: -(polynom(A, B), polynom(A, [0-rational(C, D)]), R).
-(polynom(A, B), radical(C, D), R) :- !,
   polynom: -(polynom(A, B), polynom(A, [0-radical(C, D)]), R).
-(X, Y, R) :- sys_freezer(Y), !,
   polynom: -(X, polynom(Y, [1-1]), R).
-(polynom(A, B), polynom(C, D), R) :- A @> C, !,
   sys_poly_sub(B, [0-polynom(C, D)], H),
   sys_make_poly(H, A, R).
-(polynom(A, B), polynom(A, D), R) :- !,
   sys_poly_sub(B, D, H),
   sys_make_poly(H, A, R).
-(X, polynom(A, B), R) :- !,
   sys_poly_sub([0-X], B, H),
   sys_make_poly(H, A, R).
-(X, fraction(C, D), R) :-
   fraction: -(fraction(X, 1), fraction(C, D), R).

/**
 * *(P, Q, R):
 * The predicate succeeds in R with the product of P and Q.
 */
% *(+Polynom, +Internal, -Internal)
:- override * /3.
:- public * /3.
*(polynom(A, B), Y, R) :- integer(Y), !,
   sys_make_coeff([], 0, Y, L),
   polynom: *(polynom(A, B), polynom(A, L), R).
*(polynom(A, B), rational(C, D), R) :- !,
   polynom: *(polynom(A, B), polynom(A, [0-rational(C, D)]), R).
*(polynom(A, B), radical(C, D), R) :- !,
   polynom: *(polynom(A, B), polynom(A, [0-radical(C, D)]), R).
*(X, Y, R) :- sys_freezer(Y), !,
   polynom: *(X, polynom(Y, [1-1]), R).
*(polynom(A, B), polynom(C, D), R) :- A @> C, !,
   sys_poly_mul(B, [0-polynom(C, D)], H),
   sys_make_poly(H, A, R).
*(polynom(A, B), polynom(A, D), R) :- !,
   sys_poly_mul(B, D, H),
   sys_make_poly(H, A, R).
*(X, polynom(A, B), R) :- !,
   sys_poly_mul([0-X], B, H),
   sys_make_poly(H, A, R).
*(X, fraction(C, D), R) :-
   fraction: *(fraction(X, 1), fraction(C, D), R).

/**
 * /(P, Q, R):
 * The predicate succeeds in R with P divided by Q.
 */
% /(+Polynom, +Internal, -Internal)
:- override / /3.
:- public / /3.
/(X, Y, R) :- integer(Y), !,
   R is X*(1/Y).
/(X, rational(C, D), R) :- !,
   R is X*(1/rational(C, D)).
/(X, radical(C, D), R) :- !,
   R is X*(1/radical(C, D)).
/(X, Y, R) :- sys_freezer(Y), !,
   make_fraction(X, Y, R).
/(X, polynom(C, D), R) :- !,
   make_fraction(X, polynom(C, D), R).
/(X, fraction(C, D), R) :-
   fraction: /(fraction(X, 1), fraction(C, D), R).

/**
 * ^(P, Q, R):
 * The predicate succeeds in R with P raised by Q.
 */
% ^(+Polynom, +Integer, -Internal)
:- override ^ /3.
:- public ^ /3.
^(P, Y, R) :- user:(Y < 0), !,
   user: -(Y, Z),
   R is (1/P)^Z.
^(_, 0, R) :- !,
   R = 1.
^(P, 1, R) :- !,
   R = P.
^(polynom(A, [N-B]), 2, R) :- !,
   user: *(N, 2, M),
   C is B^2,
   R = polynom(A, [M-C]).
^(X, 2, R) :- !,
   sys_poly_split(X, P, Q),
   R is P^2+2*P*Q+Q^2.
^(P, N, R) :- user:mod(N, 2, 1), !,
   user: -(N, 1, M),
   R is P^M*P.
^(P, N, R) :- user: //(N, 2, M),
   H is P^M,
   R is H^2.

% sys_poly_split(+Polynom, -Internal, -Internal)
:- private sys_poly_split/3.
sys_poly_split(polynom(A, B), P, Q) :-
   sys_coeff_split(B, U, V),
   sys_make_poly(U, A, P),
   sys_make_poly(V, A, Q).

% sys_coeff_split(+Map, -Map, -Map)
:- private sys_coeff_split/3.
sys_coeff_split([X, Y|L], [X|P], [Y|Q]) :- !,
   sys_coeff_split(L, P, Q).
sys_coeff_split(L, [], L).

/*********************************************************************/
/* Arithmetic Helper                                                 */
/*********************************************************************/

% sys_poly_neg(+Map, -Map)
:- private sys_poly_neg/2.
sys_poly_neg([N-A|L], [N-B|R]) :-
   B is -A,
   sys_poly_neg(L, R).
sys_poly_neg([], []).

% sys_poly_add(+Map, +Map, -Map)
:- private sys_poly_add/3.
sys_poly_add([N-A|L], [M-B|R], [N-A|S]) :- user:(N > M), !,
   sys_poly_add(L, [M-B|R], S).
sys_poly_add([N-A|L], [N-B|R], T) :- !,
   C is A+B,
   sys_poly_add(L, R, H),
   sys_make_coeff(H, N, C, T).
sys_poly_add([N-A|L], [M-B|R], [M-B|S]) :-
   sys_poly_add([N-A|L], R, S).
sys_poly_add([], L, L) :- !.
sys_poly_add(L, [], L).

% sys_poly_sub(+Map, +Map, -Map)
:- private sys_poly_sub/3.
sys_poly_sub([N-A|L], [M-B|R], [N-A|S]) :- user:(N > M), !,
   sys_poly_sub(L, [M-B|R], S).
sys_poly_sub([N-A|L], [N-B|R], T) :- !,
   C is A-B,
   sys_poly_sub(L, R, H),
   sys_make_coeff(H, N, C, T).
sys_poly_sub([N-A|L], [M-B|R], [M-C|S]) :-
   C is -B,
   sys_poly_sub([N-A|L], R, S).
sys_poly_sub([], L, R) :- !,
   sys_poly_neg(L, R).
sys_poly_sub(L, [], L).

% sys_poly_mul(+Map, +Map, -Map)
:- private sys_poly_mul/3.
sys_poly_mul([N-A|L], R, S) :-
   sys_poly_scale(R, N, A, H),
   sys_poly_mul(L, R, J),
   sys_poly_add(H, J, S).
sys_poly_mul([], _, []).

% sys_poly_scale(+Map, +Integer, +Internal, -Map)
:- private sys_poly_scale/4.
sys_poly_scale([N-A|L], M, B, [K-C|R]) :-
   K is M+N,
   C is B*A,
   sys_poly_scale(L, M, B, R).
sys_poly_scale([], _, _, []).

% sys_make_coeff(+Map, +Integer, +Internal, -Map)
:- public sys_make_coeff/4.
sys_make_coeff(L, _, 0, R) :- !,
   R = L.
sys_make_coeff(L, N, A, [N-A|L]).

% sys_make_poly(+Map, +Ref, -Internal)
:- public sys_make_poly/3.
sys_make_poly([], _, R) :- !,
   R = 0.
sys_make_poly([1-1], A, R) :- !,
   R = A.
sys_make_poly([0-A], _, R) :- !,
   R = A.
sys_make_poly(B, A, polynom(A, B)).

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
residue:sys_printable_value(polynom(A, B), F) :- !,
   sys_pretty_poly(B, A, H),
   sys_melt_expr(H, F).

% sys_pretty_poly(+Map, +Var, -Expression)
:- private sys_pretty_poly/3.
sys_pretty_poly([N-B|L], A, F) :-
   sys_pretty_poly(L, A, G),
   sys_make_power(A, N, H),
   sys_pretty_expr(B, J),
   sys_make_mul(J, H, K),
   sys_make_add(G, K, F).
sys_pretty_poly([], _, 0).

% sys_pretty_expr(+Internal, -Expression)
:- private sys_pretty_expr/2.
sys_pretty_expr(E, X) :- sys_freezer(E), !,
   X = E.
sys_pretty_expr(E, X) :- integer(E), !,
   printable(E, X).
sys_pretty_expr(rational(A, B), X) :- !,
   printable(rational(A, B), X).
sys_pretty_expr(radical(A, B), X) :- !,
   printable(radical(A, B), X).
sys_pretty_expr(polynom(A, B), X) :-
   sys_pretty_poly(B, A, X).

% sys_make_power(+Var, +Integer, -Expression)
:- private sys_make_power/3.
sys_make_power(_, 0, 1) :- !.
sys_make_power(Y, 1, Y) :- !.
sys_make_power(Y, N, Y^N).

% sys_make_mul(+Expression, +Expression, -Expression)
:- private sys_make_mul/3.
sys_make_mul(X, 1, X) :- !.
sys_make_mul(1, X, X) :- !.
sys_make_mul(X, Y, -H) :- sys_has_sign(X), !,
   sys_make_neg(X, Z),
   sys_make_mul(Z, Y, H).
sys_make_mul(X, Y, X*Y).

% sys_has_sign(+Expression)
:- private sys_has_sign/1.
sys_has_sign(-_).
sys_has_sign(X-_) :- sys_has_sign(X).
sys_has_sign(X+_) :- sys_has_sign(X).

% sys_make_neg(+Expression, -Expression)
:- private sys_make_neg/2.
sys_make_neg(-X, X).
sys_make_neg(X-Y, Z+Y) :- sys_make_neg(X, Z).
sys_make_neg(X+Y, Z-Y) :- sys_make_neg(X, Z).

% sys_make_add(+Expression, +Expression, -Expression)
:- private sys_make_add/3.
sys_make_add(0, X, X) :- !.
sys_make_add(X, Y+Z, T) :- !,
   sys_make_add(X, Y, H),
   sys_make_add(H, Z, T).
sys_make_add(X, Y-Z, T) :- !,
   sys_make_add(X, Y, H),
   sys_make_add(H, -Z, T).
sys_make_add(X, -Y, X-Y) :- !.
sys_make_add(X, Y, X+Y).

/*********************************************************************/
/* Melt Helper                                                       */
/*********************************************************************/

% sys_melt_expr(+Expression, -Expression)
:- private sys_melt_expr/2.
sys_melt_expr(E, X) :- sys_freezer(E), !,
   sys_melt_var(E, X).
sys_melt_expr(E, X) :- integer(E), !,
   X = E.
sys_melt_expr(E, X) :-
   E =.. [F|L],
   sys_melt_list(L, R),
   X =.. [F|R].

% sys_melt_expr(+List, -List)
:- private sys_melt_list/2.
sys_melt_list([E|L], [X|R]) :-
   sys_melt_expr(E, X),
   sys_melt_list(L, R).
sys_melt_list([], []).

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
:- meta_predicate generic:is(?, 1).
generic:(X is E) :- var(E), !,
   sys_ensure_serno(E),
   sys_freeze_var(E, X).
generic:(X is polynom(A, B)) :- !,
   X = polynom(A, B).

:- multifile generic:is_abnormal/1.
:- public generic:is_abnormal/1.
generic:is_abnormal(polynom(_, _)).
