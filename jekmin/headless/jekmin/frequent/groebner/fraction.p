/**
 * This module provides symbolic fractions. The module is responsible
 * for the reduction rules that perform simplification. The result
 * can be also an integer, rational or polynom. In case some extra
 * argument is an integer, rational or polynom, this argument is
 * viewed as a fraction. Common factors among the numerator and
 * denumerator are determined by a Gröbner Basis algorithm
 * and cancelled.
 *
 * Examples:
 * ?- X is 1/A+1/B.
 * X is (A+B)/(A*B)
 * ?- X is (A*B^2-B)/(A^2*B-A).
 * X is B/A
 *
 * The reduction rules are just predicates inside the fraction module
 * with a Python first argument for the method receiver. We provide
 * reduction rules for basic arithmetic. Special functions are currently
 * not supported. Error handling is rudimentary. Cancellation does not
 * yet generate non-zero side conditions.
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
:- use_package(library(jekmin/frequent/gauss)).
:- use_package(library(jekmin/reference/misc)).
:- use_package(library(jekpro/frequent/misc)).

:- module(fraction, []).
:- reexport(../gauss/element).

:- use_module(library(misc/residue)).
:- use_module(library(basic/lists)).
:- use_module(generic).
:- use_module(polynom).
:- use_module(rational).
:- use_module(../gauss/ring).
:- use_module(../leibniz/radical).

:- use_module(library(experiment/attr)).
:- use_module(library(experiment/trail)).

/*********************************************************************/
/* Arithmetic                                                        */
/*********************************************************************/

/**
 * -(P, Q):
 * The predicate succeeds in Q with the P negated.
 */
% -(+Fracton, -Fracton)
:- override (-)/2.
:- public (-)/2.
fraction(A,B) - fraction(C,B) :-
   C is -A.

/**
 * +(P, Q, R):
 * The predicate succeeds in R with the sum of P and Q.
 */
% +(+Fracton, +Internal, -Internal)
:- override (+)/3.
:- public (+)/3.
+(X, Y, R) :-
   integer(Y), !,
   fraction: +(X, fraction(Y,1), R).
+(X, rational(C,D), R) :- !,
   fraction: +(X, fraction(rational(C,D),1), R).
+(X, radical(C,D), R) :- !,
   fraction: +(X, fraction(radical(C,D),1), R).
+(X, Y, R) :-
   sys_freezer(Y), !,
   fraction: +(X, fraction(Y,1), R).
+(X, polynom(C,D), R) :- !,
   fraction: +(X, fraction(polynom(C,D),1), R).
+(fraction(A,B), fraction(C,D), R) :-
   H is A*D+B*C,
   J is B*D,
   make_fraction(H, J, R).

/**
 * -(P, Q, R):
 * The predicate succeeds in R with P subtracted by Q.
 */
% -(+Fracton, +Internal, -Internal)
:- override (-)/3.
:- public (-)/3.
-(X, Y, R) :-
   integer(Y), !,
   fraction: -(X, fraction(Y,1), R).
-(X, rational(C,D), R) :- !,
   fraction: -(X, fraction(rational(C,D),1), R).
-(X, radical(C,D), R) :- !,
   fraction: -(X, fraction(radical(C,D),1), R).
-(X, Y, R) :-
   sys_freezer(Y), !,
   fraction: -(X, fraction(Y,1), R).
-(X, polynom(C,D), R) :- !,
   fraction: -(X, fraction(polynom(C,D),1), R).
-(fraction(A,B), fraction(C,D), R) :-
   H is A*D-B*C,
   J is B*D,
   make_fraction(H, J, R).

/**
 * *(P, Q, R):
 * The predicate succeeds in R with the product of P and Q.
 */
% *(+Fracton, +Internal, -Internal)
:- override * /3.
:- public * /3.
*(X, Y, R) :-
   integer(Y), !,
   fraction: *(X, fraction(Y,1), R).
*(X, rational(C,D), R) :- !,
   fraction: *(X, fraction(rational(C,D),1), R).
*(X, radical(C,D), R) :- !,
   fraction: *(X, fraction(radical(C,D),1), R).
*(X, Y, R) :-
   sys_freezer(Y), !,
   fraction: *(X, fraction(Y,1), R).
*(X, polynom(C,D), R) :- !,
   fraction: *(X, fraction(polynom(C,D),1), R).
*(fraction(A,B), fraction(C,D), R) :-
   H is A*C,
   J is B*D,
   make_fraction(H, J, R).

/**
 * /(P, Q, R):
 * The predicate succeeds in R with P divided by Q.
 */
% /(+Fracton, +Internal, -Internal)
:- override / /3.
:- public / /3.
/(X, Y, R) :-
   integer(Y), !,
   fraction: /(X, fraction(Y,1), R).
/(X, rational(C,D), R) :- !,
   fraction: /(X, fraction(rational(C,D),1), R).
/(X, radical(C,D), R) :- !,
   fraction: /(X, fraction(radical(C,D),1), R).
/(X, Y, R) :-
   sys_freezer(Y), !,
   fraction: /(X, fraction(Y,1), R).
/(X, polynom(C,D), R) :- !,
   fraction: /(X, fraction(polynom(C,D),1), R).
/(fraction(A,B), fraction(C,D), R) :-
   H is A*D,
   J is B*C,
   make_fraction(H, J, R).

/**
 * ^(P, Q, R):
 * The predicate succeeds in R with P raised by Q.
 */
% ^(+Fracton, +Integer, -Internal)
:- override ^ /3.
:- public ^ /3.
^(fraction(A,B), Y, R) :-
   user:(Y < 0), !,
   user:Y - Z,
   new_fraction(B, A, H),
   R is H^Z.
^(_, 0, R) :- !,
   R = 1.
^(fraction(A,B), Y, fraction(H,J)) :-
   H is A^Y,
   J is B^Y.

/*********************************************************************/
/* Polynomial Normlization                                           */
/*********************************************************************/

% make_fraction(+Internal, +Internal, -Internal)
make_fraction(F, G, R) :-
   sys_poly_norm(F, G, A, B),
   new_fraction(A, B, R).

% sys_poly_norm(+Internal, +Internal, -Internal, -Internal)
:- private sys_poly_norm/4.
sys_poly_norm(0, _, A, B) :- !,
   A = 0,
   B = 1.
sys_poly_norm(_, 0, A, B) :- !,
   A = 1,
   B = 0.
sys_poly_norm(F, G, A, B) :-
   sys_poly_lcm(F, G, K),
   sys_poly_div(K, G, A, _),
   sys_poly_div(K, F, B, _).

% sys_poly_lcm(+Internal, +Internal, -Internal)
:- private sys_poly_lcm/3.
sys_poly_lcm(A, B, C) :-
   S is A*Z,
   T is B*(1-Z),
   sys_poly_groeb([S,T], L),
   sys_poly_min(L, C).

% new_fraction(+Internal, +Internal, -Internal)
:- public new_fraction/3.
new_fraction(_, 0, _) :-
   throw(error(evaluation_error(zero_divisor),_)).
new_fraction(0, _, R) :- !,
   R = 0.
new_fraction(U, V, R) :-
   integer(V), !,
   R is U/V.
new_fraction(U, rational(C,D), R) :- !,
   R is U/rational(C,D).
new_fraction(U, radical(C,D), R) :- !,
   R is U/radical(C,D).
new_fraction(U, V, R) :-
   sys_poly_factor(V, K),
   K \== 1,
   K \== -1, !,
   L is 1/K,
   A is L*U,
   B is L*V,
   new_fraction2(A, B, R).
new_fraction(U, V, R) :-
   new_fraction2(U, V, R).

% new_fraction2(+Internal, +Internal, -Internal)
:- private new_fraction2/3.
new_fraction2(U, V, R) :-
   sys_poly_sign(V, S),
   S \== 1, !,
   A is -U,
   B is -V,
   R = fraction(A,B).
new_fraction2(A, B, fraction(A,B)).

/*********************************************************************/
/* Polynomial Groebner                                               */
/*********************************************************************/

% sys_poly_groeb(+List, -List)
:- public sys_poly_groeb/2.
sys_poly_groeb(L, R) :-
   length(L, N),
   sys_init_pairs(N, P),
   sys_poly_groeb(L, P, R).

% sys_init_pairs(+Integer, -List)
:- private sys_init_pairs/2.
sys_init_pairs(1, []) :- !.
sys_init_pairs(N, L) :-
   M is N-1,
   sys_init_pairs(M, H),
   sys_nudge_pairs(H, K),
   sys_new_pairs(M, K, L).

% sys_poly_groeb(+List, +List, -List)
:- private sys_poly_groeb/3.
sys_poly_groeb(L, P, T) :-
   nth0(I, L, 0, S), !,
   sys_shrink_pairs(P, I, Q),
   sys_poly_groeb(S, Q, T).
sys_poly_groeb(L, P, V) :-
   nth0(J, L, F, S),
   member(G, S),
   sys_poly_head(G, H),
   sys_poly_div(F, H, K, N),
   K \== 0, !,
   M is N-K*(G-H),
   nth0(J, U, M, S),
   sys_poly_groeb(U, P, V).
sys_poly_groeb(L, P, R) :-
   sys_pair_best(P, L, (I,J), _, H), !,
   nth0(I, L, X),
   nth0(J, L, Y),
   sys_poly_pair(X, Y, Z),
   sys_nudge_pairs(H, K),
   length(L, N),
   sys_new_pairs(N, K, Q),
   sys_poly_groeb([Z|L], Q, R).
sys_poly_groeb(L, _, L).

% sys_nudge_pairs(+List, -List)
:- private sys_nudge_pairs/2.
sys_nudge_pairs([(A,B)|L], [(C,D)|R]) :-
   user: +(A, 1, C),
   user: +(B, 1, D),
   sys_nudge_pairs(L, R).
sys_nudge_pairs([], []).

% sys_new_pairs(+Integer, +List, -List)
:- private sys_new_pairs/3.
sys_new_pairs(0, L, L) :- !.
sys_new_pairs(N, L, [(0,N)|R]) :-
   user: -(N, 1, M),
   sys_new_pairs(M, L, R).

% sys_shrink_pairs(+List, +Integer, -List)
:- private sys_shrink_pairs/3.
sys_shrink_pairs([(K,_)|L], K, R) :- !,
   sys_shrink_pairs(L, K, R).
sys_shrink_pairs([(_,K)|L], K, R) :- !,
   sys_shrink_pairs(L, K, R).
sys_shrink_pairs([(A,B)|L], K, [(C,D)|R]) :-
   sys_shrink_index(A, K, C),
   sys_shrink_index(B, K, D),
   sys_shrink_pairs(L, K, R).
sys_shrink_pairs([], _, []).

% sys_shrink_index(+Integer, +Integer, -Integer)
:- private sys_shrink_index/3.
sys_shrink_index(A, K, C) :-
   user:(A > K), !,
   user: -(A, 1, C).
sys_shrink_index(A, _, A).

/*********************************************************************/
/* Pair Sorting                                                      */
/*********************************************************************/

% sys_pair_best(+List, +List, -Pair, -Integer, -List)
:- private sys_pair_best/5.
sys_pair_best([A,Q|R], L, C, F, J) :-
   sys_pair_best([Q|R], L, B, E, H),
   sys_pair_score(A, L, D),
   (  sys_head_compare(>, D, E)
   -> C = A,
      F = D,
      J = [B|H]
   ;  C = B,
      F = E,
      J = [A|H]).
sys_pair_best([A], L, A, D, []) :-
   sys_pair_score(A, L, D).

% sys_pair_score(+Pair, +List, -Integer)
:- private sys_pair_score/3.
sys_pair_score((I,J), L, D) :-
   nth0(I, L, X),
   nth0(J, L, Y),
   sys_poly_score(X, Y, D).

% sys_poly_score(+Internal, +Internal, -Integer)
:- private sys_poly_score/3.
sys_poly_score(X, Y, K) :-
   sys_poly_head(X, H),
   sys_poly_head(Y, J),
   sys_head_gcd(H, J, K).

/*********************************************************************/
/* Buchberger S-Polynomial                                           */
/*********************************************************************/

% sys_poly_pair(+Internal, +Internal, -Internal)
:- private sys_poly_pair/3.
sys_poly_pair(X, Y, Z) :-
   sys_poly_head(X, H),
   sys_poly_head(Y, J),
   sys_head_gcd(H, J, K),
   sys_head_div(H, K, U),
   sys_head_div(J, K, V),
   Z is X*V-Y*U.

% sys_head_gcd(+Monomial, +Monomial, -Monomial)
:- private sys_head_gcd/3.
sys_head_gcd(polynom(A,[_-B]), polynom(C,D), R) :-
   A @> C, !,
   sys_head_gcd(B, polynom(C,D), R).
sys_head_gcd(polynom(A,[N-B]), polynom(A,[M-D]), R) :- !,
   sys_head_gcd(B, D, H),
   user:min(N, M, K),
   R = polynom(A,[K-H]).
sys_head_gcd(polynom(A,B), polynom(_,[_-D]), R) :- !,
   sys_head_gcd(polynom(A,B), D, R).
sys_head_gcd(polynom(_,[_-B]), Y, R) :- !,
   sys_head_gcd(B, Y, R).
sys_head_gcd(X, polynom(_,[_-D]), R) :- !,
   sys_head_gcd(X, D, R).
sys_head_gcd(_, _, 1).

% sys_head_div(+Monomial, +Monomial, -Internal)
:- private sys_head_div/3.
sys_head_div(polynom(A,[N-B]), polynom(C,D), R) :-
   A @> C, !,
   sys_head_div(B, polynom(C,D), H),
   sys_make_poly([N-H], A, R).
sys_head_div(polynom(A,[N-B]), polynom(A,[M-D]), R) :- !,
   sys_head_div(B, D, H),
   user: -(N, M, K),
   sys_make_poly([K-H], A, R).
sys_head_div(polynom(_,_), polynom(_,_), _) :-
   throw(error(illegal_state,_)).
sys_head_div(polynom(A,[N-B]), Y, R) :- !,
   sys_head_div(B, Y, H),
   sys_make_poly([N-H], A, R).
sys_head_div(_, polynom(_,_), _) :-
   throw(error(illegal_state,_)).
sys_head_div(X, _, X).

/*********************************************************************/
/* Polynomial Ordering                                               */
/*********************************************************************/

% sys_poly_min(+List, -Internal)
:- private sys_poly_min/2.
sys_poly_min([X,Y|L], R) :-
   sys_poly_min([Y|L], H),
   sys_poly_compare(O, X, H),
   O \== <, !,
   R = H.
sys_poly_min([X|_], X).

% sys_poly_compare(-Ordering, +Internal, +Internal)
:- private sys_poly_compare/3.
sys_poly_compare(O, F, G) :-
   sys_poly_head(F, H),
   sys_poly_head(G, J),
   sys_head_compare(O, H, J).

% sys_head_compare(-Ordering, +Monominal, +Monomial)
:- private sys_head_compare/3.
sys_head_compare(O, polynom(A,_), polynom(C,_)) :-
   A @> C, !,
   O = > .
sys_head_compare(O, polynom(A,B), polynom(A,D)) :- !,
   sys_coeff_compare(O, B, D).
sys_head_compare(O, polynom(_,_), polynom(_,_)) :- !,
   O = < .
sys_head_compare(O, polynom(_,_), _) :- !,
   O = > .
sys_head_compare(O, _, polynom(_,_)) :- !,
   O = < .
sys_head_compare(=, _, _).

% sys_coeff_compare(-Ordering, +List, +List)
:- private sys_coeff_compare/3.
sys_coeff_compare(O, [N-_], [M-_]) :-
   user:(N > M), !,
   O = > .
sys_coeff_compare(O, [N-A], [N-B]) :- !,
   sys_head_compare(O, A, B).
sys_coeff_compare(<, _, _).

/*********************************************************************/
/* CAS BindCount[] Hook                                                  */
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
residue:sys_printable_value(fraction(A,B), F) :-
   sys_poly_div(A, B, Q, R),
   Q \== 0, !,
   printable(Q, K),
   sys_make_addition(K, fraction(R,B), F).
residue:sys_printable_value(fraction(A,B), F) :-
   sys_poly_sign(A, S),
   S \== 1, !,
   C is -A,
   printable(C, H),
   printable(B, J),
   F = -H/J.
residue:sys_printable_value(fraction(A,B), F) :- !,
   printable(A, H),
   printable(B, J),
   F = H/J.

% sys_make_addition(+External, +Fraction, -External)
:- private sys_make_addition/3.
sys_make_addition(K, fraction(R,B), F) :-
   sys_poly_sign(R, S),
   S \== 1, !,
   T is -R,
   printable(T, H),
   printable(B, J),
   F = K-H/J.
sys_make_addition(K, fraction(R,B), F) :-
   printable(R, H),
   printable(B, J),
   F = K+H/J.

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
generic:(X is fraction(A,B)) :- !,
   X = fraction(A,B).

:- multifile generic:is_abnormal/1.
:- public generic:is_abnormal/1.
generic:is_abnormal(fraction(_,_)).
