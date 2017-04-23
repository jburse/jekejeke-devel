/**
 * This module provides integer constants. The module is responsible
 * for the reduction rules that perform partial evaluation. In case
 * that some extra arguments is not integer or the reduction demands
 * it, the rules delegate to the rational, polynom and fraction
 * methods since an integer can be easily also viewed as a rational,
 * polynom or fraction.
 *
 * Examples:
 * ?- X is 1+2.
 * X = 3
 * ?- X is 1+1/2.
 * X is 3/2
 *
 * The reduction rules are just predicates inside the integer module
 * with a Python first argument for the method receiver. We provide
 * reduction rules for basic arithmetic. Bitwise operations and special
 * functions are currently not supported. Equality is also realized by
 * the same mechanism. Error handling is rudimentary.
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

:- module(integer, []).
:- reexport('../gauss/element').

:- use_module(generic).
:- use_module(library(experiment/trail)).
:- use_module(rational).
:- use_module(polynom).
:- use_module(fraction).

/*********************************************************************/
/* Arithmetic                                                        */
/*********************************************************************/

/**
 * -(P, Q):
 * The predicate succeeds in Q with the P negated.
 */
% -(+Integer, -Integer)
:- override (-)/2.
:- public (-)/2.
X - Y :-
   user:X - Y.

/**
 * +(P, Q, R):
 * The predicate succeeds in R with the sum of P and Q.
 */
% +(+Integer, +Internal, -Internal)
:- override (+)/3.
:- public (+)/3.
+(X, Y, Z) :-
   integer(Y), !,
   user: +(X, Y, Z).
+(X, Y, R) :-
   sys_freezer(Y), !,
   sys_make_map([], 0, X, L),
   polynom: +(polynom(Y,L), polynom(Y,[1-1]), R).
+(X, rational(A,B), R) :- !,
   rational: +(rational(X,1), rational(A,B), R).
+(X, polynom(A,B), R) :- !,
   sys_make_map([], 0, X, L),
   polynom: +(polynom(A,L), polynom(A,B), R).
+(X, fraction(A,B), R) :-
   fraction: +(fraction(X,1), fraction(A,B), R).

/**
 * -(P, Q, R):
 * The predicate succeeds in R with P subtracted by Q.
 */
% -(+Integer, +Internal, -Internal)
:- override (-)/3.
:- public (-)/3.
-(X, Y, Z) :-
   integer(Y), !,
   user: -(X, Y, Z).
-(X, Y, R) :-
   sys_freezer(Y), !,
   sys_make_map([], 0, X, L),
   polynom: -(polynom(Y,L), polynom(Y,[1-1]), R).
-(X, rational(A,B), R) :- !,
   rational: -(rational(X,1), rational(A,B), R).
-(X, polynom(A,B), R) :- !,
   sys_make_map([], 0, X, L),
   polynom: -(polynom(A,L), polynom(A,B), R).
-(X, fraction(A,B), R) :-
   fraction: -(fraction(X,1), fraction(A,B), R).

/**
 * *(P, Q, R):
 * The predicate succeeds in R with the product of P and Q.
 */
% *(+Integer, +Internal, -Internal)
:- override * /3.
:- public * /3.
*(X, Y, Z) :-
   integer(Y), !,
   user: *(X, Y, Z).
*(X, Y, R) :-
   sys_freezer(Y), !,
   sys_make_map([], 0, X, L),
   polynom: *(polynom(Y,L), polynom(Y,[1-1]), R).
*(X, rational(A,B), R) :- !,
   rational: *(rational(X,1), rational(A,B), R).
*(X, polynom(A,B), R) :- !,
   sys_make_map([], 0, X, L),
   polynom: *(polynom(A,L), polynom(A,B), R).
*(X, fraction(A,B), R) :-
   fraction: *(fraction(X,1), fraction(A,B), R).

/**
 * /(P, Q, R):
 * The predicate succeeds in R with P divided by Q.
 */
% /(+Integer, +Internal, -Internal)
:- override / /3.
:- public / /3.
/(X, Y, Z) :-
   integer(Y), !,
   make_rational(X, Y, Z).
/(X, Y, R) :-
   sys_freezer(Y), !,
   make_fraction(X, Y, R).
/(X, rational(A,B), R) :- !,
   rational: /(rational(X,1), rational(A,B), R).
/(X, polynom(A,B), R) :- !,
   make_fraction(X, polynom(A,B), R).
/(X, fraction(A,B), R) :-
   fraction: /(fraction(X,1), fraction(A,B), R).

/**
 * ^(P, Q, R):
 * The predicate succeeds in R with P raised by Q.
 */
% ^(+Integer, +Integer, -Integer)
:- override ^ /3.
:- public ^ /3.
^(X, Y, Z) :-
   user: ^(X, Y, Z).

/*********************************************************************/
/* Basic Comparison                                                  */
/*********************************************************************/

:- override gen_eq/2.
:- public gen_eq/2.
gen_eq(X, Y) :-
   integer(Y),
   user:(X =:= Y).
