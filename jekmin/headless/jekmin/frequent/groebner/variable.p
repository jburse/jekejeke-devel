/**
 * This module provides symbolic variables. The module is responsible
 * for the reduction rules that perform simplification. The result can
 * be also an integer, polynomial or fraction. The rules delegate to the
 * polynom and fraction methods since a variable can be easily also
 * viewed as a polynom or fraction.
 *
 * Examples:
 * ?- X is A-A.
 * X = 0
 * ?- X is A*A.
 * X is A^2
 *
 * The reduction rules are just predicates inside the variable module
 * with a Python first argument for the method receiver. We provide
 * reduction rules for basic arithmetic. Special functions are currently
 * not supported. Error handling is rudimentary.
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

:- module(variable, []).
:- reexport(../gauss/ring).

:- use_module(library(experiment/trail)).
:- use_module(generic).
:- use_module(polynom).
:- use_module(fraction).

/*********************************************************************/
/* Arithmetic                                                        */
/*********************************************************************/

/**
 * -(P, Q):
 * The predicate succeeds in Q with the P negated.
 */
% -(+Variable, -Polynom)
:- override (-)/2.
:- public (-)/2.
-(A, polynom(A,[1- -1])).

/**
 * +(P, Q, R):
 * The predicate succeeds in R with the sum of P and Q.
 */
% +(+Variable, +Internal, -Internal)
:- override (+)/3.
:- public (+)/3.
+(X, Y, R) :-
   integer(Y), !,
   sys_make_coeff([], 0, Y, L),
   polynom: +(polynom(X,[1-1]), polynom(X,L), R).
+(X, rational(A,B), R) :- !,
   polynom: +(polynom(X,[1-1]), polynom(X,[0-rational(A,B)]), R).
+(X, radical(A,B), R) :- !,
   polynom: +(polynom(X,[1-1]), polynom(X,[0-radical(A,B)]), R).
+(X, Y, R) :-
   sys_freezer(Y), !,
   polynom: +(polynom(X,[1-1]), polynom(Y,[1-1]), R).
+(X, polynom(A,B), R) :- !,
   polynom: +(polynom(X,[1-1]), polynom(A,B), R).
+(X, fraction(A,B), R) :- !,
   fraction: +(fraction(X,1), fraction(A,B), R).

/**
 * -(P, Q, R):
 * The predicate succeeds in R with P subtracted by Q.
 */
% -(+Variable, +Internal, -Internal)
:- override (-)/3.
:- public (-)/3.
-(X, Y, R) :-
   integer(Y), !,
   sys_make_coeff([], 0, Y, L),
   polynom: -(polynom(X,[1-1]), polynom(X,L), R).
-(X, rational(A,B), R) :- !,
   polynom: -(polynom(X,[1-1]), polynom(X,[0-rational(A,B)]), R).
-(X, radical(A,B), R) :- !,
   polynom: -(polynom(X,[1-1]), polynom(X,[0-radical(A,B)]), R).
-(X, Y, R) :-
   sys_freezer(Y), !,
   polynom: -(polynom(X,[1-1]), polynom(Y,[1-1]), R).
-(X, polynom(A,B), R) :- !,
   polynom: -(polynom(X,[1-1]), polynom(A,B), R).
-(X, fraction(A,B), R) :- !,
   fraction: -(fraction(X,1), fraction(A,B), R).

/**
 * *(P, Q, R):
 * The predicate succeeds in R with the product of P and Q.
 */
% *(+Variable, +Internal, -Internal)
:- override * /3.
:- public * /3.
*(X, Y, R) :-
   integer(Y), !,
   sys_make_coeff([], 0, Y, L),
   polynom: *(polynom(X,[1-1]), polynom(X,L), R).
*(X, rational(A,B), R) :- !,
   polynom: *(polynom(X,[1-1]), polynom(X,[0-rational(A,B)]), R).
*(X, radical(A,B), R) :- !,
   polynom: *(polynom(X,[1-1]), polynom(X,[0-radical(A,B)]), R).
*(X, Y, R) :-
   sys_freezer(Y), !,
   polynom: *(polynom(X,[1-1]), polynom(Y,[1-1]), R).
*(X, polynom(A,B), R) :- !,
   polynom: *(polynom(X,[1-1]), polynom(A,B), R).
*(X, fraction(A,B), R) :- !,
   fraction: *(fraction(X,1), fraction(A,B), R).

/**
 * /(P, Q, R):
 * The predicate succeeds in R with P divided by Q.
 */
% /(+Variable, +Internal, -Internal)
:- override / /3.
:- public / /3.
/(X, Y, R) :-
   integer(Y), !,
   R is X*(1/Y).
/(X, rational(A,B), R) :- !,
   R is X*(1/rational(A,B)).
/(X, radical(A,B), R) :- !,
   R is X*(1/radical(A,B)).
/(X, Y, R) :-
   sys_freezer(Y), !,
   make_fraction(X, Y, R).
/(X, polynom(A,B), R) :- !,
   make_fraction(X, polynom(A,B), R).
/(X, fraction(A,B), R) :- !,
   fraction: /(fraction(X,1), fraction(A,B), R).

/**
 * ^(P, Q, R):
 * The predicate succeeds in R with P raised by Q.
 */
% ^(+Variable, +Integer, -Internal)
:- override ^ /3.
:- public ^ /3.
^(X, Y, R) :-
   user:(Y < 0), !,
   user: -(Y, Z),
   R is (1/X)^Z.
^(X, Y, R) :-
   sys_make_poly([Y-1], X, R).

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
residue:sys_printable_value(E, F) :-
   sys_freezer(E), !,
   sys_melt_var(E, F).
