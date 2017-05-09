/**
 * This module provides ordered elements. The module realizes a base class
 * for the classes rep-resented by the module integer and the module
 * rational from the package groebner. The predicates (=:=)/2 and (=\=)/2
 * check equality of integers and rational numbers. The predicates (<)/2,
 * (=<)/2, (>)/2 and (>=)/2 allow comparison of integers and rational numbers.
 * The predicates override the usual built-in predicates.
 *
 * Examples:
 * ?- -9/5 > -2.
 * Yes
 * ?- X is [0,1,2], 3 < len(X).
 * No
 *
 * The predicates perform a polymorphic dispatch to the method gen_eq/2
 * respective gen_ls/2 on the class of the first argument. If a method is
 * not found comparison aborts. If a method is found, the class of the
 * second argument is checked. Derived from (<)/2 we also provide
 * constructors min/2, max/2, abs/2 and sign/2 to determine
 * corresponding values.
 *
 * Further there are the constructors integer/1, floor/1 and ceiling/1
 * that will find and return an integer near the given integer or rational
 * number. The constructor integer/1 rounds toward zero, the constructor
 * floor/1 rounds towards negative infinity and the constructor ceiling/1
 * rounds towards positive infinity.
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

:- package(library(jekmin/frequent/gauss)).
:- use_package(library(jekmin/frequent/leibniz)).
:- use_package(library(jekmin/frequent/groebner)).

:- module(ordered, []).
:- reexport(ring).

:- use_module(../groebner/generic).

/*********************************************************************/
/* Comparison                                                        */
/*********************************************************************/

/**
 * E =:= F:
 * The predicate succeeds when evaluating E and F by using
 * polymorphism gives the same result.
 */
:- override =:= /2.
:- public =:= /2.
E =:= F :-
   X is E,
   Y is F,
   sys_poly_send(X, gen_eq, [Y]).

/**
 * E =\= F:
 * The predicate succeeds when evaluating E and F by using
 * polymorphism dont give the same result.
 */
:- override =\= /2.
:- public =\= /2.
E =\= F :-
   X is E,
   Y is F,
   \+ sys_poly_send(X, gen_eq, [Y]).

/**
 * E < F:
 * The predicate succeeds when evaluating E by using polymorphism
 * is less than evaluating F by using polymorphism.
 */
:- override < /2.
:- public < /2.
E < F :-
   X is E,
   Y is F,
   sys_poly_send(X, gen_ls, [Y]).

/**
 * E =< F:
 * The predicate succeeds when evaluating E by using polymorphism
 * is less or equal than evaluating F by using polymorphism.
 */
:- override =< /2.
:- public =< /2.
E =< F :-
   X is E,
   Y is F,
   \+ sys_poly_send(Y, gen_ls, [X]).

/**
 * E > F:
 * The predicate succeeds when evaluating E by using polymorphism
 * is greater than evaluating F by using polymorphism.
 */
:- override > /2.
:- public > /2.
E > F :-
   X is E,
   Y is F,
   sys_poly_send(Y, gen_ls, [X]).

/**
 * E >= F:
 * The predicate succeeds when evaluating E by using polymorphism
 * is greater or equal than evaluating F by using polymorphism.
 */
:- override >= /2.
:- public >= /2.
E >= F :-
   X is E,
   Y is F,
   \+ sys_poly_send(X, gen_ls, [Y]).

/*********************************************************************/
/* Maximum/Minimum                                                   */
/*********************************************************************/

/**
 * min(X, Y, Z):
 * The predicate succeeds in Z with the minimum of X and Y.
 */
% element:min(+Element, +Internal,-Internal)
:- override min/3.
:- public min/3.
min(X, Y, Z) :-
   X < Y, !,
   Z = X.
min(_, X, X).

/**
 * max(X, Y, Z):
 * The predicate succeeds in Z with the maximum of X and Y.
 */
% element:max(+Element, +Internal,-Internal)
:- override max/3.
:- public max/3.
max(X, Y, Z) :-
   X < Y, !,
   Z = Y.
max(X, _, X).

/**
 * abs(X, Y):
 * The predicate succeeds in Z with the absolute of X.
 */
% abs(+Ordered, -Ordered)
:- override abs/2.
:- public abs/2.
abs(X, Y) :-
   X < 0, !,
   Y is -X.
abs(X, X).

/**
 * sign(X, Y):
 * The predicate succeeds in Z with the sign of X.
 */
% sign(+Ordered, -Ordered)
:- override sign/2.
:- public sign/2.
sign(X, Y) :-
   X < 0, !,
   Y = -1.
sign(X, Y) :-
   X > 0, !,
   Y = 1.
sign(_, 0).

/*********************************************************************/
/* Equalty                                                           */
/*********************************************************************/

/**
 * gen_eq(X, Y):
 * The predicate succeeds when X equals Y.
 */
:- public integer:gen_eq/2.
integer:gen_eq(X, Y) :-
   integer(Y), !,
   user:(X =:= Y).
integer:gen_eq(_, rational(_,_)) :- !, fail.
integer:gen_eq(_, radical(_,_)) :- !, fail.
integer:gen_eq(_, _) :-
   throw(error(evaluation_error(ordered),_)).

/**
 * gen_eq(X, Y):
 * The predicate succeeds when X equals Y.
 */
:- public rational:gen_eq/2.
rational:gen_eq(_, X) :-
   integer(X), !, fail.
rational:gen_eq(rational(A,B), rational(C,D)) :- !,
   user:(A =:= C),
   user:(B =:= D).
rational:gen_eq(_, radical(_,_)) :- !, fail.
rational:gen_eq(_, _) :-
   throw(error(evaluation_error(ordered),_)).

/**
 * gen_eq(X, Y):
 * The predicate succeeds when X equals Y.
 */
:- public radical:gen_eq/2.
radical:gen_eq(_, X) :-
   integer(X), !, fail.
radical:gen_eq(_, rational(_,_)) :- !, fail.
radical:gen_eq(radical(A,B), radical(C,D)) :- !,
   A =:= C,
   sys_radical_eq(B, D).
radical:gen_eq(_, _) :-
   throw(error(evaluation_error(ordered),_)).

% sys_radical_eq(+Map, +Map)
:- private sys_radical_eq/2.
sys_radical_eq([A-S|L], [B-T|R]) :-
   user:(S =:= T),
   A =:= B,
   sys_radical_eq(L, R).
sys_radical_eq([], []).

/*********************************************************************/
/* Less                                                              */
/*********************************************************************/

/**
 * gen_ls(X, Y):
 * The predicate succeeds when X is less than Y.
 */
:- public integer:gen_ls/2.
integer:gen_ls(X, Y) :-
   integer(Y), !,
   user:(X < Y).
integer:gen_ls(X, rational(A,B)) :- !,
   user: *(B, X, H),
   user:(H < A).
integer:gen_ls(_, _) :-
   throw(error(evaluation_error(ordered),_)).

/**
 * gen_ls(X, Y):
 * The predicate succeeds when X is less than Y.
 */
:- public rational:gen_ls/2.
rational:gen_ls(rational(A,B), X) :-
   integer(X), !,
   user: *(B, X, H),
   user:(A < H).
rational:gen_ls(rational(A,B), rational(C,D)) :- !,
   user: *(D, A, H),
   user: *(B, C, J),
   user:(H < J).
rational:gen_ls(_, _) :-
   throw(error(evaluation_error(ordered),_)).

/*********************************************************************/
/* Integer                                                           */
/*********************************************************************/

/**
 * integer(P, Q):
 * The predicate succeeds in Q with the integer of P.
 */
% integer(+Integer, -Integer)
:- override integer:integer/2.
:- public integer:integer/2.
integer:integer(X, X).

/**
 * integer(P, Q):
 * The predicate succeeds in Q with the integer of P.
 */
% integer(+Rational, -Integer)
:- override rational:integer/2.
:- public rational:integer/2.
rational:integer(rational(A,B), X) :-
   user: //(A, B, X).

/*********************************************************************/
/* Floor                                                             */
/*********************************************************************/

/**
 * floor(P, Q):
 * The predicate succeeds in Q with the floor of P.
 */
% floor(+Integer, -Integer)
:- override integer:floor/2.
:- public integer:floor/2.
integer:floor(X, X).

/**
 * floor(P, Q):
 * The predicate succeeds in Q with the floor of P.
 */
% floor(+Rational, -Integer)
:- override rational:floor/2.
:- public rational:floor/2.
rational:floor(rational(A,B), X) :-
   user:div(A, B, X).

/*********************************************************************/
/* Ceiling                                                           */
/*********************************************************************/

/**
 * ceiling(P, Q):
 * The predicate succeeds in Q with the ceiling of P.
 */
% ceiling(+Integer, -Integer)
:- override integer:ceiling/2.
:- public integer:ceiling/2.
integer:ceiling(X, X).

/**
 * ceiling(P, Q):
 * The predicate succeeds in Q with the ceiling of P.
 */
% ceiling(+Rational, -Integer)
:- override rational:ceiling/2.
:- public rational:ceiling/2.
rational:ceiling(rational(A,B), X) :-
   user: -(B, 1, H),
   user: +(A, H, J),
   user:div(J, B, X).
