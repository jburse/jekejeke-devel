/**
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

:- package(library(jekpro/reference/dispatch)).

:- module(variable, []).
:- use_module(generic).

/*********************************************************************/
/* Arithmetic                                                        */
/*********************************************************************/

:- override (-)/2.
:- public (-)/2.
X - Y :-
   Y = expression(-X).

:- override (+)/3.
:- public (+)/3.
+(X, Y, Z) :-
   var(Y), !,
   Z = expression(X+Y).
+(X, 0, Z) :- !,
   Z = X.
+(X, Y, Z) :-
   integer(Y), !,
   Z = expression(X+Y).
+(X, rational(A,B), Z) :- !,
   Z = expression(X+rational(A,B)).
+(X, expression(Y), Z) :- !,
   Z = expression(X+expression(Y)).

:- override (-)/3.
:- public (-)/3.
-(X, Y, Z) :-
   var(Y), !,
   Z = expression(X-Y).
-(X, 0, Z) :- !,
   Z = X.
-(X, Y, Z) :-
   integer(Y), !,
   Z = expression(X-Y).
-(X, rational(A,B), Z) :- !,
   Z = expression(X-rational(A,B)).
-(X, expression(Y), Z) :- !,
   Z = expression(X-expression(Y)).

:- override * /3.
:- public * /3.
*(X, Y, Z) :-
   var(Y), !,
   Z = expression(X*Y).
*(_, 0, Z) :- !,
   Z = 0.
*(X, 1, Z) :- !,
   Z = X.
*(X, -1, Z) :- !,
   Z = expression(-X).
*(X, Y, Z) :-
   integer(Y), !,
   Z = expression(X*Y).
*(X, rational(A,B), Z) :- !,
   Z = expression(X*rational(A,B)).
*(X, expression(Y), Z) :- !,
   Z = expression(X*expression(Y)).

:- override / /3.
:- public / /3.
/(X, Y, Z) :-
   var(Y), !,
   Z = expression(X/Y).
/(X, Y, Z) :-
   integer(Y), !,
   Z = expression(X/Y).
/(X, rational(A,B), Z) :- !,
   Z = expression(X/rational(A,B)).
/(X, expression(Y), Z) :- !,
   Z = expression(X/expression(Y)).

:- override ^ /3.
:- public ^ /3.
^(X, Y, Z) :-
   var(Y), !,
   Z = expression(X^Y).
^(_, 0, Z) :- !,
   Z = 1.
^(X, 1, Z) :- !,
   Z = X.
^(X, Y, Z) :-
   integer(Y), !,
   Z = expression(X^Y).
^(X, rational(A,B), Z) :- !,
   Z = expression(X^rational(A,B)).
^(X, expression(Y), Z) :- !,
   Z = expression(X^expression(Y)).

/*********************************************************************/
/* Auto Diff                                                         */
/*********************************************************************/

:- public d/3.
d(X, Y, R) :-
   X == Y, !,
   R = 1.
d(X, _, X).

:- public s/4.
s(X, Y, Z, R) :-
   X == Y, !,
   R = Z.
s(X, _, _, X).

:- public e/2.
e(X, X).

:- public m/3.
m(X, Y, R) :-
   var(Y), !,
   R is X*Y.
m(X, expression(-Y), R) :- !,
   R is -m(X,Y).
m(X, expression(Y+Z), R) :- !,
   R is m(X,Y)+m(X,Z).
m(X, expression(Y-Z), R) :- !,
   R is m(X,Y)-m(X,Z).
m(X, Y, R) :-
   R is X*Y.

:- public p/3.
p(X, Y, R) :-
   R is X^Y.
