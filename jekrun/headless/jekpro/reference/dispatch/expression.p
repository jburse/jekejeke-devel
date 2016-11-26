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

:- module(expression, []).
:- use_module(generic).

/*********************************************************************/
/* Arithmetic                                                        */
/*********************************************************************/

:- override (-)/2.
:- public (-)/2.
expression(X) - Y :-
   Y = expression(-expression(X)).

:- override (+)/3.
:- public (+)/3.
+(expression(X), Y, Z) :-
   var(Y), !,
   Z = expression(expression(X)+Y).
+(expression(X), 0, Z) :- !,
   Z = expression(X).
+(expression(X), Y, Z) :-
   integer(Y), !,
   Z = expression(expression(X)+Y).
+(expression(X), rational(A,B), Z) :- !,
   Z = expression(expression(X)+rational(A,B)).
+(expression(X), expression(Y), Z) :- !,
   Z = expression(expression(X)+expression(Y)).

:- override (-)/3.
:- public (-)/3.
-(expression(X), Y, Z) :-
   var(Y), !,
   Z = expression(expression(X)-Y).
-(expression(X), 0, Z) :- !,
   Z = expression(X).
-(expression(X), Y, Z) :-
   integer(Y), !,
   Z = expression(expression(X)-Y).
-(expression(X), rational(A,B), Z) :- !,
   Z = expression(expression(X)-rational(A,B)).
-(expression(X), expression(Y), Z) :- !,
   Z = expression(expression(X)-expression(Y)).

:- override * /3.
:- public * /3.
*(expression(X), Y, Z) :-
   var(Y), !,
   Z = expression(expression(X)*Y).
*(expression(_), 0, Z) :- !,
   Z = 0.
*(expression(X), 1, Z) :- !,
   Z = expression(X).
*(expression(X), -1, Z) :- !,
   Z = expression(-expression(X)).
*(expression(X), Y, Z) :-
   integer(Y), !,
   Z = expression(expression(X)*Y).
*(expression(X), rational(A,B), Z) :- !,
   Z = expression(expression(X)*rational(A,B)).
*(expression(X), expression(Y), Z) :- !,
   Z = expression(expression(X)*expression(Y)).

:- override / /3.
:- public / /3.
/(expression(X), Y, Z) :-
   var(Y), !,
   Z = expression(expression(X)/Y).
/(expression(X), Y, Z) :-
   integer(Y), !,
   Z = expression(expression(X)/Y).
/(expression(X), rational(A,B), Z) :- !,
   Z = expression(expression(X)/rational(A,B)).
/(expression(X), expression(Y), Z) :- !,
   Z = expression(expression(X)/expression(Y)).

:- override ^ /3.
:- public ^ /3.
^(expression(X), Y, Z) :-
   var(Y), !,
   Z = expression(expression(X)^Y).
^(_, 0, Z) :- !,
   Z = 1.
^(expression(X), 1, Z) :- !,
   Z = expression(X).
^(expression(X), Y, Z) :-
   integer(Y), !,
   Z = expression(expression(X)^Y).
^(expression(X), rational(A,B), Z) :- !,
   Z = expression(expression(X)^rational(A,B)).
^(expression(X), expression(Y), Z) :- !,
   Z = expression(expression(X)^expression(Y)).

/*********************************************************************/
/* Auto Diff                                                         */
/*********************************************************************/

:- public d/3.
d(expression(-X), Y, R) :- !,
   R is -d(X,Y).
d(expression(X+Y), Z, R) :- !,
   R is d(X,Z)+d(Y,Z).
d(expression(X-Y), Z, R) :- !,
   R is d(X,Z)-d(Y,Z).
d(expression(X*Y), Z, R) :- !,
   R is d(X,Z)*Y+X*d(Y,Z).
d(expression(X/Y), Z, R) :- !,
   R is (d(X,Z)*Y-X*d(Y,Z))/Y^2.
d(expression(X^Y), Z, R) :-
   integer(Y), !,
   user: -(Y, 1, H),
   R is Y*X^H*d(X,Z).

:- public s/4.
s(expression(-X), Y, Z, R) :- !,
   R is -s(X,Y,Z).
s(expression(X+Y), Z, T, R) :- !,
   R is s(X,Z,T)+s(Y,Z,T).
s(expression(X-Y), Z, T, R) :- !,
   R is s(X,Z,T)-s(Y,Z,T).
s(expression(X*Y), Z, T, R) :- !,
   R is s(X,Z,T)*s(Y,Z,T).
s(expression(X/Y), Z, T, R) :- !,
   R is s(X,Z,T)/s(Y,Z,T).
s(expression(X^Y), Z, T, R) :- !,
   R is s(X,Z,T)^s(Y,Z,T).

:- public e/2.
e(expression(-X), R) :- !,
   R is -e(X).
e(expression(X+Y), R) :- !,
   R is e(X)+e(Y).
e(expression(X-Y), R) :- !,
   R is e(X)-e(Y).
e(expression(X*Y), R) :- !,
   R is m(e(X),e(Y)).
e(expression(X/Y), R) :- !,
   R is e(X)/e(Y).
e(expression(X^Y), R) :- !,
   R is p(e(X),e(Y)).

:- public m/3.
m(expression(-X), Y, R) :- !,
   R is -m(X,Y).
m(expression(X+Y), Z, R) :- !,
   R is m(X,Z)+m(Y,Z).
m(expression(X-Y), Z, R) :- !,
   R is m(X,Z)-m(Y,Z).
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
   var(Y), !,
   R is X^Y.
p(X, Y, R) :-
   integer(Y),
   user:(Y > 1),
   user:(Y rem 2 =:= 1), !,
   user: -(Y, 1, Z),
   R is m(X,p(X,Z)).
p(X, Y, R) :-
   integer(Y),
   user:(Y > 1), !,
   user:div(Y, 2, Z),
   H is p(X,Z),
   R is m(H,H).
p(X, Y, R) :-
   R is X^Y.

