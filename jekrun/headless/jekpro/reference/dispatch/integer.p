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

:- module(integer, []).
:- use_module(generic).
:- use_module(library(misc/elem)).
/* gcd currently in minlog */

/*********************************************************************/
/* Arithmetic                                                        */
/*********************************************************************/

:- override (-)/2.
:- public (-)/2.
X - Y :-
   user:X - Y.

:- override (+)/3.
:- public (+)/3.
+(0, Y, Z) :-
   var(Y), !,
   Z = Y.
+(X, Y, Z) :-
   var(Y), !,
   Z = expression(X+Y).
+(X, Y, Z) :-
   integer(Y), !,
   user: +(X, Y, Z).
+(Y, rational(A,B), R) :- !,
   user: *(B, Y, H),
   user: +(H, A, C),
   integer: /(C, B, R).
+(0, expression(Y), Z) :- !,
   Z = expression(Y).
+(X, expression(Y), Z) :- !,
   Z = expression(X+expression(Y)).

:- override (-)/3.
:- public (-)/3.
-(0, Y, Z) :-
   var(Y), !,
   Z = expression(-Y).
-(X, Y, Z) :-
   var(Y), !,
   Z = expression(X-Y).
-(X, Y, Z) :-
   integer(Y), !,
   user: -(X, Y, Z).
-(Y, rational(A,B), R) :- !,
   user: *(B, Y, H),
   user: -(H, A, C),
   integer: /(C, B, R).
-(0, expression(Y), Z) :- !,
   Z = expression(-expression(Y)).
-(X, expression(Y), Z) :- !,
   Z = expression(X-expression(Y)).

:- override * /3.
:- public * /3.
*(0, Y, Z) :-
   var(Y), !,
   Z = 0.
*(1, Y, Z) :-
   var(Y), !,
   Z = Y.
*(-1, Y, Z) :-
   var(Y), !,
   Z = expression(-Y).
*(X, Y, Z) :-
   var(Y), !,
   Z = expression(X*Y).
*(X, Y, Z) :-
   integer(Y), !,
   user: *(X, Y, Z).
*(X, rational(A,B), R) :- !,
   user: *(X, A, H),
   integer: /(H, B, R).
*(0, expression(_), Z) :- !,
   Z = 0.
*(1, expression(Y), Z) :- !,
   Z = expression(Y).
*(-1, expression(Y), Z) :- !,
   Z = expression(-expression(Y)).
*(X, expression(Y), Z) :- !,
   Z = expression(X*expression(Y)).

:- override / /3.
:- public / /3.
/(X, Y, Z) :-
   var(Y), !,
   Z = expression(X/Y).
/(_, 0, _) :-
   throw(error(evaluation_error(zero_divisor),_)).
/(X, Y, R) :-
   integer(Y), !,
   user:(H is gcd(X,Y)),
   user:(A is X//H),
   user:(B is Y//H),
   make_rational(A, B, R).
/(X, rational(A,B), R) :- !,
   user: *(X, B, H),
   integer: /(H, A, R).
/(X, expression(Y), Z) :- !,
   Z = expression(X/expression(Y)).

:- override ^ /3.
:- public ^ /3.
^(X, Y, Z) :-
   var(Y), !,
   Z = expression(X^Y).
^(X, Y, Z) :-
   integer(Y),
   user:(Y >= 0), !,
   user: ^(X, Y, Z).
^(X, Y, R) :-
   integer(Y), !,
   user:Y - H,
   user: ^(X, H, Z),
   integer: /(1, Z, R).
^(X, rational(A,B), Z) :- !,
   Z = expression(X^rational(A,B)).
^(X, expression(Y), Z) :- !,
   Z = expression(X^expression(Y)).

/*********************************************************************/
/* Auto Diff                                                         */
/*********************************************************************/

:- public d/3.
d(_, _, 0).

:- public s/4.
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

/*********************************************************************/
/* Comparison                                                        */
/*********************************************************************/

:- override =:= /2.
:- public =:= /2.
X =:= Y :-
   integer(Y), !,
   user:(X =:= Y).
_ =:= rational(_,_) :- !, fail.

:- override < /2.
:- public < /2.
X < Y :-
   integer(Y), !,
   user:(X < Y).
X < rational(A,B) :- !,
   user: *(X, B, H),
   user:(H < A).

/*********************************************************************/
/* Constructor                                                       */
/*********************************************************************/

:- private make_rational/3.
make_rational(A, -1, B) :- !,
   user:A - B.
make_rational(A, 1, A) :- !.
make_rational(A, B, rational(C,D)) :-
   user:(B < 0), !,
   user:A - C,
   user:B - D.
make_rational(A, B, rational(A,B)).
