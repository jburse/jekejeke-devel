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
+(X, Y, Z) :-
   number(Y), !,
   user: +(X, Y, Z).
+(Y, rational(A,B), R) :- !,
   user: *(B, Y, H),
   user: +(H, A, C),
   integer: /(C, B, R).

:- override (-)/3.
:- public (-)/3.
-(X, Y, Z) :-
   number(Y), !,
   user: -(X, Y, Z).
-(Y, rational(A,B), R) :- !,
   user: *(B, Y, H),
   user: -(H, A, C),
   integer: /(C, B, R).

:- override * /3.
:- public * /3.
*(X, Y, Z) :-
   number(Y), !,
   user: *(X, Y, Z).
*(X, rational(A,B), R) :- !,
   user: *(X, A, H),
   integer: /(H, B, R).

:- override / /3.
:- public / /3.
/(_, 0, _) :-
   throw(error(evaluation_error(zero_divisor),_)).
/(X, Y, R) :-
   integer(Y), !,
   H is gcd(X,Y),
   A is X//H,
   B is Y//H,
   make_rational(A, B, R).
/(X, rational(A,B), R) :- !,
   user: *(X, B, H),
   integer: /(H, A, R).

:- override ^ /3.
:- public ^ /3.
^(X, Y, Z) :-
   number(Y), !,
   user: ^(X, Y, Z).

/*********************************************************************/
/* Comparison                                                        */
/*********************************************************************/

:- override =:= /2.
:- public =:= /2.
X =:= Y :-
   number(Y), !,
   user:(X =:= Y).
_ =:= rational(_,_) :- !, fail.

:- override < /2.
:- public < /2.
X < Y :-
   number(Y), !,
   user:(X < Y).
X < rational(A,B) :- !,
   user: *(X, B, H),
   user:(H < A).

/*********************************************************************/
/* Comparison                                                        */
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

