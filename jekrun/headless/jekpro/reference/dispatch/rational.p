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

:- module(rational, []).
:- use_module(generic).

/*********************************************************************/
/* Arithmetic                                                        */
/*********************************************************************/

:- override (-)/2.
:- public (-)/2.
rational(A,B) - rational(C,B) :-
   user:A - C.

:- override (+)/3.
:- public (+)/3.
+(X, Y, Z) :-
   var(Y), !,
   Z = expression(X+Y).
+(rational(A,B), Y, R) :-
   integer(Y), !,
   user: *(B, Y, H),
   user: +(A, H, C),
   integer: /(C, B, R).
+(rational(A,B), rational(C,D), R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user: +(H, J, K),
   user: *(B, D, L),
   integer: /(K, L, R).
+(X, expression(Y), Z) :- !,
   Z = expression(X+expression(Y)).

:- override (-)/3.
:- public (-)/3.
-(X, Y, Z) :-
   var(Y), !,
   Z = expression(X-Y).
-(rational(A,B), Y, R) :-
   integer(Y), !,
   user: *(B, Y, H),
   user: -(A, H, C),
   integer: /(C, B, R).
-(rational(A,B), rational(C,D), R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user: -(H, J, K),
   user: *(B, D, L),
   integer: /(K, L, R).
-(X, expression(Y), Z) :- !,
   Z = expression(X-expression(Y)).

:- override * /3.
:- public * /3.
*(X, Y, Z) :-
   var(Y), !,
   Z = expression(X*Y).
*(rational(A,B), Y, R) :-
   integer(Y), !,
   user: *(A, Y, H),
   integer: /(H, B, R).
*(rational(A,B), rational(C,D), R) :- !,
   user: *(A, C, H),
   user: *(B, D, J),
   integer: /(H, J, R).
*(X, expression(Y), Z) :- !,
   Z = expression(X*expression(Y)).

:- override / /3.
:- public / /3.
/(X, Y, Z) :-
   var(Y), !,
   Z = expression(X/Y).
/(rational(A,B), Y, R) :-
   integer(Y), !,
   user: *(B, Y, H),
   integer: /(H, A, R).
/(rational(A,B), rational(C,D), R) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   integer: /(H, J, R).
/(X, expression(Y), Z) :- !,
   Z = expression(X/expression(Y)).

:- override ^ /3.
:- public ^ /3.
^(X, Y, Z) :-
   var(Y), !,
   Z = expression(X^Y).
^(rational(A,B), Y, R) :-
   integer(Y),
   user:(Y >= 0), !,
   user: ^(A, Y, H),
   user: ^(B, Y, J),
   integer: /(H, J, R).
^(rational(A,B), Y, R) :-
   integer(Y), !,
   user:Y - K,
   user: ^(A, K, H),
   user: ^(B, K, J),
   integer: /(J, H, R).
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
rational(_,_) =:= Y :-
   integer(Y), !, fail.
rational(A,B) =:= rational(C,D) :- !,
   user:(A =:= C),
   user:(B =:= D).

:- override < /2.
:- public < /2.
rational(A,B) < Y :-
   integer(Y), !,
   user: *(B, Y, H),
   user:(A < H).
rational(A,B) < rational(C,D) :- !,
   user: *(A, D, H),
   user: *(B, C, J),
   user:(H < J).
