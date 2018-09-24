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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- package(library(jekmin/frequent/gauss)).
:- use_package(library(jekmin/frequent/leibniz)).
:- use_package(library(jekmin/frequent/groebner)).
:- use_package(library(jekmin/reference/misc)).

:- module(ordered, []).
:- reexport(ring).

:- use_module(../groebner/generic).
:- use_module(../leibniz/radical).
:- use_module(library(basic/lists)).

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
% sign(+Integer, -Integer)
:- override integer:sign/2.
:- public integer:sign/2.
integer:sign(X, Y) :-
   user:sign(X, Y).

% sign(+Rational, -Integer)
:- override rational:sign/2.
:- public rational:sign/2.
rational:sign(rational(A,_), Y) :-
   user:sign(A, Y).

% sign(+Radical, -Integer)
:- override radical:sign/2.
:- public radical:sign/2.
radical:sign(radical(0,[_-S]), Y) :- !,
   Y = S.
radical:sign(radical(A,B), Y) :-
   sys_radical_triage(radical(A,B), P, Q),
   U is sign(P),
   V is sign(Q),
   (  user:(U =:= V)
   -> Y = U
   ;  H is sign(P^2-Q^2),
      user: *(U, H, Y)).

/*********************************************************************/
/* Base Determination                                                */
/*********************************************************************/

% sys_radical_triage(+Radical, -Internal, -Internal)
:- public sys_radical_triage/3.
sys_radical_triage(radical(A,[B-S|L]), P, Q) :-
   sys_radical_level(B, M),
   sys_sqrt_triage([B-S|L], R, N, M, J),
   sys_sqrt_filter(R, N, J, U, V),
   sys_new_radical(A, U, P),
   sys_new_radical(0, V, Q).

% sys_sqrt_filter(+Classified, +Integer, +Map, -Map, -Map)
:- private sys_sqrt_filter/5.
sys_sqrt_filter([H-B-S|L], N, J, U, V) :-
   sys_sqrt_filter(L, N, J, P, Q),
   (  member(N, H)
   -> U = P,
      V = [B-S|Q]
   ;  U = [B-S|P],
      V = Q).
sys_sqrt_filter([], _, J, J, []).

% sys_sqrt_triage(+Map, -Classified, -Integer, +Integer, -Map)
:- private sys_sqrt_triage/5.
sys_sqrt_triage([B-S|L], R, N, K, J) :-
   sys_radical_level(B, T),
   user:(T =:= K), !,
   sys_sqrt_triage(L, U, M, K, J),
   (  sys_radical_prod(U, D, H),
      H = [_,_|_],
      sys_test_lindep(D, B, _)
   -> N = M,
      R = [H-B-S|U]
   ;  user: +(M, 1, N),
      R = [[N]-B-S|U]).
sys_sqrt_triage(L, [], 0, _, L).

% sys_radical_prod(+Classified, -Internal, -List)
:- private sys_radical_prod/3.
sys_radical_prod([[N]-B-_|L], D, H) :- !,
   sys_radical_prod(L, E, J),
   (  D = E,
      H = J
   ;  D is E*B,
      H = [N|J]).
sys_radical_prod([[_,_|_]-_-_|L], D, H) :-
   sys_radical_prod(L, D, H).
sys_radical_prod([], 1, []).

/*********************************************************************/
/* Dependency Test                                                   */
/*********************************************************************/

% sys_test_lindep(+Internal, +Internal, -Internal)
:- public sys_test_lindep/3.
sys_test_lindep(A, B, H) :-
   sys_radical_level(A, P),
   sys_radical_level(B, Q),
   user:(P =:= Q),
   D is A*B,
   make_radical(D, H),
   sys_radical_level(H, W),
   user:(W =< P).

% sys_new_radical(+Internal, +Map, -Internal)
:- public sys_new_radical/3.
sys_new_radical(A, [], R) :- !,
   R = A.
sys_new_radical(A, L, radical(A,L)).

/*********************************************************************/
/* Nesting Level                                                     */
/*********************************************************************/

/**
 * sys_radical_base(P, N, B):
 * The predicate succeeds in B with the base width of the ordered
 * elememt P, for the nesting level N.
 */
% sys_radical_base(+Ordered, +Integer, -Integer)
:- public sys_radical_base/3.
sys_radical_base(X, _, Y) :-
   integer(X), !,
   Y = 0.
sys_radical_base(rational(_,_), _, Y) :- !,
   Y = 0.
sys_radical_base(radical(_,B), N, Y) :-
   sys_sqrt_triage(B, _, Y, N, _).

% sys_radical_midlevel(+Radical, -Integer)
:- public sys_radical_midlevel/2.
sys_radical_midlevel(radical(_,[B-_|_]), N) :-
   sys_radical_level(B, N).

/**
 * sys_radical_level(P, N):
 * The predicate succeeds in N with the nesting level of the ordered
 * elememt P.
 */
% sys_radical_level(+Ordered, -Integer)
:- public sys_radical_level/2.
sys_radical_level(X, Y) :-
   integer(X), !,
   Y = 0.
sys_radical_level(rational(_,_), Y) :- !,
   Y = 0.
sys_radical_level(radical(_,[A-_|L]), Y) :-
   sys_radical_level(A, H),
   sys_sqrt_level(L, H, J),
   user: +(J, 1, Y).

% sys_sqrt_level(+Map, +Integer, -Integer)
:- private sys_sqrt_level/3.
sys_sqrt_level([A-_|L], H, J) :-
   sys_radical_level(A, I),
   user:max(H, I, K),
   sys_sqrt_level(L, K, J).
sys_sqrt_level([], H, H).

/*********************************************************************/
/* Equalty                                                           */
/*********************************************************************/

/**
 * gen_eq(X, Y):
 * The predicate succeeds when X equals Y.
 */
% gen_eq(+Integer, +Ordered)
:- public integer:gen_eq/2.
integer:gen_eq(X, Y) :-
   integer(Y), !,
   user:(X =:= Y).
integer:gen_eq(_, rational(_,_)) :- !, fail.
integer:gen_eq(_, radical(_,_)) :- !, fail.
integer:gen_eq(_, _) :-
   throw(error(evaluation_error(ordered),_)).

% gen_eq(+Rational, +Ordered)
:- public rational:gen_eq/2.
rational:gen_eq(_, X) :-
   integer(X), !, fail.
rational:gen_eq(rational(A,B), rational(C,D)) :- !,
   user:(A =:= C),
   user:(B =:= D).
rational:gen_eq(_, radical(_,_)) :- !, fail.
rational:gen_eq(_, _) :-
   throw(error(evaluation_error(ordered),_)).

% gen_eq(+Radical, +Ordered)
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
% gen_ls(+Integer, +Ordered)
:- public integer:gen_ls/2.
integer:gen_ls(X, Y) :-
   integer(Y), !,
   user:(X < Y).
integer:gen_ls(X, rational(A,B)) :- !,
   user: *(B, X, H),
   user:(H < A).
integer:gen_ls(X, radical(C,D)) :- !,
   1 =:= sign(radical(C,D)-X).
integer:gen_ls(_, _) :-
   throw(error(evaluation_error(ordered),_)).

% gen_ls(+Rational, +Ordered)
:- public rational:gen_ls/2.
rational:gen_ls(rational(A,B), Y) :-
   integer(Y), !,
   user: *(B, Y, H),
   user:(A < H).
rational:gen_ls(rational(A,B), rational(C,D)) :- !,
   user: *(D, A, H),
   user: *(B, C, J),
   user:(H < J).
rational:gen_ls(X, radical(C,D)) :- !,
   1 =:= sign(radical(C,D)-X).
rational:gen_ls(_, _) :-
   throw(error(evaluation_error(ordered),_)).

% gen_ls(+Radical, +Ordered)
:- public radical:gen_ls/2.
radical:gen_ls(X, Y) :-
   integer(Y), !,
   1 =:= sign(Y-X).
radical:gen_ls(X, rational(C,D)) :- !,
   1 =:= sign(rational(C,D)-X).
radical:gen_ls(X, radical(C,D)) :- !,
   1 =:= sign(radical(C,D)-X).
radical:gen_ls(_, _) :-
   throw(error(evaluation_error(ordered),_)).

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

% floor(+Rational, -Integer)
:- override rational:floor/2.
:- public rational:floor/2.
rational:floor(rational(A,B), X) :-
   user:div(A, B, X).

% floor(+Radical, -Integer)
:- override radical:floor/2.
:- public radical:floor/2.
radical:floor(radical(0,[A-S]), X) :- !,
   sys_radical_lower([A-S], X).
radical:floor(radical(A,B), X) :-
   sys_radical_lower(B, H),
   K is floor(A)+H,
   sys_radical_search(K, radical(A,B), X).

% sys_radical_lower(+Map, -Integer)
:- private sys_radical_lower/2.
sys_radical_lower([A- -1|L], K) :- !,
   sys_radical_lower(L, H),
   B is floor(A),
   elem:isqrt(B, I),
   user: +(I, 1, J),
   user: -(H, J, K).
sys_radical_lower([A-1|L], K) :-
   sys_radical_lower(L, H),
   B is floor(A),
   elem:isqrt(B, J),
   user: +(H, J, K).
sys_radical_lower([], 0).

% sys_radical_search(+Integer, +Radical, -Integer)
:- private sys_radical_search/3.
sys_radical_search(N, X, Y) :-
   user: +(N, 1, M),
   M =< X, !,
   sys_radical_search(M, X, Y).
sys_radical_search(N, _, N).

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

% ceiling(+Rational, -Integer)
:- override rational:ceiling/2.
:- public rational:ceiling/2.
rational:ceiling(rational(A,B), X) :-
   user: -(B, 1, H),
   user: +(A, H, J),
   user:div(J, B, X).

% ceiling(+Radical, -Integer)
:- override radical:ceiling/2.
:- public radical:ceiling/2.
radical:ceiling(X, Y) :-
   Y is -floor(-X).

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

% integer(+Rational, -Integer)
:- override rational:integer/2.
:- public rational:integer/2.
rational:integer(rational(A,B), X) :-
   user: //(A, B, X).

% integer(+Radical, -Integer)
:- override radical:integer/2.
:- public radical:integer/2.
radical:integer(X, Y) :-
   X >= 0, !,
   Y is floor(X).
radical:integer(X, Y) :-
   Y is -floor(-X).

/*********************************************************************/
/* Float                                                             */
/*********************************************************************/

/**
 * float(P, Q):
 * The predicate succeeds in Q with the float of P.
 */
% float(+Integer, -Float)
:- override integer:float/2.
:- public integer:float/2.
integer:float(X, Y) :-
   user:float(X, Y).

% float(+Rational, -Float)
:- override rational:float/2.
:- public rational:float/2.
rational:float(rational(A,B), X) :-
   user: /(A, B, X).

% float(+Radical, -Float)
:- override radical:float/2.
:- public radical:float/2.
radical:float(radical(A,B), Y) :-
   X is float(A),
   sys_sqrt_float(B, X, Y).

% sys_sqrt_float(+Map, +Float, -Float)
:- private sys_sqrt_float/3.
sys_sqrt_float([A-S|L], X, T) :-
   Y is float(A),
   user:sqrt(Y, H),
   user: *(S, H, J),
   user: +(X, J, Z),
   sys_sqrt_float(L, Z, T).
sys_sqrt_float([], X, X).
