/**
 * Symbolic vector.
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
:- use_package(library(jekmin/frequent/groebner)).
:- use_package(library(jekpro/frequent/misc)).

:- module(vector, []).

:- use_module('../groebner/generic').
:- use_module(library(advanced/arith)).

:- use_module(library(experiment/attr)).
:- use_module(library(experiment/trail)).

/***********************************************************/
/* Array Builder & Access                                  */
/***********************************************************/

/**
 * .(X, Y, Z):
 * The predicate unifies in Z the consing of X and Y.
 */
% .(+Vector, +List, -Matrice)
:- public '.'/3.
'.'(X, L, Z) :-
   Z =.. [matrice,X|L].

/**
 * X[Y, Z]:
 * The predicate unifies Z with the Y-the element
 * of the vector X.
 */
% [](+Vector, +Integer, -Element)
:- override []/3.
:- public []/3.
X [Y, Z] :-
   integer(Y),
   arg(Y, X, Z).

/**
 * len(X, Y):
 * The predicate unifies Y with the number of elements
 * in the vector X.
 */
% len(+Vector, -Integer)
:- public len/2.
len(X, Y) :-
   functor(X, _, Y).

/**
 * sum(X, Y):
 * The predicate unifies Y with the sum of elements
 * in the vector X.
 */
% sum(+Vector, -Internal)
:- public sum/2.
sum(X, Y) :-
   X =.. [_|L],
   sys_sum_vector(L, Y).

% sys_sum_vector(+List, -Internal)
:- private sys_sum_vector/2.
sys_sum_vector([X|L], R) :-
   sys_sum_vector(L, H),
   R is X+H.
sys_sum_vector([], 0).

/***********************************************************/
/* Basic Arithmetic                                        */
/***********************************************************/

/**
 * -(X, Y):
 * The predicate unifies Y with the sign changed vector X.
 */
% -(+Vector, -Vector)
:- override (-)/2.
:- public (-)/2.
X - Y :-
   L is len(X),
   Y is {-X[I]|between(1, L, I)}.

/**
 * +(X, Y, Z):
 * The predicate unifies Z with the sum of the vector X and
 * the vector Y.
 */
% +(+Vector, +Internal, -Vector)
:- override (+)/3.
:- public (+)/3.
+(X, Y, Z) :-
   functor(Y, vector, _),
   L is len(X),
   L =:= len(Y),
   Z is {X[I]+Y[I]|between(1, L, I)}.

/**
 * -(X, Y, Z):
 * The predicate unifizes Z with the the vector X subtracted
 * by the vector Y.
 */
% -(+Vector, +Internal, -Vector)
:- override (-)/3.
:- public (-)/3.
-(X, Y, Z) :-
   functor(Y, vector, _),
   L is len(X),
   L =:= len(Y),
   Z is {X[I]-Y[I]|between(1, L, I)}.

/***********************************************************/
/* CAS Display Hook                                        */
/***********************************************************/

/**
 * sys_portray_eq(F, G):
 * The predicate succeeds in G with a custom form of F.
 */
% sys_portray_eq(+Goal, -Goal)
:- public residue:sys_portray_eq/2.
:- multifile residue:sys_portray_eq/2.
:- meta_predicate residue:sys_portray_eq(0,0).
residue:sys_portray_eq(_ = X, _) :-
   var(X), !, fail.
residue:sys_portray_eq(X = F, X is G) :-
   functor(F, vector, _), !,
   F =.. [_|H],
   sys_portray_vector(H, G).

% sys_portray_vector(+List, -List)
:- private sys_portray_vector/2.
sys_portray_vector([X|L], [Y|R]) :-
   residue:sys_portray_eq(_ = X, _ is Y), !,
   sys_portray_vector(L, R).
sys_portray_vector([X|L], [X|R]) :-
   sys_portray_vector(L, R).
sys_portray_vector([], []).

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
generic:(X is E) :-
   functor(E, vector, _), !,
   X = E.

:- multifile generic:is_abnormal/1.
:- public generic:is_abnormal/1.
generic:is_abnormal(E) :-
   functor(E, vector, _).
