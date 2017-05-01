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

:- package(library(jekmin/frequent/gauss)).

:- module(ordered, []).
:- reexport(ring).

:- use_module('../groebner/generic').

/*********************************************************************/
/* Equality & Comparison                                             */
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
/* Maximum/Minimum                                                  */
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

