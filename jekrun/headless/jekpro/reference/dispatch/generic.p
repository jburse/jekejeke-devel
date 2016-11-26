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

:- module(generic, []).

/*********************************************************************/
/* Arithmetic                                                        */
/*********************************************************************/

/**
 * X is E:
 * The predicate succeeds in evaluating E by using polymorphism.
 */
:- override is/2.
:- public is/2.
X is E :-
   var(E), !,
   X = E.
X is E :-
   integer(E), !,
   X = E.
X is E :-
   atom(E), !,
   user:(X is E).
X is rational(A,B) :- !,
   X = rational(A,B).
X is expression(E) :- !,
   X = expression(E).
X is E :-
   compound(E), !,
   E =.. [F|L],
   sys_eval_list(L, X, [Y|T]),
   sys_poly_send(Y, F, T).

/**
 * sys_eval_list(L, X, R):
 * The predicate succeeds in R for evaluating the elements of L
 * and adding the place holder X.
 */
:- private sys_eval_list/3.
sys_eval_list([X|Y], H, [Z|T]) :-
   Z is X,
   sys_eval_list(Y, H, T).
sys_eval_list([], H, [H]).

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
   sys_poly_send(X, =:=, [Y]).

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
   \+ sys_poly_send(X, =:=, [Y]).

/**
 * E < F:
 * The preicate succeeds when evaluating E by using polymorphism
 * is less than evaluating F by using polymorphism.
 */
:- override < /2.
:- public < /2.
E < F :-
   X is E,
   Y is F,
   sys_poly_send(X, <, [Y]).

/**
 * E =< F:
 * The preicate succeeds when evaluating E by using polymorphism
 * is less or equal than evaluating F by using polymorphism.
 */
:- override =< /2.
:- public =< /2.
E =< F :-
   X is E,
   Y is F,
   \+ sys_poly_send(Y, <, [X]).

/**
 * E > F:
 * The preicate succeeds when evaluating E by using polymorphism
 * is greater than evaluating F by using polymorphism.
 */
:- override > /2.
:- public > /2.
E > F :-
   X is E,
   Y is F,
   sys_poly_send(Y, <, [X]).

/**
 * E >= F:
 * The preicate succeeds when evaluating E by using polymorphism
 * is greater or equal than evaluating F by using polymorphism.
 */
:- override >= /2.
:- public >= /2.
E >= F :-
   X is E,
   Y is F,
   \+ sys_poly_send(X, <, [Y]).

/**
 * sys_poly_send(Y, F, T):
 * The predicate succeeds for sending a message with funtor F
 * and arguments T to the object Y.
 */
:- private sys_poly_send/3.
sys_poly_send(Y, F, T) :-
   var(Y), !,
   M =.. [F,Y|T],
   variable:M.
sys_poly_send(Y, F, T) :-
   integer(Y), !,
   M =.. [F,Y|T],
   integer:M.
sys_poly_send(Y, F, T) :-
   M =.. [F|T],
   Y::M.

