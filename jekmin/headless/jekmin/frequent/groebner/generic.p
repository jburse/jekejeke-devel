/**
 * Generic expression dispatcher.
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

% ?- sys_add_path('file:/Projects/Jekejeke/Prototyping/experiment/other/').

:- package(library(jekmin/frequent/groebner)).

:- module(generic, []).
/* for the residue hooks */
:- sys_auto_load(variable).
:- sys_auto_load(integer).
:- sys_auto_load(rational).
:- sys_auto_load(polynom).
:- sys_auto_load(fraction).
:- sys_auto_load('../gauss/matrice').
:- sys_auto_load('../gauss/vector').
:- sys_auto_load('../leibniz/deriv').
:- sys_auto_load('../leibniz/subst').
:- sys_auto_load('../leibniz/series').

:- use_module(library(experiment/attr)).
:- use_module(library(experiment/trail)).

/*********************************************************************/
/* Arithmetic                                                        */
/*********************************************************************/

/**
 * X is E:
 * The predicate succeeds in evaluating E by using polymorphism.
 */
% is(-Internal, +Expr)
:- override is/2.
:- multifile is/2.
:- public is/2.
:- meta_predicate ?is#(1).
X is E :-
   var(E), !,
   sys_ensure_serno(E),
   sys_freeze_var(E, X).
X is E :-
   sys_freezer(E), !,
   X = E.
X is E :-
   integer(E), !,
   X = E.
X is E :-
   \+ is_abnormal(E), !,
   E =.. [F|L],
   sys_eval_list(L, [X], [Y|T]),
   sys_poly_send(Y, F, T).

/**
 * sys_eval_list(L, S, R):
 * The predicate succeeds in R for evaluating the elements of L
 * and adding the tail S.
 */
:- public sys_eval_list/3.
sys_eval_list([X|Y], S, [Z|T]) :-
   Z is X,
   sys_eval_list(Y, S, T).
sys_eval_list([], S, S).

/**
 * sys_poly_send(Y, F, T):
 * The predicate succeeds for sending a message with funtor F
 * and arguments T to the object Y.
 */
:- public sys_poly_send/3.
sys_poly_send(Y, F, T) :-
   sys_freezer(Y), !,
   M =.. [F,Y|T],
   variable:M.
sys_poly_send(Y, F, T) :-
   integer(Y), !,
   M =.. [F,Y|T],
   integer:M.
sys_poly_send(Y, F, T) :-
   M =.. [F|T],
   Y::M.

/**
 * is_abnormal(E):
 * The predicate succeeds when the expression E has a non-default
 * handling. This predicate is multi file and can be thus
 * extended.
 */
:- multifile is_abnormal/1.
:- public is_abnormal/1.
:- static is_abnormal/1.

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
 * The preicate succeeds when evaluating E by using polymorphism
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
 * The preicate succeeds when evaluating E by using polymorphism
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
 * The preicate succeeds when evaluating E by using polymorphism
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
 * The preicate succeeds when evaluating E by using polymorphism
 * is greater or equal than evaluating F by using polymorphism.
 */
:- override >= /2.
:- public >= /2.
E >= F :-
   X is E,
   Y is F,
   \+ sys_poly_send(X, gen_ls, [Y]).

