/**
 * Symbolic element.
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

:- module(element, []).

:- use_module('../groebner/generic').

:- use_module(library(experiment/attr)).
:- use_module(library(experiment/trail)).

/***********************************************************/
/* Array Builder & Access                                  */
/***********************************************************/

/**
 * .(X, Y, Z):
 * The predicate unifies in Z the consing of X and Y.
 */
% .(+Element, +List, -Vector)
:- public '.'/3.
'.'(X, L, Z) :-
   Z =.. [vector,X|L].

/*********************************************************************/
/* Generic Hook                                                      */
/*********************************************************************/

/**
 * Experimental pretty printing set builder expressions.
 */
:- public user:'|'/3.
:- meta_predicate user:'|'(#(1),0,?).
:- static user:'|'/3.

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
generic:(X is [F|G]) :- !,
   A is F,
   sys_eval_list(G, [], B),
   sys_poly_send(A, '.', [B,X]).
generic:(X is {H}) :- !,
   findall(Y, Y is H, [A|B]),
   sys_poly_send(A, '.', [B,X]).
generic:(X is (F|G)) :- !, G,
   X is F.

:- multifile generic:is_abnormal/1.
:- public generic:is_abnormal/1.
generic:is_abnormal([_|_]).
generic:is_abnormal({_}).
generic:is_abnormal((_|_)).
