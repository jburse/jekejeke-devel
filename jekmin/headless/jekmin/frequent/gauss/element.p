/**
 * This module provides list constructors. The module realizes a
 * base class for the classes rep-resented by the modules integer,
 * rational, variable, polynom and fraction from the package
 * groebner. The super class relationship is implemented by a
 * reexport/1 statement of the module element in each of the modules.
 * So that functions that apply to elements in general can be
 * implemented by methods for this module here.
 *
 * Among such general functions we find the consing of an element
 * with a list and returning a compound. This function is not directly
 * available as a constructor but there are two special forms that
 * make use of this general function by polymorphic dispatch. We
 * provide a list constructor [_ | _] which takes multiple expressions
 * as arguments and another list constructor {_ | _} which takes an
 * expression and a goal as arguments.
 *
 * Examples:
 * ?- X is [1+2,3*4,5^6].
 * X is [3,12,15625]
 * ?- X is { X^2 | between(1,10,X) }.
 * X is [1,4,9,16,25,36,49,64,81,100]
 *
 * The element based consing returns a vector. The consing does currently
 * not check that the second argument is a proper list and that all
 * elements of the list are elements. The intention here is to use
 * the consing to only create homogenous vectors of elements. The reader
 * interested in the methods of the vector should browse into the
 * module vector.
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
:- use_package(library(jekmin/frequent/groebner)).

:- module(element, []).

:- use_module(../groebner/generic).

:- use_module(library(experiment/attr)).
:- use_module(library(experiment/trail)).

/***********************************************************/
/* Array Builder & Access                                  */
/***********************************************************/

/**
 * .(X, Y, Z):
 * The predicate succeeds in Z with the consing of X and Y.
 */
% .(+Element, +List, -Vector)
:- override '.'/3.
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
/**
 * X is [F | G]:
 * The special form succeeds in X in evaluating F and the elements of G
 * by using polymorphism and then creating a compound.
 */
generic:(X is []) :- !,
   X = vector.
generic:(X is [F|G]) :- !,
   A is F,
   sys_eval_list(G, [], B),
   sys_poly_send(A, '.', [B,X]).
/**
 * X is {F | G}:
 * The special form succeeds in X in evaluating F by using polymorphism
 * whenever G succeeds, making copies and then creating a compound.
 */
generic:(X is {H}) :- !,
   findall(Y, Y is H, L),
   (  L = [A|B]
   -> sys_poly_send(A, '.', [B,X])
   ;  X = vector).
generic:(X is (F|G)) :- !, G,
   X is F.

:- multifile generic:is_abnormal/1.
:- public generic:is_abnormal/1.
generic:is_abnormal([]).
generic:is_abnormal([_|_]).
generic:is_abnormal({_}).
generic:is_abnormal((_|_)).
