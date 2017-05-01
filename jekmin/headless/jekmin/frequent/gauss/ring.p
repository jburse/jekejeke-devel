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

:- module(ring, []).
:- reexport(element).

:- use_module('../groebner/generic').
:- use_module('../groebner/fraction').

/*********************************************************************/
/* Division & GCD                                                    */
/*********************************************************************/

/**
 * quorem(A, B, Q, R):
 * The predicate succeeds with quotient Q and remainder R of A divided by B.
 */
:- public quorem/4.
quorem(A, B, Q, R) :-
   X is A,
   Y is B,
   sys_poly_send(X, gen_div, [Y,Q,R]).

/**
 * gen_div(A, B, Q, R):
 * The predicate succeeds with quotient Q and remainder R of A divided by B.
 */
% gen_div(+Ring, +Internal, -Internal, -Internal)
:- public gen_div/4.
gen_div(A, B, Q, R) :-
   sys_poly_div(A, B, Q, R).


