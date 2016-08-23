/**
 * This module provides temporary input/output redirection. The
 * predicate with_output_to/2 re-directs the output for the given
 * goal and retrieves the stream content for each success. The
 * predicate with_input_from/2 redirects the input for the given goal.
 *
 * Example:
 * ?- with_output_to(atom(A), (write(foo); write(bar))).
 * A = foo ;
 * A = bar ;
 * No
 *
 * ?- with_input_from(atom('foo.\n'), read(X)).
 * X = foo ;
 * No
 *
 * The predicate with_output_to/2 is non-deterministic for non-deterministic
 * goals and will return different data results for each success. The
 * predicate with_output_to/2 and with_input_from/2 recognize the same
 * data formats.
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

:- package(library(jekdev/reference/testing)).

:- module(charsio, []).
:- use_module(library(system/memory)).

/***************************************************************/
/* Temporarily Redirecting                                     */
/***************************************************************/

/**
 * with_output_to(C, G):
 * The predicate succeeds whenever the goal G succeeds and
 * unifies C with each data result. The predicate recognizes
 * the same data formats as the predicate memory_read/3.
 */
% with_output_to(+Container, +Goal)
:- public with_output_to/2.
:- meta_predicate with_output_to(?,0).
with_output_to(atom(A), G) :- !,
   current_output(S),
   try_call_finally(
      redirect_output([]),
      (  G,
         fetch_output(atom(A))),
      set_output(S)).
with_output_to(bytes(L), G) :- !,
   current_output(S),
   try_call_finally(
      redirect_output([type(binary)]),
      (  G,
         fetch_output(bytes(L))),
      set_output(S)).

/**
  * with_input_from(C, G):
  * The predicate succeeds whenever the goal G succeeds where
  * C defines the initial data. The predicate recognizes the same
  * data formats as the predicate memory_read/3.
  */
% with_input_from(+Container, +Goal)
:- public with_input_from/2.
:- meta_predicate with_input_from(?,0).
with_input_from(atom(A), G) :- !,
   current_input(S),
   memory_read(atom(A), [], T),
   try_call_finally(
      set_input(T),
      G,
      set_input(S)).
with_input_from(bytes(L), G) :- !,
   current_input(S),
   memory_read(bytes(L), [type(binary)], T),
   try_call_finally(
      set_input(T),
      G,
      set_input(S)).

/***************************************************************/
/* Helper Predicates                                           */
/***************************************************************/

% redirect_output(+Options)
:- private redirect_output/1.
redirect_output(O) :-
   memory_write(O, T),
   set_output(T).

% fetch_output(+Container)
:- private fetch_output/1.
fetch_output(D) :-
   current_output(T),
   memory_get(T, D).
