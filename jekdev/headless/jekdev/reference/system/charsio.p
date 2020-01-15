/**
 * This module provides temporary input/output redirection. The
 * predicate with_output_to/2 re-directs the output for the given
 * goal and retrieves the stream content for each success. The
 * predicate with_input_from/2 redirects the input for the given goal.
 *
 * Example:
 * ?- with_output_to(atom(A), (write(foo); write(bar))).
 * A = foo ;
 * A = bar
 *
 * ?- with_input_from(atom('foo.\n'), read(X)).
 * X = foo
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- package(library(jekdev/reference/system)).

:- module(charsio, []).
:- use_module(library(structure/bytes)).

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
:- meta_predicate with_output_to(?, 0).
with_output_to(atom(A), G) :- !,
   current_output(S),
   try_call_finally(
      redirect_text_output,
      (G, fetch_text_output(A)),
      set_output(S)).
with_output_to(bytes(L), G) :- !,
   current_output(S),
   try_call_finally(
      redirect_binary_output,
      (G, fetch_binary_output(B)),
      set_output(S)),
   atom_block(A, B),
   atom_codes(A, L).

/**
  * with_input_from(C, G):
  * The predicate succeeds whenever the goal G succeeds where
  * C defines the initial data. The predicate recognizes the same
  * data formats as the predicate memory_read/3.
  */
% with_input_from(+Container, +Goal)
:- public with_input_from/2.
:- meta_predicate with_input_from(?, 0).
with_input_from(atom(A), G) :- !,
   current_input(S),
   create_text_input(A, T),
   try_call_finally(
      set_input(T),
      G,
      set_input(S)).
with_input_from(bytes(L), G) :- !,
   current_input(S),
   atom_codes(A, L),
   atom_block(A, B),
   create_binary_input(B, T),
   try_call_finally(
      set_input(T),
      G,
      set_input(S)).

/**
 * try_call_finally(S, G, T):
 * The predicate succeeds whenever G succeeds. Calling T on the
 * exit, fail or exception port. and calling S on the call and
 * redo port.
 */
% try_call_finally(+Goal, +Goal, +Goal)
:- private try_call_finally/3.
:- meta_predicate try_call_finally(0, 0, 0).
try_call_finally(S, G, T) :-
   (S; T, fail),
   current_prolog_flag(sys_choices, X),
   sys_trap(G, E, (T, sys_raise(E))),
   current_prolog_flag(sys_choices, Y),
   (X == Y, !, T; T; S, fail).

/***************************************************************/
/* Helper Predicates                                           */
/***************************************************************/

% redirect_binary_output
:- private redirect_binary_output/0.
redirect_binary_output :-
   memory_write(S),
   open(S, write, K, [type(binary), buffer(0)]),
   set_output(K).

% redirect_text_output
:- private redirect_text_output/0.
redirect_text_output :-
   memory_write(S),
   open(S, write, K, [buffer(0)]),
   set_output(K).

% fetch_binary_output(-Bytes)
:- private fetch_binary_output/1.
fetch_binary_output(B) :-
   current_output(K),
   memory_get(K, B).

% fetch_text_output(-Atom)
:- private fetch_text_output/1.
fetch_text_output(A) :-
   current_output(K),
   flush_output(K),
   memory_get(K, [], A).

% create_binary_input(+Bytes, -Stream)
:- private create_binary_input/2.
create_binary_input(B, K) :-
   memory_read(B, S),
   open(S, read, K, [type(binary), buffer(0)]).

% create_text_input(+Atom, -Stream)
:- private create_text_input/2.
create_text_input(A, K) :-
   atom_block(A, B, []),
   memory_read(B, S),
   open(S, read, K, [buffer(0)]).
