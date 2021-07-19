/**
 * This module provides trailed named values.
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

:- package(library(jekmin/reference/term)).

:- module(state, []).

:- use_module(library(experiment/trail)).
:- use_module(library(minimal/assume)).
:- use_module(library(experiment/ref)).

:- private state/2.
:- thread_local state/2.

/**************************************************************/
/* Trailed Shared                                             */
/**************************************************************/

/**
 * b_setval(K, W):
 * The predicate assigns the value W to the key K.
 * The assignment is automatically undone upon backtracking.
 */
% b_setval(+Term, +Term)
:- public b_setval/2.
b_setval(K, W) :-
   b_delete(K),
   sys_freeze_var(H, F),
   H = wrap(W),
   assumable_ref(state(K, F), R),
   depositz_ref(R).

/**
 * nb_current(K, W):
 * The predicate succeeds for the value W of the key K.
 */
% nb_current(-Term, -Term)
:- public nb_current/2.
nb_current(K, W) :-
   state(K, F),
   sys_melt_var(F, wrap(W)).

/**
 * b_delete(K):
 * The predicate de-assigns the key K.
 * The de-assignment is automatically undone upon backtracking.
 */
% b_delete(+Term)
:- public b_delete/1.
b_delete(K) :- ground(K), !,
   b_delete2(K).
b_delete(_) :-
   throw(error(instantiation_error, _)).

% b_delete2(+Term)
:- private b_delete2/1.
b_delete2(K) :-
   clause_ref(state(K, _), true, R), !,
   withdrawz_ref(R).
b_delete2(_).