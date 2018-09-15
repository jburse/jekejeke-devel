/**
 * The predicate sys_new_surrogate/1 helps realizing relational data
 * models. It will return a new unique reference object. The reference
 * cannot be used in lexical orderings. Nevertheless the reference
 * object can be asserted inside facts and rules.
 *
 * As an example application we deliver a programming interface to
 * local state variables. The protocol includes the predicates new_local/2,
 * get_local/2, set_local/2, free_local/2 and current_local/2. Since the
 * implementation uses a thread local fact it is synchronization free.
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

:- package(library(jekpro/frequent/experiment)).
:- use_package(foreign(jekpro/frequent/experiment)).

:- module(surrogate, []).

% local(Term, Term)
:- private local/2.
:- thread_local local/2.

/**
 * new_local(N, V):
 * Creates a new local variable N with value V.
 */
% new_local(+Term, +Term)
:- public new_local/2.
new_local(N, V) :-
   sys_new_surrogate(N),
   assertz(local(N, V)).

/**
 * get_local(N, V):
 * Succeeds when V unifies with the value of the local variable N.
 */
% get_local(+Term, -Term)
:- public get_local/2.
get_local(N, W) :-
   local(N, V), !,
   V = W.
get_local(N, _) :-
   throw(error(existence_error(local,N),_)).

/**
 * set_local(N, V):
 * Sets the value of the local variable N to V.
 */
% set_local(+Term, -Term)
:- public set_local/2.
set_local(N, V) :-
   retract(local(N, _)), !,
   assertz(local(N, V)).
set_local(N, _) :-
   throw(error(existence_error(local,N),_)).

/**
 * free_local(N):
 * Removes the local variable N.
 */
% free_local(+Term)
:- public free_local/1.
free_local(N) :-
   retract(local(N, _)), !.
free_local(N) :-
   throw(error(existence_error(local,N),_)).

/**
 * current_local(N):
 * Succeeds for every local variable N.
 */
:- public current_local/1.
current_local(N) :-
   local(N, _).

/**
 * sys_new_surrogate(K):
 * Creates a new surrogate key K.
 */
% sys_new_surrogate(-Ref)
:- public sys_new_surrogate/1.
:- foreign_constructor(sys_new_surrogate/1, 'Object', new).
