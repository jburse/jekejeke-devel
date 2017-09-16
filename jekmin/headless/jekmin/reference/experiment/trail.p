/**
 * The system predicate sys_unbind/1 installs an unbind handler and
 * immediately succeeds. The unbind handler is invoked during a redo or
 * a close. In contrast to a cutter the unbind handler is not invoked
 * when choice points are removed. When the unbind handlers are executed
 * exceptions are aggregate_alld that are possibly thrown by the unbind
 * handlers. When unbind handler fails a directive failed error is thrown.
 *
 * The predicate sys_freeze_var/2 will create a new reference object that
 * captures the given variable. The reference object will have a stable
 * hash, equal and lexical ordering for the duration of the continuation.
 * The predicate sys_melt_var/2 allows retrieving the term the variable
 * is currently instantiated to. The predicate sys_bound_var/1 allows
 * checking whether the variable is currently instantiated or not.
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

:- package(library(jekmin/reference/experiment)).
:- use_package(foreign(jekmin/reference/experiment)).

:- module(trail, []).

/**
 * sys_unbind(A):
 * The predicate installs an unbind handler A and immediately succeeds.
 * The unbind handler is invoked during a redo or a close.
 */
% sys_unbind(+Goal)
:- public sys_unbind/1.
:- meta_predicate sys_unbind(0).
:- set_predicate_property(sys_unbind/1, sys_notrace).
:- special(sys_unbind/1, 'SpecialUndo', 0).

/**
 * sys_freeze_var(V, R):
 * The predicate succeeds when R unifies with a new references
 * object that captures the variable V.
 */
% sys_freeze_var(+Var, -Ref)
:- public sys_freeze_var/2.
:- special(sys_freeze_var/2, 'SpecialUndo', 1).

/**
 * sys_melt_var(R, T):
 * The predicate succeeds when T unifies with the variable captured
 * by the reference object R.
 */
% sys_melt_var(+Ref, -Var)
:- public sys_melt_var/2.
:- special(sys_melt_var/2, 'SpecialUndo', 2).

/**
 * sys_bound_var(R):
 * The predicate succeeds when the variable captured by the reference
 * object R is currently instantiated.
 */
% sys_bound_var(+Ref)
:- public sys_bound_var/1.
:- special(sys_bound_var/1, 'SpecialUndo', 3).

/**
 * sys_freezer(R):
 * The predicate succeeds when R is a reference to a variable.
 */
% sys_freezer(+Term)
:- public sys_freezer/1.
:- special(sys_freezer/1, 'SpecialUndo', 4).
