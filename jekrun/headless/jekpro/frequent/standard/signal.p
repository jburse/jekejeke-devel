/**
 * The predicates call_cleanup/2 and setup_call_cleanup/3 install a
 * choice point with a clean-up goal. During a cut the current bindings
 * are visible to the clean-up goal. During an exception the bindings
 * are undone before invoking the clean-up goal. The latter ternary
 * predicate differs from the ISO proposal in that it aggregate_alls
 * errors and in that it throws an error when the clean-up goal fails.
 * The former binary predicate is not part of the ISO proposal.
 *
 * Example:
 * ?- call_cleanup((X = 1; X = 2), (write(X), write(' '))).
 * X = 1 ;
 * 2 X = 2
 * ?- call_cleanup((X = 1; X = 2), (write(X), write(' '))), !.
 * 1 X = 1
 *
 * Situations might demand that a secondary thread controls a primary
 * thread. The program-ming interface allows raising a soft signal in
 * a primary Prolog thread from a secondary thread. The method for this
 * purpose is setSignal(). The effect on the primary Prolog thread will
 * be that the signal message is thrown as an error the first possible
 * moment a call port is reached.
 *
 * The primary Prolog thread might be in a blocking operation. Therefore
 * the method setSignal() also interrupts the primary Prolog thread. The
 * operations of the method setSignal() are disabled as long as the mask
 * flag is set to false. The mask flag can be read off from the
 * corresponding Prolog flag. It can be temporarily reset by the
 * system predicate sys_atomic/1.
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

:- use_package(foreign(jekpro/frequent/standard)).

:- module(user, []).

/**
 * sys_cleanup(A):
 * The predicate creates a choice point and succeeds once. The goal
 * A is called upon redo or when the choice point is removed.
 */
% sys_cleanup(+Goal)
:- public sys_cleanup/1.
:- meta_predicate sys_cleanup(0).
:- set_predicate_property(sys_cleanup/1, sys_notrace).
:- special(sys_cleanup/1, 'SpecialSignal', 1).

/**
 * call_cleanup(B, C):
 * The predicate succeeds whenever B succeeds. Additionally the
 * clean-up C is called when B fails or deterministically succeeds.
 * The clean-up C is also called when a cut or an exception happens
 * inside B or in the continuation.
 */
% call_cleanup(+Goal, +Goal)
:- public call_cleanup/2.
:- meta_predicate call_cleanup(0,0).
:- set_predicate_property(call_cleanup/2, sys_notrace).
call_cleanup(B, C) :-
   sys_atomic(sys_cleanup(C)),
   current_prolog_flag(sys_choices, X),
   call(B),
   current_prolog_flag(sys_choices, Y),
   (  X == Y, !; true).

/**
 * sys_atomic(A):
 * The predicate succeeds whenever A succeeds. The goal A is
 * invoked with the signal mask temporarily set to off.
 */
% sys_atomic(+Goal)
:- public sys_atomic/1.
:- meta_predicate sys_atomic(0).
:- set_predicate_property(sys_atomic/1, sys_notrace).
:- special(sys_atomic/1, 'SpecialSignal', 0).

/**
 * setup_call_cleanup(A, B, C):
 * The predicate succeeds when the setup A succeeds once and whenever
 * B succeeds. Additionally the clean-up C is called when B fails or
 * deterministically succeeds. The clean-up C is also called when a
 * cut or an exception happens inside B or in the continuation. The
 * setup A and the clean-up C are called with the signal mask temporarily
 * set to off.
 */
% setup_call_cleanup(+Goal, +Goal, +Goal)
:- public setup_call_cleanup/3.
:- meta_predicate setup_call_cleanup(0,0,0).
:- set_predicate_property(setup_call_cleanup/3, sys_notrace).
setup_call_cleanup(A, B, C) :-
   sys_atomic((  once(A),
                 sys_cleanup(C))),
   current_prolog_flag(sys_choices, X),
   call(B),
   current_prolog_flag(sys_choices, Y),
   (  X == Y, !; true).
