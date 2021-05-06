/**
 * Some of the locks can produce condition variables via the predicate
 * cond_new/2. As a convenience a pair of a lock and condition variable
 * can be created by the predicate monitor_new/2. The resulting object
 * is suitable both for the lock predicates from the module "lock" and
 * for the condition variable predicates from this module.
 *
 * A condition variable allows a thread to temporarily leave a critical
 * region via the predicates cond_wait/1 and cond_wait_timeout/2. The
 * predicates cond_notify/1 and cond_notify_all/1 on the other hand let
 * a waiting thread respectively all waiting threads enter their
 * critical region again.
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

:- package(library(jekpro/frequent/misc)).
:- use_package(foreign(jekpro/frequent/misc)).
:- use_package(foreign(matula/util/misc)).
:- use_package(foreign(java/util/concurrent/locks)).

:- module(cond, []).
:- use_module(library(misc/lock)).

/**
 * monitor_new(M):
 * The predicate succeeds in M with a new monitor. The monitor
 * implements the Java Lock interface and the Java Condition
 * interface at the same time.
 */
% monitor_new(-Monitor)
:- public monitor_new/1.
monitor_new(M) :-
   mutex_new(L),
   cond_new(L, C),
   sys_monitor_new(L, C, M).

% sys_monitor_new(+Lock, +Condition, -Monitor)
:- private sys_monitor_new/3.
:- foreign_constructor(sys_monitor_new/3, 'Monitor', new('Lock', 'Condition')).

/****************************************************************/
/* Condition Variables                                          */
/****************************************************************/

/**
 * cond_new(L, C):
 * The predicate succeeds in C with a new condition for the lock L.
 */
% cond_new(+Lock, -Condition)
:- public cond_new/2.
:- virtual cond_new/2.
:- foreign(cond_new/2, 'Lock', newCondition).

/**
 * cond_wait(C):
 * The predicate succeeds when the condition C was notified.
 */
% cond_wait(+Condition)
:- public cond_wait/1.
:- virtual cond_wait/1.
:- foreign(cond_wait/1, 'Condition', await).

/**
 * cond_wait_timeout(C, T):
 * The predicate succeeds when the condition C was notified
 * in the time-out T. Otherwise the predicate fails.
 */
% cond_wait_timeout(+Condition, +Integer)
:- public cond_wait_timeout/2.
:- foreign(cond_wait_timeout/2, 'ForeignCond', sysAwait('Condition', int)).

/**
 * cond_notify(C):
 * The predicate succeeds in notifying one thread waiting on C.
 */
% cond_notify(+Condition)
:- public cond_notify/1.
:- virtual cond_notify/1.
:- foreign(cond_notify/1, 'Condition', signal).

/**
 * cond_notify_all(C):
 * The predicate succeeds in notifying all threads waiting on C.
 */
% cond_notify_all(+Condition)
:- public cond_notify_all/1.
:- virtual cond_notify_all/1.
:- foreign(cond_notify_all/1, 'Condition', signalAll).

/****************************************************************/
/* Current Time                                                 */
/****************************************************************/

/**
 * current_time:
 * Returns the current time in milliseconds.
 */
% current_time(-Integer)
:- public current_time/1.
:- foreign(current_time/1, 'System', currentTimeMillis).
