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
:- use_package(foreign(java/util/concurrent/locks)).

:- module(monitor, []).
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
   lock_new(L),
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
 * wait(C):
 * The predicate succeeds when the condition C was notified.
 */
% wait(+Condition)
:- public wait/1.
:- virtual wait/1.
:- foreign(wait/1, 'Condition', await).

/**
 * wait_timeout(C, T):
 * The predicate succeeds when the condition C was notified
 * in the time-out. Otherwise the predicate fails.
 */
% wait_timeout(+Condition, +Integer)
:- public wait_timeout/2.
:- foreign(wait_timeout/2, 'ForeignMonitor', sysAwait('Condition', long)).

/**
 * notify(C):
 * The predicate succeeds in notifying one waiting thread.
 */
% notify(+Condition)
:- public notify/1.
:- virtual notify/1.
:- foreign(notify/1, 'Condition', signal).

/**
 * notify_all(C):
 * The predicate succeeds in notifying all waiting threads.
 */
% notify_all(+Condition)
:- public notify_all/1.
:- virtual notify_all/1.
:- foreign(notify_all/1, 'Condition', signalAll).

/****************************************************************/
/* Current Time                                                 */
/****************************************************************/

/**
 * current_time:
 * Returns the current time in milliseconds.
 */
% current_time(-Integer)
:- public current_time/1.
:- foreign_fun(current_time/1, 'System', currentTimeMillis).
