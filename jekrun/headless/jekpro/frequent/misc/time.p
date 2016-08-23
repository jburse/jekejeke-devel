/**
 * Alarm queues allow scheduling items. Items are Prolog terms and are
 * copied. An alarm queue can be created by the predicate alarm_new/1.
 * An alarm queue need not be explicitly destroyed, it will automatically
 * be reclaimed by the Java GC when not anymore used. Threads waiting
 * for an alarm queue can be interrupted.
 *
 * Example:
 * ?- time_out((repeat, write('Hello World!'), nl,
 *        thread_sleep(1000), fail), 3000).
 * Hello World!
 * Hello World!
 * Hello World!
 *
 * An item can be scheduled with the predicate alarm_schedue/4 giving
 * a delay in milliseconds. The predicate alarm_next/2 allows getting
 * an item from a queue. The predicate will block for the earliest
 * item. The predicate alarm_cancel/2 will remove an item from
 * the queue.
 *
 * The predicate time_out/2 uses a predefined alarm queue which is
 * served by a predefined thread. The predicate executes the given
 * goal once in the timeout. When the timeout is reached before the
 * goal completes an exception is thrown.
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

:- package(library(jekpro/frequent/misc)).
:- use_package(foreign(jekpro/frequent/misc)).
:- use_package(foreign(matula/util/misc)).
:- use_package(foreign(jekpro/tools/call)).
:- use_package(foreign(jekpro/tools/term)).

:- module(time, []).
:- use_module(library(system/thread)).

/**
 * alarm_new(A):
 * The predicate succeeds for a new alarm queue A.
 */
% alarm_new(-Queue)
:- public alarm_new/1.
:- foreign_constructor(alarm_new/1, 'Alarm', new).

/**
 * alarm_schedule(A, O, T, E):
 * The predicate succeeds for a new alarm entry E that schedules a copy
 * of the term O on the alarm queue A with a delay of T.
 */
% alarm_schedule(+Queue, +Term, +Integer, -Entry)
:- public alarm_schedule/4.
:- foreign(alarm_schedule/4, 'ForeignTime',
      sysAlarmSchedule('Interpreter','Alarm','Term',long)).

/**
 * alarm_next(A, O):
 * The predicate succeeds for the next term O on the alarm queue A. The
 * predicate blocks for the earliest item.
 */
% alarm_next(+Queue, -Term)
:- public alarm_next/2.
:- virtual alarm_next/2.
:- foreign(alarm_next/2, 'Alarm', next).

/**
 * alarm_cancel(A, E):
 * The predicate succeeds for cancelling the alarm entry E from
 * the alarm queue A.
 */
% alarm_cancel(+Queue, +Entry)
:- public alarm_cancel/2.
:- virtual alarm_cancel/2.
:- foreign(alarm_cancel/2, 'Alarm', cancel('AlarmEntry')).

/**
 * time_out(G, T):
 * The predicate succeeds when G succeeds in the timeout T. The predicate
 * fails when G fails in the timeout T. Otherwise the predicate throws the
 * message system_error(timelimit_exceeded).
 */
% time_out(+Term, +Integer)
:- public time_out/2.
:- meta_predicate time_out(0,?).
time_out(G, T) :-
   time_out_queue(A),
   thread_current(I),
   setup_call_cleanup(
      alarm_schedule(A, I, T, E),
      G,
      alarm_cancel(A, E)), !.

:- private time_out_queue/1.
:- dynamic time_out_queue/1.

:- alarm_new(A),
   assertz(time_out_queue(A)).

:- private time_out_loop/0.
time_out_loop :-
   time_out_queue(A), repeat,
   alarm_next(A, I),
   thread_abort(I, system_error(timelimit_exceeded)), fail.

:- thread_new(time_out_loop, I),
   thread_start(I).
