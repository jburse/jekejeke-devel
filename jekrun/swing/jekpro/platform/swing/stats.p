/**
 * This module provides some execution statistics. The predicate
 * statistics/2 returns some key figures of the memory management
 * and the runtime system, whereas the predicate statistics/0
 * displays the key figures on the standard output. The measurement
 * of the time performance of a goal is facilitated by the
 * predicate time/1.
 *
 * Example:
 * ?- statistics.
 * Max Memory             512,753,664 Bytes
 * Used Memory             68,568,872 Bytes
 * Free Memory            444,184,792 Bytes
 * Uptime                      5,293 Millis
 * GC Time                        12 Millis
 * Threads Time                1,000 Millis
 * Current Time           02/13/18 15:20:08
 *
 * The threads managed by a thread are determined from the thread
 * groups owned by the thread. The managed time is determined from
 * managed threads that are already dead, where-as the snapshot time is
 * determined from managed threads that are still alive. The CPU time
 * is determined from summing the thread, managed and snapshot CPU time.
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

:- use_package(foreign(jekpro/platform/swing)).
:- use_package(foreign(jekpro/tools/call)).

:- module(user, []).
:- use_module(library(stream/console)).
:- use_module(library(system/locale)).
:- use_module(library(system/zone)).
:- use_module(library(system/thread)).
:- use_module(library(advanced/aggregate)).
:- use_module(library(system/group)).
:- sys_load_resource(gestalt).

/*********************************************************************/
/* Statistics                                                        */
/*********************************************************************/

/**
 * statistics(K, V):
 * The predicate succeeds for the values V of the keys K. The
 * following keys are returned by the predicate. For a list of
 * keys see the API documentation.
 */
% statistics(+Atom, -Atomic)
:- public statistics/2.
statistics(K, V) :-
   var(K), !,
   sys_current_stat(K),
   sys_get_stat(K, V).
statistics(K, V) :-
   sys_get_stat(K, V).

% sys_current_stat(-Atom)
:- private sys_current_stat/1.
:- foreign(sys_current_stat/1, 'ForeignStatistics',
      sysCurrentStat('Interpreter','CallOut')).

% sys_get_stat(+Atom, -Term)
:- private sys_get_stat/2.
sys_get_stat(time, V) :- !,
   sys_get_stat(sys_time_self, Self),
   sys_get_stat(sys_time_managed, Managed),
   sys_get_stat(sys_time_snapshot, Snapshot),
   V is Self+Managed+Snapshot.
sys_get_stat(sys_time_snapshot, V) :- !,
   thread_current(Thread),
   sys_snapshot_thread(Thread, V).
sys_get_stat(K, V) :-
   sys_sys_get_stat(K, V).

% sys_sys_get_stat(+Atom, -Term)
:- private sys_sys_get_stat/2.
:- foreign(sys_sys_get_stat/2, 'ForeignStatistics',
      sysGetStat('Interpreter','String')).

% sys_snapshot_thread(+Thread, -Integer)
:- private sys_snapshot_thread/2.
sys_snapshot_thread(Thread, V) :-
   current_thread_flag(Thread, sys_thread_group, Group),
   aggregate_all(sum(T),
      (  current_group(Group, Group2),
         current_group_flag(Group2, sys_group_thread, Thread),
         sys_snapshot_group(Group2, T)), V).

% sys_snapshot_group(+Group, -Integer)
:- private sys_snapshot_group/2.
sys_snapshot_group(Group, V) :-
   aggregate_all(sum(T),
      (  current_thread(Group, Thread),
         thread_statistics(Thread, sys_time_self, T1),
         thread_statistics(Thread, sys_time_managed, T2),
         T is T1+T2), V1),
   aggregate_all(sum(T),
      (  current_group(Group, Group2),
         sys_snapshot_group(Group2, T)), V2),
   V is V1+V2.

/**
 * statistics:
 * The predicate displays the current statistics key value pairs.
 */
% statistics
:- public statistics/0.
statistics :-
   get_properties(gestalt, P),
   statistics(K, V),
   sys_convert_stat(K, V, W),
   message_make(P, statistics(K,W), M),
   write(M), nl, fail.
statistics.
:- set_predicate_property(statistics/0, sys_notrace).

% sys_convert_stat(+Atom, +Value, -Value)
:- private sys_convert_stat/3.
sys_convert_stat(wall, X, Y) :- !,
   get_time(X, Y).
sys_convert_stat(_, X, X).

/*********************************************************************/
/* Time                                                              */
/*********************************************************************/

/**
 * time(A):
 * The predicate succeeds whenever the goal A succeeds. The predicate
 * will measure the time for the execution of the goal A irrespective of
 * whether the goal A succeeds or fails. Redoing the goal A is measured
 * when the goal A has left some choice points.
 */
% time(+Goal)
:- public time/1.
:- meta_predicate time(0).
time(G) :-
   statistics(uptime, Uptime),
   statistics(gctime, Gctime),
   statistics(time, Time),
   sys_new_time_record(Uptime, Gctime, Time, T),
   sys_time_call(T),
   current_prolog_flag(sys_choices, X), G,
   current_prolog_flag(sys_choices, Y),
   (  X =:= Y -> !; true),
   sys_show_time_record(T).
:- set_predicate_property(time/1, sys_notrace).

% sys_time_call(+Record)
:- private sys_time_call/1.
sys_time_call(_).
sys_time_call(T) :-
   sys_show_time_record(T), fail.

/****************************************************************/
/* Time Record Access & Modification                            */
/****************************************************************/

% sys_show_time_record(+TimeRecord)
:- private sys_show_time_record/1.
sys_show_time_record(T) :-
   statistics(uptime, Uptime),
   statistics(gctime, Gctime),
   statistics(time, Time),
   sys_measure_time_record(T, Uptime, Gctime, Time),
   get_properties(gestalt, P),
   sys_current_record_stat(T, K, V),
   sys_convert_stat(K, V, W),
   message_make(P, time(K,W), M),
   write(M), fail.
sys_show_time_record(_) :- nl.

% sys_current_record_stat(+TimeRecord, +Atom, -Atomic)
:- private sys_current_record_stat/3.
sys_current_record_stat(T, K, V) :-
   var(K), !,
   sys_current_record_stat(K),
   sys_get_record_stat(T, K, V).
sys_current_record_stat(T, K, V) :-
   sys_get_record_stat(T, K, V).

% sys_new_time_record(+Integer, +Integer, +Integer, -TimeRecord)
:- private sys_new_time_record/4.
:- foreign_constructor(sys_new_time_record/4, 'TimeRecord',
      new('Number','Number','Number')).

% sys_measure_time_record(+Record, +Integer, +Integer, +Integer)
:- private sys_measure_time_record/4.
:- virtual sys_measure_time_record/4.
:- foreign(sys_measure_time_record/4, 'TimeRecord',
      sysMeasure('Number','Number','Number')).

% sys_current_record_stat(-Atom)
:- private sys_current_record_stat/1.
:- foreign(sys_current_record_stat/1, 'TimeRecord',
      sysCurrentStat('Interpreter','CallOut')).

% sys_get_record_stat(+Record, +Atom, -Atomic)
:- private sys_get_record_stat/3.
:- virtual sys_get_record_stat/3.
:- foreign(sys_get_record_stat/3, 'TimeRecord',
      getStat('Interpreter','String')).

/*********************************************************************/
/* Thread Statistics                                                 */
/*********************************************************************/

/**
 * thread_statistics(T, K, V):
 * The predicate succeeds for the values V of the keys K concerning the
 * thread T. The following keys are returned by the predicate. For
 * a list of keys see the API documentation.
 */
% thread_statistics(+Thread, +Atom, -Atomic)
:- public thread_statistics/3.
thread_statistics(T, K, V) :-
   var(K), !,
   sys_current_thread_stat(K),
   sys_get_thread_stat(T, K, V).
thread_statistics(T, K, V) :-
   sys_get_thread_stat(T, K, V).

:- private sys_current_thread_stat/1.
:- foreign(sys_current_thread_stat/1, 'ForeignStatistics',
      sysCurrentThreadStat('CallOut')).

:- private sys_get_thread_stat/3.
:- foreign(sys_get_thread_stat/3, 'ForeignStatistics',
      sysGetThreadStat('Thread','String')).

/***********************************************************/
/* Apropos Utility                                         */
/***********************************************************/

/**
 * sys_apropos_table(T):
 * The predicate succeeds with the file name of a apropos table.
 */
:- multifile sys_apropos_table/1.
:- public sys_apropos_table/1.
sys_apropos_table(library(swing/platform)).

/****************************************************************/
/* Thread Managed                                               */
/****************************************************************/

% sys_managed_add(+Thread, +Integer)
:- public sys_managed_add/2.
:- foreign(sys_managed_add/2, 'ForeignStatistics',
      sysManagedAdd('Thread','Number')).
