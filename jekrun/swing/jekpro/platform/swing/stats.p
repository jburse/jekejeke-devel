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
 * Thread Cpu Time             1,000 Millis
 * Current Time           02/13/18 15:20:08
 *
 * Since Jekejeke Prolog is a multi-threaded interpreter, we also provide
 * statistics for the threads in JVM known to the interpreter. The
 * predicate thread_statistics/3 returns some key figures concerning
 * the given thread, whereas the predicate threads/0 displays the
 * key figures for all known threads on the standard output.
 *
 * Example:
 * ?- threads.
 * Id      Alive Clauses
 *      21 Yes              0
 *      25 Yes      1,256,856
 *
 * The JVM will notify the Prolog interpreter about low memory. On the
 * Android platform this is a suicide notice, since foreground heap
 * compaction is not yet available  and it is therefore recommended
 * to exit the Prolog interpreter. On both the Swing and the Android
 * platforms the thread statistics are then used to decide which
 * thread will be aborted.
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
:- use_module(library(system/shell)).
:- use_module(library(system/thread)).
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

:- private sys_current_stat/1.
:- foreign(sys_current_stat/1, 'ForeignStatistics', sysCurrentStat('CallOut')).

:- private sys_get_stat/2.
:- foreign(sys_get_stat/2, 'ForeignStatistics', sysGetStat('String')).

/**
 * statistics:
 * The predicate displays the current statistics key value pairs.
 */
% statistics
:- public statistics/0.
statistics :-
   sys_get_lang(gestalt, P),
   statistics(K, V),
   sys_convert_stat(K, V, W),
   message_make(P, statistics(K,W), M),
   ttywrite(M), ttynl, fail.
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
time(G) :-
   sys_make_time_record(T),
   (  sys_start_time_record(T)
   ;  sys_time_record(T), fail), G,
   (  sys_time_record(T)
   ;  sys_start_time_record(T), fail).
:- set_predicate_property(time/1, sys_notrace).

% sys_time_record(+TimeRecord)
:- private sys_time_record/1.
sys_time_record(T) :-
   sys_end_time_record(T),
   sys_get_lang(gestalt, P),
   sys_time_record(T, K, V),
   sys_convert_stat(K, V, W),
   message_make(P, time(K,W), M),
   ttywrite(M), fail.
sys_time_record(_) :- ttynl.

% sys_time_record(+TimeRecord, +Atom, -Atomic)
:- private sys_time_record/3.
sys_time_record(T, K, V) :-
   var(K), !,
   sys_current_record_stat(K),
   sys_get_record_stat(T, K, V).
sys_time_record(T, K, V) :-
   sys_get_record_stat(T, K, V).

:- private sys_make_time_record/1.
:- foreign_constructor(sys_make_time_record/1, 'TimeRecord', new).

:- private sys_start_time_record/1.
:- virtual sys_start_time_record/1.
:- foreign(sys_start_time_record/1, 'TimeRecord', start).

:- private sys_end_time_record/1.
:- virtual sys_end_time_record/1.
:- foreign(sys_end_time_record/1, 'TimeRecord', end).

% sys_current_record_stat(-Atom)
:- private sys_current_record_stat/1.
:- foreign(sys_current_record_stat/1, 'TimeRecord', sysCurrentStat('CallOut')).

% sys_get_record_stat(+Record, +Atom, -Atomic)
:- private sys_get_record_stat/3.
:- virtual sys_get_record_stat/3.
:- foreign(sys_get_record_stat/3, 'TimeRecord', getStat('String')).

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
:- foreign(sys_current_thread_stat/1, 'ForeignStatistics', sysCurrentThreadStat('CallOut')).

:- private sys_get_thread_stat/3.
:- foreign(sys_get_thread_stat/3, 'ForeignStatistics', sysGetThreadStat('Thread','String')).

/**
 * threads:
 * The predicate displays the current threads statistics key value pairs.
 */
:- public threads/0.
threads :- thread_show_keys, ttynl, fail.
threads :-
   current_thread(T),
   thread_show_values(T), ttynl, fail.
threads.

:- private thread_show_keys/0.
thread_show_keys :-
   sys_get_lang(gestalt, P),
   sys_current_show_stat(K),
   message_make(P, thread_show_key(K), M),
   ttywrite(M), fail.
thread_show_keys.

:- private thread_show_values/1.
thread_show_values(T) :-
   sys_get_lang(gestalt, P),
   sys_current_show_stat(K),
   sys_get_show_stat(T, K, V),
   sys_convert_show(K, V, W),
   message_make(P, thread_show_value(K,W), M),
   ttywrite(M), fail.
thread_show_values(_).

:- private sys_current_show_stat/1.
sys_current_show_stat(sys_thread_id).
sys_current_show_stat(sys_is_alive).
sys_current_show_stat(sys_thread_local_clauses).

:- private sys_get_show_stat/3.
sys_get_show_stat(T, sys_thread_id, V) :-
   current_thread_flag(T, sys_thread_id, V).
sys_get_show_stat(T, sys_is_alive, V) :-
   current_thread_flag(T, sys_is_alive, V).
sys_get_show_stat(T, sys_thread_local_clauses, V) :-
   thread_statistics(T, sys_thread_local_clauses, V).

% sys_convert_show(+Atom, +Value, -Value)
:- private sys_convert_show/3.
sys_convert_show(sys_is_alive, X, Y) :- !,
   sys_get_lang(gestalt, P),
   message_make(P, boolean(X), Y).
sys_convert_show(_, X, X).

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
