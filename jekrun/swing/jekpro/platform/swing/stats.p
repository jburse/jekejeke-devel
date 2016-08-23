/**
 * This module provides some execution statistics. The predicate
 * statistics/2 returns some key figures of the memory management
 * and the runtime system, whereas the predicate statistics/0
 * displays the key figures on the standard output. The measurement
 * of the time performance of a goal is facilitated by the
 * predicate time/1.
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

:- module(user, []).
:- use_module(library(stream/console)).
:- use_module(library(system/locale)).
:- sys_load_resource(gestalt).

/**
 * statistics(K, V):
 * The predicate succeeds for the values V of the keys K. The following
 * keys are returned by the predicate. For a list of keys see the API
 * documentation.
 */
% statistics(+Atom, -Atomic)
:- public statistics/2.
statistics(K, V) :-
   ground(K), !,
   sys_get_stat(K, V).
statistics(K, V) :-
   sys_list_stats(L),
   sys_member(K, L),
   sys_get_stat(K, V).

:- private sys_get_stat/2.
:- foreign(sys_get_stat/2, 'ForeignStatistics',
      sysGetStat('String')).

:- private sys_list_stats/1.
:- foreign(sys_list_stats/1, 'ForeignStatistics',
      sysListStats).

/**
 * statistics:
 * The predicate displays the current statistics key value pairs.
 */
% statistics
:- public statistics/0.
statistics :-
   statistics(X, Y),
   sys_get_lang(gestalt, P),
   message_make(P, statistics(X,Y), M),
   ttywrite(M), ttynl, fail.
statistics.
:- set_predicate_property(statistics/0, sys_notrace).

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
   sys_time_record(T, X, Y),
   sys_get_lang(gestalt, P),
   message_make(P, time(X,Y), M),
   ttywrite(M), fail.
sys_time_record(_) :- ttynl.

% sys_time_record(+TimeRecord, +Atom, -Atomic)
:- private sys_time_record/3.
sys_time_record(T, K, V) :-
   ground(K), !,
   sys_get_record_stat(T, K, V).
sys_time_record(T, K, V) :-
   sys_list_record_stats(L),
   sys_member(K, L),
   sys_get_record_stat(T, K, V).

:- private sys_make_time_record/1.
:- foreign(sys_make_time_record/1, 'ForeignStatistics',
      sysMakeTimeRecord).

:- private sys_start_time_record/1.
:- foreign(sys_start_time_record/1, 'ForeignStatistics',
      sysStartTimeRecord('Object')).

:- private sys_end_time_record/1.
:- foreign(sys_end_time_record/1, 'ForeignStatistics',
      sysEndTimeRecord('Object')).

:- private sys_list_record_stats/1.
:- foreign(sys_list_record_stats/1, 'ForeignStatistics',
      sysListRecordStats).

:- private sys_get_record_stat/3.
:- foreign(sys_get_record_stat/3, 'ForeignStatistics',
      sysGetRecordStat('Object','String')).
