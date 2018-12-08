/**
 * A Prolog thread group is simply a Java thread group. A Prolog thread
 * group might contain Prolog threads and otherwise threads.
 *
 * Example:
 * ?- threads.
 * Thread       State           Group
 * Thread-2     WAITING         main
 * Thread-3     RUNNABLE        Group-1
 * Thread-4     WAITING         Group-1
 * Yes
 *
 * Further the predicate threads/0 allows listing all Prolog threads
 * currently known to the base knowledge base. The Prolog threads
 * are shown with their state and group. Currently the predicate also
 * lists threads across different sub knowledge bases.
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

:- package(library(jekpro/frequent/system)).
:- use_package(foreign(jekpro/frequent/system)).
:- use_package(foreign(jekpro/tools/call)).
:- use_module(library(system/thread)).
:- use_module(library(stream/console)).
:- use_module(library(system/locale)).
:- use_module(library(misc/text)).
:- sys_load_resource(show).

:- module(group, []).

/****************************************************************/
/* Group Creation                                               */
/****************************************************************/

/**
 * group_new(G):
 * The predicate succeeds for a new thread group G.
 */
% group_new(-Group)
:- public group_new/1.
:- foreign(group_new/1, 'ForeignGroup', sysGroupNew).

/****************************************************************/
/* Group Inspection                                             */
/****************************************************************/

/**
 * group_thread(G, T):
 * The predicate succeeds in T with the oldest thread
 * of the thread group G if there is any. Otherwise the
 * predicate fails.
 */
% group_thread(+Group, -Thread)
:- public group_thread/2.
:- foreign(group_thread/2, 'ForeignGroup', sysGroupThread('ThreadGroup')).

/**
 * current_group_flag(G, K, V):
 * The predicate succeeds for the values V of the keys K concerning the
 * group G. The following keys are returned by the predicate. For
 * a list of keys see the API documentation.
 */
% current_group_flag(+Group, +Atom, -Atomic)
:- public current_group_flag/3.
current_group_flag(T, K, V) :-
   var(K), !,
   sys_current_group_flag(K),
   sys_get_group_flag(T, K, V).
current_group_flag(T, K, V) :-
   sys_get_group_flag(T, K, V).

% sys_current_group_flag(-Atom)
:- private sys_current_group_flag/1.
:- foreign(sys_current_group_flag/1, 'ForeignGroup',
      sysCurrentGroupFlag('CallOut')).

% sys_get_group_flag(+Group, +Atom, -Atomic)
:- private sys_get_group_flag/3.
:- foreign(sys_get_group_flag/3, 'ForeignGroup',
      sysGetGroupFlag('ThreadGroup','String')).

/****************************************************************/
/* Managed Threads                                              */
/****************************************************************/

/**
 * threads:
 * The predicate lists the managed threads.
 */
:- public threads/0.
threads :- thread_show_keys,
   current_thread(T),
   thread_show_values(T), fail.
threads.

:- private thread_show_keys/0.
thread_show_keys :-
   sys_get_lang(show, P),
   sys_current_show_stat(K),
   message_make(P, thread_show_key(K), M),
   ttywrite(M), fail.
thread_show_keys :- ttynl.

:- private thread_show_values/1.
thread_show_values(T) :-
   sys_get_lang(show, P),
   sys_current_show_stat(K),
   sys_get_show_stat(T, K, V),
   message_make(P, thread_show_value(K,V), M),
   ttywrite(M), fail.
thread_show_values(_) :- ttynl.

:- private sys_current_show_stat/1.
sys_current_show_stat(sys_thread_name).
sys_current_show_stat(sys_thread_state).
sys_current_show_stat(sys_thread_group_name).

:- private sys_get_show_stat/3.
sys_get_show_stat(T, sys_thread_name, V) :-
   current_thread_flag(T, sys_thread_name, V).
sys_get_show_stat(T, sys_thread_state, V) :-
   current_thread_flag(T, sys_thread_state, V).
sys_get_show_stat(T, sys_thread_group_name, V) :-
   current_thread_flag(T, sys_thread_group, H),
   current_group_flag(H, sys_group_name, V).

/**
 * current_thread(T):
 * The predicate succeeds in T with the managed threads.
 */
% current_thread(-Thread)
:- public current_thread/1.
current_thread(X) :-
   var(X), !,
   sys_current_thread(L),
   sys_member(X, L).
current_thread(X) :-
   sys_current_thread_chk(X).

:- private sys_current_thread/1.
:- foreign(sys_current_thread/1, 'ForeignGroup',
      sysCurrentThread('Interpreter')).

:- private sys_current_thread_chk/1.
:- foreign(sys_current_thread_chk/1, 'ForeignGroup',
      sysCurrentThreadChk('Interpreter','Thread')).
