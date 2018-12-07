/**
 * This module provides supervised execution. Currently we support
 * the supervision of slave threads by a master thread, whereby
 * the slave threads are freshly created with a copy of the
 * given goal and then started.
 *
 * Example:
 * ?- sys_clean_thread((thread_sleep(1000), write(hello),
 *    nl)), thread_sleep(500), !.
 * Yes
 * ?- sys_clean_thread((thread_sleep(1000), write(hello),
 *    nl)), thread_sleep(1500), !.
 * hello
 * Yes
 *
 * The predicate sys_clean_thread/1 and sys_clean_threads/2 run
 * slave threads which will be cancelled for a termination event
 * in the continuation or a signal by another thread. Cancelling
 * is done by aborting and joining the slave threads.
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

:- package(library(jekpro/frequent/misc)).

:- module(clean, []).
:- use_module(library(system/group)).
:- use_module(library(system/thread)).
:- use_module(library(stream/console)).
:- use_module(library(system/locale)).
:- use_module(library(misc/text)).
:- sys_load_resource(show).

/**
 * sys_group_clean(G):
 * The predicate succeeds to create a new group G, and
 * also installs a clean-up handler to cancel the threads
 * of the group.
 */
% sys_group_clean(-Group)
:- public sys_group_clean/1.
:- meta_predicate sys_group_clean(?).
sys_group_clean(G) :-
   sys_atomic((  group_new(G),
                 sys_cleanup(sys_group_fini(G)))).

/**
 * sys_group_fini(G):
 * The predicate succeeds to abort and join the group G.
 */
% sys_group_fini(+Group)
:- private sys_group_fini/1.
sys_group_fini(G) :-
   group_thread(G, T), !,
   sys_thread_fini(T),
   sys_group_fini(G).
sys_group_fini(_).

/**
 * sys_thread_fini(T):
 * The predicate succeeds to abort and join the thread T.
 */
% sys_thread_fini(+Thread)
:- private sys_thread_fini/1.
sys_thread_fini(T) :-
   thread_abort(T, system_error(user_close)),
   thread_join(T).

/**
 * sys_thread_init(G, C):
 * The predicate succeeds to create and start a new thread
 * for a copy of the goal C in the group G.
 */
% sys_thread_init(+Group, +Goal)
:- public sys_thread_init/2.
:- meta_predicate sys_thread_init(?,0).
sys_thread_init(G, C) :-
   thread_new(G, C, I),
   thread_start(I).

/**
 * sys_thread_inits(G, C, N):
 * The predicate succeeds to create and start a new thread
 * for a copy of the goal C in the group G for as many as N times.
 */
% sys_thread_inits(+Group, +Goal, +Integer)
:- public sys_thread_inits/3.
:- meta_predicate sys_thread_inits(?,0,?).
sys_thread_inits(_, _, 0) :- !.
sys_thread_inits(G, C, N) :-
   N > 0,
   sys_thread_init(G, C),
   M is N-1,
   sys_thread_inits(G, C, M).

/**********************************************************/
/* Threads Listing                                        */
/**********************************************************/

/**
 * threads:
 * The predicate lists the current threads.
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
