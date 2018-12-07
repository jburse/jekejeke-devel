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
 * sys_clean_thread(G):
 * The predicate succeeds to create and start a new thread for a
 * copy of the goal G, and also installs a clean-up handler to
 * cancel the thread.
 */
% sys_clean_thread(+Goal)
:- public sys_clean_thread/1.
:- meta_predicate sys_clean_thread(0).
sys_clean_thread(G) :-
   sys_atomic((  sys_thread_init(G, I),
                 sys_cleanup(sys_thread_fini(I)))).

/**
 * sys_clean_threads(G, N):
 * The predicate succeeds to create and start N new threads for
 * copies of the goal G, and also installs clean-up handlers to
 * cancel the threads.
 */
% sys_clean_threads(+Goal, +Integer)
:- public sys_clean_threads/2.
:- meta_predicate sys_clean_threads(0,?).
sys_clean_threads(_, 0) :- !.
sys_clean_threads(G, N) :-
   N > 0,
   sys_clean_thread(G),
   M is N-1,
   sys_clean_threads(G, M).

/**
 * sys_thread_init(G, I):
 * The predicate succeeds to create and start a new thread I
 * for a copy of the goal G.
 */
% sys_thread_init(+Goal, -Thread)
:- private sys_thread_init/2.
:- meta_predicate sys_thread_init(0,?).
sys_thread_init(G, I) :-
   thread_new(G, I),
   thread_start(I).

/**
 * sys_thread_fini(I):
 * The predicate succeeds to abort and join the thread I.
 */
% sys_thread_fini(+Thread)
:- private sys_thread_fini/1.
sys_thread_fini(I) :-
   thread_abort(I, system_error(user_close)),
   thread_join(I).

/**********************************************************/
/* Threads Listing                                        */
/**********************************************************/

/**
 * threads:
 * The predicate displays the current threads statistics key value pairs.
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
sys_current_show_stat(sys_thread_id).
sys_current_show_stat(sys_thread_state).
sys_current_show_stat(sys_thread_group_name).

:- private sys_get_show_stat/3.
sys_get_show_stat(T, sys_thread_id, V) :-
   current_thread_flag(T, sys_thread_id, V).
sys_get_show_stat(T, sys_thread_state, V) :-
   current_thread_flag(T, sys_thread_state, V).
sys_get_show_stat(T, sys_thread_group_name, V) :-
   current_thread_flag(T, sys_thread_group, H),
   current_group_flag(H, sys_group_name, V).
