/**
 * This module provides supervised execution. Currently we support
 * the supervision of slave threads by a master thread, whereby the
 * slave threads are freshly created and started.
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
 * The predicate sys_clean_thread/1 runs a slave thread which will be
 * cancelled for a termination event in the continuation. Cancelling is
 * done by aborting and joining the slave thread.
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

:- module(clean, []).
:- use_module(library(system/thread)).

/**
 * sys_clean_thread(G):
 * The predicate succeeds to create and start a new thread for a
 * copy of the goal G, and also installs a clean-up handler to abort
 * and join the thread.
 */
% sys_clean_thread(+Goal)
:- public sys_clean_thread/1.
:- meta_predicate sys_clean_thread(0).
sys_clean_thread(G) :-
   sys_atomic((  sys_thread_init(G, I),
                 sys_cleanup(sys_thread_fini(I)))).

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
