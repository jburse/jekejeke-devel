/**
 * A Prolog thread is simply a Java thread that executes a Prolog call-in.
 * The call-in can be cre-ated by the predicate thread_new/2 and the
 * goal will be copied. The thread can then be started by the predicate
 * thread_start/1. A thread need not be explicitly destroyed, it will
 * automatically be reclaimed by the Java GC when not anymore used.
 *
 * Examples:
 * ?- thread_new((between(0,10,X),write(X),write(' '),fail;
 *      nl), I), thread_start(I).
 * I = 0r5ab10801
 * 0 1 2 3 4 5 6 7 8 9 10
 *
 * A new thread will share the knowledgebase and the display input/output
 * of the creating thread. On the other hand a new thread will have its
 * own thread local predicates. A thread can be aborted by the predicate
 * thread_abort/2. A thread can be killed by the predicate thread_kill/1.
 *
 * The predicates thread_join/1 and thread_combine/2 allow waiting for the
 * termination of a thread. The predicates will block respectively timeout
 * when the thread is alive. Every thread can be joined and joining does
 * not retrieve an exit code and/or an exit Prolog term.
 *
 * Threads are not managed. Predicates are free to implement their own
 * choreography. There is only a management in that interpreters are
 * managed by the knowledge base. Currently if a knowledge base is finished
 * in-use interpreters are aborted. In the GUI environments this means that
 * zombie threads are aborted when the last console windows closes.
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

:- package(library(jekpro/frequent/system)).
:- use_package(foreign(jekpro/frequent/system)).
:- use_package(foreign(jekpro/tools/call)).
:- use_package(foreign(jekpro/tools/term)).
:- use_package(foreign(matula/util/misc)).

:- module(thread, []).

/**
 * thread_sleep(M):
 * The predicate succeeds after M milliseconds.
 */
% thread_sleep(+Integer)
:- public thread_sleep/1.
:- foreign(thread_sleep/1, 'Alarm', sleep(long)).

/**
 * thread_current(T):
 * The predicate succeeds for the current thread T.
 */
% thread_current(-Thread)
:- public thread_current/1.
:- foreign(thread_current/1, 'Thread', currentThread).

/**
 * thread_new(G, T):
 * The predicate succeeds for a new thread T on the copy of the goal G.
 */
% thread_new(+Goal, -Thread)
:- public thread_new/2.
:- meta_predicate thread_new(0,?).
:- foreign(thread_new/2, 'ForeignThread', sysThreadNew('Interpreter','Term')).

/**
 * thread_start(T):
 * The predicate succeeds for starting the thread T.
 */
% thread_start(+Tread):
:- public thread_start/1.
:- virtual thread_start/1.
:- foreign(thread_start/1, 'Thread', start).

/**
 * thread_abort(T, M):
 * The predicate succeeds for aborting the thread T by the message M.
 */
% thread_abort(+Thread, +Message)
:- public thread_abort/2.
:- foreign(thread_abort/2, 'ForeignThread', sysThreadAbort('Thread','Term')).

/**
 * thread_kill(T):
 * The predicate succeeds for killing the thread T.
 */
% thread_kill(+Thread)
:- public thread_kill/1.
:- virtual thread_kill/1.
:- foreign(thread_kill/1, 'Thread', stop).

/**
 * thread_join(T):
 * The predicate succeeds when the thread T has terminated.
 * The predicate blocks for an alive thread.
 */
:- public thread_join/1.
:- virtual thread_join/1.
:- foreign(thread_join/1, 'Thread', join).

/**
 * thread_combine(T, W):
 * The predicate succeeds when the thread T has terminated
 * in the timeout W. Otherwise the predicate fails.
 */
:- public thread_combine/2.
:- foreign(thread_combine/2, 'ForeignThread', sysThreadCombine('Thread',long)).

