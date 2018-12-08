/**
 * A Prolog thread is simply a Java thread that executes a Prolog
 * call-in. The call-in can be created by the predicate thread_new/2
 * and the goal will be copied. The thread can then be started by the
 * predicate thread_start/1. A thread need not be explicitly destroyed,
 * it will automatically be reclaimed by the Java GC when
 * not anymore used.
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
 * thread_abort/2 and thread_down/[2,3]. A thread can be killed by the
 * predicate thread_kill/1. The later predicate should only be used in
 * emergency situation, since the receiving Prolog call-in will not be
 * able to properly clean-up.
 *
 * The predicates thread_join/1 and thread_combine/[1,2] allow waiting
 * for the termination of a thread. The predicates will block, fail or
 * timeout when the thread is alive. Every thread can be joined and joining
 * does not retrieve an exit code and/or an exit Prolog term. The predicates
 * current_thread/1 and current_thread_flag/3 allow inspecting threads
 * and their properties.
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

/****************************************************************/
/* Thread Creation                                              */
/****************************************************************/

/**
 * thread_new(C, T):
 * The predicate succeeds for a new thread T on the
 * copy of the goal C.
 */
% thread_new(+Goal, -Thread)
:- public thread_new/2.
:- meta_predicate thread_new(0,?).
:- foreign(thread_new/2, 'ForeignThread',
      sysThreadNew('Interpreter','AbstractTerm')).

/**
 * thread_start(T):
 * The predicate succeeds for starting the thread T.
 */
% thread_start(+Tread):
:- public thread_start/1.
:- virtual thread_start/1.
:- foreign(thread_start/1, 'Thread', start).

/****************************************************************/
/* Thread Signalling                                            */
/****************************************************************/

/**
 * thread_abort(T, M):
 * The predicate succeeds for signalling the error
 * message M to the thread T.
 */
% thread_abort(+Thread, +Message)
:- public thread_abort/2.
:- foreign(thread_abort/2, 'ForeignThread', sysThreadAbort('Thread','AbstractTerm')).

/**
 * thread_down(T, M):
 * The predicate succeeds for signalling the error
 * message M to the thread T. Otherwise the predicate fails.
 */
:- public thread_down/2.
:- foreign(thread_down/2, 'ForeignThread', sysThreadDown('Thread','AbstractTerm')).

/**
 * thread_downl(T, M, W):
 * The predicate succeeds for signalling the error message M
 * to the thread T in the timeout W. Otherwise the predicate fails.
 */
% thread_down(+Thread, +Message, +Integer)
:- public thread_down/3.
:- foreign(thread_down/3, 'ForeignThread', sysThreadDown('Thread','AbstractTerm',long)).

/**
 * thread_kill(T):
 * The predicate succeeds for killing the thread T.
 */
% thread_kill(+Thread)
:- public thread_kill/1.
:- virtual thread_kill/1.
:- foreign(thread_kill/1, 'Thread', stop).

/****************************************************************/
/* Thread Joining                                               */
/****************************************************************/

/**
 * thread_join(T):
 * The predicate succeeds when the thread T has terminated.
 */
:- public thread_join/1.
:- virtual thread_join/1.
:- foreign(thread_join/1, 'Thread', join).

/**
 * thread_combine(T):
 * The predicate succeeds when the thread T has terminated.
 * Otherwise the predicate fails.
 */
:- public thread_combine/1.
:- foreign(thread_combine/1, 'ForeignThread', sysThreadCombine('Thread')).

/**
 * thread_combine(T, W):
 * The predicate succeeds when the thread T has terminated
 * in the timeout W. Otherwise the predicate fails.
 */
:- public thread_combine/2.
:- foreign(thread_combine/2, 'ForeignThread', sysThreadCombine('Thread',long)).

/****************************************************************/
/* Thread Inspection                                            */
/****************************************************************/

/**
 * current_thread_flag(T, K, V):
 * The predicate succeeds for the values V of the keys K concerning the
 * thread T. The following keys are returned by the predicate. For
 * a list of keys see the API documentation.
 */
% current_thread_flag(+Thread, +Atom, -Atomic)
:- public current_thread_flag/3.
current_thread_flag(T, K, V) :-
   var(K), !,
   sys_current_thread_flag(K),
   sys_get_thread_flag(T, K, V).
current_thread_flag(T, K, V) :-
   sys_get_thread_flag(T, K, V).

% sys_current_thread_flag(-Atom)
:- private sys_current_thread_flag/1.
:- foreign(sys_current_thread_flag/1, 'ForeignThread',
      sysCurrentThreadFlag('CallOut')).

% sys_get_thread_flag(+Thread, +Atom, -Atomic)
:- private sys_get_thread_flag/3.
:- foreign(sys_get_thread_flag/3, 'ForeignThread',
      sysGetThreadFlag('Thread','String')).
