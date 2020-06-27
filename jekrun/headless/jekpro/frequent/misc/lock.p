/**
 * A mutex is a binary lock. A mutex can be created by the predicates
 * mutex_new/1 and unslotted_new/2. A mutex need not be explicitly
 * destroyed, it will automatically be reclaimed by the Java GC when
 * not anymore used. To balance acquires and releases of the same
 * lock the use of setup_call_cleanup/3 is recommended. Threads
 * waiting for a lock can be interrupted.
 *
 * Example:
 * ?- mutex_new(M), lock_acquire(M), lock_release(M).
 * M = 0r3f10bc2a
 *
 * The predicates lock_acquire/1 and lock_attempt/[1,2] allow to
 * acquire a lock. These predicates will block, fail or timeout when
 * the lock has already been acquired by other threads. The predicate
 * lock_release/1 allows releasing the lock. A read write pair can be
 * created by the predicates readwrite_new/1 and nonescalable_new/1.
 * The predicates get_read/2 and get_write/2 allow retrieving the
 * corresponding lock.
 *
 * Some of the locks can produce condition variables via the predicate
 * cond_new/2. A condition variable allows a thread to temporarily
 * leaving a critical region via the predicates cond_wait/1 and cond_wait/2.
 * The predicates cond_notify/1 and cond_notify_all/1 on the other hand
 * let a waiting thread respectively all waiting threads enter their
 * critical region again.
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
:- use_package(foreign(jekpro/frequent/misc)).
:- use_package(foreign(matula/util/misc)).
:- use_package(foreign(java/util/concurrent/locks)).

:- module(lock, []).

/**
 * synchronized(L, G):
 * The predicate succeeds whenever the goal G succeeds. The
 * lock is acquired in the call port, and released in the
 * deterministic exit port, in the fail port or when an
 * exception happens.
 */
% synchronized(+Lock, +Goal)
:- public synchronized/2.
:- meta_predicate synchronized(?, 0).
synchronized(L, G) :-
   setup_call_cleanup(lock_acquire(L),
      G,
      lock_release(L)).

/****************************************************************/
/* Binary Locks                                                 */
/****************************************************************/

/**
 * mutex_new(M):
 * The predicate succeeds for a new reentrant lock M.
 * The lock can produce condition variables.
 */
% mutex_new(-Lock)
:- public mutex_new/1.
:- foreign_constructor(mutex_new/1, 'ReentrantLock', new).

/**
 * unslotted_new(M):
 * The predicate succeeds for a new unslotted lock M.
 * The lock cannot produce condition variables.
 */
% unslotted_new(-Lock)
:- public unslotted_new/1.
:- foreign_constructor(unslotted_new/1, 'Unslotted', new).

/**
 * lock_aquire(L):
 * The predicate succeeds after locking the lock L.
 */
% lock_acquire(+Lock)
:- public lock_acquire/1.
:- virtual lock_acquire/1.
:- foreign(lock_acquire/1, 'Lock', lockInterruptibly).

/**
 * lock_attempt(L):
 * The predicate succeeds after locking the lock L.
 * Otherwise the predicate fails.
 */
% lock_attempt(+Lock)
:- public lock_attempt/1.
:- virtual lock_attempt/1.
:- foreign(lock_attempt/1, 'Lock', tryLock).

/**
 * lock_attempt_timeout(L, T):
 * The predicate succeeds after locking the lock L
 * in the time-out T. Otherwise the predicate fails.
 */
% lock_attempt_timeout(+Lock, +Integer)
:- public lock_attempt_timeout/2.
:- foreign(lock_attempt_timeout/2, 'ForeignLock', sysTryLock('Lock', long)).

/**
 * lock_release(L):
 * The predicate succeeds after unlocking the lock L.
 */
% lock_release(+Lock)
:- public lock_release/1.
:- virtual lock_release/1.
:- foreign(lock_release/1, 'Lock', unlock).

/****************************************************************/
/* Read/Write Locks                                             */
/****************************************************************/

/**
 * readwrite_new(P):
 * The predicate succeeds for a new reentrant and gradeable
 * read write pair P. The locks can produce condition variables.
 */
% readwrite_new(-ReadWriteLock)
:- public readwrite_new/1.
:- foreign_constructor(readwrite_new/1, 'ReentrantReadWriteLock', new).

/**
 * nonescalable_new(P):
 * The predicate succeeds for a new unslotted and non-escalable
 * read write pair P. The locks cannot produce condition variables.
 */
% nonescalable_new(-ReadWriteLock)
:- public nonescalable_new/1.
:- foreign_constructor(nonescalable_new/1, 'Nonescalable', new).

/**
 * get_read(P, R):
 * The predicate succeeds for the read lock R of the read write pair P.
 */
% get_read(+ReadWriteLock, -Lock)
:- public get_read/2.
:- virtual get_read/2.
:- foreign(get_read/2, 'ReadWriteLock', readLock).

/**
 * get_write(P, W):
 * The predicate succeeds for the write lock W of the read write pair P.
 */
% get_write(+ReadWriteLock, -Lock)
:- public get_write/2.
:- virtual get_write/2.
:- foreign(get_write/2, 'ReadWriteLock', writeLock).
