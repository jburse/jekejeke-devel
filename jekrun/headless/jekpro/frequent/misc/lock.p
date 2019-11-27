/**
 * A mutex is a binary semaphore. A mutex can be created by the predicates
 * mutex_new/1 and unslotted_new/2. A mutex need not be explicitly
 * destroyed, it will automatically be reclaimed by the Java GC when
 * not anymore used. To balance acquires and releases of the same
 * semaphore the use of setup_call_cleanup/3 is recommended. Threads
 * waiting for a semaphore can be interrupted.
 *
 * Example:
 * ?- mutex_new(M), lock_acquire(M), lock_release(M).
 * M = 0r3f10bc2a
 *
 * The predicates lock_acquire/1 and lock_attempt/[1,2] allow incrementing
 * a semaphore by one. These predicates will block, fail or timeout when
 * the semaphore has already reached its maximum by other threads. The
 * predicate lock_release/1 allows decrementing the semaphore by one,
 * provided it is not already zero. The slotted versions check that the
 * owner doesn't change, but currently do not allow re-entrancy.
 *
 * A read write pair can be created by the predicates lock_new/1 and
 * nonescalable_new/1. In the non-escalable version the non-binary read
 * semaphore can be retrieved by the predicate get_read/2 and it can be
 * incremented provided the write semaphore is zero. The binary write
 * semaphore can be retrieved by the predicate get_write/2 and it can be
 * incremented provided the read semaphore is zero.
 *
 * For the escalated version of the read write pair it is also allowed
 * that the same thread holds a read and a write lock from a read write
 * pair. This can for example be used to upgrade or downgrade a read write
 * pair by using unbalanced locking. For example if a thread already holds
 * a write lock, it can acquire the read lock and then release the write
 * lock. The result is that the write lock was changed into a read lock.
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

/****************************************************************/
/* Binary Locks                                                 */
/****************************************************************/

/**
 * mutex_new(M):
 * The predicate succeeds for a new reentrant lock M.
 */
% mutex_new(-Lock)
:- public mutex_new/1.
:- foreign_constructor(mutex_new/1, 'ReentrantLock', new).

/**
 * unslotted_new(M):
 * The predicate succeeds for a new unslotted lock M.
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
 * lock_attempt(L, T):
 * The predicate succeeds after locking the lock L
 * in the time-out T. Otherwise the predicate fails.
 */
% lock_attempt(+Lock, +Integer)
:- public lock_attempt/2.
:- foreign(lock_attempt/2, 'ForeignLock', sysTryLock('Lock', long)).

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
 * The predicate succeeds for a new slotted and escalable
 * read write pair P.
 */
% readwrite_new(-ReadWriteLock)
:- public readwrite_new/1.
:- foreign_constructor(readwrite_new/1, 'Locker', new).

/**
 * nonescalable_new(P):
 * The predicate succeeds for a new unslotted and non-escalable
 * read write pair P.
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
