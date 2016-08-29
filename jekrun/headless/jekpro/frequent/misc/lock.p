/**
 * A mutex is a binary semaphore. A mutex can be created by the predicate
 * mutex_new/1. A mutex need not be explicitly destroyed, it will
 * automatically be reclaimed by the Java GC when not anymore used. Threads
 * waiting for a mutex can be interrupted.
 *
 * Example:
 * ?- mutex_new(M), lock_acquire(M), lock_acquire(M).
 * Error: The thread already holds the lock.
 *    	lock_acquire/1
 * ?- mutex_new(M), lock_acquire(M), lock_release(M), lock_acquire(M).
 * M = 0r3f10bc2a
 *
 * The predicates lock_acquire/1 and lock_attempt/[1,2] allow incrementing a
 * semaphore by one. These predicates will block, fail or timeout when
 * the semaphore has already reached its maximum by other threads. The
 * predicate lock_release/1 allows decrementing the semaphore by one,
 * provided it is not already zero.
 *
 * A read write pair can be create by the predicate readwrite_new/1. The
 * non-binary read semaphore can be retrieved by the predicate get_read/2 and
 * it can be incremented provided the write semaphore is zero. The binary write
 * semaphore can be retrieved by the predicate get_write/2 and it can be
 * incremented provided the read semaphore is zero.
 *
 * It is allowed that the same thread holds a read and a write lock from
 * a read write pair. This can also be used to upgrade or downgrade a
 * read write pair by using unbalanced locking. For example if a thread
 * already holds a read lock, it can acquire the write lock and then
 * release the read lock. The result is that the read lock was changed
 * into a write lock.
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
:- use_package(foreign(jekpro/frequent/misc)).
:- use_package(foreign(matula/util/misc)).

:- module(lock, []).

/**
 * mutex_new(M):
 * The predicate succeeds for a new mutex M.
 */
% mutex_new(-Mutex)
:- public mutex_new/1.
:- foreign_constructor(mutex_new/1, 'Mutex', new).

/**
 * lock_aquire(L):
 * The predicate succeeds after locking the lock L.
 */
% lock_acquire(+Lock)
:- public lock_acquire/1.
:- virtual lock_acquire/1.
:- foreign(lock_acquire/1, 'AbstractLock', acquire).

/**
 * lock_attempt(L):
 * The predicate succeeds after locking the lock L.
 * Otherwise the predicate fails.
 */
% lock_attempt(+Lock)
:- public lock_attempt/1.
:- virtual lock_attempt/1.
:- foreign(lock_attempt/1, 'AbstractLock', attempt).

/**
 * lock_attempt(L, T):
 * The predicate succeeds after locking the lock L
 * in the timeout T. Otherwise the predicate fails.
 */
% lock_attempt(+Lock, +Integer)
:- public lock_attempt/2.
:- virtual lock_attempt/2.
:- foreign(lock_attempt/2, 'AbstractLock', attempt(long)).

/**
 * lock_release(L):
 * The predicate succeeds after unlocking the lock L.
 */
% lock_release(+AbstractLock)
:- public lock_release/1.
:- virtual lock_release/1.
:- foreign(lock_release/1, 'AbstractLock', release).

/**
 * readwrite_new(P):
 * The predicate succeeds for a new read write pair P.
 */
% readwrite_new(-ReadWrite)
:- public readwrite_new/1.
:- foreign_constructor(readwrite_new/1, 'ReadWrite', new).

/**
 * get_read(P, R):
 * The predicate succeeds for the read lock R of the read write pair P.
 */
:- public get_read/2.
:- virtual get_read/2.
:- foreign(get_read/2, 'ReadWrite', getRead).

/**
 * get_write(P, W):
 * The predicate succeeds for the write lock W of the read write pair P.
 */
:- public get_write/2.
:- virtual get_write/2.
:- foreign(get_write/2, 'ReadWrite', getWrite).
