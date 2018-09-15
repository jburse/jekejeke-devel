package matula.util.misc;

/**
 * <p>This class provides a write lock object.</p>
 * <p/>
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 * <p/>
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 * <p/>
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 * <p/>
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
final class LockWrite extends AbstractLock {
    Thread locked;
    private final Lock parent;

    /**
     * <p>Create a write lock.</p>
     *
     * @param p The read write state.
     */
    LockWrite(Lock p) {
        parent = p;
    }

    /**
     * <p>Acquire the lock.</p>
     * <p>Blocks if lock is already held.</p>
     *
     * @throws InterruptedException If the request was cancelled.
     */
    public void acquire() throws InterruptedException {
        Thread thread = Thread.currentThread();
        synchronized (parent) {
            if (locked == thread)
                throw new IllegalStateException("alread_locked");
            while (parent.read.otherReaders(thread) != 0 ||
                    locked != null)
                parent.wait();
            locked = thread;
        }
    }

    /**
     * <p>Attempt the lock.</p>
     * <p>Fails if lock is already held.</p>
     *
     * @return True if lock was acquired, or false otherwise.
     */
    public boolean attempt() {
        Thread thread = Thread.currentThread();
        synchronized (parent) {
            if (locked == thread)
                throw new IllegalStateException("alread_locked");
            if (parent.read.otherReaders(thread) == 0 &&
                    locked == null) {
                locked = thread;
                return true;
            } else {
                return false;
            }
        }
    }

    /**
     * <p>Acquire the write lock or time-out.</p>
     *
     * @param sleep The time-out.
     * @return True if lock was acquired, or false otherwise.
     * @throws InterruptedException If the request was cancelled.
     */
    public boolean attempt(long sleep) throws InterruptedException {
        Thread thread = Thread.currentThread();
        long when = System.currentTimeMillis() + sleep;
        synchronized (parent) {
            if (locked == thread)
                throw new IllegalStateException("alread_locked");
            while ((parent.read.otherReaders(thread) != 0 ||
                    locked != null) && sleep > 0) {
                parent.wait(sleep);
                sleep = when - System.currentTimeMillis();
            }
            if (sleep > 0) {
                locked = thread;
                return true;
            } else {
                return false;
            }
        }
    }

    /**
     * <p>Release the write lock.</p>
     */
    public void release() {
        Thread thread = Thread.currentThread();
        synchronized (parent) {
            if (locked != thread)
                throw new IllegalStateException("not_locked");
            locked = null;
            parent.notifyAll();
        }
    }

    /**
     * <p>Determine wether there is another writer.</p>
     *
     * @param thread The current thread.
     * @return True if there is another writer, otherwise false.
     */
    boolean otherWriter(Thread thread) {
        return (locked != null && locked != thread);
    }

}
