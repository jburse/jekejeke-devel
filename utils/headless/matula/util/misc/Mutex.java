package matula.util.misc;

/**
 * <p>This class provides a slotted mutex object.</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class Mutex extends AbstractLock {
    private Thread locked;

    /**
     * <p>Acquire the lock.</p>
     * <p>Blocks if lock is already held.</p>
     *
     * @throws InterruptedException If the request was cancelled.
     */
    public void acquire()
            throws InterruptedException {
        Thread thread = Thread.currentThread();
        synchronized (this) {
            if (locked == thread)
                throw new IllegalStateException("alread_locked");
            while (locked != null)
                this.wait();
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
        synchronized (this) {
            if (locked == thread)
                throw new IllegalStateException("alread_locked");
            if (locked == null) {
                locked = thread;
                return true;
            } else {
                return false;
            }
        }
    }

    /**
     * <p>Acquire the lock or time-out.</p>
     *
     * @param sleep The time-out.
     * @return True if lock was acquired, or false otherwise.
     * @throws InterruptedException If the request was cancelled.
     */
    public boolean attempt(long sleep)
            throws InterruptedException {
        Thread thread = Thread.currentThread();
        long when = System.currentTimeMillis() + sleep;
        synchronized (this) {
            if (locked == thread)
                throw new IllegalStateException("alread_locked");
            while (locked != null && sleep > 0) {
                this.wait(sleep);
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
     * <p>Release the lock.</p>
     */
    public void release() {
        Thread thread = Thread.currentThread();
        synchronized (this) {
            if (locked != thread)
                throw new IllegalStateException("not_locked");
            locked = null;
            this.notifyAll();
        }
    }

}
