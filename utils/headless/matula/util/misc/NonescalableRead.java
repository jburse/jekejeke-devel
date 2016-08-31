package matula.util.misc;

/**
 * <p>This class provides a unslotted and non-escalable read lock object.</p>
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
public final class NonescalableRead extends AbstractLock {
    int set;
    private final Nonescalable parent;

    /**
     * <p>Create a read lock.</p>
     *
     * @param p The read write state.
     */
    NonescalableRead(Nonescalable p) {
        parent = p;
    }

    /**
     * <p>Acquire the lock.</p>
     * <p>Blocks if lock is already held.</p>
     *
     * @throws InterruptedException If the request was cancelled.
     */
    public void acquire() throws InterruptedException {
        synchronized (parent) {
            while (parent.write.locked)
                parent.wait();
            set++;
        }
    }

    /**
     * <p>Attempt the lock.</p>
     * <p>Fails if lock is already held.</p>
     *
     * @return True if lock was acquired, or false otherwise.
     */
    public boolean attempt() {
        synchronized (parent) {
            if (!parent.write.locked) {
                set++;
                return true;
            } else {
                return false;
            }
        }
    }

    /**
     * <p>Acquire the read lock or time-out.</p>
     *
     * @param sleep The time-out.
     * @return True if lock was acquired, or false otherwise.
     * @throws InterruptedException If the request was cancelled.
     */
    public boolean attempt(long sleep) throws InterruptedException {
        long when = System.currentTimeMillis() + sleep;
        synchronized (parent) {
            while (parent.write.locked && sleep > 0) {
                parent.wait(sleep);
                sleep = when - System.currentTimeMillis();
            }
            if (sleep > 0) {
                set++;
                return true;
            } else {
                return false;
            }
        }
    }

    /**
     * <p>Release a read lock.</p>
     *
     * @throws IllegalStateException If the write lock was not yet acquired.
     */
    public void release() throws IllegalStateException {
        synchronized (parent) {
            set--;
            parent.notifyAll();
        }
    }

}
