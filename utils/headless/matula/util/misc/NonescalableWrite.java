package matula.util.misc;

import java.util.concurrent.TimeUnit;

/**
 * <p>This class provides a unslotted and non-escalable write lock object.</p>
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
public final class NonescalableWrite extends AbstractLock {
    boolean locked;
    private final Nonescalable parent;

    /**
     * <p>Create a write lock.</p>
     *
     * @param p The read write state.
     */
    NonescalableWrite(Nonescalable p) {
        parent = p;
    }

    /**
     * <p>Acquire the lock.</p>
     * <p>Blocks if lock is already held.</p>
     *
     * @throws InterruptedException If the request was cancelled.
     */
    public void lockInterruptibly() throws InterruptedException {
        synchronized (parent) {
            while (parent.read.set != 0 || locked)
                parent.wait();
            locked = true;
        }
    }

    /**
     * <p>Attempt the lock.</p>
     * <p>Fails if lock is already held.</p>
     *
     * @return True if lock was acquired, or false otherwise.
     */
    public boolean tryLock() {
        synchronized (parent) {
            if (parent.read.set == 0 && !locked) {
                locked = true;
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
     * @param tu  The time unit.
     * @return True if lock was acquired, or false otherwise.
     * @throws InterruptedException If the request was cancelled.
     */
    public boolean tryLock(long sleep, TimeUnit tu)
            throws InterruptedException {
        sleep = tu.toMillis(sleep);
        long when = System.currentTimeMillis() + sleep;
        synchronized (parent) {
            while ((parent.read.set != 0 || locked) && sleep > 0) {
                parent.wait(sleep);
                sleep = when - System.currentTimeMillis();
            }
            if (sleep > 0) {
                locked = true;
                return true;
            } else {
                return false;
            }
        }
    }

    /**
     * <p>Release the write lock.</p>
     */
    public void unlock() {
        synchronized (parent) {
            if (!locked)
                throw new IllegalStateException("not_locked");
            locked = false;
            parent.notifyAll();
        }
    }

}
