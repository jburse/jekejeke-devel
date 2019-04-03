package matula.util.misc;

import java.util.concurrent.TimeUnit;

/**
 * <p>This class provides an unslotted mutex object.</p>
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
public final class Unslotted extends AbstractLock {
    private boolean locked;

    /**
     * <p>Acquire the lock.</p>
     * <p>Blocks if lock is already held.</p>
     *
     * @throws InterruptedException If the request was cancelled.
     */
    public void lockInterruptibly()
            throws InterruptedException {
        synchronized (this) {
            while (locked)
                this.wait();
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
        synchronized (this) {
            if (!locked) {
                locked = true;
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
     * @param tu  The time unit.
     * @return True if lock was acquired, or false otherwise.
     * @throws InterruptedException If the request was cancelled.
     */
    public boolean tryLock(long sleep, TimeUnit tu)
            throws InterruptedException {
        sleep = tu.toMillis(sleep);
        long when = System.currentTimeMillis() + sleep;
        synchronized (this) {
            while (!locked && sleep > 0) {
                this.wait(sleep);
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
     * <p>Release the lock.</p>
     */
    public void unlock() {
        synchronized (this) {
            if (!locked)
                throw new IllegalStateException("not_locked");
            locked = false;
            this.notifyAll();
        }
    }

}
