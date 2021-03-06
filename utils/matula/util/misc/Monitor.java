package matula.util.misc;

import java.util.Date;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;

/**
 * <p>The class provides a lock and condition combo.</p>
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
public final class Monitor implements Lock, Condition {
    private final Lock lock;
    private final Condition cond;

    /**
     * <p>Create a new monitor.</p>
     *
     * @param l The lock.
     * @param c The condition.
     */
    public Monitor(Lock l, Condition c) {
        lock = l;
        cond = c;
    }

    public void lock() {
        lock.lock();
    }

    public void lockInterruptibly() throws InterruptedException {
        lock.lockInterruptibly();
    }

    public boolean tryLock() {
        return lock.tryLock();
    }

    public boolean tryLock(long time, TimeUnit unit) throws InterruptedException {
        return lock.tryLock(time, unit);
    }

    public void unlock() {
        lock.unlock();
    }

    public Condition newCondition() {
        return lock.newCondition();
    }

    public void await() throws InterruptedException {
        cond.await();
    }

    public void awaitUninterruptibly() {
        cond.awaitUninterruptibly();
    }

    public long awaitNanos(long nanosTimeout) throws InterruptedException {
        return cond.awaitNanos(nanosTimeout);
    }

    public boolean await(long time, TimeUnit unit) throws InterruptedException {
        return cond.await(time, unit);
    }

    public boolean awaitUntil(Date deadline) throws InterruptedException {
        return cond.awaitUntil(deadline);
    }

    public void signal() {
        cond.signal();
    }

    public void signalAll() {
        cond.signalAll();
    }

}
