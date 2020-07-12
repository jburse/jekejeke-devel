package matula.util.misc;

import matula.util.data.SetEntry;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * <p>This class provides an unbounded queue.</p>
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
public final class Unbounded<T> implements InterfacePipe<T> {
    private final SetLink<T> list = new SetLink<T>();
    private final Lock lock = new ReentrantLock();
    private final Condition cond = lock.newCondition();

    /*************************************************************/
    /* Recurring Helper                                          */
    /*************************************************************/

    /**
     * <p>Dequeue an element.</p>
     *
     * @return The element.
     */
    private T dequeue() {
        SetEntry<T> entry = list.getFirstEntry();
        list.removeEntry(entry);
        return entry.value;
    }

    /*************************************************************/
    /* Interface Pipe                                            */
    /*************************************************************/

    /**
     * <p>Post an object.</p>
     *
     * @param t The object, not null.
     */
    public void put(T t) {
        if (t == null)
            throw new NullPointerException("null_element");
        lock.lock();
        try {
            list.add(t);
            cond.signal();
        } finally {
            lock.unlock();
        }
    }

    /**
     * <p>Post an object.</p>
     * <p>Fails if queue is full.</p>
     *
     * @return True if object was posted, or false otherwise.
     */
    public boolean offer(T t) {
        put(t);
        return true;
    }

    /**
     * <p>Post an object or time-out.</p>
     * <p>Fails if pipe is still full after time-out.</p>
     *
     * @param t     The object, not null.
     * @param sleep The time-out.
     * @param unit  The time unit.
     * @return True if object was posted, or false otherwise.
     */
    public boolean offer(T t, long sleep, TimeUnit unit)
            throws InterruptedException {
        put(t);
        return true;
    }

    /**
     * <p>Take an object.</p>
     * <p>Blocks if queue is empty.</p>
     *
     * @return The object, not null.
     * @throws InterruptedException If the request was cancelled.
     */
    public T take()
            throws InterruptedException {
        lock.lock();
        try {
            while (list.size() == 0)
                cond.await();
            return dequeue();
        } finally {
            lock.unlock();
        }
    }

    /**
     * <p>Take an object.</p>
     * <p>Fails if queue is empty.</p>
     *
     * @return The object or null if no object was taken.
     */
    public T poll() {
        lock.lock();
        try {
            if (list.size() != 0) {
                return dequeue();
            } else {
                return null;
            }
        } finally {
            lock.unlock();
        }
    }

    /**
     * <p>Take an object or time-out.</p>
     *
     * @param sleep The time-out.
     * @return The object or null if no object was taken.
     * @throws InterruptedException If the request was cancelled.
     */
    public T poll(long sleep, TimeUnit unit)
            throws InterruptedException {
        long when = System.nanoTime() + unit.toNanos(sleep);
        lock.lock();
        try {
            for (; ; ) {
                if (list.size() != 0) {
                    return dequeue();
                } else {
                    sleep = when - System.nanoTime();
                    if (sleep > 0) {
                        cond.awaitNanos(sleep);
                    } else {
                        return null;
                    }
                }
            }
        } finally {
            lock.unlock();
        }
    }

}
