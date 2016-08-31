package matula.util.misc;

import matula.util.data.ListArray;

/**
 * <p>This class provides a bounded queue. The advantage over the
 * usual Java class ArrayBlockingQueue is that an empty bounded queue
 * doesn't occupy much space even if it has a large capacity. The
 * bounded queue is implemented by our class ListArray and a max
 * field that indicates the capacity of the bounded queue:
 * </p>
 * <pre>
 *      +----------+-----------------+------------+
 *      | table[0] |      ...        | table[n-1] |
 *      +----------+-----------------+------------+
 *      < --------------- size = n -------------- >
 *      < ----------------------- max -------------------------- >
 * </pre>
 * <p>Putting and offering appends to the end of our class ListArray.
 * Taking and polling removes from the start of our class ListArray.
 * So the bounded queue works in a first-in first-out manner. Each
 * operation is synchronized, whereby the bounded queue object itself
 * is used for synchronization.
 * </p>
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
public final class Queue extends AbstractPipe {
    private final ListArray<Object> list;
    private final int max;

    /**
     * <p>Create a queue.</p>
     *
     * @param m The maximum size, must be greater than zero.
     */
    public Queue(int m) {
        if (!(m > 0))
            throw new IndexOutOfBoundsException("maxsize underflow");
        list = new ListArray<Object>();
        max = m;
    }

    /**
     * <p>Post an object.</p>
     * <p>Blocks if queue is full.</p>
     *
     * @param t The object, not null.
     * @throws InterruptedException If the request was cancelled.
     */
    public void put(Object t)
            throws InterruptedException {
        if (t == null)
            throw new NullPointerException("null element");
        synchronized (this) {
            while (list.size() >= max)
                this.wait();
            list.add(t);
            this.notifyAll();
        }
    }

    /**
     * <p>Post an object.</p>
     * <p>Fails if queue is full.</p>
     *
     * @return True if object was posted, or false otherwise.
     */
    public boolean offer(Object t) {
        if (t == null)
            throw new NullPointerException("null element");
        synchronized (this) {
            if (list.size() < max) {
                list.add(t);
                return true;
            } else {
                return false;
            }
        }
    }

    /**
     * <p>Post an object or time-out.</p>
     *
     * @param t     The object, not null.
     * @param sleep The time-out.
     * @return True if object was posted, or false otherwise.
     */
    public boolean offer(Object t, long sleep)
            throws InterruptedException {
        if (t == null)
            throw new NullPointerException("null element");
        long when = System.currentTimeMillis() + sleep;
        synchronized (this) {
            while (list.size() >= max && sleep > 0) {
                this.wait(sleep);
                sleep = when - System.currentTimeMillis();
            }
            if (sleep > 0) {
                list.add(t);
                this.notifyAll();
                return true;
            } else {
                return false;
            }
        }
    }

    /**
     * <p>Take an object.</p>
     * <p>Blocks if queue is empty.</p>
     *
     * @return The object, not null.
     * @throws InterruptedException If the request was cancelled.
     */
    public Object take()
            throws InterruptedException {
        synchronized (this) {
            while (list.size() == 0)
                this.wait();
            Object t = list.get(0);
            list.remove(0);
            this.notifyAll();
            return t;
        }
    }

    /**
     * <p>Take an object.</p>
     * <p>Fails if queue is empty.</p>
     *
     * @return The object or null if no object was taken.
     */
    public Object poll() {
        synchronized (this) {
            if (list.size() != 0) {
                Object t = list.get(0);
                list.remove(0);
                return t;
            } else {
                return null;
            }
        }
    }

    /**
     * <p>Take an object or time-out.</p>
     *
     * @param sleep The time-out.
     * @return The object or null if no object was taken.
     * @throws InterruptedException If the request was cancelled.
     */
    public Object poll(long sleep)
            throws InterruptedException {
        long when = System.currentTimeMillis() + sleep;
        synchronized (this) {
            while (list.size() == 0 && sleep > 0) {
                this.wait(sleep);
                sleep = when - System.currentTimeMillis();
            }
            if (sleep > 0) {
                Object t = list.get(0);
                list.remove(0);
                this.notifyAll();
                return t;
            } else {
                return null;
            }
        }
    }

}
