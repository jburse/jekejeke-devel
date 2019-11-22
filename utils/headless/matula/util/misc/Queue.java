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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class Queue<T> implements InterfacePipe<T> {
    private final ListArray<Object> list = new ListArray<Object>();
    private final int max;

    /**
     * <p>Create a queue.</p>
     *
     * @param m The maximum size, must be greater than zero.
     */
    public Queue(int m) {
        if (!(m > 0))
            throw new IndexOutOfBoundsException("maxsize underflow");
        max = m;
    }

    /**
     * <p>Enquee an element.</p>
     *
     * @param t The element.
     */
    private void equene(T t) {
        list.add(t);
        this.notifyAll();
    }

    /**
     * <p>Dequeue an element.</p>
     *
     * @return The element.
     */
    private T dequeue() {
        T t = (T) list.get(0);
        list.remove(0);
        this.notifyAll();
        return t;
    }

    /**
     * <p>Post an object.</p>
     * <p>Blocks if queue is full.</p>
     *
     * @param t The object, not null.
     * @throws InterruptedException If the request was cancelled.
     */
    public void put(T t)
            throws InterruptedException {
        if (t == null)
            throw new NullPointerException("null_element");
        synchronized (this) {
            while (list.size() >= max)
                this.wait();
            equene(t);
        }
    }

    /**
     * <p>Post an object.</p>
     * <p>Fails if queue is full.</p>
     *
     * @return True if object was posted, or false otherwise.
     */
    public boolean offer(T t) {
        if (t == null)
            throw new NullPointerException("null_element");
        synchronized (this) {
            if (list.size() < max) {
                equene(t);
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
    public boolean offer(T t, long sleep)
            throws InterruptedException {
        if (t == null)
            throw new NullPointerException("null_element");
        long when = System.currentTimeMillis() + sleep;
        synchronized (this) {
            for (; ; ) {
                if (list.size() < max) {
                    equene(t);
                    return true;
                } else if (sleep > 0) {
                    this.wait(sleep);
                    sleep = when - System.currentTimeMillis();
                } else {
                    return false;
                }
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
    public T take()
            throws InterruptedException {
        synchronized (this) {
            while (list.size() == 0)
                this.wait();
            return dequeue();
        }
    }

    /**
     * <p>Take an object.</p>
     * <p>Fails if queue is empty.</p>
     *
     * @return The object or null if no object was taken.
     */
    public T poll() {
        synchronized (this) {
            if (list.size() != 0) {
                return dequeue();
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
    public T poll(long sleep)
            throws InterruptedException {
        long when = System.currentTimeMillis() + sleep;
        synchronized (this) {
            for (; ; ) {
                if (list.size() != 0) {
                    return dequeue();
                } else if (sleep > 0) {
                    this.wait(sleep);
                    sleep = when - System.currentTimeMillis();
                } else {
                    return null;
                }
            }
        }
    }

}
