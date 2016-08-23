package matula.util.misc;

import matula.util.data.ListArray;

/**
 * <p>This class provides an unbounded alarm queue. The advantage over
 * the usual Java class DelayQueue is that we dont implement the leader
 * optimization. The advantage over the usual Java class PriorityQueue
 * is that we dont need a comparable or comparator. The unbounded alarm
 * queue is implemented by our class ListArray:
 * </p>
 * <pre>
 *      +----------+-----------------+------------+
 *      | table[0] |      ...        | table[n-1] |
 *      +----------+-----------------+------------+
 *      < --------------- size = n -------------- >
 * </pre>
 * <p>Scheduling inserts at the right position in our class ListArray.
 * Ties are broken in a first-in first-out fashion. Nexting removes from
 * the start of our class ListArray. Cancelling removes the desired element
 * if it is still scheduled. Each operation is synchronized, whereby the
 * unbounded alarm queue object itself is used for synchronization.
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
public final class Alarm {
    private final ListArray<AlarmEntry> planned = new ListArray<AlarmEntry>();

    /**
     * <p>Schedule an item on this alarm queue.</p>
     *
     * @param r     The item.
     * @param sleep The time-out.
     * @return The new alarm entry.
     */
    public AlarmEntry schedule(Object r, long sleep) {
        long when = System.currentTimeMillis() + sleep;
        AlarmEntry entry = new AlarmEntry(r, when);
        synchronized (this) {
            int i = 0;
            for (; i < planned.size(); i++) {
                AlarmEntry entry2 = planned.get(i);
                if (when < entry2.getWhen())
                    break;
            }
            planned.add(i, entry);
            this.notifyAll();
        }
        return entry;
    }

    /**
     * <p>Get the next item from this alarm queue.</p>
     *
     * @return The item.
     * @throws InterruptedException Thread was interrupted.
     */
    public Object next()
            throws InterruptedException {
        synchronized (this) {
            for (; ; ) {
                if (planned.size() == 0) {
                    this.wait();
                } else {
                    AlarmEntry entry = planned.get(0);
                    long sleep = entry.getWhen() - System.currentTimeMillis();
                    if (!(sleep > 0)) {
                        planned.remove(0);
                        return entry.getItem();
                    } else {
                        this.wait(sleep);
                    }
                }
            }
        }
    }

    /**
     * <p>Cancel an alarm entry from this alarm queue.</p>
     *
     * @param entry The alarm entry.
     */
    public void cancel(AlarmEntry entry) {
        synchronized (this) {
            planned.remove(entry);
        }
    }

    /**
     * <p>Perform a sleep.</p>
     *
     * @param sleep The time-out.
     * @throws InterruptedException
     */
    public static void sleep(long sleep)
            throws InterruptedException {
        long when = System.currentTimeMillis() + sleep;
        while (sleep > 0) {
            Thread.sleep(sleep);
            sleep = when - System.currentTimeMillis();
        }
    }

}
