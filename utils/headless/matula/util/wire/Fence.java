package matula.util.wire;

import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;

/**
 * <p>This class provides a fence.</p>
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
public final class Fence {
    public static Fence DEFAULT = new Fence();

    private final MapHashLink<Thread, AbstractLivestock> active = new MapHashLink<Thread, AbstractLivestock>();
    private MapEntry<Thread, AbstractLivestock>[] cacheactive;

    /**
     * <p>Create a fence.</p>
     */
    private Fence() {
    }

    /**********************************************************/
    /* Association Handling                                   */
    /**********************************************************/

    /**
     * <p>Retrieve an enforced listener.</p>
     *
     * @param t The thread.
     * @return The enforced listener.
     */
    public AbstractLivestock getLivestock(Thread t) {
        synchronized (this) {
            return active.get(t);
        }
    }

    /**
     * <p>Add an enforced listener.</p>
     *
     * @param t The thread.
     * @param l The new enforced listener.
     */
    public void addLivestock(Thread t, AbstractLivestock l) {
        synchronized (this) {
            if (active.get(t) != null)
                throw new IllegalArgumentException("already added");
            active.add(t, l);
            cacheactive = null;
        }
    }

    /**
     * <p>Remove an enforced listener.</p>
     *
     * @param t The thread.
     */
    public void removeLivestock(Thread t) {
        synchronized (this) {
            if (active.get(t) == null)
                throw new IllegalArgumentException("already removed");
            active.remove(t);
            cacheactive = null;
            this.notifyAll();
        }
    }

    /**
     * <p>Retrieve a snapshot of the enforced listeners.</p>
     *
     * @return The enforced listeners.
     */
    public MapEntry<Thread, AbstractLivestock>[] snapshotLivestocks() {
        MapEntry<Thread, AbstractLivestock>[] res = cacheactive;
        if (res != null)
            return res;
        synchronized (this) {
            res = cacheactive;
            if (res != null)
                return res;
            res = new MapEntry[active.size];
            active.toArray(res);
            cacheactive = res;
        }
        return res;
    }

    /**********************************************************/
    /* Event Handling                                         */
    /**********************************************************/

    /**
     * <p>Fire a livestock event to a snaphot.</p>
     *
     * @param e The enforced event.
     * @throws InterruptedException The current tread was interrupted.
     */
    public void fireEvent(LivestockEvent e)
            throws InterruptedException {
        MapEntry<Thread, AbstractLivestock>[] listeners = snapshotLivestocks();
        for (int i = 0; i < listeners.length; i++) {
            MapEntry<Thread, AbstractLivestock> entry = listeners[i];
            if (entry.value.source != e.getSource())
                continue;
            entry.value.handleEvent(entry.key, e);
        }
    }

    /**
     * <p>Wait that there are no more threads for a source.</p>
     *
     * @param e The source.
     * @throws InterruptedException Shit happens.
     */
    public void waitInactive(Object e)
            throws InterruptedException {
        synchronized (this) {
            while (hasActive(e))
                this.wait();
        }
    }

    /**
     * <p>Wait that there are no more threads for a source or time-out.</p>
     *
     * @param e The source.
     * @throws InterruptedException Shit happens.
     */
    public boolean waitInactive(Object e, long sleep)
            throws InterruptedException {
        long when = System.currentTimeMillis() + sleep;
        synchronized (this) {
            while (hasActive(e) && sleep > 0) {
                this.wait(sleep);
                sleep = when - System.currentTimeMillis();
            }
            return (sleep > 0);
        }
    }

    /**
     * <p>Check whether a source has active,.</p>
     *
     * @param e The source.
     * @return True if the source has active, otherwise false.
     */
    private boolean hasActive(Object e) {
        MapEntry<Thread, AbstractLivestock>[] listeners = snapshotLivestocks();
        for (int i = 0; i < listeners.length; i++) {
            MapEntry<Thread, AbstractLivestock> entry = listeners[i];
            if (entry.value.source == e)
                return true;
        }
        return false;
    }

    /**
     * <p>Fire a livestock event to the worst offender.</p>
     *
     * @param e The enforced event.
     * @throws InterruptedException The current tread was interrupted.
     */
    public void fireEventWorstOffender(LivestockEvent e)
            throws InterruptedException {
        MapEntry<Thread, AbstractLivestock> worst = null;
        long top = 0;
        MapEntry<Thread, AbstractLivestock>[] listeners = snapshotLivestocks();
        for (int i = 0; i < listeners.length; i++) {
            MapEntry<Thread, AbstractLivestock> entry = listeners[i];
            if (entry.value.source != e.getSource())
                continue;
            long score = entry.value.getOffenderScore();
            if (worst == null || top < score) {
                worst = entry;
                top = score;
            }
        }
        if (worst != null)
            worst.value.handleEvent(worst.key, e);
    }

}
