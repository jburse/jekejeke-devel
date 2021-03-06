package matula.util.wire;

/**
 * <p>This class provides an abstract livestock.</p>
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
public abstract class AbstractLivestock extends AbstractDomestic {
    private long mills;

    /**************************************************************/
    /* Variation Point                                            */
    /**************************************************************/

    /**
     * <p>Handle some event.</p>
     *
     * @param t The thread.
     * @param e The event.
     * @throws InterruptedException The current tread was interrupted.
     */
    public abstract void handleEvent(Thread t, LivestockEvent e)
            throws InterruptedException;

    /**
     * <p>Retrieve the offender score.</p>
     *
     * @return The offender score.
     */
    public abstract long getOffenderScore();

    /*****************************************************************/
    /* Fence Livecycle                                               */
    /*****************************************************************/

    /**
     * <p>Set the fence.</p>
     *
     * @param t The new thread or null.
     * @return The old thread or null.
     */
    public final Thread setFence(Thread t) {
        Thread s = thread;
        if (s == t)
            return s;
        if (s != null) {
            setThread(null);
            Fence.DEFAULT.removeLivestock(s);
        }
        if (t != null) {
            Fence.DEFAULT.addLivestock(t, this);
            setThread(t);
        }
        return s;
    }

    /**
     * <p>Retrieve the supervisor for a thread.</p>
     *
     * @param t The thread.
     * @return The supervisor or null.
     */
    public static AbstractLivestock currentLivestock(Thread t) {
        return Fence.DEFAULT.getLivestock(t);
    }

    /****************************************************************/
    /* Livestock Helper                                             */
    /****************************************************************/

    /**
     * <p>Retrieve the signal.</p>
     *
     * @param t The the thread.
     * @return The signal, can be null.
     */
    public static Throwable liveGetSignal(Thread t) {
        AbstractLivestock live = currentLivestock(t);
        if (live == null)
            return null;
        return live.signal;
    }

    /**
     * <p>Set the signal.</p>
     *
     * @param t The thread.
     * @param m The new signal, can be null.
     * @return the old signal, can be null.
     */
    public static Throwable liveSetSignal(Thread t, Throwable m) {
        AbstractLivestock live = currentLivestock(t);
        if (live == null)
            return null;
        return live.setSignal(m);
    }

    /**
     * <p>Set the interrupt mask.</p>
     *
     * @param t The thread.
     * @param m The new interrupt mask.
     * @return the old interrupt mask.
     */
    public static boolean liveSetMask(Thread t, boolean m) {
        AbstractLivestock live = currentLivestock(t);
        if (live == null)
            return false;
        return live.setMask(m);
    }

    /*********************************************************/
    /* Managed CPU Time                                      */
    /*********************************************************/

    /**
     * <p>Retrieve the children thread cpu.</p>
     *
     * @return The the children thread cpu.
     */
    public long getMillis() {
        return mills;
    }

    /**
     * <p>Add the children thread cpu.</p>
     *
     * @param c The children thread cpu.
     */
    public void addMillis(long c) {
        synchronized (this) {
            mills += c;
        }
    }

}
