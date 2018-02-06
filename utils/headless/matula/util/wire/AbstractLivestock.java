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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public abstract class AbstractLivestock extends AbstractDomestic {

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
    /* Java Foreign Function Helper                                 */
    /****************************************************************/

    /**
     * <p>Clear the signal.</p>
     *
     * @return The old signal, can be null.
     */
    public static Object sysThreadClear() {
        Thread t = Thread.currentThread();
        synchronized (t) {
            Object m = liveSetSignal(t, null);
            t.notifyAll();
            return m;
        }
    }

    /**
     * <p>Set the interrupt mask.</p>
     *
     * @param m The new interrupt mask.
     * @return The old interupt mask.
     */
    public static boolean sysThreadMask(boolean m) {
        Thread t = Thread.currentThread();
        return liveSetMask(t, m);
    }

    /****************************************************************/
    /* Livestock Helper                                             */
    /****************************************************************/

    /**
     * <p>Set the signal.</p>
     *
     * @param t The thread.
     * @param m The new signal, can be null.
     * @return the old signal, can be null.
     */
    private static Object liveSetSignal(Thread t, Object m) {
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
    private static boolean liveSetMask(Thread t, boolean m) {
        AbstractLivestock live = currentLivestock(t);
        if (live == null)
            return false;
        return live.setMask(m);
    }

}
