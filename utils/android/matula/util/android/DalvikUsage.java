package matula.util.android;

import matula.util.data.ListArray;

/**
 * <p>Dalvik memory usage events.</p>
 * <p/>
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 * <p/>
 * Rights & License
 * All industrial property rights regarding the information - copyright§
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
public final class DalvikUsage {
    public static DalvikUsage DEFAULT = new DalvikUsage();

    private final ListArray<NotificationListener> listeners = new ListArray<NotificationListener>();

    /**
     * <p>Create a dalvik usage.</p>
     */
    private DalvikUsage() {
        /* do nothing */
    }

    /**
     * <p>Add a notification listener.</p>
     *
     * @param listener The listener.
     */
    public final void addListener(NotificationListener listener) {
        synchronized (listeners) {
            listeners.add(listener);
        }
    }

    /**
     * <p>Remove a notification listener.</p>
     *
     * @param listener The listener.
     */
    public final void removeListener(NotificationListener listener) {
        synchronized (listeners) {
            listeners.remove(listener);
        }
    }

    /**
     * <p>Fire the notification listeners.</p>
     */
    public final void fireListeners() {
        NotificationListener[] snapshot;
        synchronized (listeners) {
            snapshot = new NotificationListener[listeners.size()];
            listeners.toArray(snapshot);
        }
        for (int i = 0; i < snapshot.length; i++) {
            NotificationListener listener = snapshot[i];
            listener.handleNotification();
        }
    }

}