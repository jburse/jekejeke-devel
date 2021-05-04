package matula.util.android;

import matula.util.data.AssocArray;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class DalvikUsage {
    public static DalvikUsage DEFAULT = new DalvikUsage();

    private final AssocArray<NotificationListener, Object> listeners
            = new AssocArray<NotificationListener, Object>();

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
     * @param handback The handback.
     */
    public final void addListener(NotificationListener listener, Object handback) {
        synchronized (listeners) {
            listeners.add(listener, handback);
        }
    }

    /**
     * <p>Remove a notification listener.</p>
     *
     * @param listener The listener.
     */
    public final void removeListener(NotificationListener listener, Object handback) {
        synchronized (listeners) {
            for (int i = 0; i < listeners.size(); i++) {
                if (listeners.getKey(i) != listener)
                    continue;
                if (listeners.getValue(i) != handback)
                    continue;
                listeners.remove(i);
                return;
            }
        }
    }

    /**
     * <p>Fire the notification listeners.</p>
     */
    public final void fireListeners() {
        NotificationListener[] snapshot;
        Object[] snapshot2;
        synchronized (listeners) {
            snapshot = new NotificationListener[listeners.size()];
            snapshot2 = new Object[listeners.size()];
            listeners.toArray(snapshot, snapshot2);
        }
        for (int i = 0; i < snapshot.length; i++) {
            NotificationListener listener = snapshot[i];
            Object handback = snapshot2[i];
            listener.handleNotification(handback);
        }
    }

}