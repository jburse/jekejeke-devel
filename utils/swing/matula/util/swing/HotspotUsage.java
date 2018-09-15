package matula.util.swing;

import javax.management.ListenerNotFoundException;
import javax.management.NotificationEmitter;
import javax.management.NotificationListener;
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryPoolMXBean;
import java.util.HashMap;
import java.util.Iterator;

/**
 * <p>Hotspot memory usage events.</p>
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
public final class HotspotUsage {
    public static HotspotUsage DEFAULT = new HotspotUsage();

    private final HashMap<String, Long> thresholds = new HashMap<String, Long>();
    private int count;

    /**
     * <p>Create a hotspot usage.</p>
     */
    private HotspotUsage() {
        /* do nothing */
    }

    /**
     * <p>Add a notification listener.</p>
     * <p>If this is the first listener save and init the threshold.</p>
     *
     * @param listener The listener.
     */
    public final void addListener(NotificationListener listener) {
        synchronized (thresholds) {
            if (count == 0) {
                Iterator<MemoryPoolMXBean> pool = ManagementFactory.getMemoryPoolMXBeans().iterator();
                while (pool.hasNext()) {
                    MemoryPoolMXBean pbean = pool.next();
                    if (pbean.isUsageThresholdSupported()) {
                        long threshold = pbean.getUsageThreshold();
                        thresholds.put(pbean.getName(), Long.valueOf(threshold));
                        threshold = pbean.getUsage().getMax() * 85 / 100;
                        pbean.setUsageThreshold(threshold);
                    }
                }
            }
            count++;
        }
        NotificationEmitter emitter = (NotificationEmitter) ManagementFactory.getMemoryMXBean();
        emitter.addNotificationListener(listener, null, null);
    }

    /**
     * <p>Remove a notificatio listener.</p>
     * <p>If this is the last listener restore the threshold.</p>
     *
     * @param listener The listener.
     */
    public final void removeListener(NotificationListener listener) {
        NotificationEmitter emitter = (NotificationEmitter) ManagementFactory.getMemoryMXBean();
        try {
            emitter.removeNotificationListener(listener, null, null);
        } catch (ListenerNotFoundException e) {
            throw new RuntimeException(e);
        }
        synchronized (thresholds) {
            count--;
            if (count == 0) {
                Iterator<MemoryPoolMXBean> pool = ManagementFactory.getMemoryPoolMXBeans().iterator();
                while (pool.hasNext()) {
                    MemoryPoolMXBean pbean = pool.next();
                    if (pbean.isUsageThresholdSupported()) {
                        long threshold = thresholds.get(pbean.getName()).longValue();
                        pbean.setUsageThreshold(threshold);
                    }
                }
            }
        }
    }

}
