package jekpro.platform.swing;

import jekpro.tools.call.ArrayEnumeration;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.TermAtomic;
import sun.management.VMManagement;

import java.lang.management.GarbageCollectorMXBean;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.lang.management.ThreadMXBean;
import java.lang.reflect.Field;
import java.util.Iterator;

/**
 * The foreign predicates for the module stats.
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
public final class ForeignStatistics {
    private final static String OP_STATISTIC_MAX = "max";
    private final static String OP_STATISTIC_USED = "used";
    private final static String OP_STATISTIC_FREE = "free";
    final static String OP_STATISTIC_UPTIME = "uptime";
    final static String OP_STATISTIC_GCTIME = "gctime";
    final static String OP_STATISTIC_TIME = "time";
    final static String OP_STATISTIC_WALL = "wall";

    private final static String[] OP_STATISTICS = {
            OP_STATISTIC_MAX,
            OP_STATISTIC_USED,
            OP_STATISTIC_FREE,
            OP_STATISTIC_UPTIME,
            OP_STATISTIC_GCTIME,
            OP_STATISTIC_TIME,
            OP_STATISTIC_WALL};

    /*********************************************************************/
    /* Foreigns                                                          */
    /*********************************************************************/

    /**
     * <p>Retrieve the known statistics keys.</p>
     *
     * @param co The call out.
     * @return The statistics key.
     */
    public static String sysCurrentStat(CallOut co) {
        ArrayEnumeration<String> dc;
        if (co.getFirst()) {
            dc = new ArrayEnumeration<String>(OP_STATISTICS);
            co.setData(dc);
        } else {
            dc = (ArrayEnumeration<String>) co.getData();
        }
        if (!dc.hasMoreElements())
            return null;
        String res = dc.nextElement();
        co.setRetry(dc.hasMoreElements());
        return res;
    }

    /**
     * <p>Retrieve a statistic.</p>
     * <p>Swing version.</p>
     *
     * @param name The statistics name.
     * @return The value, or null.
     * @throws InterpreterMessage Validation error.
     */
    public static Object sysGetStat(String name) throws InterpreterMessage {
        if (OP_STATISTIC_MAX.equals(name)) {
            return TermAtomic.normBigInteger(Runtime.getRuntime().maxMemory());
        } else if (OP_STATISTIC_USED.equals(name)) {
            return TermAtomic.normBigInteger(Runtime.getRuntime().totalMemory() -
                    Runtime.getRuntime().freeMemory());
        } else if (OP_STATISTIC_FREE.equals(name)) {
            return TermAtomic.normBigInteger(Runtime.getRuntime().freeMemory());
        } else if (OP_STATISTIC_UPTIME.equals(name)) {
            return TermAtomic.normBigInteger(ManagementFactory.getRuntimeMXBean().getUptime());
        } else if (OP_STATISTIC_GCTIME.equals(name)) {
            Iterator<GarbageCollectorMXBean> iter =
                    ManagementFactory.getGarbageCollectorMXBeans().iterator();
            long gcsum = 0;
            boolean has = false;
            while (iter.hasNext()) {
                GarbageCollectorMXBean gb = iter.next();
                long gctime = gb.getCollectionTime();
                if (gctime != -1) {
                    gcsum += gctime;
                    has = true;
                }
            }
            if (has) {
                return TermAtomic.normBigInteger(gcsum);
            } else {
                return null;
            }
        } else if (OP_STATISTIC_TIME.equals(name)) {
            ThreadMXBean tb = ManagementFactory.getThreadMXBean();
            if (tb.isThreadCpuTimeEnabled()) {
                long cputime = tb.getCurrentThreadCpuTime() / 1000000L;
                return TermAtomic.normBigInteger(cputime);
            } else {
                return null;
            }
        } else if (OP_STATISTIC_WALL.equals(name)) {
            return TermAtomic.normBigInteger(System.currentTimeMillis());
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    "prolog_flag", name));
        }
    }

    public static void main(String[] args)
            throws NoSuchFieldException, IllegalAccessException {
        RuntimeMXBean mxbean = ManagementFactory.getRuntimeMXBean();
        Field jvmField = mxbean.getClass().getDeclaredField("jvm");

        jvmField.setAccessible(true);
        VMManagement management = (VMManagement) jvmField.get(mxbean);

        System.out.println("VMManagement isThreadAllocatedMemorySupported()=" + management.isThreadAllocatedMemorySupported());
        System.out.println("VMManagement isThreadAllocatedMemoryEnabled()=" + management.isThreadAllocatedMemoryEnabled());
    }

}
