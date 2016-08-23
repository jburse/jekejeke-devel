package jekpro.platform.swing;

import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.*;

import java.lang.management.GarbageCollectorMXBean;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.util.Iterator;

/**
 * <p>Provides built-in predicates for statistics.</p>
 *
 * @author Copyright 2012-2015, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.3 (a fast and small prolog interpreter)
 */
public final class ForeignStatistics {
    private final static String OP_STATISTIC_RUNTIME = "runtime";
    private final static String OP_STATISTIC_MAX = "max";
    private final static String OP_STATISTIC_USED = "used";
    private final static String OP_STATISTIC_FREE = "free";
    final static String OP_STATISTIC_UPTIME = "uptime";
    final static String OP_STATISTIC_GCTIME = "gctime";
    final static String OP_STATISTIC_TIME = "time";
    private final static String OP_STATISTIC_PROCESSORS = "processors";
    final static String OP_STATISTIC_WALL = "wall";

    private final static String[] OP_STATISTICS = {
            OP_STATISTIC_RUNTIME,
            OP_STATISTIC_MAX,
            OP_STATISTIC_USED,
            OP_STATISTIC_FREE,
            OP_STATISTIC_UPTIME,
            OP_STATISTIC_GCTIME,
            OP_STATISTIC_TIME,
            OP_STATISTIC_PROCESSORS,
            OP_STATISTIC_WALL};

    /*********************************************************************/
    /* Foreigns                                                          */
    /*********************************************************************/

    /**
     * <p>Retrieve the known statistics keys.</p>
     *
     * @return The known statistics keys.
     * @throws InterpreterMessage Validation error.
     */
    public static Object sysListStats()
            throws InterpreterMessage {
        Object res = Knowledgebase.OP_NIL;
        for (int i = OP_STATISTICS.length - 1; i >= 0; i--) {
            res = new TermCompound(Knowledgebase.OP_CONS,
                    OP_STATISTICS[i], res);
        }
        return res;
    }

    /**
     * <p>Retrieve a statistic.</p>
     * <p>Swing version.</p>
     *
     * @param name The name.
     * @return The value, or null.
     * @throws InterpreterMessage Validation error.
     */
    public static Object sysGetStat(String name) throws InterpreterMessage {
        if (OP_STATISTIC_RUNTIME.equals(name)) {
            return System.getProperty("java.vm.specification.version");
        } else if (OP_STATISTIC_MAX.equals(name)) {
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
                return TermAtomic.normBigInteger(tb.getCurrentThreadCpuTime() / 1000000L);
            } else {
                return null;
            }
        } else if (OP_STATISTIC_PROCESSORS.equals(name)) {
            return Integer.valueOf(Runtime.getRuntime().availableProcessors());
        } else if (OP_STATISTIC_WALL.equals(name)) {
            return TermAtomic.normBigInteger(System.currentTimeMillis());
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    "prolog_flag", name));
        }
    }

    /**
     * <p>Create a time record.</p>
     *
     * @return The time record.
     */
    public static Object sysMakeTimeRecord() {
        return new TimeRecord();
    }

    /**
     * <p>Start time record measurement.</p>
     *
     * @param obj The time record.
     * @throws InterpreterMessage Not a record.
     */
    public static void sysStartTimeRecord(Object obj)
            throws InterpreterMessage {
        checkRecord(obj);
        ((TimeRecord) obj).start();
    }

    /**
     * <p>End time record measurement.</p>
     *
     * @param obj The time record.
     * @throws InterpreterMessage Not a record.
     */
    public static void sysEndTimeRecord(Object obj)
            throws InterpreterMessage {
        checkRecord(obj);
        ((TimeRecord) obj).end();
    }

    /**
     * <p>Retrieve the known time record statistics keys.</p>
     *
     * @return The known time record statistics keys.
     */
    public static Object sysListRecordStats() {
        Object res = Knowledgebase.OP_NIL;
        String[] stats = TimeRecord.listStats();
        for (int i = stats.length - 1; i >= 0; i--) {
            res = new TermCompound(Knowledgebase.OP_CONS,
                    stats[i], res);
        }
        return res;
    }

    /**
     * <p>Retrieve a statistic.</p>
     * <p>Android version.</p>
     *
     * @param name The name.
     * @return The value, or null.
     * @throws InterpreterMessage Shit happens.
     */
    public static Object sysGetRecordStat(Object obj, String name)
            throws InterpreterMessage {
        checkRecord(obj);
        return ((TimeRecord) obj).getStat(name);
    }

    /****************************************************************/
    /* Validation Helper                                            */
    /****************************************************************/

    /**
     * <p>Check whether the object is a record.</p>
     *
     * @param obj The object.
     * @throws InterpreterMessage Not a record.
     */
    private static void checkRecord(Object obj)
            throws InterpreterMessage {
        if (!(obj instanceof TimeRecord)) {
            throw new InterpreterMessage(
                    InterpreterMessage.permissionError("measure",
                            "record", obj));
        }
    }

}
