package jekpro.platform.android;

import android.os.SystemClock;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.*;

/**
 * <p>Provides built-in predicates for statistics.</p>
 *
 * @author Copyright 2012-2017, XLOG Technologies GmbH, Switzerland
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
     * <p>Android version.</p>
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
            return TermAtomic.normBigInteger(SystemClock.uptimeMillis());
        } else if (OP_STATISTIC_GCTIME.equals(name)) {
            return null;
        } else if (OP_STATISTIC_TIME.equals(name)) {
            return TermAtomic.normBigInteger(SystemClock.currentThreadTimeMillis());
        } else if (OP_STATISTIC_PROCESSORS.equals(name)) {
            return Integer.valueOf(Runtime.getRuntime().availableProcessors());
        } else if (OP_STATISTIC_WALL.equals(name)) {
            return TermAtomic.normBigInteger(System.currentTimeMillis());
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    "prolog_flag", name));
        }
    }

}
