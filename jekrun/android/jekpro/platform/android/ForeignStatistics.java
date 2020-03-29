package jekpro.platform.android;

import android.os.Debug;
import android.os.SystemClock;
import jekpro.model.inter.Supervisor;
import jekpro.tools.call.*;
import jekpro.tools.term.TermAtomic;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

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
    private final static String OP_MAX = "max";
    private final static String OP_USED = "used";
    private final static String OP_FREE = "free";
    final static String OP_UPTIME = "uptime";
    final static String OP_GCTIME = "gctime";
    final static String OP_TIME = "time";
    final static String OP_SYS_TIME_SELF = "sys_time_self";
    final static String OP_WALL = "wall";

    private static Method getRuntimeStat;

    private final static String[] OP_STATISTICS = {
            OP_MAX,
            OP_USED,
            OP_FREE,
            OP_UPTIME,
            OP_GCTIME,
            OP_TIME,
            OP_WALL};

    private final static String OP_SYS_LOCAL_CLAUSES = "sys_local_clauses";
    private final static String OP_SYS_TIME_MANAGED = "sys_time_managed";

    private final static String[] OP_THREAD_STATISTICS = {
            OP_SYS_LOCAL_CLAUSES,
            OP_SYS_TIME_MANAGED};

    static {
        try {
            Class<?> debugClass = Debug.class;
            getRuntimeStat = debugClass.getDeclaredMethod("getRuntimeStat", String.class);
        } catch (NoSuchMethodException x) {
            getRuntimeStat = null;
        }
    }

    /*********************************************************************/
    /* Statistics                                                        */
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
     * <p>Android version.</p>
     *
     * @param inter The interpreter.
     * @param name  The statistics name.
     * @return The value, or null.
     * @throws InterpreterMessage Validation error.
     */
    public static Object sysGetStat(Interpreter inter, String name)
            throws InterpreterMessage {
        if (OP_MAX.equals(name)) {
            return TermAtomic.normBigInteger(Runtime.getRuntime().maxMemory());
        } else if (OP_USED.equals(name)) {
            return TermAtomic.normBigInteger(Runtime.getRuntime().totalMemory() -
                    Runtime.getRuntime().freeMemory());
        } else if (OP_FREE.equals(name)) {
            return TermAtomic.normBigInteger(Runtime.getRuntime().freeMemory());
        } else if (OP_UPTIME.equals(name)) {
            return TermAtomic.normBigInteger(SystemClock.uptimeMillis());
        } else if (OP_GCTIME.equals(name)) {
            if (getRuntimeStat == null)
                return null;
            String gctimestr;
            try {
                gctimestr = (String) getRuntimeStat.invoke(null, "art.gc.gc-time");
            } catch (IllegalAccessException e) {
                gctimestr = null;
            } catch (InvocationTargetException e) {
                gctimestr = null;
            }
            if (gctimestr != null) {
                long gctime = Long.parseLong(gctimestr);
                return TermAtomic.normBigInteger(gctime);
            } else {
                return null;
            }
        } else if (OP_SYS_TIME_SELF.equals(name)) {
            return TermAtomic.normBigInteger(SystemClock.currentThreadTimeMillis());
        } else if (OP_SYS_TIME_MANAGED.equals(name)) {
            Supervisor s = (Supervisor) inter.getController().getVisor();
            return TermAtomic.normBigInteger(s.getMillis());
        } else if (OP_WALL.equals(name)) {
            return TermAtomic.normBigInteger(System.currentTimeMillis());
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    "prolog_flag", name));
        }
    }

    /**
     * <p>Add a term integer to another term integer.</p>
     * <p>Uses long addition, if both are not null.</p>
     *
     * @param i1 The first term integer, or null.
     * @param i2 The second term integer, or null.
     * @return The result term integer, or null.
     */
    private static Number add(Number i1, Number i2) {
        if (i1 != null) {
            if (i2 != null) {
                return TermAtomic.normBigInteger(i1.longValue() + i2.longValue());
            } else {
                return i1;
            }
        } else {
            return i2;
        }
    }

    /*********************************************************************/
    /* Thread Statistics                                                 */
    /*********************************************************************/

    /**
     * <p>Retrieve the known thread statistics names.</p>
     *
     * @param co The call out.
     * @return The thread statistics name.
     */
    public static String sysCurrentThreadStat(CallOut co) {
        ArrayEnumeration<String> dc;
        if (co.getFirst()) {
            dc = new ArrayEnumeration<String>(OP_THREAD_STATISTICS);
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
     * <p>Retrieve a thread statistic.</p>
     *
     * @param name The thread statistics name.
     * @return The value, or null.
     * @throws InterpreterMessage Validation error.
     */
    public static Object sysGetThreadStat(Thread t, String name)
            throws InterpreterMessage {
        if (OP_SYS_LOCAL_CLAUSES.equals(name)) {
            Controller contr = Controller.currentController(t);
            if (contr != null) {
                long total = contr.getThreadLocalClauses();
                return TermAtomic.normBigInteger(total);
            } else {
                return Integer.valueOf(0);
            }
        } else if (OP_SYS_TIME_MANAGED.equals(name)) {
            Controller contr = Controller.currentController(t);
            if (contr != null) {
                Supervisor s = (Supervisor) contr.getVisor();
                return TermAtomic.normBigInteger(s.getMillis());
            } else {
                return Integer.valueOf(0);
            }
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    "prolog_flag", name));
        }
    }

    /****************************************************************/
    /* Thread Managed                                               */
    /****************************************************************/

    /**
     * <p>Add CPU time to managed.</p>
     *
     * @param t The thread.
     * @param n The CPU time.
     */
    public static void sysManagedAdd(Thread t, Number n) {
        Controller contr = Controller.currentController(t);
        if (contr != null) {
            Supervisor s = (Supervisor) contr.getVisor();
            s.addMillis(n.longValue());
        }
    }

}
