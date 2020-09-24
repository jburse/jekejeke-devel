package jekpro.platform.swing;

import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.tools.call.*;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermAtomic;
import matula.util.config.ArrayEnumeration;

/**
 * The value object for the module stats.
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
public final class TimeRecord {
    private Number uptime;
    private Number gctime;
    private Number time;

    private Number uptime1;
    private Number gctime1;
    private Number time1;

    private final static String[] OP_STATISTICS = {
            ForeignStatistics.OP_UPTIME,
            ForeignStatistics.OP_GCTIME,
            ForeignStatistics.OP_TIME,
            ForeignStatistics.OP_WALL};

    private final static String[] OP_STATISTICS_WEB = {
            ForeignStatistics.OP_UPTIME,
            ForeignStatistics.OP_WALL};

    /**
     * <p>Create a time record.</p>
     *
     * @param u The up time.
     * @param g The gc time.
     * @param t The threads time.
     */
    public TimeRecord(Number u, Number g, Number t) {
        uptime1 = u;
        gctime1 = g;
        time1 = t;
    }

    /**
     * <p>End time record measurement.</p>
     *
     * @param u The up time.
     * @param g The gc time.
     * @param t The threads time.
     */
    public void sysMeasure(Number u, Number g, Number t) {
        uptime = uptime1;
        gctime = gctime1;
        time = time1;

        uptime1 = u;
        gctime1 = g;
        time1 = t;

        uptime = subtract(uptime1, uptime);
        gctime = subtract(gctime1, gctime);
        time = subtract(time1, time);
    }

    /**
     * <p>Subtract a term integer from another term integer.</p>
     * <p>Uses long substraction, if both are not null.</p>
     *
     * @param i1 The first term integer, or null.
     * @param i2 The second term integer, or null.
     * @return The result term integer, or null.
     */
    private static Number subtract(Number i1, Number i2) {
        if (i1 != null && i2 != null) {
            return TermAtomic.normBigInteger(i1.longValue() - i2.longValue());
        } else {
            return null;
        }
    }

    /**
     * <p>Retrieve the known statistics keys.</p>
     *
     * @param inter The interpreter.
     * @param co    The call out.
     * @return The statistics key.
     */
    public static String sysCurrentStat(Interpreter inter, CallOut co) {
        ArrayEnumeration<String> dc;
        if (co.getFirst()) {
            int hint = ((Integer) inter.getKnowledgebase().getProperty(Knowledgebase.PROP_SYS_HINT)).intValue();
            if ((hint & Foyer.HINT_MASK_LMTD) != 0) {
                dc = new ArrayEnumeration<String>(OP_STATISTICS_WEB);
            } else {
                dc = new ArrayEnumeration<String>(OP_STATISTICS);
            }
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
     * <p>Retrieve a time record statistic.</p>
     *
     * @param inter The interpreter.
     * @param name  The name.
     * @return The value, or null.
     * @throws InterpreterMessage Shit happens.
     */
    public Object getStat(Interpreter inter, String name)
            throws InterpreterMessage, InterpreterException {
        if (ForeignStatistics.OP_UPTIME.equals(name)) {
            return uptime;
        } else if (ForeignStatistics.OP_GCTIME.equals(name)) {
            return gctime;
        } else if (ForeignStatistics.OP_TIME.equals(name)) {
            return time;
        } else if (ForeignStatistics.OP_WALL.equals(name)) {
            return ForeignStatistics.sysGetStat(inter,
                    ForeignStatistics.OP_WALL);
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    EngineMessage.OP_DOMAIN_PROLOG_FLAG, name));
        }
    }

}
