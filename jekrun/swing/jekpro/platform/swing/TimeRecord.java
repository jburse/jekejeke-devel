package jekpro.platform.swing;

import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.tools.call.ArrayEnumeration;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.TermAtomic;

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

    private final static String[] OP_STATISTICS = {
            ForeignStatistics.OP_STATISTIC_UPTIME,
            ForeignStatistics.OP_STATISTIC_GCTIME,
            ForeignStatistics.OP_STATISTIC_TIME,
            ForeignStatistics.OP_STATISTIC_WALL};

    private final static String[] OP_STATISTICS_WEB = {
            ForeignStatistics.OP_STATISTIC_UPTIME,
            ForeignStatistics.OP_STATISTIC_WALL};

    /**
     * <p>Start time record measurement.</p>
     *
     * @param inter The interpreter.
     * @throws InterpreterMessage Shit happens.
     */
    public void start(Interpreter inter)
            throws InterpreterMessage {
        uptime = (Number) ForeignStatistics.sysGetStat(inter,
                ForeignStatistics.OP_STATISTIC_UPTIME);
        gctime = (Number) ForeignStatistics.sysGetStat(inter,
                ForeignStatistics.OP_STATISTIC_GCTIME);
        time = (Number) ForeignStatistics.sysGetStat(inter,
                ForeignStatistics.OP_STATISTIC_TIME);
    }

    /**
     * <p>End time record measurement.</p>
     *
     * @param inter The interpreter.
     * @throws InterpreterMessage Shit happens.
     */
    public void end(Interpreter inter)
            throws InterpreterMessage {
        uptime = subtract((Number) ForeignStatistics.sysGetStat(inter,
                ForeignStatistics.OP_STATISTIC_UPTIME), uptime);
        gctime = subtract((Number) ForeignStatistics.sysGetStat(inter,
                ForeignStatistics.OP_STATISTIC_GCTIME), gctime);
        time = subtract((Number) ForeignStatistics.sysGetStat(inter,
                ForeignStatistics.OP_STATISTIC_TIME), time);
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
            int hint = ((Integer) inter.getProperty("sys_hint")).intValue();
            switch (hint) {
                case Foyer.HINT_WEB:
                    dc = new ArrayEnumeration<String>(OP_STATISTICS_WEB);
                    break;
                default:
                    dc = new ArrayEnumeration<String>(OP_STATISTICS);
                    break;
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
     * @param name The name.
     * @return The value, or null.
     * @throws InterpreterMessage Shit happens.
     */
    public Object getStat(Interpreter inter, String name)
            throws InterpreterMessage {
        if (ForeignStatistics.OP_STATISTIC_UPTIME.equals(name)) {
            return uptime;
        } else if (ForeignStatistics.OP_STATISTIC_GCTIME.equals(name)) {
            return gctime;
        } else if (ForeignStatistics.OP_STATISTIC_TIME.equals(name)) {
            return time;
        } else if (ForeignStatistics.OP_STATISTIC_WALL.equals(name)) {
            return ForeignStatistics.sysGetStat(inter, ForeignStatistics.OP_STATISTIC_WALL);
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    EngineMessage.OP_DOMAIN_PROLOG_FLAG, name));
        }
    }

}
