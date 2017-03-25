package jekpro.platform.swing;

import jekpro.model.molec.EngineMessage;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermAtomic;
import jekpro.tools.term.TermCompound;

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

    /**
     * <p>Start time record measurement.</p>
     *
     * @throws InterpreterMessage Shit happens.
     */
    public void start() throws InterpreterMessage {
        uptime = (Number) ForeignStatistics.sysGetStat(ForeignStatistics.OP_STATISTIC_UPTIME);
        gctime = (Number) ForeignStatistics.sysGetStat(ForeignStatistics.OP_STATISTIC_GCTIME);
        time = (Number) ForeignStatistics.sysGetStat(ForeignStatistics.OP_STATISTIC_TIME);
    }

    /**
     * <p>End time record measurement.</p>
     *
     * @throws InterpreterMessage Shit happens.
     */
    public void end() throws InterpreterMessage {
        uptime = subtract((Number) ForeignStatistics.sysGetStat(ForeignStatistics.OP_STATISTIC_UPTIME), uptime);
        gctime = subtract((Number) ForeignStatistics.sysGetStat(ForeignStatistics.OP_STATISTIC_GCTIME), gctime);
        time = subtract((Number) ForeignStatistics.sysGetStat(ForeignStatistics.OP_STATISTIC_TIME), time);
    }

    /**
     * <p>Subtract a term integer from another term integer.</p>
     * <p>Uses long substraction, if both are not null.</p>
     *
     * @param i1 The first term integer, or null.
     * @param i2 The second term integer, or null.
     * @return The result term integer, or null.
     * @throws InterpreterMessage Shit happens.
     */
    private static Number subtract(Number i1, Number i2) throws InterpreterMessage {
        if (i1 != null && i2 != null) {
            return TermAtomic.normBigInteger(i1.longValue() - i2.longValue());
        } else {
            return null;
        }
    }

    /**
     * <p>Retrieve the known time record statistics keys.</p>
     *
     * @return The known time record statistics keys.
     */
    public static Object sysListRecordStats() {
        Object res = Knowledgebase.OP_NIL;
        for (int i = OP_STATISTICS.length - 1; i >= 0; i--) {
            res = new TermCompound(Knowledgebase.OP_CONS,
                    OP_STATISTICS[i], res);
        }
        return res;
    }

    /**
     * <p>Retrieve a time record statistic.</p>
     *
     * @param name The name.
     * @return The value, or null.
     * @throws InterpreterMessage Shit happens.
     */
    public Object getStat(String name) throws InterpreterMessage {
        if (ForeignStatistics.OP_STATISTIC_UPTIME.equals(name)) {
            return uptime;
        } else if (ForeignStatistics.OP_STATISTIC_GCTIME.equals(name)) {
            return gctime;
        } else if (ForeignStatistics.OP_STATISTIC_TIME.equals(name)) {
            return time;
        } else if (ForeignStatistics.OP_STATISTIC_WALL.equals(name)) {
            return ForeignStatistics.sysGetStat(ForeignStatistics.OP_STATISTIC_WALL);
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    EngineMessage.OP_DOMAIN_PROLOG_FLAG, name));
        }
    }

}
