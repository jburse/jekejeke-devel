package jekpro.frequent.misc;

import jekpro.tools.call.Interpreter;
import jekpro.tools.term.AbstractTerm;
import matula.util.misc.Alarm;
import matula.util.misc.AlarmEntry;

/**
 * Provides the methods for the module misc/time.
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
public final class ForeignTime {

    /**
     * <p>Schedule an item on an alarm queue.</p>
     *
     * @param inter The interpreter.
     * @param a     The alarm queue.
     * @param t     The item.
     * @param sleep The timeout.
     * @return The alarm entry.
     */
    public static AlarmEntry sysAlarmSchedule(Interpreter inter, Alarm a,
                                              AbstractTerm t, long sleep) {
        t = AbstractTerm.copyTermWrapped(inter, t);
        return a.schedule(t, sleep);
    }

    /**
     * <p>Get the next item from an alarm queue.</p>
     *
     * @return The item.
     * @throws InterruptedException Thread was interrupted.
     */
    public static Object sysAlarmNext(Alarm a)
            throws InterruptedException {
        return a.next();
    }

}
