package jekpro.frequent.misc;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;

/**
 * The foreign predicates for the module "monitor".
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
public final class ForeignCond {

    /**
     * <p>Perform a millisecond await on a condition.</p>
     *
     * @param cond    The condition.
     * @param timeout The time-out in milliseconds.
     * @return True if condition was notified, or false otherwise.
     * @throws InterruptedException If the request was cancelled.
     */
    public static boolean sysAwait(Condition cond, int timeout)
            throws InterruptedException {
        return cond.await(timeout, TimeUnit.MILLISECONDS);
    }

}