package jekdev.reference.inspection;

import jekpro.tools.call.ArrayEnumeration;
import jekpro.tools.call.CallOut;
import matula.util.data.MapEntry;
import matula.util.wire.AbstractLivestock;
import matula.util.wire.Fence;

/**
 * <p>The foreign predicates for the module inspection/fence.</p>
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
public class ForeignFence {

    /**
     * <p>Non-deterministic predicate for the livestock.</p>
     *
     * @param co The call out.
     * @return The livestock.
     */
    public static AbstractLivestock sysCurrentLivestock(CallOut co) {
        ArrayEnumeration<MapEntry<Thread, AbstractLivestock>> dc;
        if (co.getFirst()) {
            dc = new ArrayEnumeration<MapEntry<Thread, AbstractLivestock>>(Fence.DEFAULT.snapshotLivestocks());
            co.setData(dc);
        } else {
            dc = (ArrayEnumeration<MapEntry<Thread, AbstractLivestock>>)co.getData();
        }
        if (!dc.hasMoreElements())
            return null;
        AbstractLivestock res=dc.nextElement().value;
        co.setRetry(dc.hasMoreElements());
        return res;
    }

}