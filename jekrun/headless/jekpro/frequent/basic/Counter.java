package jekpro.frequent.basic;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * <p>This class provides an atomic integer counter.</p>
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
public final class Counter {
    private AtomicInteger counter = new AtomicInteger();

    /**
     * <p>Increment this counter and return the old value.</p>
     *
     * @return The old value.
     */
    public int next() {
        int oldcount, newcount;
        AtomicInteger atomic = counter;
        do {
            oldcount = atomic.get();
            newcount = oldcount + 1;
        } while (!atomic.compareAndSet(oldcount, newcount));
        return oldcount;
    }

}