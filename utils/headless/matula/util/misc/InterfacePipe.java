package matula.util.misc;

import java.util.concurrent.TimeUnit;

/**
 * <p>This class provides an abstract pipe.</p>
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
public interface InterfacePipe<T> {

    /**
     * <p>Add an object to the end of the pipe.</p>
     * <p>Blocks if pipe is full.</p>
     *
     * @param t The object, not null.
     * @throws InterruptedException If the request was cancelled.
     */
    void put(T t)
            throws InterruptedException;

    /**
     * <p>Remove an object from the front of the pipe.</p>
     * <p>Blocks if pipe is empty.</p>
     *
     * @return The object, not null.
     * @throws InterruptedException If the request was cancelled.
     */
    T take()
            throws InterruptedException;

    /**
     * <p>Remove an object from the front of the pipe.</p>
     * <p>Fails if pipe is empty.</p>
     *
     * @return The object or null if no object was taken.
     */
    T poll();

    /**
     * <p>Remove an object from the front of the pipe.</p>
     * <p>Fails if pipe is still empty after time-out.</p>
     *
     * @param sleep The time-out.
     * @param unit The time unit.
     * @return The object or null if no object was taken.
     * @throws InterruptedException If the request was cancelled.
     */
    T poll(long sleep, TimeUnit unit)
            throws InterruptedException;

}
