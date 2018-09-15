package jekpro.tools.call;

import java.util.Enumeration;

/**
 * <p>Helper for array enumeration.</p>
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
public final class ArrayEnumeration<T> implements Enumeration<T> {
    private T[] data;
    private int pos;

    /**
     * <p>Create a new dom cursor.</p>
     *
     * @param d The elements.
     */
    public ArrayEnumeration(T[] d) {
        if (d == null)
            throw new NullPointerException("data missing");
        data = d;
    }

    /**
     * <p>Check whether there are more elements.</p>
     *
     * @return True if there are more elements, otherwise false.
     */
    public boolean hasMoreElements() {
        return pos < data.length;
    }

    /**
     * <p>Retrieve the next element and advance the cursor.</p>
     *
     * @return The next element.
     */
    public T nextElement() {
        return data[pos++];
    }

}
