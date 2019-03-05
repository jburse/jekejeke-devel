package matula.util.misc;

/**
 * <p>This class provides an unsynchronized pivot.</p>
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
public final class Pivot {
    private Object val;

    /**
     * <p>Add an object to the pivot.</p>
     * <p>Throws error if pivot is full.</p>
     *
     * @param v The object, not null,
     */
    public void put(Object v) {
        if (v == null)
            throw new NullPointerException("null_element");
        if (val != null)
            throw new IllegalStateException("alread_full");
        val = v;
    }

    /**
     * <p>Remove an object from the pivot.</p>
     * <p>Throws error if pivot is empty.</p>
     *
     * @return The object, not null.
     */
    public Object take() {
        Object v = val;
        if (v == null)
            throw new IllegalStateException("alread_empty");
        val = null;
        return v;
    }

}