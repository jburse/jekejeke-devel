package jekpro.model.pretty;

/**
 * <p>An object that is used to lookup predicates in the store</p>
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
public class StoreKey implements Comparable<StoreKey> {
    private final String fun;
    private final int arity;

    /**
     * <p>Create a store key from functor and length.</p>
     *
     * @param f The functor.
     * @param a The length.
     */
    public StoreKey(String f, int a) {
        if (f == null)
            throw new NullPointerException("functor missing");
        if (a < 0)
            throw new ArrayIndexOutOfBoundsException("negative length");
        fun = f;
        arity = a;
    }

    /**
     * <p>Check whether this store key equals to the other store key.</p>
     *
     * @param o The other store key.
     * @return True if store keys are equal, otherwise false.
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof StoreKey))
            return false;
        StoreKey sk = (StoreKey) o;
        return (fun.equals(sk.fun) && arity == sk.arity);
    }

    /**
     * <p>Compute hash code of this store key.</p>
     *
     * @return The hash code.
     */
    public int hashCode() {
        return fun.hashCode() * 31 + arity;
    }

    /**
     * Compares this object with an object for order.
     *
     * @param o The other object.
     * @return < 0 if less than, 0 if equal, > 0 if greater than.
     */
    public int compareTo(StoreKey o) {
        if (o instanceof StoreKeyQuali)
            return -1;
        int res = fun.compareTo(o.fun);
        if (res != 0) return res;
        return arity - o.arity;
    }

    /**
     * <p>Retrieve the functor.</p>
     *
     * @return The functor.
     */
    public final String getFun() {
        return fun;
    }

    /**
     * <p>Retrieve the length.</p>
     *
     * @return The length.
     */
    public final int getArity() {
        return arity;
    }

}
