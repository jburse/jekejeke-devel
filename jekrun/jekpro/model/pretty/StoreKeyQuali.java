package jekpro.model.pretty;

import jekpro.reference.runtime.EvaluableLogic;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

/**
 * <p>An object that is used to lookup qualified predicates in the store</p>
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
public final class StoreKeyQuali extends StoreKey {
    private final String mod;

    /**
     * <p>Create a store key from functor and length.</p>
     *
     * @param f The functor.
     * @param a The length.
     * @param m The module.
     */
    public StoreKeyQuali(String f, int a, String m) {
        super(f, a);
        mod = m;
    }

    /**
     * <p>Retrieve the module.</p>
     *
     * @return The module source.
     */
    public String getModule() {
        return mod;
    }

    /**
     * <p>Check whether this qualified store key equals to the other qualified store key.</p>
     *
     * @param o The other qualified store key.
     * @return True if qualified store keys are equal, otherwise false.
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof StoreKeyQuali))
            return false;
        StoreKeyQuali skq = (StoreKeyQuali) o;
        return (super.equals(o) && mod.equals(skq.mod));
    }

    /**
     * <p>Compute hash code of this qualified store key.</p>
     *
     * @return The hash code.
     */
    public int hashCode() {
        return super.hashCode() * 31 + mod.hashCode();
    }

    /**
     * Compares this object with an object for order.
     *
     * @param o The other object.
     * @return < 0 if less than, 0 if equal, > 0 if greater than.
     */
    public int compareTo(StoreKey o) {
        if (o instanceof StoreKey)
            return 1;
        int res = super.compareTo(o);
        if (res != 0) return res;
        return mod.compareTo(((StoreKeyQuali) o).mod);
    }

    /**
     * <p>Convert this qualified store key to a compound.</p>
     *
     * @return The compound.
     */
    public Object storeKeyToSkel() {
        return new SkelCompound(new SkelAtom(EvaluableLogic.OP_COLON),
                new SkelAtom(getModule()),
                super.storeKeyToSkel());
    }

}