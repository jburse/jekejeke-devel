package jekpro.frequent.advanced;

import jekpro.tools.term.AbstractSkel;
import matula.util.data.SetEntry;

/**
 * <p>Provides a reference data type that acts as a variant key.</p>
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
public final class VariantKey extends SetEntry
        implements Comparable<VariantKey> {

    /**
     * <p>Retrieve the hash code.</p>
     *
     * @return The hash code.
     */
    public int hashCode() {
        return value.hashCode();
    }

    /**
     * <p>Check equals another variant key.</p>
     *
     * @param obj The other object.
     * @return True of equals other object, otherwise false.
     */
    public boolean equals(Object obj) {
        if (!(obj instanceof SetEntry))
            return false;
        return value.equals(((SetEntry)obj).value);
    }

    /**
     * <p>Compare this ref var to another ref var.</p>
     *
     * @param o The other ref var.
     * @return <0 less, 0 equal, >0 greater
     */
    public int compareTo(VariantKey o) {
        return AbstractSkel.compareSkel(value, o.value);
    }

}