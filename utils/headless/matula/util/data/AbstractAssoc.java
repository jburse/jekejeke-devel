package matula.util.data;

/**
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
public abstract class AbstractAssoc<K, V>
        implements Cloneable {
    public int size;

    /**
     * <p>Retrieve size.</p>
     *
     * @return The size.
     */
    public int size() {
        return size;
    }

    /**
     * <p>Find the key in the map.</p>
     *
     * @param key The key.
     * @return The value, or null.
     */
    public abstract V get(K key);

    /**
     * <p>Add the key to the map.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key   The key.
     * @param value The value.
     */
    public abstract void add(K key, V value);

    /**
     * <p>Remove the key from the map.</p>
     *
     * @param key The key.
     */
    public abstract void remove(K key);

    /**
     * <p>Clear the map.</p>
     */
    public abstract void clear();

    /***************************************************************/
    /* Object Protocol                                             */
    /***************************************************************/

    /**
     * <p>Create a shallow copy.</p>
     *
     * @return The shallow copy.
     */
    public Object clone() {
        AbstractAssoc<K, V> res;
        try {
            res = (AbstractAssoc<K, V>) super.clone();
        } catch (CloneNotSupportedException x) {
            throw new RuntimeException("internal error", x);
        }
        res.reinitialize(size());
        return res;
    }

    /**
     * Reset to initial default state.
     *
     * @param capa The ahead capacity.
     */
    void reinitialize(int capa) {
        size = 0;
    }

}