package matula.util.data;

/**
 * <p>The base class for the sets.</p>
 * </p>
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

public abstract class AbstractSet<K> {

    /**
     * <p>Retrieve the stored key.</p>
     *
     * @param key The search key, can be null.
     * @return The stored key or null.
     */
    public abstract K getKey(K key);

    /**
     * <p>Add key to the set.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key The key, can be null.
     */
    public abstract void putKey(K key);

    /**
     * <p>Retrieve the last entry.</p>
     *
     * @return The last entry.
     */
    public abstract SetEntry<K> getLastEntry();

    /**
     * <p>Retrieve the predecessor for a given entry.</p>
     *
     * @param t The entry.
     * @return The predecessor of the entry.
     */
    public abstract SetEntry<K> predecessor(SetEntry<K> t);

    /**
     * <p>Clear the set.</p>
     */
    public abstract void clear();

}
