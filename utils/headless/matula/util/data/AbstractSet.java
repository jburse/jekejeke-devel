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

public abstract class AbstractSet<E> {

    /**
     * <p>Retrieve the stored key.</p>
     *
     * @param key The search key, can be null.
     * @return The stored key or null.
     */
    public abstract E getKey(E key);

    /**
     * <p>Find the key in the map.</p>
     *
     * @param key The key.
     * @return The entry, or null.
     */
    public abstract SetEntry<E> getEntry(E key);

    /**
     * <p>Add key to the set.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key The key, can be null.
     */
    public abstract void putKey(E key);

    /**
     * <p>Remove the key from the set.</p>
     *
     * @param key The key, can be null.
     */
    public abstract void remove(E key);

    /**
     * <p>Retrieve the last entry.</p>
     *
     * @return The last entry, can be null.
     */
    public abstract SetEntry<E> getLastEntry();

    /**
     * <p>Retrieve the first entry.</p>
     *
     * @return The first entry, can be null.
     */
    public abstract SetEntry<E> getFirstEntry();

    /**
     * <p>Retrieve the predecessor for a given entry.</p>
     *
     * @param s The entry, not null.
     * @return The predecessor, can be null.
     */
    public abstract SetEntry<E> predecessor(SetEntry<E> s);

    /**
     * <p>Retrieve the successor for a given entry.</p>
     *
     * @param s The entry, not null.
     * @return The successor, can be null.
     */
    public abstract SetEntry<E> successor(SetEntry<E> s);

    /**
     * <p>Clear the set.</p>
     */
    public abstract void clear();

    /***********************************************************/
    /* Bootstraped Methods                                     */
    /***********************************************************/

    /**
     * <p>Copy the set entries to an array.</p>
     *
     * @param target The array.
     */
    public void toArray(E[] target) {
        toArray(target, 0);
    }

    /**
     * <p>Copy the hash map entries to an array.</p>
     *
     * @param target The array.
     * @param pos    The start index.
     */
    public void toArray(E[] target, int pos) {
        for (SetEntry<E> entry = getFirstEntry();
             entry != null; entry = successor(entry)) {
            target[pos] = entry.key;
            pos++;
        }
    }

}
