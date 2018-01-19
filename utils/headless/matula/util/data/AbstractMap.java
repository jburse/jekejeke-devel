package matula.util.data;

/**
 * <p>The base class for the maps.</p>
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
public abstract class AbstractMap<K, V>
        extends AbstractAssoc<K, V> {

    /************************************************************/
    /* Derived Methods                                          */
    /************************************************************/

    /**
     * <p>Add the key to the map.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key   The key.
     * @param value The value.
     * @return The new enry.
     */
    public final MapEntry<K, V> put(K key, V value) {
        MapEntry<K, V> e = newEntry(key, value);
        putEntry(e);
        return e;
    }

    /**
     * <p>Add the key to the map.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key   The key.
     * @param value The value.
     */
    public final void add(K key, V value) {
        put(key, value);
    }

    /**
     * <p>Find the key in the map.</p>
     *
     * @param key The key.
     * @return The value.
     */
    public final V get(K key) {
        MapEntry<K, V> e = getEntry(key);
        return (e != null ? e.value : null);
    }

    /**
     * <p>Remove the key from the map.</p>
     *
     * @param key The key.
     */
    public final void remove(K key) {
        MapEntry<K, V> e = getEntry(key);
        if (e == null)
            return;
        removeEntry(e);
        resize();
    }

    /**
     * <p>Copy the hash map entries to an array.</p>
     *
     * @param target The array.
     */
    public final void toArray(MapEntry<K, V>[] target) {
        toArray(target, 0);
    }

    /**
     * <p>Copy the hash map entries to an array.</p>
     *
     * @param target The array.
     * @param pos    The start index.
     */
    public final void toArray(MapEntry<K, V>[] target, int pos) {
        for (MapEntry<K, V> entry = getFirstEntry();
             entry != null; entry = successor(entry)) {
            target[pos] = entry;
            pos++;
        }
    }

    /************************************************************/
    /* Variation Points                                         */
    /************************************************************/

    /**
     * <p>Find the key in the map.</p>
     *
     * @param key The key.
     * @return The entry, or null.
     */
    public abstract MapEntry<K, V> getEntry(K key);

    /**
     * <p>Add the key to the map.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param f The enry, not null.
     */
    public abstract void putEntry(MapEntry<K, V> f);

    /**
     * <p>Create a new entry.</p>
     *
     * @param key   The key.
     * @param value The value.
     * @return The entry.
     */
    public abstract MapEntry<K, V> newEntry(K key, V value);

    /**
     * <p>Remove the key from the map.</p>
     *
     * @param f The entry, not null.
     */
    public abstract void removeEntry(MapEntry<K, V> f);

    /**
     * <p>Resize after remove entry.</p>
     */
    public abstract void resize();

    /**
     * <p>Retrieve the last entry.</p>
     *
     * @return The last entry, can be null.
     */
    public abstract MapEntry<K, V> getLastEntry();

    /**
     * <p>Retrieve the first entry.</p>
     *
     * @return The first entry, can be null.
     */
    public abstract MapEntry<K, V> getFirstEntry();

    /**
     * <p>Retrieve the predecessor for a given entry.</p>
     *
     * @param s The entry, not null.
     * @return The predecessor, can be null.
     */
    public abstract MapEntry<K, V> predecessor(MapEntry<K, V> s);

    /**
     * <p>Retrieve the successor for a given entry.</p>
     *
     * @param s The entry, not null.
     * @return The successor, can be null.
     */
    public abstract MapEntry<K, V> successor(MapEntry<K, V> s);

    /***************************************************************/
    /* Object Protocol                                             */
    /***************************************************************/

    /**
     * <p>Returns a string representation of this list array.</p>
     *
     * @return A string representation of this list array.
     */
    public String toString() {
        MapEntry<K, V> entry = getFirstEntry();
        if (entry == null)
            return "{}";
        StringBuilder buf = new StringBuilder();
        buf.append("{");
        buf.append(entry.key);
        buf.append(":");
        buf.append(entry.value);
        entry = successor(entry);
        while (entry != null) {
            buf.append(",");
            buf.append(entry.key);
            buf.append(":");
            buf.append(entry.value);
            entry = successor(entry);
        }
        buf.append("}");
        return buf.toString();
    }

    /**
     * <p>Create a shallow copy.</p>
     *
     * @return The shallow copy.
     */
    public Object clone() {
        AbstractMap<K, V> res = (AbstractMap<K, V>) super.clone();
        for (MapEntry<K, V> entry = getFirstEntry();
             entry != null; entry = successor(entry))
            res.add(entry.key, entry.value);
        return res;
    }

}
