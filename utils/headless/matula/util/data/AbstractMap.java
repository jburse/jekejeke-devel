package matula.util.data;

/**
 * <p>The base class for the maps.</p>
 *
 * @author Copyright 2012-2015, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.6 (a fast and small prolog interpreter)
 */
public abstract class AbstractMap<K, V> {

    /**
     * <p>Find the key in the map.</p>
     *
     * @param key The key.
     * @return The value, or null.
     */
    public abstract V get(K key);

    /**
     * <p>Find the key in the map.</p>
     *
     * @param key The key.
     * @return The entry, or null.
     */
    public abstract MapEntry<K, V> getEntry(K key);

    /**
     * <p>Add the key to the map.</p>
     *
     * @param key   The key.
     * @param value The value.
     * @return The new enry.
     */
    public abstract MapEntry<K, V> put(K key, V value);

    /**
     * <p>Remove the key from the map.</p>
     *
     * @param key The key.
     */
    public abstract void remove(K key);

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

    /**
     * <p>Clear the map.</p>
     */
    public abstract void clear();

    /***********************************************************/
    /* Bootstraped Methods                                     */
    /***********************************************************/

    /**
     * <p>Copy the hash map entries to an array.</p>
     *
     * @param target The array.
     */
    public void toArray(MapEntry<K, V>[] target) {
        toArray(target, 0);
    }

    /**
     * <p>Copy the hash map entries to an array.</p>
     *
     * @param target The array.
     * @param pos    The start index.
     */
    public void toArray(MapEntry<K, V>[] target, int pos) {
        for (MapEntry<K, V> entry = getFirstEntry();
             entry != null; entry = successor(entry)) {
            target[pos] = entry;
            pos++;
        }
    }

}
