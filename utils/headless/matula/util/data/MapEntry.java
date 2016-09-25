package matula.util.data;

/**
 * <p>The base class for the map entries.</p>
 * <p>The entry value can be modified.</p>
 *
 * @author Copyright 2012-2015, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.6 (a fast and small prolog interpreter)
 */
public abstract class MapEntry<K, V> {
    public K key;
    public V value;

    /**
     * <p>Create an entry.</p>
     *
     * @param k The key.
     * @param v The value.
     */
    MapEntry(K k, V v) {
        key = k;
        value = v;
    }

}
