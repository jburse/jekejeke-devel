package matula.util.data;

/**
 * <p>Entry for the hash map.</p>
 *
 * @author Copyright 2012, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.3 (a fast and small prolog interpreter)
 */
final class MapHashEntry<K, V> extends MapEntry<K, V> {
    MapHashEntry<K, V> next;
    MapHashEntry<K, V> prev;

    /**
     * <p>Create a clause entry.</p>
     *
     * @param k The key.
     * @param v The value;
     */
    public MapHashEntry(K k, V v) {
        super(k, v);
    }

}
