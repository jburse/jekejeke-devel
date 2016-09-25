package matula.util.data;

/**
 * <p>Implementation of a linked hash map entry.</p>
 *
 * @author Copyright 2012, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.6 (a fast and small prolog interpreter)
 */
final class MapHashLinkEntry<K, V> extends MapEntry<K, V> {
    MapHashLinkEntry<K, V> next;
    MapHashLinkEntry<K, V> after;
    MapHashLinkEntry<K, V> before;

    /**
     * <p>Create an entry.</p>
     *
     * @param k The key.
     * @param v The value.
     */
    MapHashLinkEntry(K k, V v) {
        super(k, v);
    }

}
