package matula.util.data;

/**
 * <p>Implementation of a tree map entry.</p>
 *
 * @author Copyright 2012, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.6 (a fast and small prolog interpreter)
 */
final class MapTreeEntry<K, V> extends MapEntry<K, V> {
    final static boolean RED = false;
    final static boolean BLACK = true;

    MapTreeEntry<K, V> left;
    MapTreeEntry<K, V> right;
    MapTreeEntry<K, V> parent;
    boolean color = BLACK;

    /**
     * <p>Create an entry.</p>
     *
     * @param k The key.
     * @param v The value.
     * @param p The parent.
     */
    MapTreeEntry(K k, V v, MapTreeEntry<K, V> p) {
        super(k, v);
        parent = p;
    }

}
