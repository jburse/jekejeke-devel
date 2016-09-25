package matula.util.data;

/**
 * <p>Implementation of a tree set entry.</p>
 *
 * @author Copyright 2014, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.0.3 (a fast and small prolog interpreter)
 */
final class SetTreeEntry<K> extends SetEntry<K> {
    final static boolean RED = false;
    final static boolean BLACK = true;

    SetTreeEntry<K> left;
    SetTreeEntry<K> right;
    SetTreeEntry<K> parent;
    boolean color = BLACK;

    /**
     * <p>Create a tree set entry.</p>
     *
     * @param k The key.
     * @param p The parent.
     */
    SetTreeEntry(K k, SetTreeEntry<K> p) {
        super(k);
        parent = p;
    }

}
