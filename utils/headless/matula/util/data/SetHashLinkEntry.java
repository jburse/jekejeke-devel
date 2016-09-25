package matula.util.data;

/**
 * <p>Entry for the linked list hash set.</p>
 *
 * @author Copyright 2012-2014, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.3 (a fast and small prolog interpreter)
 */
final class SetHashLinkEntry<K> extends SetEntry<K> {
    SetHashLinkEntry<K> next;
    SetHashLinkEntry<K> before;
    SetHashLinkEntry<K> after;

    /**
     * <p>Create a linked list hash set.</p>
     *
     * @param k The key.
     */
    SetHashLinkEntry(K k) {
        super(k);
    }

}
