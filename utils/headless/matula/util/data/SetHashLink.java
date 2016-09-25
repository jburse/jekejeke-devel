package matula.util.data;

/**
 * <p>Implementation of a linked hash set.</p>
 * <p>Not based on some linked hash map.</p>
 * <p>Smaller initial size, and shrinks also.</p>
 * <p>No iterator provided, iterate over first and after.</p>
 *
 * @author Copyright 2012-2016, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.3 (a fast and small prolog interpreter)
 */
public final class SetHashLink<E> extends AbstractSet<E> {
    private static final int MIN_SIZE = 2;

    SetHashLinkEntry<E>[] table = new SetHashLinkEntry[MIN_SIZE];
    public int size;
    SetHashLinkEntry<E> first;
    SetHashLinkEntry<E> last;

    /**
     * <p>Retrieve the stored key.</p>
     *
     * @param key The search key, can be null.
     * @return The stored key or null.
     */
    public E getKey(E key) {
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);

        SetHashLinkEntry<E> e;
        for (e = table[i]; e != null &&
                !(key != null ? key.equals(e.key) : null == e.key); e = e.next)
            ;

        return (e != null ? e.key : null);
    }

    /**
     * <p>Find the key in the map.</p>
     *
     * @param key The key.
     * @return The entry, or null.
     */
    public SetEntry<E> getEntry(E key) {
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);

        SetHashLinkEntry<E> e;
        for (e = table[i]; e != null &&
                !(key != null ? key.equals(e.key) : null == e.key); e = e.next)
            ;

        return e;
    }

    /**
     * <p>Add key to the set.</p>
     * <p>Assumption is that key is not yet present.</p>
     * <p>Entry is created at the bottom.</p>
     *
     * @param key The key, can be null.
     */
    public void putKey(E key) {
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);

        SetHashLinkEntry<E> e = new SetHashLinkEntry<E>(key);
        e.next = table[i];
        table[i] = e;

        e.before = last;
        if (last != null) {
            last.after = e;
        } else {
            first = e;
        }
        last = e;

        size++;
        if (size > table.length * 3 / 4)
            resize(table.length * 2);
    }

    /**
     * <p>Add key to the set.</p>
     * <p>Assumption is that key is not yet present.</p>
     * <p>Entry is created at the top.</p>
     *
     * @param key The key.
     */
    public void putKeyFirst(E key) {
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);

        SetHashLinkEntry<E> e = new SetHashLinkEntry<E>(key);
        e.next = table[i];
        table[i] = e;

        e.after = first;
        if (first != null) {
            first.before = e;
        } else {
            last = e;
        }
        first = e;

        size++;
        if (size > table.length * 3 / 4)
            resize(table.length * 2);
    }

    /**
     * <p>Remove key from the set.</p>
     *
     * @param key The key.
     */
    public void remove(E key) {
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);

        SetHashLinkEntry<E> e;
        SetHashLinkEntry<E> b = null;
        for (e = table[i]; e != null &&
                !(key != null ? key.equals(e.key) : null == e.key); b = e, e = b.next)
            ;
        if (e == null)
            return;

        if (b == null) {
            table[i] = e.next;
        } else {
            b.next = e.next;
        }
        size--;

        if (e.before != null) {
            e.before.after = e.after;
        } else {
            first = e.after;
        }
        if (e.after != null) {
            e.after.before = e.before;
        } else {
            last = e.before;
        }

        if (size < table.length / 4 && table.length / 2 > MIN_SIZE)
            resize(table.length / 2);
    }

    /**
     * <p>Resize the hash table.</p>
     *
     * @param s The new size.
     */
    private void resize(int s) {
        SetHashLinkEntry<E>[] newtable = new SetHashLinkEntry[s];

        SetHashLinkEntry<E> e = first;
        while (e != null) {
            Object key = e.key;
            int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                    (s - 1) : 0);
            e.next = newtable[i];
            newtable[i] = e;
            e = e.after;
        }

        table = newtable;
    }

    /**
     * <p>Retrieve the last entry.</p>
     *
     * @return The last entry, can be null.
     */
    public SetEntry<E> getLastEntry() {
        return last;
    }

    /**
     * <p>Retrieve the first entry.</p>
     *
     * @return The first entry, can be null.
     */
    public SetEntry<E> getFirstEntry() {
        return first;
    }

    /**
     * <p>Retrieve the predecessor for a given entry.</p>
     *
     * @param s The entry, not null.
     * @return The predecessor, can be null.
     */
    public SetEntry<E> predecessor(SetEntry<E> s) {
        SetHashLinkEntry<E> t = (SetHashLinkEntry<E>) s;
        return t.before;
    }

    /**
     * <p>Retrieve the successor for a given entry.</p>
     *
     * @param s The entry, not null.
     * @return The successor, can be null.
     */
    public SetEntry<E> successor(SetEntry<E> s) {
        SetHashLinkEntry<E> t = (SetHashLinkEntry<E>) s;
        return t.after;
    }

    /**
     * <p>Clear the set.</p>
     */
    public void clear() {
        if (size == 0)
            return;
        if (table.length != MIN_SIZE) {
            table = new SetHashLinkEntry[MIN_SIZE];
        } else {
            int n = Math.min(size, MIN_SIZE);
            for (int i = 0; i < n; i++)
                table[i] = null;
        }
        size = 0;
        first = null;
        last = null;
    }

}
