package matula.util.data;

/**
 * <p>Implementation of a linked hash map.</p>
 *
 * @author Copyright 2012-2016, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.6 (a fast and small prolog interpreter)
 */
public class MapHashLink<K, V> extends AbstractMap<K, V> {
    private static final int MIN_SIZE = 2;

    MapHashLinkEntry<K, V>[] table = new MapHashLinkEntry[MIN_SIZE];
    public int size;
    MapHashLinkEntry<K, V> first;
    MapHashLinkEntry<K, V> last;

    /**
     * <p>Retrieve a value for a key.</p>
     *
     * @param key The key.
     * @return The value.
     */
    public V get(K key) {
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);

        MapHashLinkEntry<K, V> e;
        for (e = table[i]; e != null &&
                !(key != null ? key.equals(e.key) : null == e.key); e = e.next)
            ;

        if (e == null)
            return null;
        return e.value;
    }

    /**
     * <p>Retrieve an entry for a key.</p>
     *
     * @param key The key.
     * @return The entry.
     */
    public MapEntry<K, V> getEntry(K key) {
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);

        MapHashLinkEntry<K, V> e;
        for (e = table[i]; e != null &&
                !(key != null ? key.equals(e.key) : null == e.key); e = e.next)
            ;

        return e;
    }

    /**
     * <p>Add the key to the map.</p>
     * <p>Entry is create at the bottom.</p>
     *
     * @param key   The key.
     * @param value The value.
     * @return The new enry.
     */
    public MapEntry<K, V> put(K key, V value) {
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);

        MapHashLinkEntry<K, V> e = new MapHashLinkEntry<K, V>(key, value);
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
        return e;
    }

    /**
     * <p>Add the key to the map.</p>
     * <p>Entry is create at the top.</p>
     *
     * @param key   The key.
     * @param value The value.
     */
    public void putFirst(K key, V value) {
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);

        MapHashLinkEntry<K, V> e = new MapHashLinkEntry<K, V>(key, value);
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
     * <p>Remove the key from the map.</p>
     *
     * @param key The key.
     */
    public void remove(K key) {
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);

        MapHashLinkEntry<K, V> e;
        MapHashLinkEntry<K, V> b = null;
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

        if (e.after != null) {
            e.after.before = e.before;
        } else {
            last = e.before;
        }
        if (e.before != null) {
            e.before.after = e.after;
        } else {
            first = e.after;
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
        MapHashLinkEntry<K, V>[] newtable = new MapHashLinkEntry[s];

        MapHashLinkEntry<K, V> e = last;
        while (e != null) {
            Object key = e.key;
            int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                    (s - 1) : 0);
            e.next = newtable[i];
            newtable[i] = e;
            e = e.before;
        }

        table = newtable;
    }

    /**
     * <p>Retrieve the last entry.</p>
     *
     * @return The last entry, can be null.
     */
    public MapEntry<K, V> getLastEntry() {
        return last;
    }

    /**
     * <p>Retrieve the first entry.</p>
     *
     * @return The first entry, can be null.
     */
    public MapEntry<K, V> getFirstEntry() {
        return first;
    }

    /**
     * <p>Retrieve the preceeding entry of a given entry.</p>
     *
     * @param s The entry, not null.
     * @return The preceeding entry.
     */
    public MapEntry<K, V> predecessor(MapEntry<K, V> s) {
        MapHashLinkEntry<K, V> t = (MapHashLinkEntry<K, V>) s;
        return t.before;
    }

    /**
     * <p>Retrieve the successor entry of a given entry.</p>
     *
     * @param s The entry, not null.
     * @return The succeeding entry.
     */
    public MapEntry<K, V> successor(MapEntry<K, V> s) {
        MapHashLinkEntry<K, V> t = (MapHashLinkEntry<K, V>) s;
        return t.after;
    }

    /**
     * <p>Clear the map.</p>
     */
    public void clear() {
        if (size == 0)
            return;
        if (table.length != MIN_SIZE) {
            table = new MapHashLinkEntry[MIN_SIZE];
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
