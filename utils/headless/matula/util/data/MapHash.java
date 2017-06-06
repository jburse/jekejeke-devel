package matula.util.data;

/**
 * <p>Implementation of a hash map.</p>
 * <p>Smaller initial size, and shrinks also.</p>
 * <p>No iterator provided, iterate by yourself over table and entries.</p>
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
public final class MapHash<K, V> extends AbstractMap<K, V> {
    private static final int MIN_SIZE = 2;

    MapHashEntry<K, V>[] table = new MapHashEntry[MIN_SIZE];
    public int size;

    /**
     * <p>Find the key in the map.</p>
     *
     * @param key The key.
     * @return The value, or null.
     */
    public V get(K key) {
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);

        MapHashEntry<K, V> e;
        for (e = table[i]; e != null &&
                !(key != null ? key.equals(e.key) : null == e.key); e = e.next)
            ;

        if (e == null)
            return null;
        return e.value;
    }

    /**
     * <p>Find the key in the map.</p>
     *
     * @param key The key.
     * @return The entry, or null.
     */
    public MapEntry<K, V> getEntry(K key) {
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);

        MapHashEntry<K, V> e;
        for (e = table[i]; e != null &&
                !(key != null ? key.equals(e.key) : null == e.key); e = e.next)
            ;

        return e;
    }

    /**
     * <p>Add the key to the map.</p>
     *
     * @param key   The key.
     * @param value The value.
     * @return The new enry.
     */
    public MapEntry<K, V> put(K key, V value) {
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);

        MapHashEntry<K, V> e = new MapHashEntry<K, V>(key, value);
        MapHashEntry<K, V> f = table[i];
        if (f != null)
            f.prev = e;
        e.next = f;
        table[i] = e;
        size++;

        if (size > table.length * 3 / 4)
            resize(table.length * 2);

        return e;
    }

    /**
     * <p>Remove the key from the map.</p>
     *
     * @param key The key.
     */
    public void remove(K key) {
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);

        MapHashEntry<K, V> e;
        for (e = table[i]; e != null &&
                !(key != null ? key.equals(e.key) : null == e.key); e = e.next)
            ;

        if (e == null)
            return;

        MapHashEntry<K, V> f = e.prev;
        MapHashEntry<K, V> g = e.next;
        if (f != null) {
            f.next = g;
        } else {
            table[i] = g;
        }
        if (g != null)
            g.prev = f;
        size--;

        if (size < table.length / 4 && table.length / 2 > MIN_SIZE)
            resize(table.length / 2);
    }

    /**
     * <p>Remove an entry, but do not resize.</p>
     *
     * @param s The entry, not null.
     */
    public void removeEntry(MapEntry<K, V> s) {
        MapHashEntry<K, V> e = (MapHashEntry<K, V>)s;
        Object key = e.key;
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);

        MapHashEntry<K, V> f = e.prev;
        MapHashEntry<K, V> g = e.next;
        if (f != null) {
            f.next = g;
        } else {
            table[i] = g;
        }
        if (g != null)
            g.prev = f;
        size--;
    }

    /**
     * <p>Resize after remove entry.</p>
     */
    public void resize() {
        int len = table.length;
        while (size < len / 4 && len / 2 > MIN_SIZE)
            len = len / 2;
        if (len != table.length)
            resize(len);
    }

    /**
     * <p>Retrieve the table length.</p>
     *
     * @return The table length.
     */
    public int length() {
        return table.length;
    }

    /**
     * <p>Compute the index of a key.</p>
     *
     * @param key The key.
     * @return The index.
     */
    public int index(K key) {
        return (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);
    }

    /**
     * <p>Resize the hash table.</p>
     *
     * @param s The new size.
     */
    private void resize(int s) {
        MapHashEntry<K, V>[] newtable = new MapHashEntry[s];

        for (int i = 0; i < table.length; i++) {
            MapHashEntry<K, V> e = table[i];
            while (e != null) {
                MapHashEntry<K, V> b = e;
                e = b.next;
                Object key = b.key;
                int j = (key != null ? HashScrambler.murmur(key.hashCode()) &
                        (s - 1) : 0);

                b.prev = null;
                MapHashEntry<K, V> f = newtable[j];
                if (f != null)
                    f.prev = b;
                b.next = f;
                newtable[j] = b;
            }
        }

        table = newtable;
    }

    /**
     * <p>Retrieve the last entry.</p>
     *
     * @return The last entry, can be null.
     */
    public MapEntry<K, V> getLastEntry() {
        return getPrevEntry(table.length - 1);
    }

    /**
     * <p>Retrieve the first entry.</p>
     *
     * @return The first entry, can be null.
     */
    public MapEntry<K, V> getFirstEntry() {
        return getNextEntry(0);
    }

    /**
     * <p>Retrieve the predecessor for a given entry.</p>
     *
     * @param s The entry.
     * @return The predecessor of the entry.
     */
    public MapEntry<K, V> predecessor(MapEntry<K, V> s) {
        MapHashEntry<K, V> e = (MapHashEntry<K, V>) s;
        MapHashEntry<K, V> h = e.next;
        if (h != null)
            return h;
        Object key = e.key;
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);
        return getPrevEntry(i - 1);
    }

    /**
     * <p>Retrieve the successor entry of a given entry.</p>
     *
     * @param s The entry, not null.
     * @return The succeeding entry.
     */
    public MapEntry<K, V> successor(MapEntry<K, V> s) {
        MapHashEntry<K, V> e = (MapHashEntry<K, V>) s;
        MapHashEntry<K, V> h = e.next;
        if (h != null)
            return h;
        Object key = e.key;
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);
        return getNextEntry(i + 1);
    }

    /**
     * <p>Retrieve the prev entry.</p>
     *
     * @return The prev entry, can be null.
     */
    private MapEntry<K, V> getPrevEntry(int start) {
        for (int i = start; i >= 0; i--) {
            MapHashEntry<K, V> e = table[i];
            if (e != null)
                return e;
        }
        return null;
    }

    /**
     * <p>Retrieve the next entry.</p>
     *
     * @return The next entry, can be null.
     */
    private MapEntry<K, V> getNextEntry(int start) {
        for (int i = start; i < table.length; i++) {
            MapHashEntry<K, V> e = table[i];
            if (e != null)
                return e;
        }
        return null;
    }

    /**
     * <p>Clear the map.</p>
     */
    public void clear() {
        if (size == 0)
            return;
        if (table.length != MIN_SIZE) {
            table = new MapHashEntry[MIN_SIZE];
        } else {
            int n = Math.min(size, MIN_SIZE);
            for (int i = 0; i < n; i++)
                table[i] = null;
        }
        size = 0;
    }

}
