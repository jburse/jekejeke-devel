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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public class MapHash<K, V> extends AbstractMap<K, V> {
    private static final int MIN_SIZE = 2;

    MapHashEntry<K, V>[] table;

    /**
     * <p>Create a map hash.</p>
     */
    public MapHash() {
        reinitialize(0);
    }

    /**
     * <p>Create a map hash.</p>
     *
     * @param capa The ahead capacity.
     */
    public MapHash(int capa) {
        reinitialize(capa);
    }

    /************************************************************/
    /* Variation Points                                         */
    /************************************************************/

    /**
     * <p>Find the entry in the map.</p>
     *
     * @param key The key.
     * @return The entry, or null.
     */
    public MapEntry<K, V> getEntry(K key) {
        int i = index(key);

        MapHashEntry<K, V> e;
        for (e = table[i]; e != null &&
                !(key != null ? key.equals(e.key) : null == e.key); e = e.next)
            ;

        return e;
    }

    /**
     * <p>Add the key to the map.</p>
     *
     * @param f The entry, not null.
     */
    public void putEntry(MapEntry<K, V> f) {
        if (f == null)
            throw new NullPointerException("entry missing");
        MapHashEntry<K, V> e = (MapHashEntry<K, V>) f;

        int i = index(e.key);

        MapHashEntry<K, V> g = table[i];
        if (g != null)
            g.prev = e;
        e.next = g;
        table[i] = e;

        size++;
        if (size > table.length * 3 / 4)
            resize(table.length * 2);
    }

    /**
     * <p>Add the key to the map at beginning.</p>
     * <p>Assumption is that key is not yet present.</p>
     * <p>Entry is create at the top.</p>
     *
     * @param f The entry.
     */
    public void putEntryFirst(MapEntry<K, V> f) {
        throw new IllegalArgumentException("not supported");
    }

    /**
     * <p>Create a new entry.</p>
     *
     * @param key   The key.
     * @param value The value.
     * @return The entry.
     */
    public MapEntry<K, V> newEntry(K key, V value) {
        MapEntry<K, V> h = new MapHashEntry<K, V>();
        h.key = key;
        h.value = value;
        return h;
    }

    /**
     * <p>Remove an entry, but do not resize.</p>
     *
     * @param f The entry, not null.
     */
    public void removeEntry(MapEntry<K, V> f) {
        if (f == null)
            throw new NullPointerException("entry missing");
        MapHashEntry<K, V> e = (MapHashEntry<K, V>) f;

        int i = index(e.key);

        MapHashEntry<K, V> h = e.prev;
        e.prev = null;
        MapHashEntry<K, V> g = e.next;
        e.next = null;
        if (h != null) {
            h.next = g;
        } else {
            table[i] = g;
        }
        if (g != null)
            g.prev = h;

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
        return (key != null ? HashScrambler.murmur(
                key.hashCode()) &
                (table.length - 1) : 0);
    }

    /**
     * <p>Resize the hash table.</p>
     *
     * @param s The new size.
     */
    private void resize(int s) {
        MapHashEntry<K, V>[] oldtable = table;
        table = new MapHashEntry[s];

        for (int i = 0; i < oldtable.length; i++) {
            MapHashEntry<K, V> e = oldtable[i];
            while (e != null) {
                MapHashEntry<K, V> b = e;
                e = b.next;
                int j = index(b.key);

                b.prev = null;
                MapHashEntry<K, V> f = table[j];
                if (f != null)
                    f.prev = b;
                b.next = f;
                table[j] = b;
            }
        }
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
        int i = index(e.key);
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
        int i = index(e.key);
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

    /***************************************************************/
    /* Object Protocol                                             */
    /***************************************************************/

    /**
     * Reset to initial default state.
     *
     * @param capa The ahead capacity.
     */
    void reinitialize(int capa) {
        super.reinitialize(capa);
        int len = MIN_SIZE;
        while (capa > len * 3 / 4)
            len = len * 2;
        table = new MapHashEntry[len];
    }

}
