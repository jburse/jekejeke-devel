package matula.util.data;

/**
 * <p>Implementation of a linked hash map.</p>
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
public class MapHashLink<K, V> extends AbstractMap<K, V> {
    private static final int MIN_SIZE = 2;

    MapHashLinkEntry<K, V>[] table;
    MapHashLinkEntry<K, V> first;
    MapHashLinkEntry<K, V> last;

    /**
     * <p>Create a map hash link.</p>
     */
    public MapHashLink() {
        reinitialize(0);
    }

    /**
     * <p>Create a map hash link.</p>
     *
     * @param capa The ahead capacity.
     */
    public MapHashLink(int capa) {
        reinitialize(capa);
    }

    /************************************************************/
    /* Variation Points                                         */
    /************************************************************/

    /**
     * <p>Find the key in the map.</p>
     *
     * @param key The key.
     * @return The entry.
     */
    public MapEntry<K, V> getEntry(K key) {
        int i = index(key);

        MapHashLinkEntry<K, V> e;
        for (e = table[i]; e != null &&
                !(key != null ? key.equals(e.key) : null == e.key); e = e.next)
            ;

        return e;
    }

    /**
     * <p>Add the key to the map.</p>
     * <p>Assumption is that key is not yet present.</p>
     * <p>Entry is add at the bottom.</p>
     *
     * @param f The entry.
     */
    public void putEntry(MapEntry<K, V> f) {
        if (f == null)
            throw new NullPointerException("entry missing");
        MapHashLinkEntry<K, V> e = (MapHashLinkEntry<K, V>) f;

        int i = index(e.key);

        MapHashLinkEntry<K, V> g = table[i];
        if (g != null)
            g.prev = e;
        e.next = g;
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
     * <p>Create a new entry.</p>
     *
     * @param key   The key.
     * @param value The value.
     * @return The entry.
     */
    public MapEntry<K, V> newEntry(K key, V value) {
        MapEntry<K, V> h = new MapHashLinkEntry<K, V>();
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
        MapHashLinkEntry<K, V> e = (MapHashLinkEntry<K, V>) f;

        int i = index(e.key);

        MapHashLinkEntry<K, V> g = e.next;
        e.next = null;
        MapHashLinkEntry<K, V> h = e.prev;
        e.prev = null;
        if (g != null)
            g.prev = h;
        if (h != null) {
            h.next = g;
        } else {
            table[i] = g;
        }

        g = e.after;
        e.after = null;
        h = e.before;
        e.before = null;
        if (g != null) {
            g.before = h;
        } else {
            last = h;
        }
        if (h != null) {
            h.after = g;
        } else {
            first = g;
        }

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
     * <p>Add the key to the map.</p>
     * <p>Assumption is that key is not yet present.</p>
     * <p>Entry is create at the top.</p>
     *
     * @param key   The key.
     * @param value The value.
     */
    public void putFirst(K key, V value) {
        MapHashLinkEntry<K, V> e = (MapHashLinkEntry<K, V>) newEntry(key, value);

        int i = index(e.key);

        MapHashLinkEntry<K, V> g = table[i];
        if (g != null)
            g.prev = e;
        e.next = g;
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
        MapHashLinkEntry<K, V>[] oldtable = table;
        table = new MapHashLinkEntry[s];

        for (int i = 0; i < oldtable.length; i++) {
            MapHashLinkEntry<K, V> e = oldtable[i];
            while (e != null) {
                MapHashLinkEntry<K, V> b = e;
                e = b.next;
                int j = index(b.key);

                b.prev = null;
                MapHashLinkEntry<K, V> f = table[j];
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
        table = new MapHashLinkEntry[len];
    }

}
