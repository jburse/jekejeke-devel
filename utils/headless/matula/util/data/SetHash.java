package matula.util.data;

/**
 * <p>Implementation of a hash set.</p>
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
public final class SetHash<K> extends AbstractSet<K> {
    private static final int MIN_SIZE = 2;

    public SetHashEntry<K>[] table = new SetHashEntry[MIN_SIZE];
    public int size;

    /**
     * <p>Retrieve the stored key.</p>
     *
     * @param key The search key, can be null.
     * @return The stored key or null.
     */
    public K getKey(K key) {
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);

        SetHashEntry<K> e;
        for (e = table[i]; e != null &&
                !(key != null ? key.equals(e.key) : null == e.key); e = e.next)
            ;

        if (e == null)
            return null;
        return e.key;
    }

    /**
     * <p>Add key to the set.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key The key, can be null.
     */
    public void putKey(K key) {
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);

        SetHashEntry<K> e = new SetHashEntry<K>(key);
        e.next = table[i];
        table[i] = e;

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

        SetHashEntry<K> e;
        SetHashEntry<K> b = null;
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

        if (size < table.length / 4 && table.length / 2 > MIN_SIZE)
            resize(table.length / 2);
    }

    /**
     * <p>Resize after bulk delete.</p>
     */
    public void resize() {
        int len = table.length;
        while (size < len / 4 && len / 2 > MIN_SIZE)
            len = len / 2;
        if (len != table.length)
            resize(len);
    }

    /**
     * <p>Resize the hash table.</p>
     *
     * @param s The new size.
     */
    private void resize(int s) {
        SetHashEntry<K>[] newtable = new SetHashEntry[s];

        for (int i = 0; i < table.length; i++) {
            SetHashEntry<K> e = table[i];
            while (e != null) {
                SetHashEntry<K> b = e;
                e = b.next;
                Object key = b.key;
                int j = (key != null ? HashScrambler.murmur(key.hashCode()) &
                        (s - 1) : 0);
                b.next = newtable[j];
                newtable[j] = b;
            }
        }

        table = newtable;
    }

    /**
     * <p>Copy the hash set entries to an array.</p>
     *
     * @param target The array.
     */
    public void toArray(K[] target) {
        int pos = 0;
        for (int i = 0; i < table.length; i++) {
            SetHashEntry<K> e = table[i];
            while (e != null) {
                target[pos] = e.key;
                pos++;
                e = e.next;
            }
        }
    }

    /**
     * <p>Retrieve the last entry.</p>
     *
     * @return The last entry.
     */
    public SetEntry<K> getLastEntry() {
        return getLastEntry(table.length - 1);
    }

    /**
     * <p>Retrieve the predecessor for a given entry.</p>
     *
     * @param t The entry.
     * @return The predecessor of the entry.
     */
    public SetEntry<K> predecessor(SetEntry<K> t) {
        SetHashEntry<K> e = (SetHashEntry<K>) t;
        SetHashEntry<K> h = e.next;
        if (h != null)
            return h;
        Object key = e.key;
        int i = (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);
        return getLastEntry(i - 1);
    }

    /**
     * <p>Retrieve the last entry.</p>
     *
     * @return The last entry.
     */
    private SetEntry<K> getLastEntry(int start) {
        for (int i = start; i >= 0; i--) {
            SetHashEntry<K> e = table[i];
            if (e != null)
                return e;
        }
        return null;
    }

    /**
     * <p>Clear the set.</p>
     */
    public void clear() {
        if (size == 0)
            return;
        if (table.length != MIN_SIZE) {
            table = new SetHashEntry[MIN_SIZE];
        } else {
            int n = Math.min(size, MIN_SIZE);
            for (int i = 0; i < n; i++)
                table[i] = null;
        }
        size = 0;
    }

}
