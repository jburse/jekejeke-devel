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
public final class SetHash<E> extends AbstractSet<E> {
    private static final int MIN_SIZE = 2;

    SetHashEntry<E>[] table;

    /**
     * <p>Create a set hash.</p>
     */
    public SetHash() {
        reinitialize();
    }

    /**
     * <p>Retrieve the stored key.</p>
     *
     * @param key The search key, can be null.
     * @return The stored key or null.
     */
    public E getKey(E key) {
        int i = index(key);

        SetHashEntry<E> e;
        for (e = table[i]; e != null &&
                !(key != null ? key.equals(e.key) : null == e.key); e = e.next)
            ;

        return (e != null ? e.key : null);
    }

    /**
     * <p>Find the entry in the set.</p>
     *
     * @param key The key.
     * @return The entry, or null.
     */
    public SetEntry<E> getEntry(E key) {
        int i = index(key);

        SetHashEntry<E> e;
        for (e = table[i]; e != null &&
                !(key != null ? key.equals(e.key) : null == e.key); e = e.next)
            ;

        return e;
    }

    /**
     * <p>Add key to the set.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key The key, can be null.
     */
    public void add(E key) {
        int i = index(key);

        SetHashEntry<E> e = new SetHashEntry<E>(key);
        SetHashEntry<E> f = table[i];
        if (f != null)
            f.prev = e;
        e.next = f;
        table[i] = e;
        size++;

        if (size > table.length * 3 / 4)
            resize(table.length * 2);
    }

    /**
     * <p>Remove the key from the set.</p>
     *
     * @param key The key.
     */
    public void remove(E key) {
        int i = index(key);

        SetHashEntry<E> e;
        for (e = table[i]; e != null &&
                !(key != null ? key.equals(e.key) : null == e.key); e = e.next)
            ;

        if (e == null)
            return;

        SetHashEntry<E> f = e.prev;
        SetHashEntry<E> g = e.next;
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
     * <p>Compute the index of a key.</p>
     *
     * @param key The key.
     * @return The index.
     */
    public int index(E key) {
        return (key != null ? HashScrambler.murmur(key.hashCode()) &
                (table.length - 1) : 0);
    }

    /**
     * <p>Resize the hash table.</p>
     *
     * @param s The new size.
     */
    private void resize(int s) {
        SetHashEntry<E>[] oldtable = table;
        table = new SetHashEntry[s];

        for (int i = 0; i < oldtable.length; i++) {
            SetHashEntry<E> e = oldtable[i];
            while (e != null) {
                SetHashEntry<E> b = e;
                e = b.next;
                int j = index(b.key);

                b.prev = null;
                SetHashEntry<E> f = table[j];
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
    public SetEntry<E> getLastEntry() {
        return getPrevEntry(table.length - 1);
    }

    /**
     * <p>Retrieve the first entry.</p>
     *
     * @return The first entry, can be null.
     */
    public SetEntry<E> getFirstEntry() {
        return getNextEntry(0);
    }

    /**
     * <p>Retrieve the predecessor for a given entry.</p>
     *
     * @param s The entry, not null.
     * @return The predecessor, can be null.
     */
    public SetEntry<E> predecessor(SetEntry<E> s) {
        SetHashEntry<E> e = (SetHashEntry<E>) s;
        SetHashEntry<E> h = e.next;
        if (h != null)
            return h;
        int i = index(e.key);
        return getPrevEntry(i - 1);
    }

    /**
     * <p>Retrieve the successor for a given entry.</p>
     *
     * @param s The entry, not null.
     * @return The successor, can be null.
     */
    public SetEntry<E> successor(SetEntry<E> s) {
        SetHashEntry<E> e = (SetHashEntry<E>) s;
        SetHashEntry<E> h = e.next;
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
    private SetEntry<E> getPrevEntry(int start) {
        for (int i = start; i >= 0; i--) {
            SetHashEntry<E> e = table[i];
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
    private SetEntry<E> getNextEntry(int start) {
        for (int i = start; i < table.length; i++) {
            SetHashEntry<E> e = table[i];
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

    /***************************************************************/
    /* Object Protocol                                             */
    /***************************************************************/

    /**
     * Reset to initial default state.  Called by clone and readObject.
     */
    void reinitialize() {
        super.reinitialize();
        table = new SetHashEntry[MIN_SIZE];
    }

}
