package matula.util.data;

/**
 * <p>Implementation of a linked hash set.</p>
 * <p>Not based on some linked hash map.</p>
 * <p>Smaller initial size, and shrinks also.</p>
 * <p>No iterator provided, iterate over first and after.</p>
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
public class SetHashLink<E> extends AbstractSet<E> {
    private static final int MIN_SIZE = 2;

    SetHashLinkEntry<E>[] table;
    SetHashLinkEntry<E> first;
    SetHashLinkEntry<E> last;

    /**
     * <p>Create a set hash link.</p>
     */
    public SetHashLink() {
        reinitialize(null);
    }

    /**
     * <p>Retrieve the stored key.</p>
     *
     * @param key The search key, can be null.
     * @return The stored key or null.
     */
    public E getKey(E key) {
        int i = index(key);

        SetHashLinkEntry<E> e;
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

        SetHashLinkEntry<E> e;
        for (e = table[i]; e != null &&
                !(key != null ? key.equals(e.key) : null == e.key); e = e.next)
            ;

        return e;
    }

    /**
     * <p>Add key to the set at end.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key The key, can be null.
     */
    public void add(E key) {
        int i = index(key);

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
     * <p>Add key to the set at beginning.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key The key.
     */
    public void addFirst(E key) {
        int i = index(key);

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
        int i = index(key);

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
        table = new SetHashLinkEntry[s];

        SetHashLinkEntry<E> e = first;
        while (e != null) {
            int i = index(e.key);
            e.next = table[i];
            table[i] = e;
            e = e.after;
        }
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

    /***************************************************************/
    /* Object Protocol                                             */
    /***************************************************************/

    /**
     * Reset to initial default state.  Called by clone and readObject.
     *
     * @param other The other abstract set, or null.
     */
    void reinitialize(AbstractSet other) {
        super.reinitialize(other);
        int len = MIN_SIZE;
        while (other != null && other.size() > len * 3 / 4)
            len = len * 2;
        table = new SetHashLinkEntry[len];
    }

}
