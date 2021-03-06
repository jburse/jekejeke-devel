package matula.util.data;

/**
 * <p>The base class for the sets.</p>
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
public abstract class AbstractSet<E>
        extends AbstractList<E> {

    /************************************************************/
    /* Derived Methods                                          */
    /************************************************************/

    /**
     * <p>Find the stored key.</p>
     *
     * @param key The search key, can be null.
     * @return The stored key or null.
     */
    public final E getKey(E key) {
        SetEntry<E> e = getEntry(key);
        return (e != null ? e.value : null);
    }

    /**
     * <p>Add key to the set at end.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key The key, can be null.
     * @return The new entry.
     */
    public final SetEntry<E> put(E key) {
        SetEntry<E> e = newEntry(key);
        putEntry(e);
        return e;
    }

    /**
     * <p>Add key to the set at end.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key The key, can be null.
     */
    public final void add(E key) {
        SetEntry<E> e = newEntry(key);
        putEntry(e);
    }

    /**
     * <p>Add key to the set at beginning.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key The key, can be null.
     */
    public final void addFirst(E key) {
        SetEntry<E> e = newEntry(key);
        putEntryFirst(e);
    }

    /**
     * <p>Remove the key from the set.</p>
     *
     * @param key The key.
     */
    public final void remove(E key) {
        SetEntry<E> e = getEntry(key);
        if (e == null)
            return;
        removeEntry(e);
        resize();
    }

    /**
     * <p>Copy the hash map entries to an array.</p>
     *
     * @param target The array.
     * @param pos    The start index.
     */
    public final void toArray(E[] target, int pos) {
        for (SetEntry<E> entry = getFirstEntry();
             entry != null; entry = successor(entry)) {
            target[pos] = entry.value;
            pos++;
        }
    }

    /**
     * <ü>Copy elements to a list.</ü>
     *
     * @param list The list.
     */
    public void toList(AbstractList<E> list) {
        for (SetEntry<E> entry = getFirstEntry();
             entry != null; entry = successor(entry)) {
            list.add(entry.value);
        }
    }

    /**
     * <ü>Copy unique elements to a list.</ü>
     *
     * @param list The list.
     */
    public void toListUnique(AbstractList<E> list) {
        for (SetEntry<E> entry = getFirstEntry();
             entry != null; entry = successor(entry)) {
            if (list.getKey(entry.value) == null)
                list.add(entry.value);
        }
    }

    /************************************************************/
    /* Variation Points                                         */
    /************************************************************/

    /**
     * <p>Find the entry in the set.</p>
     *
     * @param key The key.
     * @return The entry, or null.
     */
    public abstract SetEntry<E> getEntry(E key);

    /**
     * <p>Add entry to the set at end.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param f The entry.
     */
    public abstract void putEntry(SetEntry<E> f);

    /**
     * <p>Add entry to the set at beginning.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param f The entry.
     */
    public abstract void putEntryFirst(SetEntry<E> f);

    /**
     * <p>Create a new entry.</p>
     *
     * @param key The key.
     * @return The entry.
     */
    public abstract SetEntry<E> newEntry(E key);

    /**
     * <p>Remove the entry from the set.</p>
     *
     * @param f The entry.
     */
    public abstract void removeEntry(SetEntry<E> f);

    /**
     * <p>Resize after remove entry.</p>
     */
    public abstract void resize();

    /**
     * <p>Retrieve the last entry.</p>
     *
     * @return The last entry, can be null.
     */
    public abstract SetEntry<E> getLastEntry();

    /**
     * <p>Retrieve the first entry.</p>
     *
     * @return The first entry, can be null.
     */
    public abstract SetEntry<E> getFirstEntry();

    /**
     * <p>Retrieve the predecessor for a given entry.</p>
     *
     * @param s The entry, not null.
     * @return The predecessor, can be null.
     */
    public abstract SetEntry<E> predecessor(SetEntry<E> s);

    /**
     * <p>Retrieve the successor for a given entry.</p>
     *
     * @param s The entry, not null.
     * @return The successor, can be null.
     */
    public abstract SetEntry<E> successor(SetEntry<E> s);

    /***************************************************************/
    /* Object Protocol                                             */
    /***************************************************************/

    /**
     * <p>Returns a string representation of this list array.</p>
     *
     * @return A string representation of this list array.
     */
    public String toString() {
        SetEntry<E> entry = getFirstEntry();
        if (entry == null)
            return "[]";
        StringBuilder buf = new StringBuilder();
        buf.append("[");
        buf.append(entry.value);
        entry = successor(entry);
        while (entry != null) {
            buf.append(",");
            buf.append(entry.value);
            entry = successor(entry);
        }
        buf.append("]");
        return buf.toString();
    }

    /**
     * <p>Create a shallow copy.</p>
     *
     * @return The shallow copy.
     */
    public Object clone() {
        AbstractSet<E> res = (AbstractSet<E>) super.clone();
        for (SetEntry<E> entry = getFirstEntry();
             entry != null; entry = successor(entry))
            res.add(entry.value);
        return res;
    }

}
