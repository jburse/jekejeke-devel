package matula.util.misc;

import matula.util.data.AbstractSet;
import matula.util.data.SetEntry;

/**
 * <p>Implementation of a linked list set.</p>
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SetLink<E> extends AbstractSet<E> {
    SetLinkEntry<E> first;
    SetLinkEntry<E> last;

    /**
     * <p>Create a set hash link.</p>
     */
    public SetLink() {
        reinitialize(0);
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
    public SetEntry<E> getEntry(E key) {
        throw new IllegalArgumentException("not supported");
    }

    /**
     * <p>Add entry to the set at end.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param f The entry.
     */
    public void putEntry(SetEntry<E> f) {
        if (f == null)
            throw new NullPointerException("entry missing");
        SetLinkEntry<E> e = (SetLinkEntry<E>) f;

        e.before = last;
        if (last != null) {
            last.after = e;
        } else {
            first = e;
        }
        last = e;

        size++;
    }

    /**
     * <p>Add key to the set at beginning.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param f The entry.
     */
    public void putEntryFirst(SetEntry<E> f) {
        if (f == null)
            throw new NullPointerException("entry missing");
        SetLinkEntry<E> e = (SetLinkEntry<E>) f;


        e.after = first;
        if (first != null) {
            first.before = e;
        } else {
            last = e;
        }
        first = e;

        size++;
    }

    /**
     * <p>Create a new entry.</p>
     *
     * @param key The key.
     * @return The entry.
     */
    public SetEntry<E> newEntry(E key) {
        SetEntry<E> h = new SetLinkEntry<>();
        h.value = key;
        return h;
    }

    /**
     * <p>Remove the entry from the set.</p>
     *
     * @param f The entry.
     */
    public void removeEntry(SetEntry<E> f) {
        if (f == null)
            throw new NullPointerException("entry missing");
        SetLinkEntry<E> e = (SetLinkEntry<E>) f;

        SetLinkEntry<E> g = e.after;
        e.after = null;
        SetLinkEntry<E> h = e.before;
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
        /* do nothing */
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
        SetLinkEntry<E> t = (SetLinkEntry<E>) s;
        return t.before;
    }

    /**
     * <p>Retrieve the successor for a given entry.</p>
     *
     * @param s The entry, not null.
     * @return The successor, can be null.
     */
    public SetEntry<E> successor(SetEntry<E> s) {
        SetLinkEntry<E> t = (SetLinkEntry<E>) s;
        return t.after;
    }

    /**
     * <p>Clear the set.</p>
     */
    public void clear() {
        if (size == 0)
            return;
        size = 0;
        first = null;
        last = null;
    }

}