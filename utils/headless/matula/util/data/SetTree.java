package matula.util.data;

import java.util.Comparator;

/**
 * <p>Implementation of a tree map.</p>
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
public final class SetTree<E> extends AbstractSet<E> {
    private final Comparator<E> comparator;
    SetTreeEntry<E> root;

    /**
     * <p>Create a map tree.</p>
     *
     * @param c The comparator.
     */
    public SetTree(Comparator<E> c) {
        comparator = c;
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
        SetTreeEntry<E> p = root;
        while (p != null) {
            int k = comparator.compare(key, p.key);
            if (k < 0) {
                p = p.left;
            } else if (k > 0) {
                p = p.right;
            } else {
                return p;
            }
        }
        return null;
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
        SetTreeEntry<E> e = (SetTreeEntry<E>) f;

        SetTreeEntry<E> p = root;
        SetTreeEntry<E> b = null;
        int k = 0;
        while (p != null) {
            b = p;
            k = comparator.compare(e.key, p.key);
            if (k < 0) {
                p = p.left;
            } else if (k > 0) {
                p = p.right;
            } else {
                throw new IllegalStateException("duplicate key");
            }
        }

        e.parent = b;

        if (b == null) {
            root = e;
        } else {
            if (k < 0) {
                b.left = e;
            } else {
                b.right = e;
            }
            fixAfterInsertion(e);
        }
        size++;
    }

    /**
     * <p>Create a new entry.</p>
     *
     * @param key The key.
     * @return The entry.
     */
    public SetEntry<E> newEntry(E key) {
        return new SetTreeEntry<E>(key);
    }

    /**
     * <p>Add key to the set at beginning.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key The key, can be null.
     */
    public void addFirst(E key) {
        throw new IllegalArgumentException("not supported");
    }

    /**
     * <p>Remove the entry from the set.</p>
     *
     * @param f The entry.
     */
    public void removeEntry(SetEntry<E> f) {
        if (f == null)
            throw new NullPointerException("entry missing");
        SetTreeEntry<E> p = (SetTreeEntry<E>) f;

        // If strictly internal, copy successor's element to p and then make p
        // point to successor.
        if (p.left != null && p.right != null) {
            SetTreeEntry<E> s = (SetTreeEntry<E>) successor(p);
            p.key = s.key;
            p = s;
        } // p has 2 children

        // Start fixup at replacement node, if it exists.
        SetTreeEntry<E> replacement = (p.left != null ? p.left : p.right);

        if (replacement != null) {
            // Link replacement to parent
            replacement.parent = p.parent;
            if (p.parent == null)
                root = replacement;
            else if (p == p.parent.left)
                p.parent.left = replacement;
            else
                p.parent.right = replacement;

            // Null out links so they are OK to use by fixAfterDeletion.
            p.left = p.right = p.parent = null;

            // Fix replacement
            if (p.color == SetTreeEntry.BLACK)
                fixAfterDeletion(replacement);
        } else if (p.parent == null) { // return if we are the only node.
            root = null;
        } else { //  No children. Use self as phantom replacement and unlink.
            if (p.color == SetTreeEntry.BLACK)
                fixAfterDeletion(p);

            if (p.parent != null) {
                if (p == p.parent.left)
                    p.parent.left = null;
                else if (p == p.parent.right)
                    p.parent.right = null;
                p.parent = null;
            }
        }
        size--;
    }

    /**
     * <p>Resize after bulk delete.</p>
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
        SetTreeEntry<E> p = root;
        if (p != null)
            while (p.right != null)
                p = p.right;
        return p;
    }

    /**
     * <p>Retrieve the first entry.</p>
     *
     * @return The first entry, can be null.
     */
    public SetEntry<E> getFirstEntry() {
        SetTreeEntry<E> p = root;
        if (p != null)
            while (p.left != null)
                p = p.left;
        return p;
    }

    /**
     * <p>Retrieve the predecessor for a given entry.</p>
     *
     * @param s The entry, not null.
     * @return The predecessor, can be null.
     */
    public SetEntry<E> predecessor(SetEntry<E> s) {
        SetTreeEntry<E> t = (SetTreeEntry<E>) s;
        if (t.left != null) {
            SetTreeEntry<E> p = t.left;
            while (p.right != null)
                p = p.right;
            return p;
        } else {
            SetTreeEntry<E> p = t.parent;
            SetTreeEntry<E> ch = t;
            while (p != null && ch == p.left) {
                ch = p;
                p = p.parent;
            }
            return p;
        }
    }

    /**
     * <p>Retrieve the successor for a given entry.</p>
     *
     * @param s The entry, not null.
     * @return The successor, can be null.
     */
    public SetEntry<E> successor(SetEntry<E> s) {
        SetTreeEntry<E> t = (SetTreeEntry<E>) s;
        if (t.right != null) {
            SetTreeEntry<E> p = t.right;
            while (p.left != null)
                p = p.left;
            return p;
        } else {
            SetTreeEntry<E> p = t.parent;
            SetTreeEntry<E> ch = t;
            while (p != null && ch == p.right) {
                ch = p;
                p = p.parent;
            }
            return p;
        }
    }

    /**
     * <p>Clear the set.</p>
     */
    public void clear() {
        root = null;
        size = 0;
    }

    /**********************************************************************/
    /* Some Helpers                                                       */

    /**********************************************************************/

    // Copied from Java Tree
    private void fixAfterInsertion(SetTreeEntry<E> x) {
        x.color = SetTreeEntry.RED;

        while (x != null && x != root && x.parent.color == SetTreeEntry.RED) {
            if (parentOf(x) == leftOf(parentOf(parentOf(x)))) {
                SetTreeEntry<E> y = rightOf(parentOf(parentOf(x)));
                if (colorOf(y) == SetTreeEntry.RED) {
                    setColor(parentOf(x), SetTreeEntry.BLACK);
                    setColor(y, SetTreeEntry.BLACK);
                    setColor(parentOf(parentOf(x)), SetTreeEntry.RED);
                    x = parentOf(parentOf(x));
                } else {
                    if (x == rightOf(parentOf(x))) {
                        x = parentOf(x);
                        rotateLeft(x);
                    }
                    setColor(parentOf(x), SetTreeEntry.BLACK);
                    setColor(parentOf(parentOf(x)), SetTreeEntry.RED);
                    rotateRight(parentOf(parentOf(x)));
                }
            } else {
                SetTreeEntry<E> y = leftOf(parentOf(parentOf(x)));
                if (colorOf(y) == SetTreeEntry.RED) {
                    setColor(parentOf(x), SetTreeEntry.BLACK);
                    setColor(y, SetTreeEntry.BLACK);
                    setColor(parentOf(parentOf(x)), SetTreeEntry.RED);
                    x = parentOf(parentOf(x));
                } else {
                    if (x == leftOf(parentOf(x))) {
                        x = parentOf(x);
                        rotateRight(x);
                    }
                    setColor(parentOf(x), SetTreeEntry.BLACK);
                    setColor(parentOf(parentOf(x)), SetTreeEntry.RED);
                    rotateLeft(parentOf(parentOf(x)));
                }
            }
        }
        root.color = SetTreeEntry.BLACK;
    }

    // Copied from Java Tree
    private void fixAfterDeletion(SetTreeEntry<E> x) {
        while (x != root && colorOf(x) == SetTreeEntry.BLACK) {
            if (x == leftOf(parentOf(x))) {
                SetTreeEntry<E> sib = rightOf(parentOf(x));

                if (colorOf(sib) == SetTreeEntry.RED) {
                    setColor(sib, SetTreeEntry.BLACK);
                    setColor(parentOf(x), SetTreeEntry.RED);
                    rotateLeft(parentOf(x));
                    sib = rightOf(parentOf(x));
                }

                if (colorOf(leftOf(sib)) == SetTreeEntry.BLACK &&
                        colorOf(rightOf(sib)) == SetTreeEntry.BLACK) {
                    setColor(sib, SetTreeEntry.RED);
                    x = parentOf(x);
                } else {
                    if (colorOf(rightOf(sib)) == SetTreeEntry.BLACK) {
                        setColor(leftOf(sib), SetTreeEntry.BLACK);
                        setColor(sib, SetTreeEntry.RED);
                        rotateRight(sib);
                        sib = rightOf(parentOf(x));
                    }
                    setColor(sib, colorOf(parentOf(x)));
                    setColor(parentOf(x), SetTreeEntry.BLACK);
                    setColor(rightOf(sib), SetTreeEntry.BLACK);
                    rotateLeft(parentOf(x));
                    x = root;
                }
            } else { // symmetric
                SetTreeEntry<E> sib = leftOf(parentOf(x));

                if (colorOf(sib) == SetTreeEntry.RED) {
                    setColor(sib, SetTreeEntry.BLACK);
                    setColor(parentOf(x), SetTreeEntry.RED);
                    rotateRight(parentOf(x));
                    sib = leftOf(parentOf(x));
                }

                if (colorOf(rightOf(sib)) == SetTreeEntry.BLACK &&
                        colorOf(leftOf(sib)) == SetTreeEntry.BLACK) {
                    setColor(sib, SetTreeEntry.RED);
                    x = parentOf(x);
                } else {
                    if (colorOf(leftOf(sib)) == SetTreeEntry.BLACK) {
                        setColor(rightOf(sib), SetTreeEntry.BLACK);
                        setColor(sib, SetTreeEntry.RED);
                        rotateLeft(sib);
                        sib = leftOf(parentOf(x));
                    }
                    setColor(sib, colorOf(parentOf(x)));
                    setColor(parentOf(x), SetTreeEntry.BLACK);
                    setColor(leftOf(sib), SetTreeEntry.BLACK);
                    rotateRight(parentOf(x));
                    x = root;
                }
            }
        }

        setColor(x, SetTreeEntry.BLACK);
    }

    // Copied from Java Tree
    private void rotateLeft(SetTreeEntry<E> p) {
        if (p != null) {
            SetTreeEntry<E> r = p.right;
            p.right = r.left;
            if (r.left != null)
                r.left.parent = p;
            r.parent = p.parent;
            if (p.parent == null)
                root = r;
            else if (p.parent.left == p)
                p.parent.left = r;
            else
                p.parent.right = r;
            r.left = p;
            p.parent = r;
        }
    }

    // Copied from Java Tree
    private void rotateRight(SetTreeEntry<E> p) {
        if (p != null) {
            SetTreeEntry<E> l = p.left;
            p.left = l.right;
            if (l.right != null) l.right.parent = p;
            l.parent = p.parent;
            if (p.parent == null)
                root = l;
            else if (p.parent.right == p)
                p.parent.right = l;
            else p.parent.left = l;
            l.right = p;
            p.parent = l;
        }
    }

    // Copied from Java Tree
    private static <E> boolean colorOf(SetTreeEntry<E> p) {
        return (p == null ? SetTreeEntry.BLACK : p.color);
    }

    // Copied from Java Tree
    private static <E> void setColor(SetTreeEntry<E> p, boolean c) {
        if (p != null)
            p.color = c;
    }

    // Copied from Java Tree
    private static <E> SetTreeEntry<E> parentOf(SetTreeEntry<E> p) {
        return (p == null ? null : p.parent);
    }

    // Copied from Java Tree
    private static <E> SetTreeEntry<E> leftOf(SetTreeEntry<E> p) {
        return (p == null) ? null : p.left;
    }

    // Copied from Java Tree
    private static <E> SetTreeEntry<E> rightOf(SetTreeEntry<E> p) {
        return (p == null) ? null : p.right;
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
        root = null;
    }

}
