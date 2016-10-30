package matula.util.data;

import java.util.Comparator;

/**
 * <p>Implementation of a tree map.</p>
 *
 * @author Copyright 2012-2015, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.6 (a fast and small prolog interpreter)
 */
public final class MapTree<K, V> extends AbstractMap<K, V> {
    final Comparator<K> comparator;
    MapTreeEntry<K, V> root;
    public int size;

    /**
     * <p>Create a map tree.</p>
     *
     * @param c The comparator.
     */
    public MapTree(Comparator<K> c) {
        comparator = c;
    }

    /**
     * <p>Retrieve a value for a key.</p>
     *
     * @param key The key.
     * @return The value.
     */
    public V get(K key) {
        MapTreeEntry<K, V> p = root;
        while (p != null) {
            int k = comparator.compare(key, p.key);
            if (k < 0) {
                p = p.left;
            } else if (k > 0) {
                p = p.right;
            } else {
                return p.value;
            }
        }
        return null;
    }

    /**
     * <p>Retrieve an entry for a key.</p>
     *
     * @param key The key.
     * @return The entry.
     */
    public MapEntry<K, V> getEntry(K key) {
        MapTreeEntry<K, V> p = root;
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
     * <p>Set a value for a key.</p>
     * <p>Assumption is that key does not yet a value.</p>
     *
     * @param key   The key.
     * @param value The value.
     * @return The new enry.
     */
    public MapEntry<K, V> put(K key, V value) {
        MapTreeEntry<K, V> p = root;
        MapTreeEntry<K, V> b = null;
        int k = 0;
        while (p != null) {
            b = p;
            k = comparator.compare(key, p.key);
            if (k < 0) {
                p = p.left;
            } else if (k > 0) {
                p = p.right;
            } else {
                throw new IllegalStateException("duplicate key");
            }
        }
        MapTreeEntry<K, V> e = new MapTreeEntry<K, V>(key, value, b);
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
        return e;
    }

    /**
     * <p>Remove the key from the map.</p>
     *
     * @param key The key.
     */

    public void remove(K key) {
        MapTreeEntry<K, V> p = (MapTreeEntry<K, V>) getEntry(key);
        if (p == null)
            return;

        // If strictly internal, copy successor's element to p and then make p
        // point to successor.
        if (p.left != null && p.right != null) {
            MapTreeEntry<K, V> s = (MapTreeEntry<K, V>)successor(p);
            p.key = s.key;
            p.value = s.value;
            p = s;
        } // p has 2 children

        // Start fixup at replacement node, if it exists.
        MapTreeEntry<K, V> replacement = (p.left != null ? p.left : p.right);

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
            if (p.color == MapTreeEntry.BLACK)
                fixAfterDeletion(replacement);
        } else if (p.parent == null) { // return if we are the only node.
            root = null;
        } else { //  No children. Use self as phantom replacement and unlink.
            if (p.color == MapTreeEntry.BLACK)
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
     * <p>Retrieve the last entry.</p>
     *
     * @return The last entry, can be null.
     */
    public MapEntry<K, V> getLastEntry() {
        MapTreeEntry<K, V> p = root;
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
    public MapEntry<K, V> getFirstEntry() {
        MapTreeEntry<K, V> p = root;
        if (p != null)
            while (p.left != null)
                p = p.left;
        return p;
    }

    /**
     * <p>Retrieve the preceeding entry of a given entry.</p>
     *
     * @param s The entry, not null.
     * @return The preceeding entry.
     */
    public MapEntry<K, V> predecessor(MapEntry<K, V> s) {
        MapTreeEntry<K, V> t = (MapTreeEntry<K, V>) s;
        if (t.left != null) {
            MapTreeEntry<K, V> p = t.left;
            while (p.right != null)
                p = p.right;
            return p;
        } else {
            MapTreeEntry<K, V> p = t.parent;
            MapTreeEntry<K, V> ch = t;
            while (p != null && ch == p.left) {
                ch = p;
                p = p.parent;
            }
            return p;
        }
    }

    /**
     * <p>Retrieve the successor entry of a given entry.</p>
     *
     * @param s The entry.
     * @return The succeeding entry.
     */
    public MapEntry<K, V> successor(MapEntry<K, V> s) {
        MapTreeEntry<K, V> t = (MapTreeEntry<K, V>) s;
        if (t.right != null) {
            MapTreeEntry<K, V> p = t.right;
            while (p.left != null)
                p = p.left;
            return p;
        } else {
            MapTreeEntry<K, V> p = t.parent;
            MapTreeEntry<K, V> ch = t;
            while (p != null && ch == p.right) {
                ch = p;
                p = p.parent;
            }
            return p;
        }
    }

    /**
     * <p>Clear the map.</p>
     */
    public void clear() {
        root = null;
    }

    /**********************************************************************/
    /* Some Helpers                                                       */
    /**********************************************************************/

    // Copied from Java Tree
    private void fixAfterInsertion(MapTreeEntry<K, V> x) {
        x.color = MapTreeEntry.RED;

        while (x != null && x != root && x.parent.color == MapTreeEntry.RED) {
            if (parentOf(x) == leftOf(parentOf(parentOf(x)))) {
                MapTreeEntry<K, V> y = rightOf(parentOf(parentOf(x)));
                if (colorOf(y) == MapTreeEntry.RED) {
                    setColor(parentOf(x), MapTreeEntry.BLACK);
                    setColor(y, MapTreeEntry.BLACK);
                    setColor(parentOf(parentOf(x)), MapTreeEntry.RED);
                    x = parentOf(parentOf(x));
                } else {
                    if (x == rightOf(parentOf(x))) {
                        x = parentOf(x);
                        rotateLeft(x);
                    }
                    setColor(parentOf(x), MapTreeEntry.BLACK);
                    setColor(parentOf(parentOf(x)), MapTreeEntry.RED);
                    rotateRight(parentOf(parentOf(x)));
                }
            } else {
                MapTreeEntry<K, V> y = leftOf(parentOf(parentOf(x)));
                if (colorOf(y) == MapTreeEntry.RED) {
                    setColor(parentOf(x), MapTreeEntry.BLACK);
                    setColor(y, MapTreeEntry.BLACK);
                    setColor(parentOf(parentOf(x)), MapTreeEntry.RED);
                    x = parentOf(parentOf(x));
                } else {
                    if (x == leftOf(parentOf(x))) {
                        x = parentOf(x);
                        rotateRight(x);
                    }
                    setColor(parentOf(x), MapTreeEntry.BLACK);
                    setColor(parentOf(parentOf(x)), MapTreeEntry.RED);
                    rotateLeft(parentOf(parentOf(x)));
                }
            }
        }
        root.color = MapTreeEntry.BLACK;
    }

    // Copied from Java Tree
    private void fixAfterDeletion(MapTreeEntry<K, V> x) {
        while (x != root && colorOf(x) == MapTreeEntry.BLACK) {
            if (x == leftOf(parentOf(x))) {
                MapTreeEntry<K, V> sib = rightOf(parentOf(x));

                if (colorOf(sib) == MapTreeEntry.RED) {
                    setColor(sib, MapTreeEntry.BLACK);
                    setColor(parentOf(x), MapTreeEntry.RED);
                    rotateLeft(parentOf(x));
                    sib = rightOf(parentOf(x));
                }

                if (colorOf(leftOf(sib)) == MapTreeEntry.BLACK &&
                        colorOf(rightOf(sib)) == MapTreeEntry.BLACK) {
                    setColor(sib, MapTreeEntry.RED);
                    x = parentOf(x);
                } else {
                    if (colorOf(rightOf(sib)) == MapTreeEntry.BLACK) {
                        setColor(leftOf(sib), MapTreeEntry.BLACK);
                        setColor(sib, MapTreeEntry.RED);
                        rotateRight(sib);
                        sib = rightOf(parentOf(x));
                    }
                    setColor(sib, colorOf(parentOf(x)));
                    setColor(parentOf(x), MapTreeEntry.BLACK);
                    setColor(rightOf(sib), MapTreeEntry.BLACK);
                    rotateLeft(parentOf(x));
                    x = root;
                }
            } else { // symmetric
                MapTreeEntry<K, V> sib = leftOf(parentOf(x));

                if (colorOf(sib) == MapTreeEntry.RED) {
                    setColor(sib, MapTreeEntry.BLACK);
                    setColor(parentOf(x), MapTreeEntry.RED);
                    rotateRight(parentOf(x));
                    sib = leftOf(parentOf(x));
                }

                if (colorOf(rightOf(sib)) == MapTreeEntry.BLACK &&
                        colorOf(leftOf(sib)) == MapTreeEntry.BLACK) {
                    setColor(sib, MapTreeEntry.RED);
                    x = parentOf(x);
                } else {
                    if (colorOf(leftOf(sib)) == MapTreeEntry.BLACK) {
                        setColor(rightOf(sib), MapTreeEntry.BLACK);
                        setColor(sib, MapTreeEntry.RED);
                        rotateLeft(sib);
                        sib = leftOf(parentOf(x));
                    }
                    setColor(sib, colorOf(parentOf(x)));
                    setColor(parentOf(x), MapTreeEntry.BLACK);
                    setColor(leftOf(sib), MapTreeEntry.BLACK);
                    rotateRight(parentOf(x));
                    x = root;
                }
            }
        }

        setColor(x, MapTreeEntry.BLACK);
    }

    // Copied from Java Tree
    private void rotateLeft(MapTreeEntry<K, V> p) {
        if (p != null) {
            MapTreeEntry<K, V> r = p.right;
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
    private void rotateRight(MapTreeEntry<K, V> p) {
        if (p != null) {
            MapTreeEntry<K, V> l = p.left;
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
    private static <K, V> boolean colorOf(MapTreeEntry<K, V> p) {
        return (p == null ? MapTreeEntry.BLACK : p.color);
    }

    // Copied from Java Tree
    private static <K, V> void setColor(MapTreeEntry<K, V> p, boolean c) {
        if (p != null)
            p.color = c;
    }

    // Copied from Java Tree
    private static <K, V> MapTreeEntry<K, V> parentOf(MapTreeEntry<K, V> p) {
        return (p == null ? null : p.parent);
    }

    // Copied from Java Tree
    private static <K, V> MapTreeEntry<K, V> leftOf(MapTreeEntry<K, V> p) {
        return (p == null) ? null : p.left;
    }

    // Copied from Java Tree
    private static <K, V> MapTreeEntry<K, V> rightOf(MapTreeEntry<K, V> p) {
        return (p == null) ? null : p.right;
    }

}