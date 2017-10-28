package matula.util.data;

/**
 * <p>This class provides a list array. The advantage over the usual Java
 * class ArrayList is no mod count, no bounds check and more important
 * automatic shrinking. The list array is implemented by an object array
 * plus a size field that indicates the size which might differ from the
 * length of the object array:
 * </p>
 * <pre>
 *           +----------+-----------------+------------+
 *           | table[0] |      ...        | table[n-1] |
 *           +----------+-----------------+------------+
 *           < ---------- table.length = n ----------- >
 *           < ----------- size --------- >
 * </pre>
 * <p>Each operation automatically resizes. Its also possible to access
 * the object array and size field directly and perform some bulk
 * operation possibly update the size field and then call resize. Each
 * operation is not synchronized. Application code needs to synchronize on
 * its own if necessary and might execute multiple operations insided its
 * synchronized block.
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
public class ListArray<E> extends AbstractList<E> implements Cloneable {
    public static final int MIN_SIZE = 2;

    public Object[] table = new Object[MIN_SIZE];

    /***************************************************************/
    /* List API                                                    */
    /***************************************************************/

    /**
     * <p>Retrieve the stored key.</p>
     *
     * @param key The search key, can be null.
     * @return The stored key or null.
     */
    public E getKey(E key) {
        int i = indexOf(key);
        return (i != -1 ? get(i) : null);
    }

    /**
     * <p>Add key to the set.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key The key, can be null.
     */
     public void add(E key) {
         if (size >= table.length)
             resize(table.length * 2);
         table[size++] = key;
    }

    /**
     * <p>Remove the key from the set.</p>
     *
     * @param key The key, can be null.
     */
    public void remove(E key) {
        int k = indexOf(key);
        if (k >= 0)
            remove(k);
    }

    /***************************************************************/
    /* Getter/Setter                                               */
    /***************************************************************/

    /**
     * <p>Retrieve an element.</p>
     *
     * @param i The index.
     * @return The element.
     */
    public E get(int i) {
        return (E) table[i];
    }

    /**
     * <p>Set an element.</p>
     *
     * @param i The index.
     * @param e The element.
     */
    public void set(int i, E e) {
        table[i] = e;
    }

    /***************************************************************/
    /* More Routines                                               */
    /***************************************************************/

    /**
     * <p>Add an element at a position.</p>
     *
     * @param i The index.
     * @param e The element.
     */
    public void add(int i, E e) {
        if (size >= table.length)
            resize(table.length * 2);
        if (i != size)
            System.arraycopy(table, i, table, i + 1, size - i);
        table[i] = e;
        size++;
    }


    /**
     * <p>Returns the index of the first occurence.</p>
     *
     * @param o The object.
     * @return The index, or -1.
     */
    public int indexOf(Object o) {
        for (int i = 0; i < size; i++)
            if (o != null ? o.equals(table[i]) : null == table[i])
                return i;
        return -1;
    }

    /**
     * <p>Checks whether the object is contained in the list.</p>
     *
     * @param o The object.
     * @return True if the object is contained in the list, otherwise false.
     */
    public boolean contains(Object o) {
        return indexOf(o) >= 0;
    }

    /***************************************************************/
    /* Remove Family                                               */
    /***************************************************************/

    /**
     * <p>Remove an element at a position.</p>
     *
     * @param i The index.
     */
    public void remove(int i) {
        int k = size - i - 1;
        if (k > 0)
            System.arraycopy(table, i + 1, table, i, k);
        table[--size] = null;
        if (size < table.length / 4 && table.length / 2 > MIN_SIZE)
            resize(table.length / 2);
    }

    /**
     * <p>Clear the table.</p>
     */
    public void clear() {
        if (size == 0)
            return;
        if (table.length != MIN_SIZE) {
            table = new Object[MIN_SIZE];
        } else {
            int n = Math.min(size, MIN_SIZE);
            for (int i = 0; i < n; i++)
                table[i] = null;
        }
        size = 0;
    }

    /***************************************************************/
    /* Copy Family                                                 */
    /***************************************************************/

    /**
     * <p>Copy elements to an array.</p>
     *
     * @param target The array.
     */
    public void toArray(E[] target) {
        toArray(target, 0);
    }

    /**
     * <p>Copy elements to an array.</p>
     *
     * @param target The array.
     * @param pos    The start index.
     */
    public void toArray(E[] target, int pos) {
        if (size > 0)
            System.arraycopy(table, 0, target, pos, size);
    }

    /***************************************************************/
    /* Resize Helper                                              */
    /***************************************************************/

    /**
     * <p>Resize the list array.</p>
     */
    public void resize() {
        int len = table.length;
        while (size < len / 4 && len / 2 > MIN_SIZE)
            len = len / 2;
        if (len != table.length)
            resize(len);
    }

    /**
     * <p>Resize the list array.</p>
     *
     * @param s The new size.
     */
    private void resize(int s) {
        Object[] newtable = new Object[s];
        int k = Math.min(s, table.length);
        System.arraycopy(table, 0, newtable, 0, k);
        table = newtable;
    }

    /***************************************************************/
    /* Object Protocol                                             */
    /***************************************************************/

    /**
     * <p>Returns a string representation of this list array.</p>
     *
     * @return A string representation of this list array.
     */
    public String toString() {
        if (size == 0)
            return "[]";
        StringBuilder buf = new StringBuilder();
        buf.append("[");
        buf.append(get(0));
        for (int i = 1; i < size; i++) {
            buf.append(",");
            buf.append(get(i));
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
        ListArray<?> res;
        try {
            res = (ListArray<?>) super.clone();
        } catch (CloneNotSupportedException x) {
            throw new RuntimeException("internal error", x);
        }
        res.table = new Object[table.length];
        System.arraycopy(table, 0, res.table, 0, table.length);
        return res;
    }

}
