package matula.util.data;

/**
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
public class AssocArray<K,V>
        extends AbstractAssoc<K,V>
        implements Cloneable {
    public static final int MIN_SIZE = 2;

    public Object[] keys = new Object[MIN_SIZE];
    public Object[] values = new Object[MIN_SIZE];

    /**
     * <p>Find the key in the map.</p>
     *
     * @param key The key.
     * @return The value, or null.
     */
    public V get(K key) {
        int i = indexOf(key);
        return (i != -1 ? getValue(i) : null);
    }

    /**
     * <p>Add the key to the map.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key   The key.
     * @param value The value.
     */
    public void add(K key, V value) {
        if (size >= keys.length)
            resize(keys.length * 2);
        keys[size] = key;
        values[size++]=value;
    }

    /**
     * <p>Remove the key from the map.</p>
     *
     * @param key The key.
     */
    public void remove(K key) {
        int k = indexOf(key);
        if (k >= 0)
            remove(k);
    }

    /***************************************************************/
    /* Modify Family                                               */
    /***************************************************************/

    /**
     * <p>Add an element at a position.</p>
     *
     * @param i The index.
     * @param key   The key.
     * @param value The value.
     */
    public void add(int i, K key, V value) {
        if (size >= keys.length)
            resize(keys.length * 2);
        int k=size-i;
        if (k != 0) {
            System.arraycopy(keys, i, keys, i + 1, k);
            System.arraycopy(values, i, values, i + 1, k);
        }
        keys[i] = key;
        values[i]=value;
        size++;
    }

    /**
     * <p>Remove an element at a position.</p>
     *
     * @param i The index.
     */
    public void remove(int i) {
        int k = size - i - 1;
        if (k > 0) {
            System.arraycopy(keys, i + 1, keys, i, k);
            System.arraycopy(values, i + 1, values, i, k);
        }
        keys[--size] = null;
        values[size] = null;
        if (size < keys.length / 4 && keys.length / 2 > MIN_SIZE)
            resize(keys.length / 2);
    }

    /**
     * <p>Clear the map.</p>
     */
    public void clear() {
        if (size == 0)
            return;
        if (keys.length != MIN_SIZE) {
            keys = new Object[MIN_SIZE];
            values = new Object[MIN_SIZE];
        } else {
            int n = Math.min(size, MIN_SIZE);
            for (int i = 0; i < n; i++) {
                keys[i] = null;
                values[i] = null;
            }
        }
        size = 0;
    }

    /***************************************************************/
    /* Getter/Setter                                               */
    /***************************************************************/

    /**
     * <p>Retrieve a key.</p>
     *
     * @param i The index.
     * @return The key.
     */
    public K getKey(int i) {
        return (K) keys[i];
    }

    /**
     * <p>Retrieve a value.</p>
     *
     * @param i The index.
     * @return The value.
     */
    public V getValue(int i) {
        return (V) values[i];
    }

    /**
     * <p>Set a key.</p>
     *
     * @param i The index.
     * @param e The key.
     */
    public void setKey(int i, K e) {
        keys[i] = e;
    }

    /**
     * <p>Set a value.</p>
     *
     * @param i The index.
     * @param e The value.
     */
    public void setValue(int i, V e) {
        values[i] = e;
    }

    /***************************************************************/
    /* Find Family                                                 */
    /***************************************************************/

    /**
     * <p>Returns the index of the first key occurence.</p>
     *
     * @param o The key.
     * @return The index, or -1.
     */
    public int indexOf(Object o) {
        for (int i = 0; i < size; i++)
            if (o != null ? o.equals(keys[i]) : null == keys[i])
                return i;
        return -1;
    }

    /**
     * <p>Checks whether the object is contained in the key list.</p>
     *
     * @param o The key.
     * @return True if the key is contained in the list, otherwise false.
     */
    public boolean contains(Object o) {
        return indexOf(o) >= 0;
    }

    /***************************************************************/
    /* Resize Helper                                              */
    /***************************************************************/

    /**
     * <p>Resize the list array.</p>
     */
    public void resize() {
        int len = keys.length;
        while (size < len / 4 && len / 2 > MIN_SIZE)
            len = len / 2;
        if (len != keys.length)
            resize(len);
    }

    /**
     * <p>Resize the list array.</p>
     *
     * @param s The new size.
     */
    private void resize(int s) {
        Object[] newobs = new Object[s];
        int k = Math.min(s, keys.length);
        System.arraycopy(keys, 0, newobs, 0, k);
        keys = newobs;

        newobs = new Object[s];
        System.arraycopy(values, 0, newobs, 0, k);
        values = newobs;
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
            return "{}";
        StringBuilder buf = new StringBuilder();
        buf.append("{");
        buf.append(getKey(0));
        buf.append(":");
        buf.append(getValue(0));
        for (int i = 1; i < size; i++) {
            buf.append(",");
            buf.append(getKey(i));
            buf.append(":");
            buf.append(getValue(i));
        }
        buf.append("}");
        return buf.toString();
    }

    /**
     * <p>Create a shallow copy.</p>
     *
     * @return The shallow copy.
     */
    public Object clone() {
        AssocArray<K, V> res;
        try {
            res = (AssocArray<K, V>) super.clone();
        } catch (CloneNotSupportedException x) {
            throw new RuntimeException("internal error", x);
        }
        res.keys = new Object[keys.length];
        System.arraycopy(keys, 0, res.keys, 0, keys.length);
        res.values = new Object[values.length];
        System.arraycopy(values, 0, res.values, 0, values.length);
        return res;
    }

}