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
public class AssocArray<K, V>
        extends AbstractAssoc<K, V> {
    public static final int MIN_SIZE = 2;

    public Object[] kvs;

    /**
     * <p>Create an assoc array.</p>
     */
    public AssocArray() {
        reinitialize(0);
    }

    /**
     * <p>Create an assoc array.</p>
     *
     * @param capa The ahead capacity.
     */
    public AssocArray(int capa) {
        reinitialize(capa);
    }

    /**
     * <p>Find the key in the map.</p>
     *
     * @param key The key.
     * @return The value, or null.
     */
    public V get(K key) {
        int i = indexOf(key);
        return (i >= 0 ? getValue(i) : null);
    }

    /**
     * <p>Add the key to the map.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key   The key.
     * @param value The value.
     */
    public void add(K key, V value) {
        if (size >= kvs.length / 2)
            resize(kvs.length);
        kvs[2 * size] = key;
        kvs[2 * size + 1] = value;
        size++;
    }

    /**
     * <p>Remove the key from the map.</p>
     *
     * @param key The key.
     */
    public void remove(K key) {
        int i = indexOf(key);
        if (i < 0)
            return;

        int k = size - i - 1;
        if (k > 0)
            System.arraycopy(kvs, 2 * i + 2, kvs, 2 * i, 2 * k);
        --size;
        kvs[2 * size] = null;
        kvs[2 * size + 1] = null;
        if (size < kvs.length / 8 && kvs.length / 4 > MIN_SIZE)
            resize(kvs.length / 4);
    }

    /**
     * <p>Remove an element at a position.</p>
     *
     * @param i The index.
     */
    public void removeEntry(int i) {
        int k = size - i - 1;
        if (k > 0)
            System.arraycopy(kvs, 2 * i + 2, kvs, 2 * i, 2 * k);
        --size;
        kvs[2 * size] = null;
        kvs[2 * size + 1] = null;
    }

    /***************************************************************/
    /* Modify Family                                               */
    /***************************************************************/

    /**
     * <p>Add an element at a position.</p>
     *
     * @param i     The index.
     * @param key   The key.
     * @param value The value.
     */
    public void add(int i, K key, V value) {
        if (size >= kvs.length / 2)
            resize(kvs.length);
        int k = size - i;
        if (k != 0)
            System.arraycopy(kvs, 2 * i, kvs, 2 * i + 2, 2 * k);
        kvs[2 * i] = key;
        kvs[2 * i + 1] = value;
        size++;
    }

    /**
     * <p>Clear the map.</p>
     */
    public void clear() {
        if (size == 0)
            return;
        if (kvs.length != 2 * MIN_SIZE) {
            kvs = new Object[2 * MIN_SIZE];
        } else {
            int n = Math.min(size, MIN_SIZE);
            for (int i = 0; i < n; i++) {
                kvs[2 * i] = null;
                kvs[2 * i + 1] = null;
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
        return (K) kvs[2 * i];
    }

    /**
     * <p>Retrieve a value.</p>
     *
     * @param i The index.
     * @return The value.
     */
    public V getValue(int i) {
        return (V) kvs[2 * i + 1];
    }

    /**
     * <p>Set a key.</p>
     *
     * @param i The index.
     * @param e The key.
     */
    public void setKey(int i, K e) {
        kvs[2 * i] = e;
    }

    /**
     * <p>Set a value.</p>
     *
     * @param i The index.
     * @param e The value.
     */
    public void setValue(int i, V e) {
        kvs[2 * i + 1] = e;
    }

    /***************************************************************/
    /* Find Family                                                 */
    /***************************************************************/

    /**
     * <p>Returns the first index of the key occurence.</p>
     *
     * @param o The key.
     * @return The index, or -1.
     */
    public int indexOf(Object o) {
        for (int i = 0; i < size; i++)
            if (o != null ? o.equals(kvs[2 * i]) : null == kvs[2 * i])
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
        int len = kvs.length / 2;
        while (size < len / 4 && len / 2 > MIN_SIZE)
            len = len / 2;
        if (len != kvs.length / 2)
            resize(len);
    }

    /**
     * <p>Resize the list array.</p>
     *
     * @param s The new size.
     */
    private void resize(int s) {
        Object[] newobs = new Object[2 * s];
        int k = Math.min(s, kvs.length / 2);
        System.arraycopy(kvs, 0, newobs, 0, 2 * k);
        kvs = newobs;
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
        AssocArray<K, V> res = (AssocArray<K, V>) super.clone();
        if (size() > 0) {
            System.arraycopy(kvs, 0, res.kvs, 0, 2 * size());
            res.size = size();
        }
        return res;
    }

    /**
     * Reset to initial default state.
     *
     * @param capa The ahead capacity.
     */
    void reinitialize(int capa) {
        super.reinitialize(capa);
        int len = MIN_SIZE;
        while (capa > len)
            len = len * 2;
        kvs = new Object[2 * len];
    }

}