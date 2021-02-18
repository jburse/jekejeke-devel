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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public abstract class AbstractList<E>
        implements Cloneable {
    public int size;

    /**
     * <p>Retrieve size.</p>
     *
     * @return The size.
     */
    public int size() {
        return size;
    }

    /**
     * <p>Find the stored key.</p>
     *
     * @param key The search key, can be null.
     * @return The stored key or null.
     */
    public abstract E getKey(E key);

    /**
     * <p>Add key to the set at end.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key The key, can be null.
     */
    public abstract void add(E key);

    /**
     * <p>Add key to the set at beginning.</p>
     * <p>Assumption is that key is not yet present.</p>
     *
     * @param key The key, can be null.
     */
    public abstract void addFirst(E key);

    /**
     * <p>Remove the key from the set.</p>
     *
     * @param key The key, can be null.
     */
    public abstract void remove(E key);

    /**
     * <p>Clear the set.</p>
     */
    public abstract void clear();

    /**
     * <p>Copy elements to an array.</p>
     *
     * @param target The array.
     */
    public void toArray(E[] target) {
        toArray(target, 0);
    }

    /**
     * <p>Copy the hash map entries to an array.</p>
     *
     * @param target The array.
     * @param pos    The start index.
     */
    public abstract void toArray(E[] target, int pos);

    /**
     * <ü>Copy elements to a list.</ü>
     *
     * @param list The list.
     */
    public abstract void toList(AbstractList<E> list);

    /***************************************************************/
    /* Object Protocol                                             */
    /***************************************************************/

    /**
     * <p>Create a shallow copy.</p>
     *
     * @return The shallow copy.
     */
    public Object clone() {
        AbstractList<E> res;
        try {
            res = (AbstractList<E>) super.clone();
        } catch (CloneNotSupportedException x) {
            throw new RuntimeException("internal error", x);
        }
        res.reinitialize(size());
        return res;
    }

    /**
     * Reset to initial default state.
     *
     * @param capa The ahead capacity.
     */
    public void reinitialize(int capa) {
        size = 0;
    }

}