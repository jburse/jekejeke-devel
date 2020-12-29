package matula.util.misc;

import matula.util.data.AssocArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;

/**
 * <p>This class provides a string integer table.</p>
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
public class Table<T> extends MapHash<String, AssocArray<Integer, T>> {

    /**
     * <p>Find string integer in table.</p>
     *
     * @param s The string.
     * @param i The integer.
     * @return The value or null.
     */
    public T get(String s, int i) {
        AssocArray<Integer, T> map = get(s);
        if (map == null)
            return null;
        return map.get(Integer.valueOf(i));
    }

    /**
     * <p>Add string integer value to the table.</p>
     *
     * @param s   The string.
     * @param i   The integer.
     * @param val The value.
     */
    public void add(String s, int i, T val) {
        AssocArray<Integer, T> map = get(s);
        if (map == null) {
            map = new AssocArray<>();
            add(s, map);
        }
        map.add(Integer.valueOf(i), val);
    }

    /**
     * <p>Remove string integer from table.</p>
     *
     * @param s The string.
     * @param i The integer.
     */
    public void remove(String s, int i) {
        AssocArray<Integer, T> map = get(s);
        if (map == null)
            return;
        map.remove(Integer.valueOf(i));
        if (map.size() == 0)
            remove(s);
    }

    /**
     * <p>Determine the deep size.</p>
     *
     * @return The deep size.
     */
    public int deepSize() {
        int deepsize = 0;
        for (MapEntry<String, AssocArray<Integer, T>> entry = getFirstEntry();
             entry != null; entry = successor(entry))
            deepsize += entry.value.size();
        return deepsize;
    }

    /**
     * <p>Copy the values deeply.</p>
     *
     * @param target The value array.
     */
    public void toDeepArrayValues(T[] target) {
        int pos = 0;
        for (MapEntry<String, AssocArray<Integer, T>> entry = getFirstEntry();
             entry != null; entry = successor(entry)) {
            AssocArray<Integer, T> map = entry.value;
            map.toArrayValues(target, pos);
            pos += map.size();
        }
    }

}