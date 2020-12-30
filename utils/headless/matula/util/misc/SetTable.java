package matula.util.misc;

import matula.util.data.AssocArray;
import matula.util.data.ListArray;
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
public final class SetTable extends MapHash<String, ListArray<Integer>> {

    /**
     * <p>Find string integer in table.</p>
     *
     * @param s The string.
     * @param i The integer.
     * @return The value or null.
     */
    public boolean contains(String s, int i) {
        ListArray<Integer> list = get(s);
        if (list == null)
            return false;
        return list.contains(Integer.valueOf(i));
    }

    /**
     * <p>Add string integer value to the table.</p>
     *
     * @param s   The string.
     * @param i   The integer.
     */
    public void add(String s, int i) {
        ListArray<Integer> list = get(s);
        if (list == null) {
            list = new ListArray<>();
            add(s, list);
        }
        list.add(Integer.valueOf(i));
    }

    /**
     * <p>Remove string integer from table.</p>
     *
     * @param s The string.
     * @param i The integer.
     */
    public void remove(String s, int i) {
        ListArray<Integer> list = get(s);
        if (list == null)
            return;
        list.remove(Integer.valueOf(i));
        if (list.size() == 0)
            remove(s);
    }

    /**
     * <p>Determine the deep size.</p>
     *
     * @return The deep size.
     */
    public int deepSize() {
        int deepsize = 0;
        for (MapEntry<String, ListArray<Integer>> entry = getFirstEntry();
             entry != null; entry = successor(entry))
            deepsize += entry.value.size();
        return deepsize;
    }

    /**
     * <p>Copy the elements deeply.</p>
     *
     * @param target The elements array.
      */
    public void toDeepArray(Key[] target) {
        int pos = 0;
        for (MapEntry<String, ListArray<Integer>> entry = getFirstEntry();
             entry != null; entry = successor(entry)) {
            ListArray<Integer> list = entry.value;
            for (int i = 0; i < list.size; i++) {
                target[pos] = new Key(entry.key, list.get(i).intValue());
                pos++;
            }
        }
    }

}