package jekdev.model.pretty;

import jekpro.model.builtin.Branch;
import jekpro.model.pretty.StoreKey;
import jekpro.model.pretty.StoreKeyQuali;
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
public final class SetTable extends MapHash<String, MapHash<String, ListArray<Integer>>> {

    /**
     * <p>Find string integer in table.</p>
     *
     * @param fun The functor.
     * @param arity The arity.
     * @param mod The module.
     * @return The value or null.
     */
    public boolean contains(String fun, int arity, String mod) {
        MapHash<String, ListArray<Integer>> map = get(mod);
        if (map == null)
            return false;
        ListArray<Integer> list = map.get(fun);
        if (list == null)
            return false;
        return list.contains(Integer.valueOf(arity));
    }

    /**
     * <p>Add string integer value to the table.</p>
     *
     * @param fun The functor.
     * @param arity The arity.
     * @param mod The module.
     */
    public void add(String fun, int arity, String mod) {
        MapHash<String, ListArray<Integer>> map = get(mod);
        if (map==null) {
            map = new MapHash<>();
            add(mod, map);
        }
        ListArray<Integer> list = map.get(fun);
        if (list == null) {
            list = new ListArray<>();
            map.add(fun, list);
        }
        list.add(Integer.valueOf(arity));
    }

    /**
     * <p>Remove string integer from table.</p>
     *
     * @param fun The functor.
     * @param arity The arity.
     * @param mod The module.
     */
    public void remove(String fun, int arity, String mod) {
        MapHash<String, ListArray<Integer>> map = get(mod);
        if (map==null)
            return;
        ListArray<Integer> list = map.get(fun);
        if (list == null)
            return;
        list.remove(Integer.valueOf(arity));
        if (list.size() == 0)
            map.remove(fun);
        if (map.size()==0)
            remove(mod);
    }

    /**
     * <p>Determine the deep size.</p>
     *
     * @return The deep size.
     */
    public int deepSize() {
        int deepsize = 0;
        for (MapEntry<String, MapHash<String, ListArray<Integer>>> base = getFirstEntry();
             base != null; base = successor(base)) {
            MapHash<String, ListArray<Integer>> map = base.value;
            for (MapEntry<String, ListArray<Integer>> entry = map.getFirstEntry();
                 entry != null; entry = map.successor(entry))
                deepsize += entry.value.size();
        }
        return deepsize;
    }

    /**
     * <p>Copy the elements deeply.</p>
     *
     * @param target The elements array.
     */
    public void toDeepArray(StoreKey[] target) {
        int pos = 0;
        for (MapEntry<String, MapHash<String, ListArray<Integer>>> base = getFirstEntry();
             base != null; base = successor(base)) {
            String mod = base.key;
            MapHash<String, ListArray<Integer>> map = base.value;
            for (MapEntry<String, ListArray<Integer>> entry = map.getFirstEntry();
                 entry != null; entry = map.successor(entry)) {
                ListArray<Integer> list = entry.value;
                for (int i = 0; i < list.size; i++) {
                    StoreKey key;
                    if (!Branch.OP_USER.equals(mod)) {
                        key = new StoreKeyQuali(entry.key, list.get(i).intValue(), mod);
                    } else {
                        key = new StoreKey(entry.key, list.get(i).intValue());
                    }
                    target[pos] = key;
                    pos++;
                }
            }
        }
    }

}