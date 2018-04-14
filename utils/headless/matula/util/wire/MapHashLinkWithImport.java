package matula.util.wire;

import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;

/**
 * <p>This class provides a map hash with import.</p>
 * <p/>
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
public final class MapHashLinkWithImport<K, V> extends MapHashLink<K, V> {
    private ListArray<MapHashLink<K, V>> imports = new ListArray<MapHashLink<K, V>>();

    /**
     * <p>Add an import.</p>
     *
     * @param map The import.
     */
    public void addImport(MapHashLink<K, V> map) {
        if (map == null)
            throw new NullPointerException("map missing");
        imports.add(map);
    }

    /**
     * <p>Find the entry in the map.</p>
     *
     * @param key The key.
     * @return The entry, or null.
     */
    public MapEntry<K, V> getEntry(K key) {
        for (int i = 0; i < imports.size(); i++) {
            MapEntry<K, V> res = imports.get(i).getEntry(key);
            if (res != null)
                return res;
        }
        return super.getEntry(key);
    }

    /***************************************************************/
    /* Object Protocol                                             */
    /***************************************************************/

    /**
     * <p>Returns a string representation of this abstract map.</p>
     *
     * @return A string representation of this abstract map.
     */
    public String toString() {
        StringBuilder buf = new StringBuilder();
        buf.append(super.toString());
        buf.append(imports.toString());
        return buf.toString();
    }

}