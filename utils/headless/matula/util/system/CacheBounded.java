package matula.util.system;

import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;

/**
 * <p>This class provides an unsynchronized LRU cache.</p>
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
public final class CacheBounded<K, V> extends MapHashLink<K, V> {
    public static final int DEFAULT_MAX = 100;

    private int max;

    /**
     * <p>Create a new cache.</p>
     */
    public CacheBounded() {
        this(DEFAULT_MAX);
    }

    /**
     * <p>Create a new cache.</p>
     *
     * @param m The max size.
     */
    public CacheBounded(int m) {
        max = m;
    }

    /**
     * <p>Set the max size.</p>
     *
     * @param m The max size.
     */
    public void setMax(int m) {
        if (m < 0)
            throw new IllegalArgumentException("negative max");
        while (m < size)
            removeEntry(getFirstEntry());
        resize();
        max = m;
    }

    /**
     * <p>Add the key to the map.</p>
     * <p>Assumption is that key is not yet present.</p>
     * <p>Entry is create at the bottom.</p>
     * <p>Superflows entry at the top is removed.</p>
     *
     * @param f The entry.
     */
    public void putEntry(MapEntry<K, V> f) {
        super.putEntry(f);
        if (max < size)
            removeEntry(getFirstEntry());
    }

    /**
     * <p>Find the key in the map.</p>
     * <p>If there is a hit, then move pair to botton.</p>
     *
     * @param key The key.
     * @return The entry.
     */
    public MapEntry<K, V> getEntry(K key) {
        MapEntry<K, V> pair = super.getEntry(key);
        if (pair != null) {
            removeEntry(pair);
            super.putEntry(pair);
        }
        return pair;
    }

}
