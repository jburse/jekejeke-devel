package jekdev.model.pretty;

import jekpro.model.inter.Predicate;
import jekpro.model.pretty.AbstractLocator;
import jekpro.model.pretty.AbstractSource;
import jekpro.tools.term.PositionKey;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;

/**
 * <p>This class provides a trace locator.</p>
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
 * Restrictions
 * Only to be distributed with programs that add sgnificant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class LocatorTrace extends AbstractLocator {
    private final MapHashLink<Predicate, MapHashLink<PositionKey, Integer>> locs
            = new MapHashLink<Predicate, MapHashLink<PositionKey, Integer>>();

    private MapEntry<Predicate, PositionKey>[] cachefirstlocs;
    private MapEntry<Predicate, PositionKey[]>[] cachelocs;

    public final static MapEntry<Predicate, PositionKey>[] VOID_FIRST_ALL = new MapEntry[0];
    public final static MapEntry<Predicate, PositionKey[]>[] VOID_ALL = new MapEntry[0];

    /**
     * <p>Create a locator.</p>
     *
     * @param s The source.
     */
    public LocatorTrace(AbstractSource s) {
        super(s);
    }

    /**
     * <p>Add a position.</p>
     * .
     *
     * @param pick  The predicate.
     * @param pos   The position, can be null.
     * @param flags The flags.
     */
    public void addPosition(Predicate pick, PositionKey pos, int flags) {
        synchronized (src) {
            MapHashLink<PositionKey, Integer> loc = locs.get(pick);
            if (loc == null) {
                loc = new MapHashLink<PositionKey, Integer>();
                locs.add(pick, loc);
            }
            Integer val = loc.get(pos);
            if (val == null) {
                val = Integer.valueOf(flags);
                loc.add(pos, val);
            } else {
                int f1 = val.intValue();
                int f2 = f1 | flags;
                if (f2 == f1)
                    return;
                val = Integer.valueOf(f2);
                loc.remove(pos);
                loc.add(pos, val);
            }
            cachefirstlocs = null;
            cachelocs = null;
        }
    }

    /**
     * <p>Clear all positions.</p>
     */
    public void clearPositions() {
        synchronized (src) {
            locs.clear();
            cachefirstlocs = null;
            cachelocs = null;
        }
    }

    /**
     * <p>Retrieve the first indicator or static position.</p>
     *
     * @param pick The predicate.
     * @return The first indicator or static position.
     */
    public PositionKey firstPosition(Predicate pick) {
        synchronized (src) {
            MapHashLink<PositionKey, Integer> loc = locs.get(pick);
            if (loc == null)
                return null;
            MapEntry<PositionKey, Integer> first = loc.getFirstEntry();
            return (first != null ? first.key : null);
        }
    }

    /**
     * <p>Retrieve all the static positions.</p>
     *
     * @return All the static positions.
     */
    public MapEntry<Predicate, PositionKey>[] allFirstPositions() {
        MapEntry<Predicate, PositionKey>[] res = cachefirstlocs;
        if (res != null)
            return res;
        synchronized (src) {
            res = cachefirstlocs;
            if (res != null)
                return res;
            ListArray<MapEntry<Predicate, PositionKey>> help = null;
            for (MapEntry<Predicate, MapHashLink<PositionKey, Integer>> entry = locs.getFirstEntry();
                 entry != null; entry = locs.successor(entry)) {
                MapHashLink<PositionKey, Integer> loc = entry.value;
                MapEntry<PositionKey, Integer> entry2 = loc.getFirstEntry();
                if (entry2 == null)
                    continue;
                MapEntry<Predicate, PositionKey> entry3 = new MapEntry<Predicate, PositionKey>();
                entry3.key = entry.key;
                entry3.value = entry2.key;
                if (help == null)
                    help = new ListArray<MapEntry<Predicate, PositionKey>>();
                help.add(entry3);
            }
            if (help != null) {
                res = new MapEntry[help.size()];
                help.toArray(res);
            } else {
                res = VOID_FIRST_ALL;
            }
            cachefirstlocs = res;
        }
        return res;
    }

    /**
     * <p>Retrieve all the static positions.</p>
     *
     * @return All the static positions.
     */
    public MapEntry<Predicate, PositionKey[]>[] allPositions() {
        MapEntry<Predicate, PositionKey[]>[] res = cachelocs;
        if (res != null)
            return res;
        synchronized (src) {
            res = cachelocs;
            if (res != null)
                return res;
            ListArray<MapEntry<Predicate, PositionKey[]>> help = null;
            for (MapEntry<Predicate, MapHashLink<PositionKey, Integer>> entry = locs.getFirstEntry();
                 entry != null; entry = locs.successor(entry)) {
                MapHashLink<PositionKey, Integer> loc = entry.value;
                ListArray<PositionKey> temp = null;
                for (MapEntry<PositionKey, Integer> entry2 = loc.getFirstEntry();
                     entry2 != null; entry2 = loc.successor(entry2)) {
                    if ((entry2.value.intValue() & AbstractLocator.MASK_LOC_STAT) == 0)
                        continue;
                    if (temp == null)
                        temp = new ListArray<PositionKey>();
                    temp.add(entry2.key);
                }
                if (temp == null)
                    continue;
                PositionKey[] res2 = new PositionKey[temp.size()];
                temp.toArray(res2);
                if (help == null)
                    help = new ListArray<MapEntry<Predicate, PositionKey[]>>();
                MapEntry<Predicate, PositionKey[]> entry3 = new MapEntry<Predicate, PositionKey[]>();
                entry3.key = entry.key;
                entry3.value = res2;
                help.add(entry3);
            }
            if (help != null) {
                res = new MapEntry[help.size()];
                help.toArray(res);
            } else {
                res = VOID_ALL;
            }
            cachelocs = res;
        }
        return res;
    }

}