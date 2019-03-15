package jekdev.reference.testing;

import jekpro.model.inter.Predicate;
import jekpro.model.pretty.AbstractSource;
import jekpro.tools.term.PositionKey;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;
import matula.util.data.SetHashLink;

/**
 * <p>This class provides a position map.</p>
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
public final class PositionMap {
    private final MapHashLink<PositionKey, SetHashLink<Predicate>> locs
            = new MapHashLink<PositionKey, SetHashLink<Predicate>>();
    private MapEntry<PositionKey, Predicate[]>[] cachelocs;

    private final static MapEntry<PositionKey, Predicate[]>[] VOID_ALL = new MapEntry[0];

    protected AbstractSource src;

    /**
     * <p>Create a locator.</p>
     *
     * @param s The source.
     */
    public PositionMap(AbstractSource s) {
        src = s;
    }

    /**
     * <p>Add a position.</p>
     *
     * @param pos  The position.
     * @param pick The predicate.
     */
    public void addPosition(PositionKey pos, Predicate pick) {
        synchronized (src) {
            SetHashLink<Predicate> loc = locs.get(pos);
            if (loc == null) {
                loc = new SetHashLink<Predicate>();
                locs.add(pos, loc);
            }
            if (loc.getKey(pick) != null)
                return;
            loc.add(pick);
            cachelocs = null;
        }
    }

    /**
     * <p>Clear all positions.</p>
     */
    public void clearPositions() {
        synchronized (src) {
            locs.clear();
            cachelocs = null;
        }
    }

    /**
     * <p>Retrieve all positions.</p>
     *
     * @return Snapshot of the positions.
     */
    public MapEntry<PositionKey, Predicate[]>[] allPositions() {
        MapEntry<PositionKey, Predicate[]>[] res = cachelocs;
        if (res != null)
            return res;
        synchronized (src) {
            res = cachelocs;
            if (res != null)
                return res;
            if (locs.size() != 0) {
                res = new MapEntry[locs.size()];
                int k = 0;
                for (MapEntry<PositionKey, SetHashLink<Predicate>> entry = locs.getFirstEntry();
                     entry != null; entry = locs.successor(entry)) {
                    SetHashLink<Predicate> loc = entry.value;
                    Predicate[] temp = new Predicate[loc.size()];
                    loc.toArray(temp);
                    MapEntry<PositionKey, Predicate[]> help = new MapEntry<PositionKey, Predicate[]>();
                    help.key = entry.key;
                    help.value = temp;
                    res[k] = help;
                    k++;
                }
            } else {
                res = VOID_ALL;
            }
            cachelocs = res;
        }
        return res;
    }

}