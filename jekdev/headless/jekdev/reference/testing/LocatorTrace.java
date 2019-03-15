package jekdev.reference.testing;

import jekpro.model.inter.Predicate;
import jekpro.model.pretty.AbstractLocator;
import jekpro.model.pretty.AbstractSource;
import jekpro.tools.term.PositionKey;
import matula.util.data.MapEntry;

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
    private final PositionMap firstlocs;
    private final PositionMap locs;

    /**
     * <p>Create a locator.</p>
     *
     * @param s The source.
     */
    public LocatorTrace(AbstractSource s) {
        firstlocs = new PositionMap(s);
        locs = new PositionMap(s);
    }

    /**
     * <p>Add a position.</p>
     *
     * @param pos   The position, can be null.
     * @param pick  The predicate.
     * @param flags The flags.
     */
    public void addPosition(PositionKey pos, Predicate pick, int flags) {
        if ((flags & AbstractLocator.MASK_LOC_INDI) != 0)
            firstlocs.addPosition(pos, pick);
        if ((flags & AbstractLocator.MASK_LOC_STAT) != 0)
            locs.addPosition(pos, pick);
    }

    /**
     * <p>Clear all positions.</p>
     */
    public void clearPositions() {
        firstlocs.clearPositions();
        locs.clearPositions();
    }

    /**
     * <p>Retrieve all the static positions.</p>
     *
     * @return All the static positions.
     */
    public MapEntry<PositionKey, Predicate[]>[] allFirstPositions() {
        return firstlocs.allPositions();
    }

    /**
     * <p>Retrieve all the static positions.</p>
     *
     * @return All the static positions.
     */
    public MapEntry<PositionKey, Predicate[]>[] allPositions() {
        return locs.allPositions();
    }

}