package jekdev.model.pretty;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.inter.Predicate;
import jekpro.model.pretty.AbstractSource;
import jekpro.tools.term.PositionKey;
import matula.util.data.MapHashLink;

/**
 * <p>This class provides a synchronized position map.</p>
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
public final class PositionMap extends MapHashLink<PositionKey, PredicateSet> {
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
            PredicateSet loc = get(pos);
            if (loc == null) {
                loc = new PredicateSet();
                add(pos, loc);
            }
            loc.addPredicate(pick);
        }
    }

    /**
     * <p>Clear all positions.</p>
     */
    public void clearPositions() {
        synchronized (src) {
            clear();
        }
    }

    /**
     * <p>Retrieve all predicates.</p>
     *
     * @return Snapshot of the predicates.
     */
    public Predicate[] allPredicates(PositionKey pos) {
        Predicate[] res;
        synchronized (src) {
            PredicateSet loc = get(pos);
            res = (loc != null ? loc.allPredicates() :
                    AbstractBranch.FALSE_PREDS);
        }
        return res;
    }

}