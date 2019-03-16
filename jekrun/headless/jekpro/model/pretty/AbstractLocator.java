package jekpro.model.pretty;

import jekpro.model.inter.Predicate;
import jekpro.tools.term.PositionKey;

/**
 * <p>This class provides an abstract locator.</p>
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
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public abstract class AbstractLocator {
    public final static int MASK_LOC_INDI = 0x00000001;
    public final static int MASK_LOC_STAT = 0x00000002;

    /**
     * <p>Add a position.</p>
     *
     * @param pos   The position, can be null.
     * @param pick  The predicate.
     * @param flags The flags.
     */
    public abstract void addPosition(PositionKey pos, Predicate pick,
                                     int flags);

    /**
     * <p>Clear all positions.</p>
     */
    public abstract void clearPositions();

}