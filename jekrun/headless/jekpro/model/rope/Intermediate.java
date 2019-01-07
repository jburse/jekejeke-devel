package jekpro.model.rope;

import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;

/**
 * <p>The class provides the base class for intermediate code.</p>
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
public abstract class Intermediate {
    public final static int MASK_INTER_MUTE = 0x00000001;
    public final static int MASK_INTER_NLST = 0x00000002;

    public final static int MASK_INTER_BOTH =
            MASK_INTER_MUTE | MASK_INTER_NLST;

    public Intermediate next;
    public int flags;

    /**********************************************************/
    /* Variation Points                                       */
    /**********************************************************/

    /**
     * <p>Retrieve the next goal depending on debug mode.</p>
     * <p>Should be implemented by subclasses.</p>
     *
     * @param en The engine.
     * @return The next goal.
     */
    public Intermediate getNextRaw(Engine en) {
        return next;
    }

    /**
     * <p>Retrieve the retire flag depending on debug mode.</p>
     *
     * @param en The engine.
     * @return The retire flag.
     */
    public boolean getRetire(Engine en) {
        return (en.hasCont() &&
                (flags & MASK_INTER_MUTE) == 0);
    }

    /**
     * <p>Retrieve the clause.</p>
     *
     * @return The clause.
     */
    public abstract Clause getClause();

    /**
     * <p>Resolve the current goal.</p>
     *
     * @param en The engine.
     * @return The delegate.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public abstract boolean resolveNext(Engine en)
            throws EngineException, EngineMessage;

}