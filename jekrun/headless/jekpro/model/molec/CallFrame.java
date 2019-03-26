package jekpro.model.molec;

import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Goal;

/**
 * <p>The class provides a clause display.</p>
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
public final class CallFrame extends StackElement {
    public Display disp;
    public int number;

    /**
     * <p>Create a new call frame.</p>
     *
     * @param d  The display.
     * @param en The engine.
     */
    public CallFrame(Display d, Engine en) {
        disp = d;
        contskel = en.contskel;
        contdisplay = en.contdisplay;
        number = en.number;
    }

    /**
     * <p>Retrieve a new or old frame.</p>
     *
     * @param clause The clause.
     * @param en     The engine.
     * @return The new or old frame.
     */
    public final CallFrame getFrame(Clause clause, Engine en) {
        if ((clause.flags & Clause.MASK_CLAUSE_NLST) == 0) {
            if ((contskel.flags & Goal.MASK_GOAL_CEND) != 0) {
                CallFrame u1 = contdisplay;
                if (u1.number >= number) {
                    Display d1 = u1.disp;
                    d1.lastCollect(en);
                    if ((d1.flags & Display.MASK_DISP_NOBR) == 0)
                        disp.flags &= ~Display.MASK_DISP_NOBR;
                    u1.disp = disp;
                    return u1;
                }
            }
        }
        return this;
    }

    /**
     * <p>Retrieve a new or old frame.</p>
     *
     * @param d      The display.
     * @param clause The clause.
     * @param en     The engine.
     * @return The new or old frame.
     */
    public static CallFrame getFrame(Display d, Clause clause, Engine en) {
        if ((clause.flags & Clause.MASK_CLAUSE_NLST) == 0) {
            if ((en.contskel.flags & Goal.MASK_GOAL_CEND) != 0) {
                CallFrame u1 = en.contdisplay;
                if (u1.number >= en.number) {
                    Display d1 = u1.disp;
                    d1.lastCollect(en);
                    if ((d1.flags & Display.MASK_DISP_NOBR) == 0)
                        d.flags &= ~Display.MASK_DISP_NOBR;
                    u1.disp = d;
                    return u1;
                }
            }
        }
        return new CallFrame(d, en);
    }

}