package jekpro.model.inter;

import jekpro.model.molec.CallFrame;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Directive;

/**
 * <p>The base class for choice points.</p>
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
public abstract class AbstractChoice {
    public AbstractChoice next;
    public final CallFrame goaldisplay;

    /**
     * <p>Create a choice point.</p>
     *
     * @param c The parent choice point.
     * @param d The call frame;
     */
    public AbstractChoice(AbstractChoice c, CallFrame d) {
        next = c;
        goaldisplay = d;
    }

    /**
     * <p>Logically evaluate a term in a list of goals for an additional time.</p>
     * <p>The result is returned via the contskel and contdisplay of the engine.</p>
     *
     * @param en The engine.
     * @return True if the term succeeded, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public abstract boolean moniNext(Engine en)
            throws EngineException, EngineMessage;

    /**
     * <p>Free data used to logically evaluate a term an additional time.</p>
     * <p>The current exception is passed via the engine fault.</p>
     * <p>The new current exception is returned via the engine fault.</p>
     * <p>The current contskel and contdisplay of the engine is not changed.</p>
     *
     * @param n  The cut level.
     * @param en The engine.
     */
    public abstract void moniCut(int n, Engine en);

    /**
     * <p>Replay the success.</p>
     *
     * @param en The engine.
     */
    public void replayNext(Engine en) {
        CallFrame u = goaldisplay;
        while (u != en.contdisplay && (((u.flags & Directive.MASK_DIRE_MORE) != 0) ?
                u.number + 1 : u.number) >= en.number) {
            if ((u.flags & Directive.MASK_DIRE_LTGC) == 0) {
                if ((u.flags & Directive.MASK_DIRE_NBDY) == 0) {
                    Display d = u.disp;
                    if (d.bind.length > 0)
                        d.remTab(en);
                }
                u.flags |= Directive.MASK_DIRE_LTGC;
            }
            u = u.contdisplay;
        }
    }

}