package jekpro.model.builtin;

import jekpro.model.inter.AbstractChoice;
import jekpro.model.inter.Engine;
import jekpro.model.molec.AbstractUndo;
import jekpro.model.molec.CallFrame;
import jekpro.model.molec.EngineException;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides a choice point for ;/2.</p>
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
public final class ChoiceAlter extends AbstractChoice {
    protected Object at;
    protected final Intermediate goalskel;
    protected final CallFrame goaldisplay;
    protected final AbstractUndo mark;
    public int barrier = -2;

    /**
     * <p>Create an abstract choice inspect.</p>
     *
     * @param n The molec.
     * @param r The continuation skel.
     * @param u The continuation display.
     * @param m The mark.
     */
    ChoiceAlter(AbstractChoice n, Object a,
                Intermediate r, CallFrame u,
                AbstractUndo m) {
        super(n);
        goalskel = r;
        goaldisplay = u;
        mark = m;
        at = a;
    }

    /**
     * <p>Logically evaluate a term in a list of goals for an additional time.</p>
     * <p>The result is returned via the skel and display of the engine.</p>
     * <p>A new exception sliding window is returned via the engine display.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineException Shit happens.
     */
    public boolean moniNext(Engine en)
            throws EngineException {
        /* remove choice point */
        en.choices = next;
        en.number--;

        /* undo begin condition */
        if (barrier != -2) {
            goaldisplay.disp.barrier = barrier;
            barrier = -2;
        }

        /* undo bindings */
        en.contskel = goalskel;
        en.contdisplay = goaldisplay;
        en.fault = null;
        en.releaseBind(mark);
        if (en.fault != null)
            throw en.fault;

        if (Goal.isAlternative(at)) {
            SkelCompound sc = (SkelCompound) at;
            at = sc.args[1];
            /* reuse choice point */
            en.choices = this;
            en.number++;
            en.contskel = (Directive) sc.args[0];
            return true;
        } else {
            en.contskel = (Directive) at;
            return true;
        }
    }

    /**
     * <p>Free data used to logically evaluate a term an additional time.</p>
     * <p>The current exception and sliding window are passed via the engine skel and display.</p>
     * <p>The new current exception and sliding window are returned via the engine skel and display.</p>
     *
     * @param n  The cut level.
     * @param en The engine.
     */
    public final void moniCut(int n, Engine en) {
        /* remove choice point */
        en.choices = next;
        en.number--;

        /* undo begin condition */
        if (barrier != -2) {
            goaldisplay.disp.barrier = barrier;
            barrier = -2;
        }
    }

}