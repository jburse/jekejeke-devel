package jekpro.frequent.advanced;

import jekpro.model.inter.AbstractChoice;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Intermediate;
import jekpro.tools.term.SkelAtom;

/**
 * <p>The class provides a choice point for cleanup calls.</p>
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
final class ChoiceCleanup extends AbstractChoice {
    private final Intermediate goalskel;
    private final AbstractUndo mark;
    private final boolean mask;
    private final boolean verify;
    private final Directive dire;
    private final Display d2;

    /**
     * <p>Create a choice cleanup.</p>
     *
     * @param n The parent choice.
     * @param r The term list skeleton.
     * @param u The term list display.
     * @param m The mark.
     */
    ChoiceCleanup(AbstractChoice n, Intermediate r, CallFrame u,
                  AbstractUndo m, boolean f, boolean v,
                  Directive d, Display k) {
        super(n, u);
        goalskel = r;
        mark = m;
        mask = f;
        verify = v;
        d2 = k;
        dire = d;
    }

    /**
     * <p>Logically evaluate a term in a list of goals for an additional time.</p>
     * <p>The result is returned via the contskel and contdisplay of the engine.</p>
     *
     * @param en The engine.
     * @return True if the term succeeded, otherwise false.
     * @throws EngineException Shit happens.
     */
    public boolean moniNext(Engine en)
            throws EngineException {
        // remove choice point
        en.choices = next;
        en.number--;

        // undo bindings
        en.contskel = goalskel;
        en.contdisplay = goaldisplay;
        en.fault = null;
        en.releaseBind(mark);
        if (en.fault != null)
            throw en.fault;

        // call cleanup handler
        unfoldCleanup(en);
        return false;
    }

    /**
     * <p>Free data used to logically evaluate a term an additional time.</p>
     * <p>The current exception is passed via the engine fault.</p>
     * <p>The new current exception is returned via the engine skel and display.</p>
     *
     * @param en The engine.
     */
    public final void moniCut(Engine en) {
        // remove choice point
        en.choices = next;
        en.number--;

        // backup exception
        EngineException back2 = en.fault;

        // fetch term argument
        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;
        en.contskel = goalskel;
        en.contdisplay = goaldisplay;

        // call cleanup handler
        try {
            unfoldCleanup(en);
        } catch (EngineException x) {
            // aggregate_all exception
            if (back2 != null) {
                back2 = new EngineException(back2, x);
            } else {
                back2 = x;
            }
        }

        // restore continuation
        en.contskel = r;
        en.contdisplay = u;

        // restore exception
        en.fault = back2;
    }

    /**
     * <p>Search the given term once and close it.</p>
     * <p>The term is passed via skel and display.</p>
     * <p>Throws an error when it fails.</p>
     * <p>Do not undo the bindings.</p>
     *
     * @param en The engine.
     * @throws EngineException Shit happens.
     */
    private void unfoldCleanup(Engine en)
            throws EngineException {
        boolean backmask = en.visor.setMask(mask);
        boolean backverify = en.visor.setVerify(verify);
        int snap = en.number;
        try {
            CallFrame ref2 = CallFrame.getFrame(d2, dire, en);
            en.contskel = dire;
            en.contdisplay = ref2;
            if (!en.runLoop(snap, true))
                throw new EngineMessage(EngineMessage.syntaxError(
                        EngineMessage.OP_SYNTAX_DIRECTIVE_FAILED));
        } catch (EngineException x) {
            en.contskel = goalskel;
            en.contdisplay = goaldisplay;
            en.fault = x;
            en.cutChoices(snap);
            en.visor.setMask(backmask);
            en.visor.setVerify(backverify);
            throw en.fault;
        } catch (EngineMessage y) {
            EngineException x = new EngineException(y,
                    EngineException.fetchStack(en));
            en.contskel = goalskel;
            en.contdisplay = goaldisplay;
            en.fault = x;
            en.cutChoices(snap);
            en.visor.setMask(backmask);
            en.visor.setVerify(backverify);
            throw en.fault;
        }
        en.contskel = goalskel;
        en.contdisplay = goaldisplay;
        en.fault = null;
        en.cutChoices(snap);
        en.visor.setMask(backmask);
        en.visor.setVerify(backverify);
        if (en.fault != null)
            throw en.fault;
    }

}
