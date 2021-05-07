package jekmin.reference.experiment;

import jekpro.frequent.standard.SupervisorCall;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Intermediate;

/**
 * <p>This class provides a variable binder for undo.</p>
 * <p>Will invoke the bound value before unbind.</p>
 *
 * @author Copyright 1985-2016, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Minlog 0.5 (minimal logic extension)
 */
final class UndoGoal extends BindUniv {
    private final boolean mask;
    private final boolean verify;

    /**
     * <p>Create a goal binder.</p>
     *
     * @param f The mask flag.
     * @param v The verify flag.
     */
    UndoGoal(boolean f, boolean v) {
        mask = f;
        verify = v;
    }

    /**
     * <p>Restore state as desired and remove bind from the engine.</p>
     * <p>The current exception is passed via the engine skel.</p>
     * <p>The new current exception is returned via the engine skel.</p>
     *
     * @param en The engine.
     */
    public void unbind(Engine en) {
        /* backup exception */
        EngineException back2 = en.fault;

        /* call unbind handler */
        en.skel = skel;
        en.display = display;
        en.deref();
        try {
            invokeMaskVerify(en);
        } catch (EngineException x) {
            /* aggregate exception */
            if (back2 != null) {
                back2 = new EngineException(back2, x);
            } else {
                back2 = x;
            }
        }

        /* restore exception */
        en.fault = back2;

        super.unbind(en);
    }

    /**
     * <p>Search the given goal once and close it.</p>
     * <p>The goal is passed via skel and display.</p>
     * <p>Throws an error when it fails.</p>
     * <p>Undo the bindings.</p>
     *
     * @param en The engine.
     * @throws EngineException Shit happens.
     */
    private void invokeMaskVerify(Engine en)
            throws EngineException {
        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;
        boolean backmask = en.visor.setMask(mask);
        boolean backverify = en.visor.setVerify(verify);
        AbstractUndo mark = en.bind;
        int snap = en.number;
        try {
            Directive dire = SupervisorCall.callGoal(AbstractDefined.MASK_DEFI_CALL, en);
            Display d2 = en.display;

            CallFrame ref2 = CallFrame.getFrame(d2, dire, en);
            en.contskel = dire;
            en.contdisplay = ref2;
            if (!en.runLoop(snap, true))
                throw new EngineMessage(EngineMessage.syntaxError(
                        EngineMessage.OP_SYNTAX_DIRECTIVE_FAILED));
        } catch (EngineException x) {
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            en.releaseBind(mark);
            en.visor.setMask(backmask);
            en.visor.setVerify(backverify);
            throw en.fault;
        } catch (EngineMessage y) {
            EngineException x = new EngineException(y,
                    EngineException.fetchStack(en));
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            en.releaseBind(mark);
            en.visor.setMask(backmask);
            en.visor.setVerify(backverify);
            throw en.fault;
        }
        en.contskel = r;
        en.contdisplay = u;
        en.fault = null;
        en.cutChoices(snap);
        en.releaseBind(mark);
        en.visor.setMask(backmask);
        en.visor.setVerify(backverify);
        if (en.fault != null)
            throw en.fault;
    }

}
