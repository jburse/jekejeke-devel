package jekmin.reference.experiment;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.inter.AbstractChoice;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>Choice point to enumerate hooks.</p>
 *
 * @author Copyright 2013-2016, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Minlog 0.6.6 (minimal logic extension)
 */
final class ChoiceHook extends AbstractChoice {
    private int at;
    private final Hook[] hooks;
    private final Intermediate goalskel;
    protected Display newdisp;
    private final AbstractUndo mark;

    /**
     * <p>Create a choice hook.</p>
     *
     * @param n The term.
     * @param l The hooks.
     * @param a The position.
     * @param r The continuation skel.
     * @param u The continuation display.
     * @param d The new display.
     * @param m The mark.
     */
    ChoiceHook(AbstractChoice n, int a, Hook[] l,
               Intermediate r, CallFrame u,
               Display d, AbstractUndo m) {
        super(n, u);
        at = a;
        hooks = l;
        goalskel = r;
        newdisp = d;
        mark = m;
    }

    /**
     * <p>Logically evaluate a goal in a list of goals for an additional time.</p>
     * <p>The result is returned via the contskel and contdisplay of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineException Shit happens.
     */
    public final boolean moniNext(Engine en)
            throws EngineException {
        /* remove choice point */
        en.choices = next;
        en.number--;

        /* undo binding */
        en.contskel = goalskel;
        en.contdisplay = goaldisplay;
        en.fault = null;
        en.releaseBind(mark);
        if (en.fault != null)
            throw en.fault;

        Object t = ((Goal) goalskel).term;
        Display d = goaldisplay.disp;
        /* inlined deref */
        BindUniv b1;
        while (t instanceof SkelVar &&
                (b1 = d.bind[((SkelVar) t).id]).display != null) {
            t = b1.skel;
            d = b1.display;
        }

        Hook hook;
        /* search rope */
        int size;
        for (; ; ) {
            hook = hooks[at];
            Object m = hook.term;
            size = SupervisorCopy.displaySize(m);
            newdisp.setSize(size);
            at++;
            if (en.unify(m, newdisp, ((SkelCompound) t).args[1], d) &&
                    en.unify(hook, Display.DISPLAY_CONST, ((SkelCompound) t).args[2], d))
                break;

            /* end of cursor */
            if (at == hooks.length)
                return false;

            /* undo binding */
            en.fault = null;
            en.releaseBind(mark);
            if (en.fault != null)
                throw en.fault;
        }

        if (size != 0)
            newdisp.remTab(en);

        if (at != hooks.length) {
            /* reuse choice point */
            en.choices = this;
            en.number++;
        }

        /* succeed */
        return true;
    }

    /**
     * <p>Free data used to logically evaluate a term an additional time.</p>
     * <p>The current exception is passed via the engine fault.</p>
     * <p>The new current exception is returned via the engine fault.</p>
     * <p>The current contskel and contdisplay of the engine is not changed.</p>
     *
     * @param en The engine.
     */
    public void moniCut(Engine en) {
        /* remove choice point */
        en.choices = next;
        en.number--;
    }

}
