package jekpro.model.inter;

import jekpro.model.molec.*;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.reference.runtime.SpecialLogic;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>The class provides a choice point for clause retrieval.</p>
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
class ChoiceInspect extends AbstractChoice {
    protected int at;
    protected final Clause[] list;
    protected final int flags;
    protected final Intermediate goalskel;
    protected Display newdisp;
    protected final AbstractUndo mark;

    /**
     * <p>Create an choice inspect.</p>
     *
     * @param n The molec.
     * @param a The position.
     * @param c The clause list.
     * @param f The flags.
     * @param r The continuation skel.
     * @param u The continuation display.
     * @param d The new display.
     * @param m The mark.
     */
    ChoiceInspect(AbstractChoice n, int a,
                  Clause[] c, int f,
                  Intermediate r, CallFrame u,
                  Display d, AbstractUndo m) {
        super(n, u);
        at = a;
        list = c;
        flags = f;
        goalskel = r;
        newdisp = d;
        mark = m;
    }

    /**
     * <p>Logically evaluate a term in a list of goals for an additional time.</p>
     * <p>The result is returned via the contskel and contdisplay of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public boolean moniNext(Engine en)
            throws EngineException, EngineMessage {
        /* remove choice point */
        en.choices = next;
        en.number--;

        /* end of cursor */
        if (at == list.length)
            return false;

        /* undo bindings */
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
        Object[] temp = ((SkelCompound) t).args;

        /* detect term and body */
        SpecialLogic.colonToCallable(temp[0], d, true, en);
        Object head = en.skel;
        Display refhead = en.display;

        Clause clause;
        boolean ext = refhead.getAndReset();
        /* search rope */
        for (; ; ) {
            clause = list[at++];
            newdisp.setSize(clause.size);
            if (!(clause.head instanceof SkelCompound) ||
                    AbstractDefined.unifyArgs(((SkelCompound) head).args, refhead,
                            ((SkelCompound) clause.head).args, newdisp, en)) {
                Object end = clause.interToBody(en);
                if (en.unifyTerm(temp[1], d, end, newdisp)) {
                    if ((flags & AbstractDefined.OPT_RSLT_CREF) != 0) {
                        if (en.unifyTerm(temp[2], d,
                                clause, Display.DISPLAY_CONST))
                            break;
                    } else {
                        break;
                    }
                }
            }

            /* end of cursor */
            if (at == list.length)
                return false;

            /* undo bindings */
            en.fault = null;
            en.releaseBind(mark);
            if (en.fault != null)
                throw en.fault;
        }
        newdisp.vars = clause.vars;

        if (ext)
            refhead.remTab(en);
        if (clause.size != 0)
            newdisp.remTab(en);

        if (at != list.length) {
            /* reuse choice point */
            en.choices = this;
            en.number++;
        } else {
            /* */
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
    public final void moniCut(Engine en) {
        /* remove choice point */
        en.choices = next;
        en.number--;
    }

}
