package jekpro.model.inter;

import jekpro.model.molec.*;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.reference.runtime.SpecialLogic;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>The class provides a choice point for clause inspection.</p>
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
        Display ref = goaldisplay.disp;
        /* inlined deref */
        BindUniv b1;
        while (t instanceof SkelVar &&
                (b1 = ref.bind[((SkelVar) t).id]).display != null) {
            t = b1.skel;
            ref = b1.display;
        }
        Object[] temp = ((SkelCompound) t).args;

        /* detect term and body */
        SpecialLogic.colonToCallable(temp[0], ref, true, en);
        Object head = en.skel;
        Display refhead = en.display;

        Clause clause;
        Display d2 = newdisp;
        boolean ext = refhead.getAndReset();
        /* search rope */
        for (; ; ) {
            clause = list[at++];
            if (d2 == Display.DISPLAY_CONST) {
                d2 = Display.valueOf(clause.size);
            } else {
                d2.setSize(clause.size);
            }
            if (AbstractDefined.unifySearch(head, refhead,
                    clause, d2, en)) {
                Object end = Directive.interToBodySkel(clause, clause.last, en);
                if (en.unify(end, d2, temp[1], ref)) {
                    if ((flags & AbstractDefined.OPT_RSLT_CREF) == 0)
                        break;
                    if (en.unify(clause, Display.DISPLAY_CONST, temp[2], ref))
                        break;
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
        if (d2 != Display.DISPLAY_CONST)
            d2.vars = clause.vars;
        if (ext)
            refhead.remTab(en);
        if (d2.bind.length > 0)
            d2.remTab(en);

        if (at != list.length) {
            newdisp = d2;
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
    public final void moniCut(Engine en) {
        /* remove choice point */
        en.choices = next;
        en.number--;
    }

}