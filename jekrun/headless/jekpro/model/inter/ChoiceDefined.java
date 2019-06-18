package jekpro.model.inter;

import jekpro.model.molec.*;
import jekpro.model.rope.*;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>The class provides a choice point for defined predicates.</p>
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
public class ChoiceDefined extends AbstractChoice {
    protected int at;
    protected final Clause[] list;
    protected final AbstractUndo mark;

    /**
     * <p>Create pick choice.</p>
     *
     * @param n The molec.
     * @param a The position.
     * @param c The clause list.
     * @param m The mark.
     */
    ChoiceDefined(AbstractChoice n, int a,
                  Clause[] c,
                  CallFrame u, AbstractUndo m) {
        super(n, u);
        at = a;
        list = c;
        mark = m;
    }

    /**
     * <p>Logically evaluate a term in a list of goals for an additional time.</p>
     * <p>The result is returned via the contskel and contdisplay of the engine.</p>
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

        /* undo bindings */
        en.contskel = goaldisplay.contskel;
        en.contdisplay = goaldisplay.contdisplay;
        en.fault = null;
        en.releaseBind(mark);
        if (en.fault != null)
            throw en.fault;

        Intermediate ir = goaldisplay.contskel;
        Object t = ir.term;
        Display d = goaldisplay.contdisplay.disp;
        if ((ir.flags & Goal.MASK_GOAL_NAKE) != 0) {
            /* inlined deref */
            BindUniv b1;
            while (t instanceof SkelVar &&
                    (b1 = d.bind[((SkelVar) t).id]).display != null) {
                t = b1.skel;
                d = b1.display;
            }
        }

        Clause clause;
        Display d2 = goaldisplay.disp;
        /* search rope */
        for (; ; ) {
            clause = list[at++];
            d2.setSize(clause.sizerule);
            if (clause.intargs == null ||
                    AbstractDefined.unifyDefined(((SkelCompound) t).args, d,
                            ((SkelCompound) clause.term).args, d2,
                            clause.intargs, en))
                break;

            /* end of cursor */
            if (at == list.length)
                return false;

            /* undo bindings */
            en.fault = null;
            en.releaseBind(mark);
            if (en.fault != null)
                throw en.fault;
        }
        d2.vars = clause.vars;

        if (at != list.length) {
            if ((clause.flags & AbstractDefined.MASK_DEFI_NBDY) == 0)
                goaldisplay.flags &= ~Directive.MASK_DIRE_LTGC;
            /* reuse choice point */
            en.choices = this;
            en.number++;
            en.contskel = clause;
            en.contdisplay = goaldisplay;
            return true;
        } else if (clause.getNextRaw(en) != Success.DEFAULT) {
            CallFrame dc = goaldisplay.getFrame(en);
            if ((clause.flags & AbstractDefined.MASK_DEFI_NBDY) == 0)
                dc.flags &= ~Directive.MASK_DIRE_LTGC;
            dc.flags &= ~Directive.MASK_DIRE_MORE;
            en.contskel = clause;
            en.contdisplay = dc;
            return true;
        } else {
            if ((clause.flags & AbstractDefined.MASK_DEFI_NBDY) == 0) {
                if (d2.bind.length > 0)
                    d2.remTab(en);
            }
            return true;
        }
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

        goaldisplay.flags &= ~Directive.MASK_DIRE_MORE;
    }

}
