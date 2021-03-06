package jekpro.model.inter;

import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>The class provides a choice point for clause show.</p>
 * <p>Refinement for multifile clauses.</p>
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
final class ChoiceShowMultifile extends ChoiceShow {

    /**
     * <p>Create an abstract choice inspect.</p>
     *
     * @param n The molec.
     * @param a The position.
     * @param c The clause list.
     * @param s The source filter.
     * @param r The continuation skel.
     * @param u The continuation display.
     * @param d The new display.
     * @param m The mark.
     */
    ChoiceShowMultifile(AbstractChoice n, int a,
                           Clause[] c, AbstractSource s,
                           Intermediate r, CallFrame u,
                           Display d, AbstractUndo m) {
        super(n, a, c, s, r, u, d, m);
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

        Clause clause;
        Display d2 = newdisp;
        /* search rope */
        for (; ; ) {
            clause = list[at++];
            if (d2 == Display.DISPLAY_CONST) {
                d2 = Display.valueOf(clause.size);
            } else {
                d2.setSize(clause.size);
            }

            SkelAtom sa = StackElement.callableToName(clause.head);
            if (src == sa.scope) {
                Object term = Clause.interToClauseSkel(clause, en);
                if (en.unify(term, d2, temp[2], ref))
                    break;
            }

            /* end of cursor */
            for (; ; ) {
                if (at == list.length)
                    return false;
                if (AbstractDefinedMultifile.multiVisible(list[at], en))
                    break;
                at++;
            }

            /* undo bindings */
            en.fault = null;
            en.releaseBind(mark);
            if (en.fault != null)
                throw en.fault;
        }
        if (d2 != Display.DISPLAY_CONST)
            d2.vars = clause.vars;
        if (d2.bind.length > 0)
            d2.remTab(en);

        while (at != list.length) {
            if (AbstractDefinedMultifile.multiVisible(list[at], en))
                break;
            at++;
        }

        if (at != list.length) {
            newdisp = d2;
            /* reuse choice point */
            en.choices = this;
            en.number++;
        } else {
            /* */
        }

        /* succeed */
        return true;
    }

}