package jekpro.model.inter;

import jekpro.model.molec.*;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Success;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

/**
 * <p>The base delegate class for a multifile delegate.</p>
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
public abstract class AbstractDefinedMultifile extends AbstractDefined {

    /**
     * <p>Create a multifile delegate.</p>
     *
     * @param flags The store flags.
     */
    AbstractDefinedMultifile(int flags) {
        super(flags);
    }

    /**
     * <p>Logically evaluate a term in a list of goals for the first time.</p>
     * <p>The term is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        Object t = en.skel;
        Display d = en.display;
        Clause[] list = definedClauses(t, d, en);
        int at = 0;

        /* end of cursor */
        for (; ; ) {
            if (at == list.length)
                return false;
            if (multiVisible(list[at], en))
                break;
            at++;
        }

        AbstractUndo mark = en.bind;
        Clause clause;
        Display d2 = null;
        /* search rope */
        for (; ; ) {
            clause = list[at++];
            if (d2 == null) {
                d2 = new Display(clause.sizerule);
            } else {
                d2.setSize(clause.sizerule);
            }
            int[] arr = clause.intargs;
            if (arr == null)
                break;
            if (AbstractDefined.unifyExecute(((SkelCompound) t).args, d,
                    ((SkelCompound) clause.head).args, d2,
                    arr, en))
                break;

            /* end of cursor */
            for (; ; ) {
                if (at == list.length)
                    return false;
                if (multiVisible(list[at], en))
                    break;
                at++;
            }

            /* undo bindings */
            en.fault = null;
            en.releaseBind(mark);
            if (en.fault != null)
                throw en.fault;
        }
        d2.vars = clause.vars;

        while (at != list.length) {
            if (multiVisible(list[at], en))
                break;
            at++;
        }

        if (at != list.length) {
            CallFrame dc = new CallFrame(d2, en);
            dc.flags = clause.flags & Directive.MASK_DIRE_CALL;
            dc.flags |= Directive.MASK_DIRE_MORE;
            /* create choice point */
            en.choices = new ChoiceDefinedMultifile(en.choices, at, list, dc, mark);
            en.number++;
            en.contskel = clause;
            en.contdisplay = dc;
            return true;
        } else if (clause.getNextRaw(en) != Success.DEFAULT) {
            CallFrame dc = CallFrame.getFrame(d2, clause, en);
            en.contskel = clause;
            en.contdisplay = dc;
            return true;
        } else {
            if (d2.bind.length > 0)
                d2.remTab(en);
            return true;
        }
    }

    /**
     * <p>Perform the search inside the delegate.</p></ü>
     *
     * @param flags   The flags.
     * @param head    The term skeleton.
     * @param refhead The term display.
     * @param temp    The arguments skeleton.
     * @param ref     The arguments display.
     * @param en      The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean searchFirst(int flags,
                                     Object head, Display refhead,
                                     Object[] temp, Display ref,
                                     Engine en)
            throws EngineException, EngineMessage {
        Clause[] list = definedClauses(head, refhead, en);
        int at = 0;

        /* end of cursor */
        for (; ; ) {
            if (at == list.length)
                return false;
            if (multiVisible(list[at], en))
                break;
            at++;
        }

        AbstractUndo mark = en.bind;
        Clause clause;
        Display ref1 = null;
        boolean ext = refhead.getAndReset();
        /* search rope */
        for (; ; ) {
            clause = list[at++];
            if (ref1 == null) {
                ref1 = new Display(clause.size);
            } else {
                ref1.setSize(clause.size);
            }
            if (!(clause.head instanceof SkelCompound) ||
                    AbstractDefined.unifySearch(((SkelCompound) head).args, refhead,
                            ((SkelCompound) clause.head).args, ref1,
                            clause.head, en)) {
                Object end = Directive.interToBodySkel(clause, clause.last, en);
                if (BindUniv.unifyTerm(end, ref1, temp[1], ref, en)) {
                    if ((flags & OPT_RSLT_CREF) != 0) {
                        if (BindUniv.unifyTerm(clause, Display.DISPLAY_CONST, temp[2], ref, en))
                            break;
                    } else {
                        break;
                    }
                }
            }

            /* end of cursor */
            for (; ; ) {
                if (at == list.length)
                    return false;
                if (multiVisible(list[at], en))
                    break;
                at++;
            }

            /* undo bindings */
            en.fault = null;
            en.releaseBind(mark);
            if (en.fault != null)
                throw en.fault;
        }
        ref1.vars = clause.vars;

        if (ext)
            refhead.remTab(en);
        if (clause.size != 0)
            ref1.remTab(en);

        while (at != list.length) {
            if (multiVisible(list[at], en))
                break;
            at++;
        }

        if (at != list.length) {
            /* create choice point */
            en.choices = new ChoiceInspectMultifile(en.choices, at, list,
                    flags, en.contskel, en.contdisplay,
                    ref1, mark);
            en.number++;
        }
        /* succeed */
        return true;
    }

    /**
     * <p>Check whether the clause is multifile visible.</p>
     *
     * @param clause The clause.
     * @param en     The engine.
     * @return True if the clause is multifile visible.
     */
    static boolean multiVisible(Clause clause, Engine en) {
        SkelAtom sa = StackElement.callableToName(clause.head);
        return Clause.ancestorSource(sa.scope, en);
    }

}