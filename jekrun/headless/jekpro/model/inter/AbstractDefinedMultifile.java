package jekpro.model.inter;

import jekpro.model.molec.*;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Goal;
import jekpro.model.rope.PreClause;
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
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
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
        BindCount[] d = en.display;
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

        AbstractBind mark = en.bind;
        Clause clause;
        BindCount[] ref = null;
        int lastalloc;
        /* search rope */
        for (; ; ) {
            clause = list[at++];
            if (ref == null) {
                ref = BindCount.newBindClause(clause.dispsize);
            } else {
                ref = BindCount.setSizeClause(clause.dispsize, ref);
            }
            lastalloc = (clause.intargs != null ?
                    AbstractDefined.unifyDefined(((SkelCompound) t).args, d,
                            ((SkelCompound) clause.head).args, ref,
                            clause.intargs, en) : 0);
            if (lastalloc != -1)
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
        Display u = en.contdisplay;
        Display dc = new Display();
        dc.bind = ref;
        dc.lastalloc = lastalloc;
        dc.number = en.number;
        dc.prune = ((subflags & MASK_DEFI_NOBR) != 0 ? u.prune : dc);
        dc.goalskel = en.contskel;
        dc.goaldisplay = u;

        int nextat = at;
        while (nextat != list.length) {
            if (multiVisible(list[nextat], en))
                break;
            nextat++;
        }

        if (nextat != list.length) {
            /* create choice point */
            en.choices = new ChoiceDefinedMultfile(en.choices, at, list, dc, mark, nextat);
            en.number++;
            dc.flags |= Display.MASK_DPCL_MORE;
        }
        en.contskel = clause;
        en.contdisplay = dc;
        return en.getNext();
    }

    /**
     * <p>Perform the search inside the delegate.</p></ü>
     *
     * @param flags   The flags.
     * @param head    The head skeleton.
     * @param refhead The head display.
     * @param temp    The arguments skeleton.
     * @param ref     The arguments display.
     * @param en      The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean searchFirst(int flags,
                                     Object head, BindCount[] refhead,
                                     Object[] temp, BindCount[] ref,
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

        AbstractBind mark = en.bind;
        Clause clause;
        BindCount[] ref1 = null;
        /* search rope */
        for (; ; ) {
            clause = list[at++];
            if (ref1 == null) {
                ref1 = BindCount.newBind(clause.size);
            } else {
                ref1 = BindCount.setSize(clause.size, ref1);
            }
            if (!(clause.head instanceof SkelCompound) ||
                    AbstractDefined.unifyArgs(((SkelCompound) head).args, refhead,
                            ((SkelCompound) clause.head).args, ref1, en)) {
                Object end = PreClause.intermediateToBody(clause.next, en.store);
                if (en.unifyTerm(temp[1], ref, end, ref1)) {
                    if ((flags & OPT_RSLT_FRME) != 0) {
                        Frame frame = new Frame(clause, ref1);
                        if (en.unifyTerm(temp[2], ref,
                                frame, BindCount.DISPLAY_CONST)) {
                            if ((flags & OPT_RSLT_CREF) != 0) {
                                if (en.unifyTerm(temp[3], ref,
                                        clause, BindCount.DISPLAY_CONST))
                                    break;
                            } else {
                                break;
                            }
                        }
                    } else if ((flags & OPT_RSLT_CREF) != 0) {
                        if (en.unifyTerm(temp[2], ref,
                                clause, BindCount.DISPLAY_CONST))
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
        if (clause.size != 0)
            BindCount.remTab(ref1, en);

        while (at != list.length) {
            if (multiVisible(list[at], en))
                break;
            at++;
        }

        if (at != list.length) {
            /* create choice point */
            en.choices = new ChoiceInspectMultifile(en.choices, at, list,
                    flags, (Goal) en.contskel, en.contdisplay,
                    ref1, mark);
            en.number++;
        }
        /* succeed */
        return en.getNext();
    }

    /**
     * <p>Check whether the clause is multifile visible.</p>
     *
     * @param clause The clause.
     * @param en     The engine.
     * @return True if the clause is multifile visible.
     */
    static boolean multiVisible(Clause clause, Engine en) {
        SkelAtom sa = Frame.callableToName(clause.head);
        return Clause.ancestorSource(sa.scope, en);
    }

}