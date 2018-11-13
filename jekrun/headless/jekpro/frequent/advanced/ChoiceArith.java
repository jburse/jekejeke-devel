package jekpro.frequent.advanced;

import jekpro.model.inter.AbstractChoice;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.rope.Goal;
import jekpro.reference.arithmetic.EvaluableElem;
import jekpro.reference.arithmetic.SpecialCompare;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>Choice point for module arith built-ins.</p>
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
final class ChoiceArith extends AbstractChoice {
    private Number cur;
    private final Goal goalskel;
    private final Display goaldisplay;
    private final AbstractBind mark;
    private final int id;

    /**
     * <p>Create pick choice.</p>
     *
     * @param n The molec.
     * @param m The mark.
     */
    ChoiceArith(AbstractChoice n,
                Number c, Goal r, Display u,
                AbstractBind m,
                int i) {
        super(n);
        goalskel = r;
        goaldisplay = u;
        cur = c;
        mark = m;
        id = i;
    }

    /**
     * <p>Logically evaluate a goal in a list of goals for an additional time.</p>
     * <p>The result is returned via the skel and display of the engine.</p>
     * <p>A new exception sliding window is returned via the engine display.</p>
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

        /* undo bindings */
        en.contskel = goalskel;
        en.contdisplay = goaldisplay;
        en.fault = null;
        en.releaseBind(mark);
        if (en.fault != null)
            throw en.fault;

        Goal ir = goalskel;
        Object t = ir.goal;
        BindCount[] d = goaldisplay.bind;
        if ((ir.flags & Goal.MASK_GOAL_NAKE) != 0) {
            /* inlined deref */
            BindVar b1;
            while (t instanceof SkelVar &&
                    (b1 = d[((SkelVar) t).id]).display != null) {
                t = b1.skel;
                d = b1.display;
            }
        }
        Object[] temp = ((SkelCompound) t).args;

        switch (id) {
            case SpecialArith.SPECIAL_BETWEEN:
                Number num2 = SpecialEval.derefAndCastNumber(temp[1], d);

                cur = EvaluableElem.add(cur, Integer.valueOf(1));

                int res = SpecialCompare.computeCmp(cur, num2);
                while (res <= 0) {
                    if (en.unifyTerm(temp[2], d, cur, BindCount.DISPLAY_CONST)) {
                        if (res != 0) {
                            /* reuse choice point */
                            en.choices = this;
                            en.number++;
                        }
                        return en.getNext();
                    }

                    /* undo bindings */
                    en.fault = null;
                    en.releaseBind(mark);
                    if (en.fault != null)
                        throw en.fault;

                    cur = EvaluableElem.add(cur, Integer.valueOf(1));
                    res = SpecialCompare.computeCmp(cur, num2);
                }
                return false;
            case SpecialArith.SPECIAL_ABOVE:
                cur = EvaluableElem.add(cur, Integer.valueOf(1));

                while (true) {
                    if (en.unifyTerm(temp[1], d, cur, BindCount.DISPLAY_CONST)) {
                        /* reuse choice point */
                        en.choices = this;
                        en.number++;
                        return en.getNext();
                    }

                    /* undo bindings */
                    en.fault = null;
                    en.releaseBind(mark);
                    if (en.fault != null)
                        throw en.fault;

                    cur = EvaluableElem.add(cur, Integer.valueOf(1));
                }
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Free data used to logically evaluate a goal an additional time.</p>
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
    }

}