package jekpro.frequent.standard;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.reference.arithmetic.EvaluableElem;
import jekpro.reference.arithmetic.SpecialCompare;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides built-in predicates for the module arith.</p>
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
public final class SpecialArith extends AbstractSpecial {
    final static int SPECIAL_BETWEEN = 0;
    final static int SPECIAL_ABOVE = 1;

    /**
     * <p>Create an arith special.</p>
     *
     * @param i The id.
     */
    public SpecialArith(int i) {
        super(i);
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
        switch (id) {
            case SPECIAL_BETWEEN:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Number num1 = SpecialEval.derefAndCastNumber(temp[0], ref);
                Number num2 = SpecialEval.derefAndCastNumber(temp[1], ref);
                AbstractUndo mark = en.bind;
                int res = SpecialCompare.computeCmp(num1, num2);
                while (res <= 0) {
                    if (BindUniv.unifyTerm(num1, Display.DISPLAY_CONST, temp[2], ref, en)) {
                        if (res != 0) {
                            /* create choice point */
                            en.choices = new ChoiceArith(en.choices, num1,
                                    en.contskel, en.contdisplay, mark, id);
                            en.number++;
                        }
                        return true;
                    }

                    /* undo bindings */
                    en.fault = null;
                    en.releaseBind(mark);
                    if (en.fault != null)
                        throw en.fault;

                    num1 = EvaluableElem.add(num1, Integer.valueOf(1));
                    res = SpecialCompare.computeCmp(num1, num2);
                }
                return false;
            case SPECIAL_ABOVE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                num1 = SpecialEval.derefAndCastNumber(temp[0], ref);
                mark = en.bind;
                while (true) {
                    if (BindUniv.unifyTerm(num1, Display.DISPLAY_CONST, temp[1], ref, en)) {
                        /* create choice point */
                        en.choices = new ChoiceArith(en.choices, num1,
                                en.contskel, en.contdisplay, mark, id);
                        en.number++;
                        return true;
                    }

                    /* undo bindings */
                    en.fault = null;
                    en.releaseBind(mark);
                    if (en.fault != null)
                        throw en.fault;

                    num1 = EvaluableElem.add(num1, Integer.valueOf(1));
                }
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

}