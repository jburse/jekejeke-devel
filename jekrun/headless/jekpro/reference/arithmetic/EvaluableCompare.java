package jekpro.reference.arithmetic;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;

import java.math.BigDecimal;

/**
 * <p>Provides the compare evaluables.</p>
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
public final class EvaluableCompare extends AbstractSpecial {
    private final static int EVALUABLE_MIN = 0;
    private final static int EVALUABLE_MAX = 1;

    /**
     * <p>Create a compare evaluable.</p>
     *
     * @param i The built-in ID.
     */
    public EvaluableCompare(int i) {
        super(i);
        subflags |= MASK_DELE_ARIT;
    }

    /**
     * <p>Arithmetically evaluate an evaluable.</p>
     * <p>The evaluable is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the contskel and contdisplay of the engine.</p>
     * <p>The result is passed via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public final void moniEvaluate(Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case EVALUABLE_MIN:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.computeExpr(temp[0], ref);
                Display d = en.display;
                boolean multi = d.getAndReset();
                Number alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                if (multi)
                    d.remTab(en);
                en.computeExpr(temp[1], ref);
                d = en.display;
                multi = d.getAndReset();
                Number beta = SpecialEval.derefAndCastNumber(en.skel, d);
                if (multi)
                    d.remTab(en);
                en.skel = min(alfa, beta);
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_MAX:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref);
                d = en.display;
                multi = d.getAndReset();
                alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                if (multi)
                    d.remTab(en);
                en.computeExpr(temp[1], ref);
                d = en.display;
                multi = d.getAndReset();
                beta = SpecialEval.derefAndCastNumber(en.skel, d);
                if (multi)
                    d.remTab(en);
                en.skel = max(alfa, beta);
                en.display = Display.DISPLAY_CONST;
                return;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /********************************************************************/
    /* Compare:                                                         */
    /*      (min)/2: min()                                              */
    /*      (max)/2: max()                                              */
    /********************************************************************/

    /**
     * <p>Min the two number.</p>
     *
     * @param m The first number.
     * @param n The second number.
     * @return The minimum of the two numbers.
     */
    private static Number min(Number m, Number n) {
        switch (Math.max(SpecialCompare.numType(m), SpecialCompare.numType(n))) {
            case SpecialCompare.NUM_INTEGER:
            case SpecialCompare.NUM_BIG_INTEGER:
                if (SpecialCompare.compareIntegerArithmetical(m, n) > 0) {
                    return n;
                } else {
                    return m;
                }
            case SpecialCompare.NUM_FLOAT:
                float x = m.floatValue();
                float y = n.floatValue();
                if (x > y) {
                    m = n;
                    x = y;
                }
                return (m instanceof Float ? m :
                        TermAtomic.makeFloat(x));
            case SpecialCompare.NUM_DOUBLE:
                double a = m.doubleValue();
                double b = n.doubleValue();
                if (a > b) {
                    m = n;
                    a = b;
                }
                return (m instanceof Double ? m :
                        TermAtomic.makeDouble(a));
            case SpecialCompare.NUM_LONG:
            case SpecialCompare.NUM_BIG_DECIMAL:
                BigDecimal u = TermAtomic.widenBigDecimal(m);
                BigDecimal v = TermAtomic.widenBigDecimal(n);
                if (u.compareTo(v) > 0) {
                    m = n;
                    u = v;
                }
                return ((m instanceof Long || m instanceof BigDecimal) ? m :
                        TermAtomic.normBigDecimal(u));
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Max the two number.</p>
     *
     * @param m The first number.
     * @param n The second number.
     * @return The minimum of the two numbers.
     */
    private static Number max(Number m, Number n) {
        switch (Math.max(SpecialCompare.numType(m), SpecialCompare.numType(n))) {
            case SpecialCompare.NUM_INTEGER:
            case SpecialCompare.NUM_BIG_INTEGER:
                if (SpecialCompare.compareIntegerArithmetical(m, n) < 0) {
                    return n;
                } else {
                    return m;
                }
            case SpecialCompare.NUM_FLOAT:
                float x = m.floatValue();
                float y = n.floatValue();
                if (x < y) {
                    m = n;
                    x = y;
                }
                return (m instanceof Float ? m :
                        TermAtomic.makeFloat(x));
            case SpecialCompare.NUM_DOUBLE:
                double a = m.doubleValue();
                double b = n.doubleValue();
                if (a < b) {
                    m = n;
                    a = b;
                }
                return (m instanceof Double ? m :
                        TermAtomic.makeDouble(a));
            case SpecialCompare.NUM_LONG:
            case SpecialCompare.NUM_BIG_DECIMAL:
                BigDecimal u = TermAtomic.widenBigDecimal(m);
                BigDecimal v = TermAtomic.widenBigDecimal(n);
                if (u.compareTo(v) < 0) {
                    m = n;
                    u = v;
                }
                return ((m instanceof Long || m instanceof BigDecimal) ? m :
                        TermAtomic.normBigDecimal(u));
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

}
