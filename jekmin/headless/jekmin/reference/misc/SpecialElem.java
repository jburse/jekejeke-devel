package jekmin.reference.misc;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.arithmetic.SpecialCompare;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * <p>Provides additional elementary predicates.</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SpecialElem extends AbstractSpecial {
    private final static int SPECIAL_DIVMOD = 0;

    /**
     * <p>Create a additional elementary predicates.</p>
     *
     * @param i The id.
     */
    public SpecialElem(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        try {
            switch (id) {
                case SPECIAL_DIVMOD:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    Number alfa = SpecialEval.derefAndCastNumber(temp[0], ref);
                    Number beta = SpecialEval.derefAndCastNumber(temp[1], ref);
                    Number[] res = divMod(alfa, beta);
                    if (!en.unifyTerm(temp[2], ref, res[0], Display.DISPLAY_CONST))
                        return false;
                    if (!en.unifyTerm(temp[3], ref, res[1], Display.DISPLAY_CONST))
                        return false;
                    return en.getNext();
                default:
                    throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
            }
        } catch (ArithmeticException x) {
            throw new EngineMessage(EngineMessage.evaluationError(x.getMessage()));
        }
    }

    /********************************************************************/
    /* Rounding Operations (Supplement):                                */
    /*      divmod/4: divMod()                                          */
    /********************************************************************/

    /**
     * <p>Divide and truncate the two numbers, and also compute modulo.</p>
     * <p>The results corresponds to the truncation of the real division.</p>
     * <pre>
     *       divmod(X,Y,Z,T) where
     *             Z = integer(X / Y) and
     *             T = X - integer(X / Y) * Y
     * </pre>
     *
     * @param a The first operand.
     * @param b The second operand.
     * @return The division result and the modulo.
     * @throws ArithmeticException Shit happens.
     */
    private static Number[] divMod(Number a, Number b)
            throws ArithmeticException {
        switch (Math.max(SpecialCompare.category(a), SpecialCompare.category(b))) {
            case SpecialCompare.CATEGORY_INTEGER:
                int u = b.intValue();
                if (u == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                Number[] res = new Number[2];
                res[0] = TermAtomic.normBigInteger((long) a.intValue() / u);
                res[1] = TermAtomic.normBigInteger((long) a.intValue() % u);
                return res;
            case SpecialCompare.CATEGORY_BIG_INTEGER:
                BigInteger p = TermAtomic.widenBigInteger(b);
                if (p.signum() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                res = new Number[2];
                BigInteger[] res2 = TermAtomic.widenBigInteger(a).divideAndRemainder(p);
                res[0] = TermAtomic.normBigInteger(res2[0]);
                res[1] = TermAtomic.normBigInteger(res2[1]);
                return res;
            case SpecialCompare.CATEGORY_FLOAT:
                float f = b.floatValue();
                if (f == 0.0f)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                res = new Number[2];
                float g1 = a.floatValue();
                float g2 = g1 / f;
                if (Integer.MIN_VALUE <= g2 && g2 <= Integer.MAX_VALUE) {
                    res[0] = Integer.valueOf((int) g2);
                } else {
                    res[0] = TermAtomic.normBigInteger(new BigDecimal(g2).toBigInteger());
                }
                res[1] = TermAtomic.makeFloat(g1 % f);
                return res;
            case SpecialCompare.CATEGORY_DOUBLE:
                double d = b.doubleValue();
                if (d == 0.0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                res = new Number[2];
                double e1 = a.doubleValue();
                double e2 = e1 / d;
                if (Long.MIN_VALUE <= e2 && e2 <= Long.MAX_VALUE) {
                    res[0] = TermAtomic.normBigInteger((long) e2);
                } else {
                    res[0] = TermAtomic.normBigInteger(new BigDecimal(e2).toBigInteger());
                }
                res[1] = TermAtomic.makeDouble(e1 % d);
                return res;
            case SpecialCompare.CATEGORY_LONG:
            case SpecialCompare.CATEGORY_BIG_DECIMAL:
                BigDecimal h = TermAtomic.widenBigDecimal(b);
                if (h.signum() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                res = new Number[2];
                BigDecimal j = TermAtomic.widenBigDecimal(a);
                BigDecimal k = j.divide(h, 0, BigDecimal.ROUND_DOWN);
                res[0] = TermAtomic.normBigInteger(k.unscaledValue());
                res[1] = TermAtomic.normBigDecimal(j.subtract(k.multiply(h)));
                return res;
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

}