package jekpro.reference.arithmetic;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Special;
import jekpro.model.molec.Display;
import jekpro.model.molec.DisplayClause;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Goal;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

/**
 * <p>The foreign predicates for the module arithmetic/elem.</p>
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
public final class EvaluableElem extends Special {
    private final static int EVALUABLE_MINUS = 0;
    private final static int EVALUABLE_PLUS = 1;
    private final static int EVALUABLE_ABS = 2;
    private final static int EVALUABLE_SIGN = 3;
    private final static int EVALUABLE_FLOAT = 4;
    private final static int EVALUABLE_DECIMAL = 5;
    private final static int EVALUABLE_FLOAT32 = 6;
    private final static int EVALUABLE_ADD = 7;
    private final static int EVALUABLE_SUB = 8;
    private final static int EVALUABLE_MUL = 9;
    private final static int EVALUABLE_SLASH = 10;
    private final static int EVALUABLE_INT_POW = 11;

    /**
     * <p>Create an evaluable elem.</p>
     *
     * @param i The index.
     */
    public EvaluableElem(int i) {
        super(i);
        subflags |= MASK_DELE_ARIT;
    }

    /**
     * <p>Arithmetically evaluate an evaluable.</p>
     * <p>The evaluable is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The result is passed via the skel and display of the engine.</p>
     *
     * @param r  The continuation skel.
     * @param u  The continuation display.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public final void evalEvaluable(Goal r, DisplayClause u,
                                    Engine en)
            throws EngineMessage, EngineException {
        try {
            switch (id) {
                case EVALUABLE_MINUS:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    en.computeExpr(temp[0], ref, r, u);
                    Number alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = neg(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_PLUS:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref, r, u);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = alfa;
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_ABS:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref, r, u);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = abs(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_SIGN:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref, r, u);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = sign(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_FLOAT:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref, r, u);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = (alfa instanceof Double ? alfa :
                            TermAtomic.guardDouble(Double.valueOf(alfa.doubleValue())));
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_DECIMAL:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref, r, u);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = decimal(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_FLOAT32:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref, r, u);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = (alfa instanceof Float ? alfa :
                            TermAtomic.guardFloat(Float.valueOf(alfa.floatValue())));
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_ADD:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref, r, u);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.computeExpr(temp[1], ref, r, u);
                    Number beta = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = add(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_SUB:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref, r, u);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.computeExpr(temp[1], ref, r, u);
                    beta = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = sub(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_MUL:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref, r, u);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.computeExpr(temp[1], ref, r, u);
                    beta = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = mul(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_SLASH:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref, r, u);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.computeExpr(temp[1], ref, r, u);
                    beta = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = slash(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_INT_POW:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref, r, u);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.computeExpr(temp[1], ref, r, u);
                    beta = EngineMessage.castInteger(en.skel, en.display);
                    en.skel = intPow(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                default:
                    throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
            }
        } catch (ArithmeticException x) {
            throw new EngineMessage(EngineMessage.evaluationError(x.getMessage()));
        }
    }

    /**
     * <p>Negate the Prolog number.</p>
     *
     * @param m The Prolog number.
     * @return The negated Prolog number.
     * @throws ArithmeticException Not a Prolog number.
     */
    public static Number neg(Number m) throws ArithmeticException {
        if (m instanceof Integer) {
            int x = m.intValue();
            if (x != Integer.MIN_VALUE) {
                return Integer.valueOf(-x);
            } else {
                return BigInteger.valueOf(-(long) x);
            }
        } else if (m instanceof BigInteger) {
            return TermAtomic.normBigInteger(((BigInteger) m).negate());
        } else if (m instanceof Float) {
            return TermAtomic.guardFloat(Float.valueOf(-m.floatValue()));
        } else if (m instanceof Double) {
            return TermAtomic.guardDouble(Double.valueOf(-m.doubleValue()));
        } else if (m instanceof Long) {
            long y = m.longValue();
            if (y != Long.MIN_VALUE) {
                return Long.valueOf(-y);
            } else {
                return BigDecimal.valueOf(y).negate();
            }
        } else if (m instanceof BigDecimal) {
            return TermAtomic.normBigDecimal(((BigDecimal) m).negate());
        } else {
            throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }


    /**
     * <p>Abs the Prolog number.</p>
     *
     * @param m The Prolog number.
     * @return The absed Prolog number.
     * @throws ArithmeticException Not a Prolog number.
     */
    private static Number abs(Number m) throws ArithmeticException {
        if (m instanceof Integer) {
            int x = Math.abs(m.intValue());
            if (x != Integer.MIN_VALUE) {
                return Integer.valueOf(x);
            } else {
                return BigInteger.valueOf(-(long) x);
            }
        } else if (m instanceof BigInteger) {
            return TermAtomic.normBigInteger(((BigInteger) m).abs());
        } else if (m instanceof Float) {
            return TermAtomic.guardFloat(Float.valueOf(Math.abs(m.floatValue())));
        } else if (m instanceof Double) {
            return TermAtomic.guardDouble(Double.valueOf(Math.abs(m.doubleValue())));
        } else if (m instanceof Long) {
            long y = m.longValue();
            if (y != Long.MIN_VALUE) {
                return Long.valueOf(Math.abs(y));
            } else {
                return BigDecimal.valueOf(y).abs();
            }
        } else if (m instanceof BigDecimal) {
            return TermAtomic.normBigDecimal(((BigDecimal) m).abs());
        } else {
            throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Sign the Prolog number.</p>
     *
     * @param m The Prolog number.
     * @return The signed Prolog number.
     * @throws ArithmeticException Not a Prolog number.
     */
    public static Number sign(Number m) throws ArithmeticException {
        if (m instanceof Integer) {
            int x = m.intValue();
            return Integer.valueOf(Integer.signum(x));
        } else if (m instanceof BigInteger) {
            return Integer.valueOf(((BigInteger) m).signum());
        } else if (m instanceof Float) {
            return TermAtomic.guardFloat(Float.valueOf(Math.signum(m.floatValue())));
        } else if (m instanceof Double) {
            return TermAtomic.guardDouble(Double.valueOf(Math.signum(m.doubleValue())));
        } else if (m instanceof Long) {
            long y = m.longValue();
            return Long.valueOf(Long.signum(y));
        } else if (m instanceof BigDecimal) {
            return Long.valueOf(((BigDecimal) m).signum());
        } else {
            throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Convert the Prolog number to float.</p>
     *
     * @param m The number.
     * @return The float.
     */
    private static Number decimal(Number m) {
        if (m instanceof Integer) {
            return Long.valueOf(m.intValue());
        } else if (m instanceof BigInteger) {
            return TermAtomic.normBigDecimal((BigInteger) m, 0);
        } else if (m instanceof Float || m instanceof Double) {
            return TermAtomic.normBigDecimal(new BigDecimal(m.doubleValue()));
        } else if (m instanceof Long || m instanceof BigDecimal) {
            return m;
        } else {
            throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Add the two Prolog numbers.</p>
     *
     * @param m The first Prolog number.
     * @param n The second Prolog number.
     * @return The sum of the two numbers.
     * @throws ArithmeticException Not a Prolog number.
     */
    private static Number add(Number m, Number n) throws ArithmeticException {
        switch (Math.max(SpecialCompare.category(m), SpecialCompare.category(n))) {
            case SpecialCompare.CATEGORY_INTEGER:
                return TermAtomic.normBigInteger((long) m.intValue() + n.intValue());
            case SpecialCompare.CATEGORY_BIG_INTEGER:
                return TermAtomic.normBigInteger(
                        TermAtomic.widenBigInteger(m).add(
                                TermAtomic.widenBigInteger(n)));
            case SpecialCompare.CATEGORY_FLOAT:
                return TermAtomic.guardFloat(Float.valueOf(m.floatValue() +
                        n.floatValue()));
            case SpecialCompare.CATEGORY_DOUBLE:
                return TermAtomic.guardDouble(Double.valueOf(m.doubleValue() +
                        n.doubleValue()));
            case SpecialCompare.CATEGORY_LONG:
            case SpecialCompare.CATEGORY_BIG_DECIMAL:
                return TermAtomic.normBigDecimal(
                        TermAtomic.widenBigDecimal(m).add(
                                TermAtomic.widenBigDecimal(n)));
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Subtract the two Prolog numbers.</p>
     *
     * @param m The first Prolog number.
     * @param n The second Prolog number.
     * @return The first number subtracted by the second number.
     * @throws ArithmeticException Not a Prolog number.
     */
    private static Number sub(Number m, Number n) throws ArithmeticException {
        switch (Math.max(SpecialCompare.category(m), SpecialCompare.category(n))) {
            case SpecialCompare.CATEGORY_INTEGER:
                return TermAtomic.normBigInteger((long) m.intValue() - n.intValue());
            case SpecialCompare.CATEGORY_BIG_INTEGER:
                return TermAtomic.normBigInteger(
                        TermAtomic.widenBigInteger(m).subtract(
                                TermAtomic.widenBigInteger(n)));
            case SpecialCompare.CATEGORY_FLOAT:
                return TermAtomic.guardFloat(Float.valueOf(m.floatValue() -
                        n.floatValue()));
            case SpecialCompare.CATEGORY_DOUBLE:
                return TermAtomic.guardDouble(Double.valueOf(m.doubleValue() -
                        n.doubleValue()));
            case SpecialCompare.CATEGORY_LONG:
            case SpecialCompare.CATEGORY_BIG_DECIMAL:
                return TermAtomic.normBigDecimal(
                        TermAtomic.widenBigDecimal(m).subtract(
                                TermAtomic.widenBigDecimal(n)));
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Multiply the two Prolog numbers.</p>
     *
     * @param m The first Prolog number.
     * @param n The second Prolog number.
     * @return The product of the two numbers.
     * @throws ArithmeticException Not a Prolog number.
     */
    private static Number mul(Number m, Number n) throws ArithmeticException {
        switch (Math.max(SpecialCompare.category(m), SpecialCompare.category(n))) {
            case SpecialCompare.CATEGORY_INTEGER:
                return TermAtomic.normBigInteger((long) m.intValue() * n.intValue());
            case SpecialCompare.CATEGORY_BIG_INTEGER:
                return TermAtomic.normBigInteger(
                        TermAtomic.widenBigInteger(m).multiply(
                                TermAtomic.widenBigInteger(n)));
            case SpecialCompare.CATEGORY_FLOAT:
                return TermAtomic.guardFloat(Float.valueOf(m.floatValue() *
                        n.floatValue()));
            case SpecialCompare.CATEGORY_DOUBLE:
                return TermAtomic.guardDouble(Double.valueOf(m.doubleValue() *
                        n.doubleValue()));
            case SpecialCompare.CATEGORY_LONG:
            case SpecialCompare.CATEGORY_BIG_DECIMAL:
                return TermAtomic.normBigDecimal(
                        TermAtomic.widenBigDecimal(m).multiply(
                                TermAtomic.widenBigDecimal(n)));
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Slash the two Prolog numbers.</p>
     *
     * @param m The first Prolog number.
     * @param n The second Prolog number.
     * @return The first number slashed by the second number.
     * @throws ArithmeticException Not a Prolog number.
     */
    private static Number slash(Number m, Number n) throws ArithmeticException {
        switch (Math.max(SpecialCompare.category(m), SpecialCompare.category(n))) {
            case SpecialCompare.CATEGORY_INTEGER:
                double b = n.doubleValue();
                if (b == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.guardDouble(Double.valueOf(m.doubleValue() / b));
            case SpecialCompare.CATEGORY_BIG_INTEGER:
                BigDecimal c = TermAtomic.widenBigDecimal(n, MathContext.DECIMAL64);
                if (BigDecimal.ZERO.compareTo(c) == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                c = TermAtomic.widenBigDecimal(m, MathContext.DECIMAL64).divide(c, MathContext.DECIMAL64);
                return TermAtomic.guardDouble(Double.valueOf(c.doubleValue()));
            case SpecialCompare.CATEGORY_FLOAT:
            case SpecialCompare.CATEGORY_DOUBLE:
                b = n.doubleValue();
                if (b == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.guardDouble(Double.valueOf(m.doubleValue() / b));
            case SpecialCompare.CATEGORY_LONG:
                b = n.doubleValue();
                if (b == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.guardDouble(Double.valueOf(m.doubleValue() / b));
            case SpecialCompare.CATEGORY_BIG_DECIMAL:
                c = TermAtomic.widenBigDecimal(n, MathContext.DECIMAL64);
                if (BigDecimal.ZERO.compareTo(c) == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                c = TermAtomic.widenBigDecimal(m, MathContext.DECIMAL64).divide(c, MathContext.DECIMAL64);
                return TermAtomic.guardDouble(Double.valueOf(c.doubleValue()));
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Power the two Prolog number.</p>
     *
     * @param m The Prolog number.
     * @param n The Prolog integer.
     * @return The first integer raised to the power of the second integer.
     * @throws EngineMessage Not a Prolog number.
     */
    private static Number intPow(Number m, Number n) throws EngineMessage {
        EngineMessage.checkNotLessThanZero(n);
        int x = EngineMessage.castIntValue(n);
        if (m instanceof Integer || m instanceof BigInteger) {
            return TermAtomic.normBigInteger(
                    TermAtomic.widenBigInteger(m).pow(x));
        } else if (m instanceof Float) {
            return TermAtomic.guardFloat(Float.valueOf(
                    (float) Math.pow(m.floatValue(), x)));
        } else if (m instanceof Double) {
            return TermAtomic.guardDouble(Double.valueOf(
                    Math.pow(m.doubleValue(), x)));
        } else if (m instanceof Long || m instanceof BigDecimal) {
            return TermAtomic.normBigDecimal(
                    TermAtomic.widenBigDecimal(m).pow(x));
        } else {
            throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

}
