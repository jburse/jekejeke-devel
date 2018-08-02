package jekpro.reference.arithmetic;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;

import java.math.BigDecimal;
import java.math.BigInteger;

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
public final class EvaluableElem extends AbstractSpecial {
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
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public final boolean moniEvaluate(Engine en)
            throws EngineMessage, EngineException {
        try {
            switch (id) {
                case EVALUABLE_MINUS:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    boolean multi = en.computeExpr(temp[0], ref);
                    Display d = en.display;
                    Number alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = neg(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return false;
                case EVALUABLE_PLUS:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = en.computeExpr(temp[0], ref);
                    d = en.display;
                    alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = alfa;
                    en.display = Display.DISPLAY_CONST;
                    return false;
                case EVALUABLE_ABS:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = en.computeExpr(temp[0], ref);
                    d = en.display;
                    alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = abs(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return false;
                case EVALUABLE_SIGN:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = en.computeExpr(temp[0], ref);
                    d = en.display;
                    alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = sign(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return false;
                case EVALUABLE_FLOAT:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = en.computeExpr(temp[0], ref);
                    d = en.display;
                    alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = (alfa instanceof Double ? alfa :
                            TermAtomic.makeDouble(alfa.doubleValue()));
                    en.display = Display.DISPLAY_CONST;
                    return false;
                case EVALUABLE_DECIMAL:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = en.computeExpr(temp[0], ref);
                    d = en.display;
                    alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = decimal(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return false;
                case EVALUABLE_FLOAT32:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = en.computeExpr(temp[0], ref);
                    d = en.display;
                    alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = (alfa instanceof Float ? alfa :
                            TermAtomic.makeFloat(alfa.floatValue()));
                    en.display = Display.DISPLAY_CONST;
                    return false;
                case EVALUABLE_ADD:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = en.computeExpr(temp[0], ref);
                    d = en.display;
                    alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    multi = en.computeExpr(temp[1], ref);
                    d = en.display;
                    Number beta = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = add(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return false;
                case EVALUABLE_SUB:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = en.computeExpr(temp[0], ref);
                    d = en.display;
                    alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    multi = en.computeExpr(temp[1], ref);
                    d = en.display;
                    beta = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = sub(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return false;
                case EVALUABLE_MUL:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = en.computeExpr(temp[0], ref);
                    d = en.display;
                    alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    multi = en.computeExpr(temp[1], ref);
                    d = en.display;
                    beta = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = mul(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return false;
                case EVALUABLE_SLASH:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = en.computeExpr(temp[0], ref);
                    d = en.display;
                    alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    multi = en.computeExpr(temp[1], ref);
                    d = en.display;
                    beta = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = TermAtomic.makeDouble(slash(alfa, beta));
                    en.display = Display.DISPLAY_CONST;
                    return false;
                case EVALUABLE_INT_POW:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = en.computeExpr(temp[0], ref);
                    d = en.display;
                    alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    multi = en.computeExpr(temp[1], ref);
                    d = en.display;
                    beta = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    EngineMessage.checkNotLessThanZero(beta);
                    int x = EngineMessage.castIntValue(beta);
                    en.skel = intPow(alfa, x);
                    en.display = Display.DISPLAY_CONST;
                    return false;
                default:
                    throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
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
    public static Number neg(Number m)
            throws ArithmeticException {
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
            return TermAtomic.makeFloat(-m.floatValue());
        } else if (m instanceof Double) {
            return TermAtomic.makeDouble(-m.doubleValue());
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
    public static Number abs(Number m) throws ArithmeticException {
        if (m instanceof Integer) {
            int x = m.intValue();
            if (x != Integer.MIN_VALUE) {
                return Integer.valueOf(Math.abs(x));
            } else {
                return BigInteger.valueOf(Math.abs((long) x));
            }
        } else if (m instanceof BigInteger) {
            return TermAtomic.normBigInteger(((BigInteger) m).abs());
        } else if (m instanceof Float) {
            return TermAtomic.makeFloat(Math.abs(m.floatValue()));
        } else if (m instanceof Double) {
            return TermAtomic.makeDouble(Math.abs(m.doubleValue()));
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
            return Integer.valueOf(Integer.signum(m.intValue()));
        } else if (m instanceof BigInteger) {
            return Integer.valueOf(((BigInteger) m).signum());
        } else if (m instanceof Float) {
            return TermAtomic.makeFloat(Math.signum(m.floatValue()));
        } else if (m instanceof Double) {
            return TermAtomic.makeDouble(Math.signum(m.doubleValue()));
        } else if (m instanceof Long) {
            return Long.valueOf(Long.signum(m.longValue()));
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

    /********************************************************************/
    /* Basic Arithmetical Operations:                                   */
    /*      +/2: add()                                                  */
    /*      -/2: sub()                                                  */
    /*      * /2: mul()                                                 */
    /*      //2: slash()                                                */
    /********************************************************************/

    /**
     * <p>Add the two Prolog numbers.</p>
     *
     * @param m The first Prolog number.
     * @param n The second Prolog number.
     * @return The sum of the two numbers.
     * @throws ArithmeticException Not a Prolog number.
     */
    public static Number add(Number m, Number n) throws ArithmeticException {
        switch (Math.max(SpecialCompare.category(m), SpecialCompare.category(n))) {
            case SpecialCompare.CATEGORY_INTEGER:
                return TermAtomic.normBigInteger((long) m.intValue() + n.intValue());
            case SpecialCompare.CATEGORY_BIG_INTEGER:
                return TermAtomic.normBigInteger(
                        TermAtomic.widenBigInteger(m).add(
                                TermAtomic.widenBigInteger(n)));
            case SpecialCompare.CATEGORY_FLOAT:
                return TermAtomic.makeFloat(m.floatValue() +
                        n.floatValue());
            case SpecialCompare.CATEGORY_DOUBLE:
                return TermAtomic.makeDouble(m.doubleValue() +
                        n.doubleValue());
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
                return TermAtomic.makeFloat(m.floatValue() -
                        n.floatValue());
            case SpecialCompare.CATEGORY_DOUBLE:
                return TermAtomic.makeDouble(m.doubleValue() -
                        n.doubleValue());
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
                return TermAtomic.makeFloat(m.floatValue() *
                        n.floatValue());
            case SpecialCompare.CATEGORY_DOUBLE:
                return TermAtomic.makeDouble(m.doubleValue() *
                        n.doubleValue());
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
    private static double slash(Number m, Number n) throws ArithmeticException {
        double b = n.doubleValue();
        if (!TermAtomic.guardDouble(b))
            throw new ArithmeticException(
                    EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
        return m.doubleValue() / b;
    }

    /********************************************************************/
    /* New Arithmetical Operations:                                     */
    /*      ^/2: intPow()                                               */
    /********************************************************************/

    /**
     * <p>Power the two Prolog number.</p>
     *
     * @param m The first operand.
     * @param x The second operand.
     * @return The first integer raised to the power of the second integer.
     */
    private static Number intPow(Number m, int x) {
        if (m instanceof Integer) {
            int y = m.intValue();
            if (y == 0) {
                return Integer.valueOf(x == 0 ? 1 : 0);
            } else {
                int k = absBitlength(y);
                if (k == 0 || 62 / k >= x) {
                    return TermAtomic.normBigInteger(pow((long) y, x));
                } else {
                    return TermAtomic.normBigInteger(
                            BigInteger.valueOf(m.intValue()).pow(x));
                }
            }
        } else if (m instanceof BigInteger) {
            return TermAtomic.normBigInteger(((BigInteger) m).pow(x));
        } else if (m instanceof Float) {
            return TermAtomic.makeFloat(
                    (float) Math.pow(m.floatValue(), x));
        } else if (m instanceof Double) {
            return TermAtomic.makeDouble(
                    Math.pow(m.doubleValue(), x));
        } else if (m instanceof Long || m instanceof BigDecimal) {
            return TermAtomic.normBigDecimal(
                    TermAtomic.widenBigDecimal(m).pow(x));
        } else {
            throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Compute the bitlength.</p>
     *
     * @param m The base, positive or negative.
     * @return The bitlength.
     */
    private static int absBitlength(int m) {
        if (m != Integer.MIN_VALUE) {
            return 32 - Integer.numberOfLeadingZeros(Math.abs(m));
        } else {
            return 31;
        }
    }

    /**
     * <p>Compute the power.</p>
     *
     * @param m The base, positive or negative.
     * @param n The exponent, positive.
     * @return The exponentiation.
     */
    private static long pow(long m, int n) {
        long r = 1;
        while (n != 0) {
            if ((n & 1) != 0)
                r *= m;
            n >>= 1;
            if (n != 0)
                m *= m;
        }
        return r;
    }

}
