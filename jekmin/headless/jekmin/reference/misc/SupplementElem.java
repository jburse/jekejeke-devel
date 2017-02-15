package jekmin.reference.misc;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Special;
import jekpro.model.molec.Display;
import jekpro.model.molec.DisplayClause;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Goal;
import jekpro.reference.arithmetic.SpecialCompare;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;

/**
 * <p>Provides additional elementary evaluables.</p>
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
public final class SupplementElem extends Special {
    private final static int EVALUABLE_ULP = 0;
    private final static int EVALUABLE_SCALE = 1;
    private final static int EVALUABLE_PRECISION = 2;
    private final static int EVALUABLE_UNSCALED_VALUE = 3;
    private final static int EVALUABLE_GCD = 4;
    private final static int EVALUABLE_DECIMAL = 5;
    private final static int EVALUABLE_ADD = 6;
    private final static int EVALUABLE_SUB = 7;
    private final static int EVALUABLE_MUL = 8;
    private final static int EVALUABLE_SLASH = 9;
    private final static int EVALUABLE_INT_POW = 10;

    /**
     * <p>Create an elementary evaluable.</p>
     *
     * @param i The built-in ID.
     */
    public SupplementElem(int i) {
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
            if (id < EVALUABLE_GCD) {
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.computeExpr(temp[0], ref, r, u);
                switch (id) {
                    case EVALUABLE_ULP:
                        Number alfa = EngineMessage.castNumber(en.skel, en.display);
                        en.skel = ulp(alfa);
                        en.display = Display.DISPLAY_CONST;
                        return;
                    case EVALUABLE_SCALE:
                        alfa = EngineMessage.castDecimal(en.skel, en.display);
                        en.skel = Integer.valueOf(TermAtomic.scale(alfa));
                        en.display = Display.DISPLAY_CONST;
                        return;
                    case EVALUABLE_PRECISION:
                        alfa = EngineMessage.castDecimal(en.skel, en.display);
                        en.skel = Integer.valueOf(TermAtomic.precision(alfa));
                        en.display = Display.DISPLAY_CONST;
                        return;
                    case EVALUABLE_UNSCALED_VALUE:
                        alfa = EngineMessage.castDecimal(en.skel, en.display);
                        en.skel = TermAtomic.normBigInteger(TermAtomic.unscaledValue(alfa));
                        en.display = Display.DISPLAY_CONST;
                        return;
                    default:
                        throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
                }
            } else if (id < EVALUABLE_ADD) {
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.computeExpr(temp[0], ref, r, u);
                switch (id) {
                    case EVALUABLE_GCD:
                        Number alfa = EngineMessage.castInteger(en.skel, en.display);
                        en.computeExpr(temp[1], ref, r, u);
                        Number beta = EngineMessage.castInteger(en.skel, en.display);
                        en.skel = gcd(alfa, beta);
                        en.display = Display.DISPLAY_CONST;
                        return;
                    case EVALUABLE_DECIMAL:
                        alfa = EngineMessage.castNumber(en.skel, en.display);
                        en.computeExpr(temp[1], ref, r, u);
                        beta = EngineMessage.castInteger(en.skel, en.display);
                        en.skel = decimal(alfa, beta);
                        en.display = Display.DISPLAY_CONST;
                        return;
                    default:
                        throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
                }
            } else {
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.computeExpr(temp[0], ref, r, u);
                Number alfa = EngineMessage.castNumber(en.skel, en.display);
                en.computeExpr(temp[1], ref, r, u);
                switch (id) {
                    case EVALUABLE_ADD:
                        Number beta = EngineMessage.castNumber(en.skel, en.display);
                        en.computeExpr(temp[2], ref, r, u);
                        Number gamma = EngineMessage.castInteger(en.skel, en.display);
                        en.skel = add(alfa, beta, gamma);
                        en.display = Display.DISPLAY_CONST;
                        return;
                    case EVALUABLE_SUB:
                        beta = EngineMessage.castNumber(en.skel, en.display);
                        en.computeExpr(temp[2], ref, r, u);
                        gamma = EngineMessage.castInteger(en.skel, en.display);
                        en.skel = sub(alfa, beta, gamma);
                        en.display = Display.DISPLAY_CONST;
                        return;
                    case EVALUABLE_MUL:
                        beta = EngineMessage.castNumber(en.skel, en.display);
                        en.computeExpr(temp[2], ref, r, u);
                        gamma = EngineMessage.castInteger(en.skel, en.display);
                        en.skel = mul(alfa, beta, gamma);
                        en.display = Display.DISPLAY_CONST;
                        return;
                    case EVALUABLE_SLASH:
                        beta = EngineMessage.castNumber(en.skel, en.display);
                        en.computeExpr(temp[2], ref, r, u);
                        gamma = EngineMessage.castInteger(en.skel, en.display);
                        en.skel = div(alfa, beta, gamma);
                        en.display = Display.DISPLAY_CONST;
                        return;
                    case EVALUABLE_INT_POW:
                        beta = EngineMessage.castInteger(en.skel, en.display);
                        en.computeExpr(temp[2], ref, r, u);
                        gamma = EngineMessage.castInteger(en.skel, en.display);
                        en.skel = intPow(alfa, beta, gamma);
                        en.display = Display.DISPLAY_CONST;
                        return;
                    default:
                        throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
                }
            }
        } catch (ArithmeticException x) {
            throw new EngineMessage(EngineMessage.evaluationError(x.getMessage()));
        }
    }

    /********************************************************************/
    /* Additional Unary Number Operations:                              */
    /*      ulp/1: ulp()                                                */
    /********************************************************************/

    /**
     * <p>Return the ulp.</p>
     *
     * @param m The number.
     * @return The ulp.
     * @throws ArithmeticException Not a Prolog number.
     */
    private static Number ulp(Number m) throws ArithmeticException {
        if (m instanceof Integer || m instanceof BigInteger) {
            return Integer.valueOf(1);
        } else if (m instanceof Float) {
            return TermAtomic.guardFloat(Float.valueOf(Math.ulp(m.floatValue())));
        } else if (m instanceof Double) {
            return TermAtomic.guardDouble(Double.valueOf(Math.ulp(m.doubleValue())));
        } else if (m instanceof Long) {
            return Long.valueOf(1);
        } else {
            return BigDecimal.valueOf(1, ((BigDecimal) m).scale());
        }
    }


    /********************************************************************/
    /* Additional Binary Number Operations:                             */
    /*      gcd/2: gcd()                                                */
    /*      round/2: round()                                            */
    /*      decimal/2: decimal()                                        */
    /********************************************************************/

    /**
     * <p>Return the gcd.</p>
     *
     * @param m The first number.
     * @param n The second number.
     * @return The gcd.
     */
    private static Number gcd(Number m, Number n) {
        if (m instanceof Integer && n instanceof Integer) {
            int x = binaryGcd(Math.abs(m.intValue()), Math.abs(n.intValue()));
            if (x != Integer.MIN_VALUE) {
                return Integer.valueOf(x);
            } else {
                return BigInteger.valueOf(-(long) x);
            }
        } else {
            return TermAtomic.normBigInteger(
                    TermAtomic.widenBigInteger(m).gcd(
                            TermAtomic.widenBigInteger(n)));
        }
    }

    /**
     * <p>Return the gcd of two integers.</p>
     *
     * @param m The first number.
     * @param n The second number.
     * @return The gcd.
     */
    private static int binaryGcd(int m, int n) {
        if (n == 0)
            return m;
        if (m == 0)
            return n;

        // Right shift a & b till their last bits equal to 1.
        int aZeros = Integer.numberOfTrailingZeros(m);
        int bZeros = Integer.numberOfTrailingZeros(n);
        m >>>= aZeros;
        n >>>= bZeros;

        int t = (aZeros < bZeros ? aZeros : bZeros);

        while (m != n) {
            if ((m + 0x80000000) > (n + 0x80000000)) {  // a > b as unsigned
                m -= n;
                m >>>= Integer.numberOfTrailingZeros(m);
            } else {
                n -= m;
                n >>>= Integer.numberOfTrailingZeros(n);
            }
        }
        return m << t;
    }

    /**
     * <p>Return the round.</p>
     *
     * @param m The decimal number.
     * @param n The new scale.
     * @return The rounded decimal number.
     * @throws EngineMessage Not an integer.
     */
    private static Number decimal(Number m, Number n) throws EngineMessage {
        EngineMessage.checkNotLessThanZero(n);
        int y = EngineMessage.castIntValue(n);
        MathContext mc = new MathContext(y, RoundingMode.HALF_EVEN);
        if (m instanceof Integer) {
            return TermAtomic.normBigDecimal(
                    new BigDecimal(m.intValue(), mc));
        } else if (m instanceof BigInteger) {
            return TermAtomic.normBigDecimal(
                    new BigDecimal((BigInteger)m, mc));
        } else if (m instanceof Long) {
            if (mc.getPrecision() != 0 && (TermAtomic.log10(m.longValue()) > mc.getPrecision())) {
                return TermAtomic.normBigDecimal(
                        new BigDecimal(m.longValue(), mc));
            } else {
                return m;
            }
        } else if (m instanceof BigDecimal) {
            BigDecimal d = (BigDecimal) m;
            if (mc.getPrecision() != 0 && (d.precision() > mc.getPrecision())) {
                return TermAtomic.normBigDecimal(new BigDecimal(d.unscaledValue(), d.scale(), mc));
            } else {
                return d;
            }
        } else {
            return TermAtomic.normBigDecimal(
                    new BigDecimal(m.doubleValue(), mc));
        }
    }

    /********************************************************************/
    /* Additional Ternary Number Operations:                            */
    /*      add/3: add()                                                */
    /*      sub/3: sub()                                                */
    /*      mul/3: mul()                                                */
    /*      div/3: div()                                                */
    /*      int_pow/3: intPow()                                         */
    /********************************************************************/

    /**
     * <p>Add two decimal with some precision.</p>
     *
     * @param m The first Prolog decimal.
     * @param n The second Prolog decimal.
     * @param p The precision.
     * @return The result.
     * @throws EngineMessage Not an integer.
     */
    private static Number add(Number m, Number n, Number p) throws EngineMessage {
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
                EngineMessage.checkNotLessThanZero(p);
                int y = EngineMessage.castIntValue(p);
                MathContext mc = new MathContext(y, RoundingMode.HALF_EVEN);
                return TermAtomic.normBigDecimal(
                        TermAtomic.widenBigDecimal(m, mc).add(
                                TermAtomic.widenBigDecimal(n, mc), mc));
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Subtract a decimal by another decimal with some precision.</p>
     *
     * @param m The first Prolog decimal.
     * @param n The second Prolog decimal.
     * @param p The precision.
     * @return The result.
     * @throws EngineMessage Not an integer.
     */
    private static Number sub(Number m, Number n, Number p) throws EngineMessage {
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
                EngineMessage.checkNotLessThanZero(p);
                int y = EngineMessage.castIntValue(p);
                MathContext mc = new MathContext(y, RoundingMode.HALF_EVEN);
                return TermAtomic.normBigDecimal(
                        TermAtomic.widenBigDecimal(m, mc).subtract(
                                TermAtomic.widenBigDecimal(n, mc), mc));
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Multiply two decimal with some precision.</p>
     *
     * @param m The first Prolog decimal.
     * @param n The second Prolog decimal.
     * @param p The precision.
     * @return The result.
     * @throws EngineMessage Not an integer.
     */
    private static Number mul(Number m, Number n, Number p) throws EngineMessage {
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
                EngineMessage.checkNotLessThanZero(p);
                int y = EngineMessage.castIntValue(p);
                MathContext mc = new MathContext(y, RoundingMode.HALF_EVEN);
                return TermAtomic.normBigDecimal(
                        TermAtomic.widenBigDecimal(m, mc).multiply(
                                TermAtomic.widenBigDecimal(n, mc), mc));
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Divide a decimal by another decimal with some precision.</p>
     *
     * @param m The first Prolog decimal.
     * @param n The second Prolog decimal.
     * @param p The precision.
     * @return The result.
     * @throws EngineMessage Not an integer.
     */
    private static Number div(Number m, Number n, Number p) throws EngineMessage {
        EngineMessage.checkNotLessThanZero(p);
        int y = EngineMessage.castIntValue(p);
        MathContext mc = new MathContext(y, RoundingMode.HALF_EVEN);
        return TermAtomic.normBigDecimal(
                TermAtomic.widenBigDecimal(m, mc).divide(TermAtomic.widenBigDecimal(n, mc), mc));
    }

    /**
     * <p>Raise a decimal to an integer power with some precision.</p>
     *
     * @param m The Prolog decimal.
     * @param n The Prolog integer.
     * @param p The precision.
     * @return The result.
     * @throws EngineMessage Not an integer.
     */
    private static Number intPow(Number m, Number n, Number p) throws EngineMessage {
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
        } else {
            EngineMessage.checkNotLessThanZero(p);
            int y = EngineMessage.castIntValue(p);
            MathContext mc = new MathContext(y, RoundingMode.HALF_EVEN);
            return TermAtomic.normBigDecimal(
                    TermAtomic.widenBigDecimal(m, mc).pow(x, mc));
        }
    }

}
