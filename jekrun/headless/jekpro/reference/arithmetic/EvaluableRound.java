package jekpro.reference.arithmetic;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Special;
import jekpro.model.molec.*;
import jekpro.model.rope.Goal;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * <p>Provides the round evaluables.</p>
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
public final class EvaluableRound extends Special {
    private final static int EVALUABLE_TRUNCATE = 0;
    private final static int EVALUABLE_FLOOR = 1;
    private final static int EVALUABLE_CEILING = 2;
    private final static int EVALUABLE_ROUND = 3;
    private final static int EVALUABLE_SLASH_SLASH = 4;
    private final static int EVALUABLE_REM = 5;
    private final static int EVALUABLE_DIV = 6;
    private final static int EVALUABLE_MOD = 7;

    /**
     * <p>Create a round evaluable.</p>
     *
     * @param i The built-in ID.
     */
    public EvaluableRound(int i) {
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
            if (id < EVALUABLE_SLASH_SLASH) {
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.computeExpr(temp[0], ref, r, u);
                Number alfa = EngineMessage.castNumber(en.skel, en.display);
                switch (id) {
                    case EVALUABLE_TRUNCATE:
                        en.skel = truncate(alfa);
                        en.display = Display.DISPLAY_CONST;
                        return;
                    case EVALUABLE_FLOOR:
                        en.skel = floor(alfa);
                        en.display = Display.DISPLAY_CONST;
                        return;
                    case EVALUABLE_CEILING:
                        en.skel = ceiling(alfa);
                        en.display = Display.DISPLAY_CONST;
                        return;
                    case EVALUABLE_ROUND:
                        en.skel = round(alfa);
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
                Number beta = EngineMessage.castNumber(en.skel, en.display);
                switch (id) {
                    case EVALUABLE_SLASH_SLASH:
                        en.skel = slashSlash(alfa, beta);
                        en.display = Display.DISPLAY_CONST;
                        return;
                    case EVALUABLE_REM:
                        en.skel = rem(alfa, beta);
                        en.display = Display.DISPLAY_CONST;
                        return;
                    case EVALUABLE_DIV:
                        en.skel = div(alfa, beta);
                        en.display = Display.DISPLAY_CONST;
                        return;
                    case EVALUABLE_MOD:
                        en.skel = mod(alfa, beta);
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
    /* Rounding Operations (Part II):                                   */
    /*      (truncate)/1: truncate()                                    */
    /*      (floor)/1: floor()                                          */
    /*      (ceiling)/1: ceiling()                                      */
    /*      (round)/1: round()                                          */
    /********************************************************************/

    /**
     * <p>Truncate the number.</p>
     * <p>For decimals this corresponds to toBigInteger().</p>
     * <p>For float this corresponds to the (long) cast.</p>
     *
     * @param m The number.
     * @return The truncated number.
     */
    private static Number truncate(Number m) {
        if (m instanceof Integer || m instanceof BigInteger) {
            return m;
        } else if (m instanceof Float) {
            float f = m.floatValue();
            if (Long.MIN_VALUE <= f && f <= Long.MAX_VALUE) {
                return TermAtomic.normBigInteger((long) f);
            } else {
                return TermAtomic.normBigInteger(new BigDecimal(f).toBigInteger());
            }
        } else if (m instanceof Double) {
            double d = m.doubleValue();
            if (Long.MIN_VALUE <= d && d <= Long.MAX_VALUE) {
                return TermAtomic.normBigInteger((long) d);
            } else {
                return TermAtomic.normBigInteger(new BigDecimal(d).toBigInteger());
            }
        } else if (m instanceof Long) {
            return TermAtomic.normBigInteger(m.longValue());
        } else {
            return TermAtomic.normBigInteger(((BigDecimal) m).toBigInteger());
        }
    }

    /**
     * <p>Floor the number.</p>
     * <p>For decimals this corresponds to ROUND_FLOOR.</p>
     * <p>For floats this corresponds to Math.floor().</p>
     *
     * @param m The number.
     * @return The floored number.
     */
    private static Number floor(Number m) {
        if (m instanceof Integer || m instanceof BigInteger) {
            return m;
        } else if (m instanceof Float) {
            float f = m.floatValue();
            if (Long.MIN_VALUE <= f && f <= Long.MAX_VALUE) {
                return TermAtomic.normBigInteger((long) Math.floor(f));
            } else {
                return TermAtomic.normBigInteger(new BigDecimal(f).setScale(0,
                        BigDecimal.ROUND_FLOOR).unscaledValue());
            }
        } else if (m instanceof Double) {
            double d = m.doubleValue();
            if (Long.MIN_VALUE <= d && d <= Long.MAX_VALUE) {
                return TermAtomic.normBigInteger((long) Math.floor(d));
            } else {
                return TermAtomic.normBigInteger(new BigDecimal(d).setScale(0,
                        BigDecimal.ROUND_FLOOR).unscaledValue());
            }
        } else if (m instanceof Long) {
            return TermAtomic.normBigInteger(m.longValue());
        } else {
            return TermAtomic.normBigInteger(((BigDecimal) m).setScale(0,
                    BigDecimal.ROUND_FLOOR).unscaledValue());
        }
    }

    /**
     * <p>Ceil the number.</p>
     * <p>For decimals this corresponds to ROUND_CEILING.</p>
     * <p>For floats this corresponds to Math.ceil().</p>
     *
     * @param m The number.
     * @return The ceiled number.
     */
    private static Number ceiling(Number m) {
        if (m instanceof Integer || m instanceof BigInteger) {
            return m;
        } else if (m instanceof Float) {
            float f = m.floatValue();
            if (Long.MIN_VALUE <= f && f <= Long.MAX_VALUE) {
                return TermAtomic.normBigInteger((long) Math.ceil(f));
            } else {
                return TermAtomic.normBigInteger(new BigDecimal(f).setScale(0,
                        BigDecimal.ROUND_CEILING).unscaledValue());
            }

        } else if (m instanceof Double) {
            double d = m.doubleValue();
            if (Long.MIN_VALUE <= d && d <= Long.MAX_VALUE) {
                return TermAtomic.normBigInteger((long) Math.ceil(d));
            } else {
                return TermAtomic.normBigInteger(new BigDecimal(d).setScale(0,
                        BigDecimal.ROUND_CEILING).unscaledValue());
            }
        } else if (m instanceof Long) {
            return TermAtomic.normBigInteger(m.longValue());
        } else {
            return TermAtomic.normBigInteger(((BigDecimal) m).setScale(0,
                    BigDecimal.ROUND_CEILING).unscaledValue());
        }
    }

    /**
     * <p>Round the number.</p>
     * <p>For decimals this corresponds to ROUND_HALF_UP.</p>
     * <p>For floats this corresponds to (long) cast of x+sign(x)*0.5.</p>
     *
     * @param m The number.
     * @return The rounded number.
     */
    private static Number round(Number m) {
        if (m instanceof Integer || m instanceof BigInteger) {
            return m;
        } else if (m instanceof Float) {
            float f = m.floatValue();
            if (Long.MIN_VALUE <= f && f <= Long.MAX_VALUE) {
                return TermAtomic.normBigInteger((long) (f >= 0 ? f + 0.5 : f - 0.5));
            } else {
                return TermAtomic.normBigInteger(new BigDecimal(f).setScale(0,
                        BigDecimal.ROUND_HALF_UP).unscaledValue());
            }
        } else if (m instanceof Double) {
            double d = m.doubleValue();
            if (Long.MIN_VALUE <= d && d <= Long.MAX_VALUE) {
                return TermAtomic.normBigInteger((long) (d >= 0 ? d + 0.5 : d - 0.5));
            } else {
                return TermAtomic.normBigInteger(new BigDecimal(d).setScale(0,
                        BigDecimal.ROUND_HALF_UP).unscaledValue());
            }
        } else if (m instanceof Long) {
            return TermAtomic.normBigInteger(m.longValue());
        } else {
            return TermAtomic.normBigInteger(((BigDecimal) m).setScale(0,
                    BigDecimal.ROUND_HALF_UP).unscaledValue());
        }
    }

    /********************************************************************/
    /* Rounding Operations (Part I):                                    */
    /*      (//)/2: slashSlash()                                        */
    /*      (div)/2: div()                                              */
    /*      (rem)/2: rem()                                              */
    /*      (mod)/2: mod()                                              */
    /********************************************************************/

    /**
     * <p>Divide and truncate the two numbers.</p>
     * <p>The results corresponds to the truncation of the real division.</p>
     * <pre>
     *       X // Y = truncate(X / Y).
     * </pre>
     *
     * @param a The first number.
     * @param b The second number.
     * @return The first number divided by the second number.
     * @throws ArithmeticException Shit happens.
     */
    private static Number slashSlash(Number a, Number b) throws ArithmeticException {
        switch (Math.max(SpecialCompare.category(a), SpecialCompare.category(b))) {
            case SpecialCompare.CATEGORY_INTEGER:
                if (b.intValue() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.normBigInteger((long) a.intValue() / b.intValue());
            case SpecialCompare.CATEGORY_BIG_INTEGER:
                if (b instanceof Integer && b.intValue() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.normBigInteger(TermAtomic.widenBigInteger(a).divide(
                        TermAtomic.widenBigInteger(b)));
            case SpecialCompare.CATEGORY_FLOAT:
                float f = b.floatValue();
                if (f == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                float g = a.floatValue() / f;
                if (Long.MIN_VALUE <= g && g <= Long.MAX_VALUE) {
                    return TermAtomic.normBigInteger((long) g);
                } else {
                    return TermAtomic.normBigInteger(new BigDecimal(g).toBigInteger());
                }
            case SpecialCompare.CATEGORY_DOUBLE:
                double d = b.doubleValue();
                if (d == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                double e = a.doubleValue() / d;
                if (Long.MIN_VALUE <= e && e <= Long.MAX_VALUE) {
                    return TermAtomic.normBigInteger((long) e);
                } else {
                    return TermAtomic.normBigInteger(new BigDecimal(e).toBigInteger());
                }
            case SpecialCompare.CATEGORY_LONG:
            case SpecialCompare.CATEGORY_BIG_DECIMAL:
                BigDecimal h = TermAtomic.widenBigDecimal(b);
                if (h.compareTo(BigDecimal.ZERO) == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.normBigInteger(TermAtomic.widenBigDecimal(a).divide(
                        h, 0, BigDecimal.ROUND_DOWN).unscaledValue());
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Divide and floor the two numbers.</p>
     * <p>The results corresponds to the floor of the real division.</p>
     * <pre>
     *       X div Y = floor(X / Y).
     * </pre>
     *
     * @param a The first number.
     * @param b The second number.
     * @return The first number divided by the second number.
     * @throws ArithmeticException Shit happens.
     */
    private static Number div(Number a, Number b) {
        switch (Math.max(SpecialCompare.category(a), SpecialCompare.category(b))) {
            case SpecialCompare.CATEGORY_INTEGER:
                int p = b.intValue();
                if (p == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                int q = a.intValue();
                if ((q < 0) && (p >= 0)) {
                    return TermAtomic.normBigInteger(((long) q - p + 1) / p);
                } else if ((q >= 0) && (p < 0)) {
                    return TermAtomic.normBigInteger(((long) q - p - 1) / p);
                } else {
                    return TermAtomic.normBigInteger((long) q / p);
                }
            case SpecialCompare.CATEGORY_BIG_INTEGER:
                if (b instanceof Integer && b.intValue() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                BigInteger k = TermAtomic.widenBigInteger(b);
                BigInteger j = TermAtomic.widenBigInteger(a);
                if ((j.compareTo(BigInteger.ZERO) < 0) &&
                        (k.compareTo(BigInteger.ZERO) >= 0)) {
                    return TermAtomic.normBigInteger(j.subtract(k).add(
                            BigInteger.ONE).divide(k));
                } else if ((j.compareTo(BigInteger.ZERO) >= 0) &&
                        (k.compareTo(BigInteger.ZERO) < 0)) {
                    return TermAtomic.normBigInteger(j.subtract(k).subtract(
                            BigInteger.ONE).divide(k));
                } else {
                    return TermAtomic.normBigInteger(j.divide(k));
                }
            case SpecialCompare.CATEGORY_FLOAT:
                float f = b.floatValue();
                if (f == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                float g = a.floatValue() / f;
                if (Long.MIN_VALUE <= g && g <= Long.MAX_VALUE) {
                    return TermAtomic.normBigInteger((long) Math.floor(g));
                } else {
                    return TermAtomic.normBigInteger(new BigDecimal(g).setScale(0,
                            BigDecimal.ROUND_FLOOR).unscaledValue());
                }
            case SpecialCompare.CATEGORY_DOUBLE:
                double d = b.doubleValue();
                if (d == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                double e = a.doubleValue() / d;
                if (Long.MIN_VALUE <= e && e <= Long.MAX_VALUE) {
                    return TermAtomic.normBigInteger((long) Math.floor(e));
                } else {
                    return TermAtomic.normBigInteger(new BigDecimal(e).setScale(0,
                            BigDecimal.ROUND_FLOOR).unscaledValue());
                }
            case SpecialCompare.CATEGORY_LONG:
            case SpecialCompare.CATEGORY_BIG_DECIMAL:
                BigDecimal h = TermAtomic.widenBigDecimal(b);
                if (h.compareTo(BigDecimal.ZERO) == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.normBigInteger(TermAtomic.widenBigDecimal(a).divide(
                        h, 0, BigDecimal.ROUND_FLOOR).unscaledValue());
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Remainder of the two numbers.</p>
     * <p>The result is related to (//)/2 as follows:</p>
     * <pre>
     *      X rem Y = X - (X // Y) * Y.
     * </pre>
     *
     * @param a The first number.
     * @param b The second number.
     * @return The remainder of the first number by the second number.
     * @throws ArithmeticException Shit happens.
     */
    private static Number rem(Number a, Number b) throws ArithmeticException {
        switch (Math.max(SpecialCompare.category(a), SpecialCompare.category(b))) {
            case SpecialCompare.CATEGORY_INTEGER:
                int p = b.intValue();
                if (p == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.normBigInteger((long) a.intValue() % p);
            case SpecialCompare.CATEGORY_BIG_INTEGER:
                if (b instanceof Integer && b.intValue() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.normBigInteger(TermAtomic.widenBigInteger(a).remainder(
                        TermAtomic.widenBigInteger(b)));
            case SpecialCompare.CATEGORY_FLOAT:
                float f = b.floatValue();
                if (f == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                float g = a.floatValue();
                return TermAtomic.guardFloat(Float.valueOf(g % f));
            case SpecialCompare.CATEGORY_DOUBLE:
                double d = b.doubleValue();
                if (d == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                double e = a.doubleValue();
                return TermAtomic.guardDouble(Double.valueOf(e % d));
            case SpecialCompare.CATEGORY_LONG:
            case SpecialCompare.CATEGORY_BIG_DECIMAL:
                BigDecimal h = TermAtomic.widenBigDecimal(b);
                if (h.compareTo(BigDecimal.ZERO) == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                BigDecimal j = TermAtomic.widenBigDecimal(a);
                return TermAtomic.normBigDecimal(j.subtract(j.divide(
                        h, 0, BigDecimal.ROUND_DOWN).multiply(h)));
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Modulus of the two numbers.</p>
     * <p>The result is related to (div)/2 as follows:</p>
     * <pre>
     *      X mod Y = X - (X div Y) * Y.
     * </pre>
     *
     * @param a The first number.
     * @param b The second number.
     * @return The remainder of the first number by the second number.
     * @throws ArithmeticException       Shit happens.
     */
    private static Number mod(Number a, Number b) throws ArithmeticException {
        switch (Math.max(SpecialCompare.category(a), SpecialCompare.category(b))) {
            case SpecialCompare.CATEGORY_INTEGER:
                int u = b.intValue();
                if (u == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                int v = a.intValue();
                if ((v < 0) != (u < 0)) {
                    long res = (long) v % u;
                    if (res != 0) {
                        return TermAtomic.normBigInteger(res + u);
                    } else {
                        return Integer.valueOf(0);
                    }
                } else {
                    return TermAtomic.normBigInteger((long) v % u);
                }
            case SpecialCompare.CATEGORY_BIG_INTEGER:
                if (b instanceof Integer && b.intValue() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                BigInteger p = TermAtomic.widenBigInteger(b);
                BigInteger q = TermAtomic.widenBigInteger(a);
                if ((q.compareTo(BigInteger.ZERO) < 0) != (p.compareTo(BigInteger.ZERO) < 0)) {
                    BigInteger res = q.remainder(p);
                    if (!res.equals(BigInteger.ZERO)) {
                        return TermAtomic.normBigInteger(res.add(p));
                    } else {
                        return Integer.valueOf(0);
                    }
                } else {
                    return TermAtomic.normBigInteger(q.remainder(p));
                }
            case SpecialCompare.CATEGORY_FLOAT:
                float f = b.floatValue();
                if (f == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                float g = a.floatValue();
                if ((g < 0) != (f < 0)) {
                    float res = g % f;
                    if (res != 0) {
                        return TermAtomic.guardFloat(Float.valueOf(res + f));
                    } else {
                        return Float.valueOf(0);
                    }
                } else {
                    return TermAtomic.guardFloat(Float.valueOf(g % f));
                }
            case SpecialCompare.CATEGORY_DOUBLE:
                double d = b.doubleValue();
                if (d == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                double e = a.doubleValue();
                if ((e < 0) != (d < 0)) {
                    double res = e % d;
                    if (res != 0) {
                        return TermAtomic.guardDouble(Double.valueOf(res + d));
                    } else {
                        return Float.valueOf(0);
                    }
                } else {
                    return TermAtomic.guardDouble(Double.valueOf(e % d));
                }
            case SpecialCompare.CATEGORY_LONG:
            case SpecialCompare.CATEGORY_BIG_DECIMAL:
                BigDecimal h = TermAtomic.widenBigDecimal(b);
                if (h.compareTo(BigDecimal.ZERO) == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                BigDecimal j = TermAtomic.widenBigDecimal(a);
                return TermAtomic.normBigDecimal(j.subtract(j.divide(
                        h, 0, BigDecimal.ROUND_FLOOR).multiply(h)));
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

}
