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
public final class EvaluableRound extends AbstractSpecial {
    private final static int EVALUABLE_INTEGER = 0;
    private final static int EVALUABLE_TRUNCATE = 1;
    private final static int EVALUABLE_FLOOR = 2;
    private final static int EVALUABLE_CEILING = 3;
    private final static int EVALUABLE_ROUND = 4;
    private final static int EVALUABLE_SLASH_SLASH = 5;
    private final static int EVALUABLE_REM = 6;
    private final static int EVALUABLE_DIV = 7;
    private final static int EVALUABLE_MOD = 8;

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
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public final void moniEvaluate(Engine en)
            throws EngineMessage, EngineException {
        try {
            switch (id) {
                case EVALUABLE_INTEGER:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    en.computeExpr(temp[0], ref);
                    Number alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = integer(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_TRUNCATE:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = truncate(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_FLOOR:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = floor(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_CEILING:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = ceiling(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_ROUND:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = round(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_SLASH_SLASH:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.computeExpr(temp[1], ref);
                    Number beta = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = slashSlash(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_REM:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.computeExpr(temp[1], ref);
                    beta = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = rem(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_DIV:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.computeExpr(temp[1], ref);
                    beta = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = div(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_MOD:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.computeExpr(temp[1], ref);
                    beta = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = mod(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                default:
                    throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
            }
        } catch (ArithmeticException x) {
            throw new EngineMessage(EngineMessage.evaluationError(x.getMessage()));
        }
    }

    /********************************************************************/
    /* Rounding Operations (Part II):                                   */
    /*      (integer)/1: integer()                                      */
    /*      (truncate)/1: truncate()                                    */
    /*      (floor)/1: floor()                                          */
    /*      (ceiling)/1: ceiling()                                      */
    /*      (round)/1: round()                                          */
    /********************************************************************/

    /**
     * <p>Integeer the number.</p>
     *
     * @param m The number.
     * @return The integer number.
     */
    private static Number integer(Number m) {
        if (m instanceof Integer || m instanceof BigInteger) {
            return m;
        } else if (m instanceof Float) {
            float f = m.floatValue();
            if (Integer.MIN_VALUE <= f && f <= Integer.MAX_VALUE) {
                return Integer.valueOf((int) f);
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
     * <p>Truncate the number.</p>
     *
     * @param m The number.
     * @return The truncated number.
     */
    private static Number truncate(Number m) {
        if (m instanceof Integer || m instanceof BigInteger) {
            return m;
        } else if (m instanceof Float) {
            float f = m.floatValue();
            if (Integer.MIN_VALUE <= f && f <= Integer.MAX_VALUE) {
                return TermAtomic.makeFloat((int) f);
            } else {
                return m;
            }
        } else if (m instanceof Double) {
            double d = m.doubleValue();
            if (Long.MIN_VALUE <= d && d <= Long.MAX_VALUE) {
                return TermAtomic.makeDouble((long) d);
            } else {
                return m;
            }
        } else if (m instanceof Long) {
            return m;
        } else {
            BigDecimal b = (BigDecimal) m;
            if (b.scale() <= 0) {
                return b;
            } else {
                return TermAtomic.normBigDecimal(b.setScale(0,
                        BigDecimal.ROUND_DOWN));
            }
        }
    }

    /**
     * <p>Floor the number.</p>
     *
     * @param m The number.
     * @return The floored number.
     */
    private static Number floor(Number m) {
        if (m instanceof Integer || m instanceof BigInteger) {
            return m;
        } else if (m instanceof Float) {
            float f = m.floatValue();
            return TermAtomic.makeFloat((float) Math.floor(f));
        } else if (m instanceof Double) {
            double d = m.doubleValue();
            return TermAtomic.makeDouble(Math.floor(d));
        } else if (m instanceof Long) {
            return m;
        } else {
            BigDecimal b = (BigDecimal) m;
            if (b.scale() <= 0) {
                return b;
            } else {
                return TermAtomic.normBigDecimal(b.setScale(0,
                        BigDecimal.ROUND_FLOOR));
            }
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
            return TermAtomic.makeFloat((float) Math.ceil(f));
        } else if (m instanceof Double) {
            double d = m.doubleValue();
            return TermAtomic.makeDouble(Math.ceil(d));
        } else if (m instanceof Long) {
            return m;
        } else {
            BigDecimal b = (BigDecimal) m;
            if (b.scale() <= 0) {
                return b;
            } else {
                return TermAtomic.normBigDecimal(b.setScale(0,
                        BigDecimal.ROUND_CEILING));
            }
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
            return TermAtomic.makeFloat(
                    (f < 0 ? -Math.round(-f) : Math.round(f)));
        } else if (m instanceof Double) {
            double d = m.doubleValue();
            return TermAtomic.makeDouble(
                    (d < 0 ? -Math.round(-d) : Math.round(d)));
        } else if (m instanceof Long) {
            return m;
        } else {
            BigDecimal b = (BigDecimal) m;
            if (b.scale() <= 0) {
                return b;
            } else {
                return TermAtomic.normBigDecimal(((BigDecimal) m).setScale(0,
                        BigDecimal.ROUND_HALF_UP));
            }
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
     *       X // Y = integer(X / Y).
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
                int u = b.intValue();
                if (u == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.normBigInteger((long) a.intValue() / u);
            case SpecialCompare.CATEGORY_BIG_INTEGER:
                BigInteger p = TermAtomic.widenBigInteger(b);
                if (p.signum() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.normBigInteger(TermAtomic.widenBigInteger(a).divide(p));
            case SpecialCompare.CATEGORY_FLOAT:
                float f = b.floatValue();
                if (f == 0.0f)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                float g = a.floatValue() / f;
                if (Integer.MIN_VALUE <= g && g <= Integer.MAX_VALUE) {
                    return Integer.valueOf((int) g);
                } else {
                    return TermAtomic.normBigInteger(new BigDecimal(g).toBigInteger());
                }
            case SpecialCompare.CATEGORY_DOUBLE:
                double d = b.doubleValue();
                if (d == 0.0)
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
                if (h.signum() == 0)
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
     *       X div Y = integer(floor(X / Y)).
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
                int u = b.intValue();
                if (u == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                int v = a.intValue();
                if ((v < 0) && (u >= 0)) {
                    return TermAtomic.normBigInteger(((long) v - u + 1) / u);
                } else if ((v >= 0) && (u < 0)) {
                    return TermAtomic.normBigInteger(((long) v - u - 1) / u);
                } else {
                    return TermAtomic.normBigInteger((long) v / u);
                }
            case SpecialCompare.CATEGORY_BIG_INTEGER:
                BigInteger p = TermAtomic.widenBigInteger(b);
                if (p.signum() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                BigInteger q = TermAtomic.widenBigInteger(a);
                if ((q.signum() < 0) && (p.signum() >= 0)) {
                    return TermAtomic.normBigInteger(q.subtract(p).add(
                            BigInteger.ONE).divide(p));
                } else if ((q.signum() >= 0) && (p.signum() < 0)) {
                    return TermAtomic.normBigInteger(q.subtract(p).subtract(
                            BigInteger.ONE).divide(p));
                } else {
                    return TermAtomic.normBigInteger(q.divide(p));
                }
            case SpecialCompare.CATEGORY_FLOAT:
                float f = b.floatValue();
                if (f == 0.0f)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                float g = a.floatValue() / f;
                if (Integer.MIN_VALUE <= g && g <= Integer.MAX_VALUE) {
                    return Integer.valueOf((int) Math.floor(g));
                } else {
                    return TermAtomic.normBigInteger(new BigDecimal(g).setScale(0,
                            BigDecimal.ROUND_FLOOR).unscaledValue());
                }
            case SpecialCompare.CATEGORY_DOUBLE:
                double d = b.doubleValue();
                if (d == 0.0)
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
                if (h.signum() == 0)
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
                int u = b.intValue();
                if (u == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.normBigInteger((long) a.intValue() % u);
            case SpecialCompare.CATEGORY_BIG_INTEGER:
                BigInteger p = TermAtomic.widenBigInteger(b);
                if (p.signum() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.normBigInteger(TermAtomic.widenBigInteger(a).remainder(p));
            case SpecialCompare.CATEGORY_FLOAT:
                float f = b.floatValue();
                if (f == 0.0f)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                float g = a.floatValue();
                return TermAtomic.makeFloat(g % f);
            case SpecialCompare.CATEGORY_DOUBLE:
                double d = b.doubleValue();
                if (d == 0.0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                double e = a.doubleValue();
                return TermAtomic.makeDouble(e % d);
            case SpecialCompare.CATEGORY_LONG:
            case SpecialCompare.CATEGORY_BIG_DECIMAL:
                BigDecimal h = TermAtomic.widenBigDecimal(b);
                if (h.signum() == 0)
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
     * @throws ArithmeticException Shit happens.
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
                BigInteger p = TermAtomic.widenBigInteger(b);
                if (p.signum() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                BigInteger q = TermAtomic.widenBigInteger(a);
                if ((q.signum() < 0) != (p.signum() < 0)) {
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
                if (f == 0.0f)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                float g = a.floatValue();
                if ((g < 0) != (f < 0)) {
                    float res = g % f;
                    if (res != 0) {
                        return TermAtomic.makeFloat(res + f);
                    } else {
                        return TermAtomic.ZERO_FLOAT;
                    }
                } else {
                    return TermAtomic.makeFloat(g % f);
                }
            case SpecialCompare.CATEGORY_DOUBLE:
                double d = b.doubleValue();
                if (d == 0.0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                double e = a.doubleValue();
                if ((e < 0) != (d < 0)) {
                    double res = e % d;
                    if (res != 0) {
                        return TermAtomic.makeDouble(res + d);
                    } else {
                        return TermAtomic.ZERO_DOUBLE;
                    }
                } else {
                    return TermAtomic.makeDouble(e % d);
                }
            case SpecialCompare.CATEGORY_LONG:
            case SpecialCompare.CATEGORY_BIG_DECIMAL:
                BigDecimal h = TermAtomic.widenBigDecimal(b);
                if (h.signum() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                BigDecimal j = TermAtomic.widenBigDecimal(a);
                return TermAtomic.normBigDecimal(j.subtract(j.divide(
                        h, 0, BigDecimal.ROUND_FLOOR).multiply(h)));
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Some testing.</p>
     *
     * @param args Not used.
     */
    /*
    public static void main(String[] args) {
        double x=19.0; double y=12.0;
        System.out.println("("+x+"%"+y+")="+(x%y));
        x=19.0; y=-12.0;
        System.out.println("("+x+"%"+y+")="+(x%y));
        x=-19.0; y=12.0;
        System.out.println("("+x+"%"+y+")="+(x%y));
        x=-19.0; y=-12.0;
        System.out.println("("+x+"%"+y+")="+(x%y));

        System.out.println();

        x=19.0; y=12.0;
        System.out.println("IEEEremainder("+x+","+y+")="+Math.IEEEremainder(x,y));
        x=19.0; y=-12.0;
        System.out.println("IEEEremainder("+x+","+y+")="+Math.IEEEremainder(x,y));
        x=-19.0; y=12.0;
        System.out.println("IEEEremainder("+x+","+y+")="+Math.IEEEremainder(x,y));
        x=-19.0; y=-12.0;
        System.out.println("IEEEremainder("+x+","+y+")="+Math.IEEEremainder(x,y));
    }
    */

}
