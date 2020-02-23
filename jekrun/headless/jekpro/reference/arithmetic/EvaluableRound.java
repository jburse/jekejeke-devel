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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
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
     * <p>The continuation is passed via the contskel and contdisplay of the engine.</p>
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
                    Display d = en.display;
                    boolean multi = d.getAndReset();
                    Number alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = EvaluableRound.integer(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_TRUNCATE:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = EvaluableRound.truncate(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_FLOOR:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = EvaluableRound.floor(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_CEILING:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = EvaluableRound.ceiling(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_ROUND:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = round(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_SLASH_SLASH:
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
                    Number beta = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = slashSlash(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_REM:
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
                    en.skel = rem(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_DIV:
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
                    en.skel = EvaluableRound.div(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_MOD:
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
                    en.skel = EvaluableRound.mod(alfa, beta);
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
            return toInteger(m.floatValue());
        } else if (m instanceof Double) {
            return toInteger(m.doubleValue());
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
            if (Integer.MIN_VALUE <= f && f <= Integer.MAX_VALUE) {
                return TermAtomic.makeFloat((float) Math.floor(f));
            } else {
                return m;
            }
        } else if (m instanceof Double) {
            double d = m.doubleValue();
            if (Long.MIN_VALUE <= d && d <= Long.MAX_VALUE) {
                return TermAtomic.makeDouble(Math.floor(d));
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
            if (Integer.MIN_VALUE <= f && f <= Integer.MAX_VALUE) {
                return TermAtomic.makeFloat((float) Math.ceil(f));
            } else {
                return m;
            }
        } else if (m instanceof Double) {
            double d = m.doubleValue();
            if (Long.MIN_VALUE <= d && d <= Long.MAX_VALUE) {
                return TermAtomic.makeDouble(Math.ceil(d));
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
            if (Integer.MIN_VALUE <= f && f <= Integer.MAX_VALUE) {
                return TermAtomic.makeFloat(
                        (f < 0 ? -Math.round(-f) : Math.round(f)));
            } else {
                return f;
            }
        } else if (m instanceof Double) {
            double d = m.doubleValue();
            if (Long.MIN_VALUE <= d && d <= Long.MAX_VALUE) {
                return TermAtomic.makeDouble(
                        (d < 0 ? -Math.round(-d) : Math.round(d)));
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
                        BigDecimal.ROUND_HALF_UP));
            }
        }
    }

    /********************************************************************/
    /* Rounding Operations (Part I):                                    */
    /*      (//)/2: slashSlash()                                        */
    /*      (rem)/2: rem()                                              */
    /*      (div)/2: div()                                              */
    /*      (mod)/2: mod()                                              */
    /********************************************************************/

    /**
     * <p>Divide and truncate the two numbers.</p>
     * <p>The results corresponds to the truncation of the real division.</p>
     * <pre>
     *       X // Y = integer(X / Y).
     * </pre>
     *
     * @param a The first operand.
     * @param b The second operand.
     * @return The first operand divided by the second operand.
     * @throws ArithmeticException Shit happens.
     */
    private static Number slashSlash(Number a, Number b)
            throws ArithmeticException {
        switch (Math.max(SpecialCompare.numType(a), SpecialCompare.numType(b))) {
            case SpecialCompare.NUM_INTEGER:
                int u = b.intValue();
                if (u == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.normBigInteger((long) a.intValue() / u);
            case SpecialCompare.NUM_BIG_INTEGER:
                BigInteger p = TermAtomic.widenBigInteger(b);
                if (p.signum() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.normBigInteger(TermAtomic.widenBigInteger(a).divide(p));
            case SpecialCompare.NUM_FLOAT:
                float f = b.floatValue();
                if (f == 0.0f)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return toInteger(a.floatValue() / f);
            case SpecialCompare.NUM_DOUBLE:
                double d = b.doubleValue();
                if (d == 0.0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return toInteger(a.doubleValue() / d);
            case SpecialCompare.NUM_LONG:
            case SpecialCompare.NUM_BIG_DECIMAL:
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
     * <p>Remainder of the two numbers.</p>
     * <p>The result is related to (//)/2 as follows:</p>
     * <pre>
     *      X rem Y = X - (X // Y) * Y.
     * </pre>
     *
     * @param a The first operand.
     * @param b The second operand.
     * @return The remainder of the first operand by the second operand.
     * @throws ArithmeticException Shit happens.
     */
    private static Number rem(Number a, Number b) throws ArithmeticException {
        switch (Math.max(SpecialCompare.numType(a), SpecialCompare.numType(b))) {
            case SpecialCompare.NUM_INTEGER:
                int u = b.intValue();
                if (u == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return Integer.valueOf(a.intValue() % u);
            case SpecialCompare.NUM_BIG_INTEGER:
                BigInteger p = TermAtomic.widenBigInteger(b);
                if (p.signum() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.normBigInteger(TermAtomic.widenBigInteger(a).remainder(p));
            case SpecialCompare.NUM_FLOAT:
                float f = b.floatValue();
                if (f == 0.0f)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.makeFloat(a.floatValue() % f);
            case SpecialCompare.NUM_DOUBLE:
                double d = b.doubleValue();
                if (d == 0.0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return TermAtomic.makeDouble(a.doubleValue() % d);
            case SpecialCompare.NUM_LONG:
            case SpecialCompare.NUM_BIG_DECIMAL:
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
     * <p>Divide and floor the two numbers.</p>
     * <p>The results corresponds to the floor of the real division.</p>
     * <pre>
     *       X div Y = integer(floor(X / Y)).
     * </pre>
     *
     * @param a The first operand.
     * @param b The second operand.
     * @return The first operand divided by the second operand.
     * @throws ArithmeticException Shit happens.
     */
    private static Number div(Number a, Number b) {
        switch (Math.max(SpecialCompare.numType(a), SpecialCompare.numType(b))) {
            case SpecialCompare.NUM_INTEGER:
                int u = b.intValue();
                if (u == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                int v = a.intValue();
                return TermAtomic.normBigInteger(SpecialCompare.floorDiv(v, u));
            case SpecialCompare.NUM_BIG_INTEGER:
                BigInteger p = TermAtomic.widenBigInteger(b);
                if (p.signum() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                BigInteger q = TermAtomic.widenBigInteger(a);
                return TermAtomic.normBigInteger(SpecialCompare.floorDiv(q, p));
            case SpecialCompare.NUM_FLOAT:
                float f = b.floatValue();
                if (f == 0.0f)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return toInteger((float)Math.floor(a.floatValue() / f));
            case SpecialCompare.NUM_DOUBLE:
                double d = b.doubleValue();
                if (d == 0.0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                return toInteger(Math.floor(a.doubleValue() / d));
            case SpecialCompare.NUM_LONG:
            case SpecialCompare.NUM_BIG_DECIMAL:
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
        switch (Math.max(SpecialCompare.numType(a), SpecialCompare.numType(b))) {
            case SpecialCompare.NUM_INTEGER:
                int u = b.intValue();
                if (u == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                int v = a.intValue();
                return Integer.valueOf(SpecialCompare.floorMod(v, u));
            case SpecialCompare.NUM_BIG_INTEGER:
                BigInteger p = TermAtomic.widenBigInteger(b);
                if (p.signum() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                BigInteger q = TermAtomic.widenBigInteger(a);
                return TermAtomic.normBigInteger(SpecialCompare.floorMod(q, p));
            case SpecialCompare.NUM_FLOAT:
                float f = b.floatValue();
                if (f == 0.0f)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                float g = a.floatValue();
                return TermAtomic.makeFloat(SpecialCompare.floorMod(g, f));
            case SpecialCompare.NUM_DOUBLE:
                double d = b.doubleValue();
                if (d == 0.0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                double e = a.doubleValue();
                return TermAtomic.makeDouble(SpecialCompare.floorMod(e, d));
            case SpecialCompare.NUM_LONG:
            case SpecialCompare.NUM_BIG_DECIMAL:
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
     * <p>Convert an float to a Prolog integer.</p>
     *
     * @param f The float.
     * @return The integer.
     */
    public static Number toInteger(float f) {
        if (Integer.MIN_VALUE <= f && f <= Integer.MAX_VALUE) {
            return Integer.valueOf((int) f);
        } else {
            return BigInteger.valueOf(EvaluableCompare.getMantissa(f)).shiftLeft(
                    Math.getExponent(f) - EvaluableCompare.FLOAT_SNIF_WIDTH);
        }
    }

    /**
     * <p>Concert a double to an integer.</p>
     *
     * @param d The double.
     * @return The integer to a Prolog integer.
     */
    public static Number toInteger(double d) {
        if (Long.MIN_VALUE <= d && d <= Long.MAX_VALUE) {
            return TermAtomic.normBigInteger((long) d);
        } else {
            return BigInteger.valueOf(EvaluableCompare.getMantissa(d)).shiftLeft(
                    Math.getExponent(d) - EvaluableCompare.DOUBLE_SNIF_WIDTH);
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

        int x=Integer.MIN_VALUE;
        int y=-1;
        System.out.println("x="+x+", y="+y+", x%y="+x%y);
        System.out.println("x="+x+", y="+y+", x%y="+((long)x)%y);
        System.out.println("x="+x+", y="+y+", x/y="+x/y);
        System.out.println("x="+x+", y="+y+", x/y="+((long)x)/y);
    }
    */

}
