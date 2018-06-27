package jekmin.frequent.decimal;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
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

/**
 * <p>This module provides built-ins for arithmetic of decimals.</p>
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
public class SpecialArith extends AbstractSpecial {
    private final static int SPECIAL_MP_DECIMAL = 0;
    private final static int SPECIAL_MP_ADD = 1;
    private final static int SPECIAL_MP_SUB = 2;
    private final static int SPECIAL_MP_MUL = 3;
    private final static int SPECIAL_MP_SLASH = 4;
    private final static int SPECIAL_MP_INT_POW = 5;

    /**
     * <p>Create a decimal arithmetic special.</p>
     *
     * @param i The id.
     */
    public SpecialArith(int i) {
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
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        try {
            switch (id) {
                case SPECIAL_MP_DECIMAL:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    en.skel = temp[0];
                    en.display = ref;
                    en.deref();
                    Number alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    MathContext mc = castContext(en.skel, en.display);
                    if (!en.unifyTerm(temp[2], ref, mpDecimal(alfa, mc),
                            Display.DISPLAY_CONST))
                        return false;
                    return en.getNext();
                case SPECIAL_MP_ADD:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.skel = temp[0];
                    en.display = ref;
                    en.deref();
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    Number beta = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = temp[2];
                    en.display = ref;
                    en.deref();
                    mc = castContext(en.skel, en.display);
                    if (!en.unifyTerm(temp[3], ref, mpAdd(alfa, beta, mc),
                            Display.DISPLAY_CONST))
                        return false;
                    return en.getNext();
                case SPECIAL_MP_SUB:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.skel = temp[0];
                    en.display = ref;
                    en.deref();
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    beta = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = temp[2];
                    en.display = ref;
                    en.deref();
                    mc = castContext(en.skel, en.display);
                    if (!en.unifyTerm(temp[3], ref, mpSub(alfa, beta, mc),
                            Display.DISPLAY_CONST))
                        return false;
                    return en.getNext();
                case SPECIAL_MP_MUL:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.skel = temp[0];
                    en.display = ref;
                    en.deref();
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    beta = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = temp[2];
                    en.display = ref;
                    en.deref();
                    mc = castContext(en.skel, en.display);
                    if (!en.unifyTerm(temp[3], ref, mpMul(alfa, beta, mc),
                            Display.DISPLAY_CONST))
                        return false;
                    return en.getNext();
                case SPECIAL_MP_SLASH:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.skel = temp[0];
                    en.display = ref;
                    en.deref();
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    beta = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = temp[2];
                    en.display = ref;
                    en.deref();
                    mc = castContext(en.skel, en.display);
                    if (!en.unifyTerm(temp[3], ref, mpSlash(alfa, beta, mc),
                            Display.DISPLAY_CONST))
                        return false;
                    return en.getNext();
                case SPECIAL_MP_INT_POW:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.skel = temp[0];
                    en.display = ref;
                    en.deref();
                    alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    beta = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = temp[2];
                    en.display = ref;
                    en.deref();
                    mc = castContext(en.skel, en.display);
                    int x = EngineMessage.castIntValue(beta);
                    if (!en.unifyTerm(temp[3], ref, mpIntPow(alfa, x, mc),
                            Display.DISPLAY_CONST))
                        return false;
                    return en.getNext();
                default:
                    throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
            }
        } catch (ArithmeticException x) {
            throw new EngineMessage(EngineMessage.evaluationError(x.getMessage()));

        }
    }

    /**
     * <p>Cast a clause.</p>
     *
     * @param m The skel.
     * @return The clause.
     * @throws EngineMessage Shit happens.
     */
    static MathContext castContext(Object m, Display d)
            throws EngineMessage {
        if (m instanceof MathContext) {
            return (MathContext) m;
        } else {
            EngineMessage.checkRef(m, d);
            throw new EngineMessage(EngineMessage.domainError(
                    "math_context", m));
        }
    }

    /********************************************************************/
    /* Additional Binary Decimal Built-in:                              */
    /*      mp_decimal/3: mpDecimal()                                 */
    /********************************************************************/

    /**
     * <p>Return the round.</p>
     *
     * @param m  The decimal number.
     * @param mc The math context.
     * @return The rounded decimal number.
     */
    private static Number mpDecimal(Number m, MathContext mc) {
        if (m instanceof Integer) {
            if (mc.getPrecision() != 0 &&
                    (SupplementScale.log10(m.intValue()) > mc.getPrecision())) {
                return TermAtomic.normBigDecimal(
                        new BigDecimal(m.intValue(), mc));
            } else {
                return Long.valueOf(m.intValue());
            }
        } else if (m instanceof BigInteger) {
            return TermAtomic.normBigDecimal(
                    new BigDecimal((BigInteger) m, mc));
        } else if (m instanceof Float || m instanceof Double) {
            return TermAtomic.normBigDecimal(
                    new BigDecimal(m.doubleValue(), mc));
        } else if (m instanceof Long) {
            if (mc.getPrecision() != 0 &&
                    (SupplementScale.log10(m.longValue()) > mc.getPrecision())) {
                return TermAtomic.normBigDecimal(
                        new BigDecimal(m.longValue(), mc));
            } else {
                return m;
            }
        } else if (m instanceof BigDecimal) {
            BigDecimal d = (BigDecimal) m;
            if (mc.getPrecision() != 0 &&
                    (d.precision() > mc.getPrecision())) {
                return TermAtomic.normBigDecimal(
                        new BigDecimal(d.unscaledValue(), d.scale(), mc));
            } else {
                return d;
            }
        } else {
            throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /********************************************************************/
    /* Additional Ternary Decimal Operations:                           */
    /*      mp_add/4: mpAdd()                                         */
    /*      mp_sub/4: mpSub()                                         */
    /*      mp_mul/4: mpMul()                                         */
    /*      mp_slash/4: mpSlash()                                     */
    /*      mp_int_pow/4: mpIntPow()                                  */
    /********************************************************************/

    /**
     * <p>Add two decimal with some precision.</p>
     *
     * @param m  The first Prolog decimal.
     * @param n  The second Prolog decimal.
     * @param mc The math context.
     * @return The result.
     */
    private static Number mpAdd(Number m, Number n,
                                MathContext mc) {
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
                        SupplementScale.widenBigDecimal(m, mc).add(
                                SupplementScale.widenBigDecimal(n, mc), mc));
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Subtract a decimal by another decimal with some precision.</p>
     *
     * @param m  The first Prolog decimal.
     * @param n  The second Prolog decimal.
     * @param mc The math context.
     * @return The result.
     */
    private static Number mpSub(Number m, Number n,
                                MathContext mc) {
        switch (Math.max(SpecialCompare.category(m), SpecialCompare.category(n))) {
            case SpecialCompare.CATEGORY_INTEGER:
                return TermAtomic.normBigInteger((long) m.intValue() - n.intValue());
            case SpecialCompare.CATEGORY_BIG_INTEGER:
                return TermAtomic.normBigInteger(
                        TermAtomic.widenBigInteger(m).subtract(
                                TermAtomic.widenBigInteger(n)));
            case SpecialCompare.CATEGORY_FLOAT:
                return TermAtomic.makeFloat(m.floatValue() - n.floatValue());
            case SpecialCompare.CATEGORY_DOUBLE:
                return TermAtomic.makeDouble(m.doubleValue() - n.doubleValue());
            case SpecialCompare.CATEGORY_LONG:
            case SpecialCompare.CATEGORY_BIG_DECIMAL:
                return TermAtomic.normBigDecimal(
                        SupplementScale.widenBigDecimal(m, mc).subtract(
                                SupplementScale.widenBigDecimal(n, mc), mc));
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Multiply two decimal with some precision.</p>
     *
     * @param m  The first Prolog decimal.
     * @param n  The second Prolog decimal.
     * @param mc The math context.
     * @return The result.
     */
    private static Number mpMul(Number m, Number n,
                                MathContext mc) {
        switch (Math.max(SpecialCompare.category(m), SpecialCompare.category(n))) {
            case SpecialCompare.CATEGORY_INTEGER:
                return TermAtomic.normBigInteger((long) m.intValue() * n.intValue());
            case SpecialCompare.CATEGORY_BIG_INTEGER:
                return TermAtomic.normBigInteger(
                        TermAtomic.widenBigInteger(m).multiply(
                                TermAtomic.widenBigInteger(n)));
            case SpecialCompare.CATEGORY_FLOAT:
                return TermAtomic.makeFloat(m.floatValue() * n.floatValue());
            case SpecialCompare.CATEGORY_DOUBLE:
                return TermAtomic.makeDouble(m.doubleValue() * n.doubleValue());
            case SpecialCompare.CATEGORY_LONG:
            case SpecialCompare.CATEGORY_BIG_DECIMAL:
                return TermAtomic.normBigDecimal(
                        SupplementScale.widenBigDecimal(m, mc).multiply(
                                SupplementScale.widenBigDecimal(n, mc), mc));
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Divide a decimal by another decimal with some precision.</p>
     *
     * @param m  The first Prolog decimal.
     * @param n  The second Prolog decimal.
     * @param mc The math context.
     * @return The result.
     */
    private static Number mpSlash(Number m, Number n,
                                  MathContext mc) {
        BigDecimal b = SupplementScale.widenBigDecimal(n, mc);
        if (b.signum() == 0)
            throw new ArithmeticException(
                    EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
        return TermAtomic.normBigDecimal(
                SupplementScale.widenBigDecimal(m, mc).divide(b, mc));
    }

    /**
     * <p>Raise a decimal to an integer power with some precision.</p>
     *
     * @param m  The first operand.
     * @param x  The second operand.
     * @param mc The math context.
     * @return The result.
     */
    private static Number mpIntPow(Number m, int x,  MathContext mc) {
        if (m instanceof Integer || m instanceof BigInteger) {
            return TermAtomic.normBigInteger(
                    TermAtomic.widenBigInteger(m).pow(x));
        } else if (m instanceof Float) {
            return TermAtomic.makeFloat( (float) Math.pow(m.floatValue(), x));
        } else if (m instanceof Double) {
            return TermAtomic.makeDouble( Math.pow(m.doubleValue(), x));
        } else if (m instanceof Long || m instanceof BigDecimal) {
            BigDecimal b = SupplementScale.widenBigDecimal(m, mc);
            if (x < 0 && b.signum() == 0)
                throw new ArithmeticException(
                        EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
            return TermAtomic.normBigDecimal(b.pow(x, mc));
        } else {
            throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

}