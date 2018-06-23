package jekmin.reference.misc;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.arithmetic.EvaluableBits;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;

import java.math.BigInteger;

/**
 * <p>Provides additional bitwise evaluables.</p>
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
public final class SupplementBits extends AbstractSpecial {
    private final static int EVALUABLE_BITCOUNT = 0;
    private final static int EVALUABLE_BITLENGTH = 1;
    private final static int EVALUABLE_LOWESTSETBIT = 2;
    private final static int EVALUABLE_SETBIT = 3;
    private final static int EVALUABLE_CLEARBIT = 4;
    private final static int EVALUABLE_GET_EXPONENT = 5;
    private final static int EVALUABLE_GET_MANTISSA = 6;
    private final static int EVALUABLE_MAKE_FLOAT = 7;
    private final static int EVALUABLE_SLASH = 8;

    private static final int SIGNIFICAND_WIDTH = 53;
    private static final int EXP_BIAS = 1023;

    /**
     * <p>Create a special arithmetic.</p>
     *
     * @param i The built-in ID.
     */
    public SupplementBits(int i) {
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
        switch (id) {
            case EVALUABLE_BITCOUNT:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.computeExpr(temp[0], ref);
                Number alfa = EngineMessage.castInteger(en.skel, en.display);
                en.skel = Integer.valueOf(bitCount(alfa));
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_BITLENGTH:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref);
                alfa = EngineMessage.castInteger(en.skel, en.display);
                en.skel = Integer.valueOf(bitLength(alfa));
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_LOWESTSETBIT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref);
                alfa = EngineMessage.castInteger(en.skel, en.display);
                en.skel = Integer.valueOf(lowestSetBit(alfa));
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_SETBIT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref);
                alfa = EngineMessage.castInteger(en.skel, en.display);
                en.computeExpr(temp[1], ref);
                Number beta = EngineMessage.castInteger(en.skel, en.display);
                EngineMessage.checkNotLessThanZero(alfa);
                int x = EngineMessage.castIntValue(alfa);
                en.skel = setBit(x, beta);
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_CLEARBIT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref);
                alfa = EngineMessage.castInteger(en.skel, en.display);
                en.computeExpr(temp[1], ref);
                beta = EngineMessage.castInteger(en.skel, en.display);
                EngineMessage.checkNotLessThanZero(alfa);
                x = EngineMessage.castIntValue(alfa);
                en.skel = clearBit(x, beta);
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_GET_EXPONENT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref);
                alfa = EngineMessage.castInteger(en.skel, en.display);
                en.skel = Integer.valueOf(getExponent(alfa));
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_GET_MANTISSA:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref);
                alfa = EngineMessage.castInteger(en.skel, en.display);
                en.skel = getMantissa(alfa);
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_MAKE_FLOAT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref);
                alfa = EngineMessage.castInteger(en.skel, en.display);
                en.computeExpr(temp[1], ref);
                beta = EngineMessage.castInteger(en.skel, en.display);
                x = EngineMessage.castIntValue(alfa);
                en.skel = TermAtomic.makeDouble(makeFloat(x, beta));
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_SLASH:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref);
                alfa = EngineMessage.castInteger(en.skel, en.display);
                en.computeExpr(temp[1], ref);
                beta = EngineMessage.castInteger(en.skel, en.display);
                en.skel = TermAtomic.makeDouble(slash2(alfa, beta));
                en.display = Display.DISPLAY_CONST;
                return;
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /********************************************************************/
    /* Additional Unary Bitwise Operations:                             */
    /*      bitcount/2: bitCount()                                      */
    /*      bitlength/2: bitLength()                                    */
    /*      lowestsetbit/2: lowestSetBit()                              */
    /********************************************************************/

    /**
     * <p>Count the bits.</p>
     *
     * @param m The operand.
     * @return The result.
     */
    private static int bitCount(Number m) {
        if (m instanceof Integer) {
            int x = m.intValue();
            if (x != Integer.MIN_VALUE) {
                int k = Integer.bitCount(Math.abs(x));
                if (x < 0) {
                    return k + Integer.numberOfTrailingZeros(Math.abs(x)) - 1;
                } else {
                    return k;
                }
            } else {
                return 31;
            }
        } else {
            return ((BigInteger) m).bitCount();
        }
    }

    /**
     * <p>The number of bits to represent this number.</p>
     *
     * @param m The operand.
     * @return The result.
     */
    private static int bitLength(Number m) {
        if (m instanceof Integer) {
            int x = m.intValue();
            if (x != Integer.MIN_VALUE) {
                int k = 32 - Integer.numberOfLeadingZeros(Math.abs(x));
                if (x < 0 && Integer.bitCount(Math.abs(x)) == 1) {
                    return k - 1;
                } else {
                    return k;
                }
            } else {
                return 31;
            }
        } else {
            return ((BigInteger) m).bitLength();
        }
    }

    /**
     * <p>The lowest set bit of this number.</p>
     *
     * @param m The operand.
     * @return The result.
     */
    private static int lowestSetBit(Number m) {
        if (m instanceof Integer) {
            int x = m.intValue();
            if (x != Integer.MIN_VALUE) {
                if (x == 0) {
                    return -1;
                } else {
                    return Integer.numberOfTrailingZeros(x);
                }
            } else {
                return 31;
            }
        } else {
            return ((BigInteger) m).getLowestSetBit();
        }
    }

    /********************************************************************/
    /* Additional Binary Bitwise Built-in:                              */
    /*      setbit/3: setBit()                                          */
    /*      clearbit/3: clearBit()                                      */
    /********************************************************************/

    /**
     * <p>Set a bit.</p>
     *
     * @param x The first operand.
     * @param n The second operand.
     * @return The result.
     */
    private static Number setBit(int x, Number n) {
        if (n instanceof Integer) {
            if (x <= 30) {
                return Integer.valueOf(n.intValue() | (1 << x));
            } else {
                return TermAtomic.normBigInteger(
                        BigInteger.valueOf(n.intValue()).setBit(x));
            }
        } else {
            return TermAtomic.normBigInteger(
                    ((BigInteger) n).setBit(x));
        }
    }

    /**
     * <p>Clear a bit.</p>
     *
     * @param x The first operand.
     * @param n The second operand.
     * @return The result.
     */
    private static Number clearBit(int x, Number n) {
        if (n instanceof Integer) {
            if (x <= 30) {
                return Integer.valueOf(n.intValue() & ~(1 << x));
            } else {
                return TermAtomic.normBigInteger(
                        BigInteger.valueOf(n.intValue()).clearBit(x));
            }
        } else {
            return TermAtomic.normBigInteger(
                    ((BigInteger) n).clearBit(x));
        }
    }

    /********************************************************************/
    /* Experimentation for new div operator:                            */
    /*      get_exponent/1: getExponent()                               */
    /*      get_mantissa/1: getMantissa()                               */
    /*      make_float/2: makeFloat()                                   */
    /*      slash2/2: slash2()                                          */
    /********************************************************************/

    /**
     * <p>Retrieve the exponent.</p>
     *
     * @param m The first operand.
     * @return The result.
     */
    private static int getExponent(Number m) {
        return SupplementBits.bitLength(m) - 2;
    }

    /**
     * <p>Retrieve the mantissa.</p>
     *
     * @param m The first operand.
     * @return The result.
     */
    private static Number getMantissa(Number m) {
        int shift = SupplementBits.bitLength(m) - SIGNIFICAND_WIDTH;
        return EvaluableBits.shiftRight(m, shift);
    }

    /**
     * <p>Make a float.</p>
     *
     * @param x The first operand.
     * @param n The second operand.
     * @return The result.
     */
    private static double makeFloat(int x, Number n) {
        long bits = ((long)x + EXP_BIAS) << (SIGNIFICAND_WIDTH - 1);
        bits += n.longValue();
        return Double.longBitsToDouble(bits);
    }

    /**
     * <p>Slash the two Prolog numbers.</p>
     *
     * @param m The first Prolog number.
     * @param n The second Prolog number.
     * @return The first number slashed by the second number.
     * @throws ArithmeticException Not a Prolog number.
     */
    private static double slash2(Number m, Number n) throws ArithmeticException {
        int e1 = getExponent(n);
        int e2 = getExponent(m);
        int e = Math.min(e1, e2);
        double b = makeFloat(e1 - e, getMantissa(n));
        if (!TermAtomic.guardDouble(b))
            throw new ArithmeticException(
                    EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
        return makeFloat(e2 - e, getMantissa(m)) / b;
    }

}
