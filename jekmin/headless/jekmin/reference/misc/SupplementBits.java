package jekmin.reference.misc;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Special;
import jekpro.model.molec.Display;
import jekpro.model.molec.DisplayClause;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Goal;
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
public final class SupplementBits extends Special {
    private final static int EVALUABLE_BITCOUNT = 0;
    private final static int EVALUABLE_BITLENGTH = 1;
    private final static int EVALUABLE_LOWESTSETBIT = 2;
    private final static int EVALUABLE_SETBIT = 3;
    private final static int EVALUABLE_CLEARBIT = 4;

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
     * @param r  The continuation skel.
     * @param u  The continuation display.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public final void evalEvaluable(Goal r, DisplayClause u, Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case EVALUABLE_BITCOUNT:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.computeExpr(temp[0], ref, r, u);
                Number alfa = EngineMessage.castInteger(en.skel, en.display);
                en.skel = bitCount(alfa);
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_BITLENGTH:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref, r, u);
                alfa = EngineMessage.castInteger(en.skel, en.display);
                en.skel = bitLength(alfa);
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_LOWESTSETBIT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref, r, u);
                alfa = EngineMessage.castInteger(en.skel, en.display);
                en.skel = lowestSetBit(alfa);
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_SETBIT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref, r, u);
                alfa = EngineMessage.castInteger(en.skel, en.display);
                en.computeExpr(temp[1], ref, r, u);
                Number beta = EngineMessage.castInteger(en.skel, en.display);
                en.skel = setBit(alfa, beta);
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_CLEARBIT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref, r, u);
                alfa = EngineMessage.castInteger(en.skel, en.display);
                en.computeExpr(temp[1], ref, r, u);
                beta = EngineMessage.castInteger(en.skel, en.display);
                en.skel = clearBit(alfa, beta);
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
    private static Number bitCount(Number m) {
        if (m instanceof Integer) {
            int x = m.intValue();
            if (x != Integer.MIN_VALUE) {
                int k = Integer.valueOf(Integer.bitCount(Math.abs(x)));
                if (x < 0) {
                    return k + Integer.numberOfTrailingZeros(Math.abs(x)) - 1;
                } else {
                    return k;
                }
            } else {
                return 31;
            }
        } else {
            return Integer.valueOf(((BigInteger) m).bitCount());
        }
    }

    /**
     * <p>The number of bits to represent this number.</p>
     *
     * @param m The operand.
     * @return The result.
     */
    private static Number bitLength(Number m) {
        if (m instanceof Integer) {
            int x = m.intValue();
            if (x != Integer.MIN_VALUE) {
                int k = Integer.valueOf(32 - Integer.numberOfLeadingZeros(Math.abs(x)));
                if (x < 0 && Integer.bitCount(Math.abs(x)) == 1) {
                    return k - 1;
                } else {
                    return k;
                }
            } else {
                return 31;
            }
        } else {
            return Integer.valueOf(((BigInteger) m).bitLength());
        }
    }

    /**
     * <p>The number of bits to represent this number.</p>
     *
     * @param m The operand.
     * @return The result.
     */
    private static Number lowestSetBit(Number m) {
        if (m instanceof Integer) {
            int x = m.intValue();
            if (x != Integer.MIN_VALUE) {
                if (x == 0) {
                    return Integer.valueOf(-1);
                } else {
                    return Integer.valueOf(Integer.numberOfTrailingZeros(x));
                }
            } else {
                return 31;
            }
        } else {
            return Integer.valueOf(((BigInteger) m).getLowestSetBit());
        }
    }

    /********************************************************************/
    /* Additional Binary Bitwise Built-in:                              */
    /*      setbit/3: setBit()                                          */
    /*      clearbit/3: clearBit()                                      */
    /*      new_decimal/3: newDecimal()                                 */
    /********************************************************************/

    /**
     * <p>Set a bit.</p>
     *
     * @param m The first operand.
     * @param n The second operand.
     * @return The result.
     * @throws EngineMessage Shit happens.
     */
    private static Number setBit(Number m, Number n) throws EngineMessage {
        EngineMessage.checkNotLessThanZero(m);
        int k = EngineMessage.castIntValue(m);
        if (n instanceof Integer) {
            if (k <= 30) {
                return Integer.valueOf(n.intValue() | (1 << k));
            } else {
                return TermAtomic.normBigInteger(
                        BigInteger.valueOf(n.intValue()).setBit(k));
            }
        } else {
            return TermAtomic.normBigInteger(
                    ((BigInteger) n).setBit(k));
        }
    }

    /**
     * <p>Clear a bit.</p>
     *
     * @param m The first operand.
     * @param n The second operand.
     * @return The result.
     * @throws EngineMessage Shit happens.
     */
    private static Number clearBit(Number m, Number n)
            throws EngineMessage {
        EngineMessage.checkNotLessThanZero(m);
        int k = EngineMessage.castIntValue(m);
        if (n instanceof Integer) {
            if (k <= 30) {
                return Integer.valueOf(n.intValue() & ~(1 << k));
            } else {
                return TermAtomic.normBigInteger(
                        BigInteger.valueOf(n.intValue()).clearBit(k));
            }
        } else {
            return TermAtomic.normBigInteger(
                    ((BigInteger) n).clearBit(k));
        }
    }

}
