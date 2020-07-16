package jekpro.reference.arithmetic;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;

import java.math.BigInteger;

/**
 * <p>The foreign predicates for the module arithmetic/bits.</p>
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
public final class EvaluableBits extends AbstractSpecial {
    private final static int EVALUABLE_NOT = 0;
    private final static int EVALUABLE_AND = 1;
    private final static int EVALUABLE_OR = 2;
    private final static int EVALUABLE_XOR = 3;
    private final static int EVALUABLE_SHIFT_LEFT = 4;
    private final static int EVALUABLE_SHIFT_RIGHT = 5;
    private final static int EVALUABLE_GCD = 6;
    private final static int EVALUABLE_MSB = 7;
    private final static int EVALUABLE_LSB = 8;
    private final static int EVALUABLE_POPCOUNT = 9;

    /**
     * <p>Create an evaluable bits.</p>
     *
     * @param i The index.
     */
    public EvaluableBits(int i) {
        super(i);
    }

    /**
     * <p>Arithmetically evaluate an evaluable.</p>
     * <p>The evaluable is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the contskel and contdisplay of the engine.</p>
     * <p>The result is passed via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     * @throws EngineException Shit happens.
     */
    public final void moniEvaluate(Engine en)
            throws EngineMessage, EngineException {
        try {
            switch (id) {
                case EVALUABLE_NOT:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    en.computeExpr(temp[0], ref);
                    Display d = en.display;
                    boolean multi = d.getAndReset();
                    Number alfa = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = EvaluableBits.not(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_AND:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    alfa = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.computeExpr(temp[1], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    Number beta = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = EvaluableBits.and(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_OR:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    alfa = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.computeExpr(temp[1], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    beta = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = EvaluableBits.or(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_XOR:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    alfa = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.computeExpr(temp[1], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    beta = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = EvaluableBits.xor(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_SHIFT_LEFT:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    alfa = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.computeExpr(temp[1], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    beta = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    int x = SpecialEval.castIntValue(beta);
                    en.skel = EvaluableBits.shiftLeft(alfa, x);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_SHIFT_RIGHT:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    alfa = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.computeExpr(temp[1], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    beta = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    x = SpecialEval.castIntValue(beta);
                    en.skel = EvaluableBits.shiftRight(alfa, x);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_GCD:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    alfa = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.computeExpr(temp[1], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    beta = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = EvaluableBits.gcd(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_MSB:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    alfa = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = Integer.valueOf(EvaluableBits.msb(alfa));
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_LSB:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    alfa = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = Integer.valueOf(EvaluableBits.lsb(alfa));
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_POPCOUNT:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    alfa = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = Integer.valueOf(EvaluableBits.popcount(alfa));
                    en.display = Display.DISPLAY_CONST;
                    return;
                default:
                    throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
            }
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
        }
    }

    /**
     * <p>Compute the bitwise negation of a Prolog integer.</p>
     *
     * @param m The Prolog integer.
     * @return The bitwise negation.
     */
    public static Number not(Number m) {
        if (m instanceof Integer) {
            return Integer.valueOf(~m.intValue());
        } else {
            return TermAtomic.normBigInteger(
                    ((BigInteger) m).not());
        }
    }

    /**
     * <p>Compute the bitwise and of two Prolog integers.</p>
     *
     * @param m The first Prolog integer.
     * @param n The second Prolog integer.
     * @return The bitwise and.
     */
    public static Number and(Number m, Number n) {
        if (m instanceof Integer && n instanceof Integer) {
            return Integer.valueOf(m.intValue() & n.intValue());
        } else {
            return TermAtomic.normBigInteger(
                    TermAtomic.widenBigInteger(m).and(
                            TermAtomic.widenBigInteger(n)));
        }
    }

    /**
     * <p>Compute the bitwise or of two Prolog integers.</p>
     *
     * @param m The first Prolog integer.
     * @param n The second Prolog integer.
     * @return The bitwise or.
     */
    public static Number or(Number m, Number n) {
        if (m instanceof Integer && n instanceof Integer) {
            return Integer.valueOf(m.intValue() | n.intValue());
        } else {
            return TermAtomic.normBigInteger(
                    TermAtomic.widenBigInteger(m).or(
                            TermAtomic.widenBigInteger(n)));
        }
    }

    /**
     * <p>Compute the bitwise xor of two Prolog integers.</p>
     *
     * @param m The first Prolog integer.
     * @param n The second Prolog integer.
     * @return The bitwise or.
     */
    public static Number xor(Number m, Number n) {
        if (m instanceof Integer && n instanceof Integer) {
            return Integer.valueOf(m.intValue() ^ n.intValue());
        } else {
            return TermAtomic.normBigInteger(
                    TermAtomic.widenBigInteger(m).xor(
                            TermAtomic.widenBigInteger(n)));
        }
    }

    /**
     * <p>Shift to the left.</p>
     * <p>If b>=0 then same as a * (2**b).</p>
     * <p>If b<0 then same as a div (2**(-b)).</p>
     * <p/>
     *
     * @param m The first operand.
     * @param x The second operand.
     * @return The shift left.
     */
    public static Number shiftLeft(Number m, int x) {
        if (m instanceof Integer) {
            if (x == 0) {
                return m;
            } else if (x > 0 && x <= 31) {
                return TermAtomic.normBigInteger((long) m.intValue() << x);
            } else if (x < 0 && -31 <= x) {
                return Integer.valueOf(m.intValue() >> (-x));
            } else {
                return TermAtomic.normBigInteger(
                        BigInteger.valueOf(m.intValue()).shiftLeft(x));
            }
        } else {
            return TermAtomic.normBigInteger(
                    ((BigInteger) m).shiftLeft(x));
        }
    }

    /**
     * <p>Shift to the right.</p>
     * <p>If b>=0 then same as a div (2**b).</p>
     * <p>If b<0 then same as a * (2**(-b)).</p>
     * <p/>
     *
     * @param m The first operand.
     * @param x The second operand.
     * @return The shift left.
     */
    public static Number shiftRight(Number m, int x) {
        if (m instanceof Integer) {
            if (x == 0) {
                return m;
            } else if (x > 0 && x <= 31) {
                return Integer.valueOf(m.intValue() >> x);
            } else if (x < 0 && -31 <= x) {
                return TermAtomic.normBigInteger((long) m.intValue() << (-x));
            } else {
                return TermAtomic.normBigInteger(
                        BigInteger.valueOf(m.intValue()).shiftRight(x));
            }
        } else {
            return TermAtomic.normBigInteger(
                    ((BigInteger) m).shiftRight(x));
        }
    }

    /********************************************************************/
    /* Additional Binary Number Operations:                             */
    /*      gcd/2: gcd()                                                */
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

    /********************************************************************/
    /* Additional Unary Number Operations:                              */
    /*      msb/1: msb()                                                */
    /*      lsb/1: lsb()                                                */
    /*      popcount/1: popcount()                                      */
    /********************************************************************/

    /**
     * <p>The index of the most significant bit.</p>
     *
     * @param m The number.
     * @return The msb.
     */
    private static int msb(Number m) {
        if (m instanceof Integer) {
            int x = m.intValue();
            int y = Math.abs(x);
            int k = 31 - Integer.numberOfLeadingZeros(y);
            if (x < 0 && Integer.bitCount(y) == 1) {
               return k - 1;
            } else {
               return k;
            }
        } else {
            return ((BigInteger) m).bitLength() - 1;
        }
    }

    /**
     * <p>The lowest set bit of this number.</p>
     *
     * @param m The operand.
     * @return The result.
     */
    private static int lsb(Number m) {
        if (m instanceof Integer) {
            int x = m.intValue();
            if (x == 0) {
                return -1;
            } else {
                return Integer.numberOfTrailingZeros(x);
            }
        } else {
            return ((BigInteger) m).getLowestSetBit();
        }
    }

    /**
     * <p>The number of bits.</p>
     *
     * @param m The operand.
     * @return The popcount.
     */
    private static int popcount(Number m) {
        if (m instanceof Integer) {
            int x = m.intValue();
            int y = Math.abs(x);
            int k = Integer.bitCount(y);
            if (x < 0) {
                return k + Integer.numberOfTrailingZeros(y) - 1;
            } else {
                return k;
            }
        } else {
            return ((BigInteger) m).bitCount();
        }
    }

}
