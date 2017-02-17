package jekpro.reference.arithmetic;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Special;
import jekpro.model.molec.*;
import jekpro.model.rope.Goal;
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class EvaluableBits extends Special {
    private final static int EVALUABLE_NOT = 0;
    private final static int EVALUABLE_AND = 1;
    private final static int EVALUABLE_OR = 2;
    private final static int EVALUABLE_XOR = 3;
    private final static int EVALUABLE_SHIFT_LEFT = 4;
    private final static int EVALUABLE_SHIFT_RIGHT = 5;

    /**
     * <p>Create an evaluable bits.</p>
     *
     * @param i The index.
     */
    public EvaluableBits(int i) {
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
        if (id < EVALUABLE_AND) {
            Object[] temp = ((SkelCompound) en.skel).args;
            Display ref = en.display;
            en.computeExpr(temp[0], ref, r, u);
            Number alfa = EngineMessage.castInteger(en.skel, en.display);
            switch (id) {
                case EVALUABLE_NOT:
                    en.skel = not(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                default:
                    throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
            }
        } else {
            Object[] temp = ((SkelCompound) en.skel).args;
            Display ref = en.display;
            en.computeExpr(temp[0], ref, r, u);
            Number alfa = EngineMessage.castInteger(en.skel, en.display);
            en.computeExpr(temp[1], ref, r, u);
            Number beta = EngineMessage.castInteger(en.skel, en.display);
            switch (id) {
                case EVALUABLE_AND:
                    en.skel = and(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_OR:
                    en.skel = or(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_XOR:
                    en.skel = xor(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_SHIFT_LEFT:
                    en.skel = shiftLeft(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_SHIFT_RIGHT:
                    en.skel = shiftRight(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                default:
                    throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
            }
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
     * @param m The Prolog integer.
     * @param n The offset.
     * @return The shift left.
     * @throws EngineMessage Not a Prolog integer or max length.
     */
    public static Number shiftLeft(Number m, Number n) throws EngineMessage {
        int x = EngineMessage.castIntValue(n);
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
     * @param m The Prolog integer.
     * @param n The offset.
     * @return The shift left.
     * @throws EngineMessage Not a Prolog integer or max length.
     */
    public static Number shiftRight(Number m, Number n) throws EngineMessage {
        int x = EngineMessage.castIntValue(n);
        if (m instanceof Integer && -31 <= x && x <= 31) {
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

}