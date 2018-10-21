package jekpro.reference.arithmetic;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.BindCount;
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
     * @param en The engine.
     * @return True if new display is returned, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public final boolean moniEvaluate(Engine en)
            throws EngineMessage, EngineException {
        try {
            switch (id) {
                case EVALUABLE_NOT:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    BindCount[] ref = en.display;
                    boolean multi = en.computeExpr(temp[0], ref);
                    BindCount[] d = en.display;
                    Number alfa = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        BindCount.remTab(d, en);
                    en.skel = not(alfa);
                    en.display = BindCount.DISPLAY_CONST;
                    return false;
                case EVALUABLE_AND:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = en.computeExpr(temp[0], ref);
                    d = en.display;
                    alfa = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        BindCount.remTab(d, en);
                    multi = en.computeExpr(temp[1], ref);
                    d = en.display;
                    Number beta = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        BindCount.remTab(d, en);
                    en.skel = and(alfa, beta);
                    en.display = BindCount.DISPLAY_CONST;
                    return false;
                case EVALUABLE_OR:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = en.computeExpr(temp[0], ref);
                    d = en.display;
                    alfa = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        BindCount.remTab(d, en);
                    multi = en.computeExpr(temp[1], ref);
                    d = en.display;
                    beta = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        BindCount.remTab(d, en);
                    en.skel = or(alfa, beta);
                    en.display = BindCount.DISPLAY_CONST;
                    return false;
                case EVALUABLE_XOR:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = en.computeExpr(temp[0], ref);
                    d = en.display;
                    alfa = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        BindCount.remTab(d, en);
                    multi = en.computeExpr(temp[1], ref);
                    d = en.display;
                    beta = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        BindCount.remTab(d, en);
                    en.skel = xor(alfa, beta);
                    en.display = BindCount.DISPLAY_CONST;
                    return false;
                case EVALUABLE_SHIFT_LEFT:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = en.computeExpr(temp[0], ref);
                    d = en.display;
                    alfa = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        BindCount.remTab(d, en);
                    multi = en.computeExpr(temp[1], ref);
                    d = en.display;
                    beta = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        BindCount.remTab(d, en);
                    int x = SpecialEval.castIntValue(beta);
                    en.skel = shiftLeft(alfa, x);
                    en.display = BindCount.DISPLAY_CONST;
                    return false;
                case EVALUABLE_SHIFT_RIGHT:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = en.computeExpr(temp[0], ref);
                    d = en.display;
                    alfa = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        BindCount.remTab(d, en);
                    multi = en.computeExpr(temp[1], ref);
                    d = en.display;
                    beta = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        BindCount.remTab(d, en);
                    x = SpecialEval.castIntValue(beta);
                    en.skel = shiftRight(alfa, x);
                    en.display = BindCount.DISPLAY_CONST;
                    return false;
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

}
