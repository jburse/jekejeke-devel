package jekmin.reference.misc;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.arithmetic.SpecialEval;
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SupplementBits extends AbstractSpecial {
    private final static int EVALUABLE_SETBIT = 3;
    private final static int EVALUABLE_CLEARBIT = 4;

    /**
     * <p>Create a special arithmetic.</p>
     *
     * @param i The built-in ID.
     */
    public SupplementBits(int i) {
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
     */
    public final void moniEvaluate(Engine en)
            throws EngineMessage, EngineException {
        try {
            switch (id) {
                case EVALUABLE_SETBIT:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    en.computeExpr(temp[0], ref);
                    Display d = en.display;
                    boolean multi = d.getAndReset();
                    Number alfa = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.computeExpr(temp[1], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    Number beta = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    SpecialEval.checkNotLessThanZero(beta);
                    int x = SpecialEval.castIntValue(beta);
                    en.skel = setBit(alfa, x);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_CLEARBIT:
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
                    SpecialEval.checkNotLessThanZero(beta);
                    x = SpecialEval.castIntValue(beta);
                    en.skel = clearBit(alfa, x);
                    en.display = Display.DISPLAY_CONST;
                    return;
                default:
                    throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
            }
        } catch (ArithmeticException x) {
            throw new EngineMessage(
                    EngineMessage.evaluationError(x.getMessage()));
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
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
     * @param n The first operand.
     * @param x The second operand.
     * @return The result.
     */
    private static Number setBit(Number n, int x) {
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
     * @param n The first operand.
     * @param x The second operand.
     * @return The result.
     */
    private static Number clearBit(Number n, int x) {
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

}
