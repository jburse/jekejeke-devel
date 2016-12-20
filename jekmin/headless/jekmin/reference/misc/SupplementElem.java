package jekmin.reference.misc;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Special;
import jekpro.model.molec.*;
import jekpro.model.rope.Goal;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * <p>Provides additional elementary evaluables.</p>
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
public final class SupplementElem extends Special {
    private final static int EVALUABLE_ULP = 0;
    private final static int EVALUABLE_GCD = 1;

    /**
     * <p>Create an elementary evaluable.</p>
     *
     * @param i The built-in ID.
     */
    public SupplementElem(int i) {
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
            if (id < EVALUABLE_GCD) {
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.computeExpr(temp[0], ref, r, u);
                Number alfa = EngineMessage.castNumber(en.skel, en.display);
                switch (id) {
                    case EVALUABLE_ULP:
                        en.skel = ulp(alfa);
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
                    case EVALUABLE_GCD:
                        en.skel = gcd(alfa, beta);
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
    /* Additional Number Operations:                                    */
    /*      ulp/1: ulp()                                                */
    /********************************************************************/

    /**
     * <p>Return the ulp.</p>
     *
     * @param m The number.
     * @return The ulp.
     * @throws ArithmeticException Not a Prolog number.
     */
    private static Number ulp(Number m) throws ArithmeticException {
        if (m instanceof Integer || m instanceof BigInteger) {
            return Integer.valueOf(1);
        } else if (m instanceof Float) {
            return TermAtomic.guardFloat(Float.valueOf(Math.ulp(m.floatValue())));
        } else if (m instanceof Double) {
            return TermAtomic.guardDouble(Double.valueOf(Math.ulp(m.doubleValue())));
        } else if (m instanceof Long) {
            return Long.valueOf(1);
        } else {
            return BigDecimal.valueOf(1, ((BigDecimal) m).scale());
        }
    }

    /********************************************************************/
    /* Additional Integer Operations:                                   */
    /*      gcd/2: gcd()                                                */
    /********************************************************************/

    /**
     * <p>Return the gcd.</p>
     *
     * @param a The first number.
     * @param b The second number.
     * @return The gcd.
     * @throws ArithmeticException Not a Prolog number.
     */
    private static Number gcd(Number a, Number b) throws ArithmeticException {
        if (a instanceof Integer && b instanceof Integer) {
            int x = binaryGcd(Math.abs(a.intValue()), Math.abs(b.intValue()));
            if (x != Integer.MIN_VALUE) {
                return Integer.valueOf(x);
            } else {
                return BigInteger.valueOf(-(long) x);
            }
        } else {
            return TermAtomic.normBigInteger(
                    TermAtomic.widenBigInteger(a).gcd(
                            TermAtomic.widenBigInteger(b)));
        }
    }

    /**
     * Calculate GCD of a and b interpreted as unsigned integers.
     */
    private static int binaryGcd(int a, int b) {
        if (b == 0)
            return a;
        if (a == 0)
            return b;

        // Right shift a & b till their last bits equal to 1.
        int aZeros = Integer.numberOfTrailingZeros(a);
        int bZeros = Integer.numberOfTrailingZeros(b);
        a >>>= aZeros;
        b >>>= bZeros;

        int t = (aZeros < bZeros ? aZeros : bZeros);

        while (a != b) {
            if ((a + 0x80000000) > (b + 0x80000000)) {  // a > b as unsigned
                a -= b;
                a >>>= Integer.numberOfTrailingZeros(a);
            } else {
                b -= a;
                b >>>= Integer.numberOfTrailingZeros(b);
            }
        }
        return a << t;
    }

}
