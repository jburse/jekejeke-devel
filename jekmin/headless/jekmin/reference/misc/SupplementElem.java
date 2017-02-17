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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

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
    private final static int EVALUABLE_LOG2 = 0;
    private final static int EVALUABLE_LOG10 = 1;
    private final static int EVALUABLE_ULP = 2;
    private final static int EVALUABLE_GCD = 3;

    public static final Double DOUBLE_LOG2 = Double.valueOf(Math.log(2));
    public static final Double DOUBLE_LOG10 = Double.valueOf(Math.log(10));

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
            switch (id) {
                case EVALUABLE_LOG2:
                    en.skel = DOUBLE_LOG2;
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_LOG10:
                    en.skel = DOUBLE_LOG10;
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_ULP:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    en.computeExpr(temp[0], ref, r, u);
                    Number alfa = EngineMessage.castNumber(en.skel, en.display);
                    en.skel = ulp(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_GCD:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.computeExpr(temp[0], ref, r, u);
                    alfa = EngineMessage.castInteger(en.skel, en.display);
                    en.computeExpr(temp[1], ref, r, u);
                    Number beta = EngineMessage.castInteger(en.skel, en.display);
                    en.skel = gcd(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                default:
                    throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);

            }
        } catch (ArithmeticException x) {
            throw new EngineMessage(EngineMessage.evaluationError(x.getMessage()));
        }
    }

    /********************************************************************/
    /* Additional Unary Number Operations:                              */
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

}
