package jekmin.reference.experiment;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.arithmetic.SpecialEval;
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SupplementElem extends AbstractSpecial {
    private final static int EVALUABLE_ULP = 0;
    private final static int EVALUABLE_MODINV = 1;
    private final static int EVALUABLE_MODPOW = 2;

    /**
     * <p>Create an elementary evaluable.</p>
     *
     * @param i The built-in ID.
     */
    public SupplementElem(int i) {
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
                case EVALUABLE_ULP:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    en.computeExpr(temp[0], ref);
                    Display d = en.display;
                    boolean multi = d.getAndReset();
                    Number alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = ulp(alfa);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_MODINV:
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
                    en.skel = modinv(alfa, beta);
                    en.display = Display.DISPLAY_CONST;
                    return;
                case EVALUABLE_MODPOW:
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
                    en.computeExpr(temp[2], ref);
                    d = en.display;
                    multi = d.getAndReset();
                    Number gamma = SpecialEval.derefAndCastInteger(en.skel, d);
                    if (multi)
                        d.remTab(en);
                    en.skel = intModPow(alfa, beta, gamma);
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
            return TermAtomic.makeFloat(Math.ulp(m.floatValue()));
        } else if (m instanceof Double) {
            return TermAtomic.makeDouble(Math.ulp(m.doubleValue()));
        } else if (m instanceof Long) {
            return Long.valueOf(1);
        } else {
            return BigDecimal.valueOf(1, ((BigDecimal) m).scale());
        }
    }

    /********************************************************************/
    /* Additional Binary Number Operations:                             */
    /*      modinv/2: modinv()                                          */
    /********************************************************************/

    /**
     * <p>Return the modpow.</p>
     *
     * @param m The first number.
     * @param n The second number.
     * @return The modinv.
     */
    private static Number modinv(Number m, Number n) {
        return TermAtomic.normBigInteger(
                TermAtomic.widenBigInteger(m).modInverse(
                        TermAtomic.widenBigInteger(n)));
    }

    /********************************************************************/
    /* Additional Ternary Number Operations:                            */
    /*      modpow/3: modpow()                                          */
    /********************************************************************/

    /**
     * <p>Return the modpow.</p>
     *
     * @param m The first number.
     * @param n The second number.
     * @param k The third number.
     * @return The modpow.
     */
    private static Number intModPow(Number m, Number n, Number k) {
           return TermAtomic.normBigInteger(
                    TermAtomic.widenBigInteger(m).modPow(
                            TermAtomic.widenBigInteger(n),
                            TermAtomic.widenBigInteger(k)));
    }

}
