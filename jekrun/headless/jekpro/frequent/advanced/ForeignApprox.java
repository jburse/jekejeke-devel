package jekpro.frequent.advanced;

import jekpro.model.molec.EngineMessage;
import jekpro.reference.arithmetic.EvaluableRound;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.TermAtomic;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * <p>Provides built-in predicates for the module approx.</p>
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
public final class ForeignApprox {

    /**
     * <p>Retrieve the mantissa of a number.</p>
     *
     * @param m The number.
     * @return The mantissa.
     * @throws InterpreterMessage Not a Prolog number.
     */
    public static Number sysFloatMantissa(Number m)
            throws InterpreterMessage {
        if (m instanceof Integer || m instanceof BigInteger) {
            return m;
        } else if (m instanceof Float) {
            float f = m.floatValue();
            return Integer.valueOf(EvaluableRound.getMantissa(f));
        } else if (m instanceof Double) {
            double d = m.doubleValue();
            return TermAtomic.normBigInteger(EvaluableRound.getMantissa(d));
        } else if (m instanceof Long || m instanceof BigDecimal) {
            return TermAtomic.normBigInteger(TermAtomic.unscaledValue(m));
        } else {
            throw new InterpreterMessage(InterpreterMessage.typeError(
                    EngineMessage.OP_TYPE_STRICT, m));
        }
    }

    /**
     * <p>Retrieve the exponent of a number.</p>
     *
     * @param m The number.
     * @return The exponent.
     * @throws InterpreterMessage Not a Prolog number.
     */
    public static int sysFloatExponent(Number m)
            throws InterpreterMessage {
        if (m instanceof Integer || m instanceof BigInteger) {
            return 0;
        } else if (m instanceof Float) {
            float f = m.floatValue();
            return EvaluableRound.getExponent(f);
        } else if (m instanceof Double) {
            double d = m.doubleValue();
            return EvaluableRound.getExponent(d);
        } else if (m instanceof Long || m instanceof BigDecimal) {
            return -TermAtomic.scale(m);
        } else {
            throw new InterpreterMessage(InterpreterMessage.typeError(
                    EngineMessage.OP_TYPE_STRICT, m));
        }
    }

    /**
     * <p>Retrieve the radix of a number.</p>
     *
     * @param m The number.
     * @return The radix.
     * @throws InterpreterMessage Not a Prolog number.
     */
    public static int sysFloatRadix(Number m)
            throws InterpreterMessage {
        if (m instanceof Integer || m instanceof BigInteger) {
            return 1;
        } else if (m instanceof Float || m instanceof Double) {
            return 2;
        } else if (m instanceof Long || m instanceof BigDecimal) {
            return 10;
        } else {
            throw new InterpreterMessage(InterpreterMessage.typeError(
                    EngineMessage.OP_TYPE_STRICT, m));
        }
    }

}