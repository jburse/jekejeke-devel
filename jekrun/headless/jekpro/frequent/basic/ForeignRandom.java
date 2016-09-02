package jekpro.frequent.basic;

import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.TermAtomic;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Random;

/**
 * <p>The foreign evaluable functions for the module basic/random.</p>
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
public final class ForeignRandom {

    /**
     * <p>Generate a random number in the given range.</p>
     * <p>The range must be greater than zero.</p>
     *
     * @param r The random number generator.
     * @param m The range.
     * @return The random number.
     * @throws InterpreterMessage Negative or zero range.
     */
    public static Number sysRandomNext(Random r, Number m)
            throws InterpreterMessage {
        if (m instanceof Integer) {
            int x = m.intValue();
            if (x <= 0)
                throw new InterpreterMessage(InterpreterMessage.evaluationError(
                        "undefined"));
            return Integer.valueOf(nextInteger(x, r));
        } else if (m instanceof BigInteger) {
            BigInteger x = (BigInteger) m;
            if (x.compareTo(BigInteger.ZERO) <= 0)
                throw new InterpreterMessage(InterpreterMessage.evaluationError(
                        "undefined"));
            return TermAtomic.normBigInteger(nextBigInteger(x, r));
        } else if (m instanceof Float) {
            float x = m.floatValue();
            if (x <= 0)
                throw new InterpreterMessage(InterpreterMessage.evaluationError(
                        "undefined"));
            return TermAtomic.guardFloat(Float.valueOf(x * r.nextFloat()));
        } else if (m instanceof Double) {
            double x = m.doubleValue();
            if (x <= 0)
                throw new InterpreterMessage(InterpreterMessage.evaluationError(
                        "undefined"));
            return TermAtomic.guardDouble(Double.valueOf(x * r.nextDouble()));
        } else if (m instanceof Long) {
            long x = m.longValue();
            if (x <= 0)
                throw new InterpreterMessage(InterpreterMessage.evaluationError(
                        "undefined"));
            return Long.valueOf(nextLong(x, r));
        } else if (m instanceof BigDecimal) {
            BigDecimal x = (BigDecimal) m;
            if (x.compareTo(BigDecimal.ZERO) <= 0)
                throw new InterpreterMessage(InterpreterMessage.evaluationError(
                        "undefined"));
            return TermAtomic.normBigDecimal(nextBigDecimal(x, r));
        } else {
            throw new InterpreterMessage(InterpreterMessage.typeError(
                    "number", m));
        }
    }

    /**
     * <p>Generate a random integer in the interval [0..m)</p>
     *
     * @param m The integer magnitude.
     * @param r The random number generator.
     * @return The random integer.
     */
    private static int nextInteger(int m, Random r) {
        int j = Integer.numberOfLeadingZeros(m);
        int k;
        do {
            k = r.nextInt() >>> j;
        } while (k >= m);
        return k;
    }

    /**
     * <p>Generate a random big integer in the interval [0..m)</p>
     *
     * @param m The big integer magnitude.
     * @param r The random number generator.
     * @return The random big integer.
     */
    private static BigInteger nextBigInteger(BigInteger m, Random r) {
        int j = m.bitLength();
        BigInteger k;
        do {
            k = new BigInteger(j, r);
        } while (k.compareTo(m) >= 0);
        return k;
    }

    /**
     * <p>Generate a random long in the interval [0..m)</p>
     *
     * @param m The integer magnitude.
     * @param r The random number generator.
     * @return The random integer.
     */
    private static long nextLong(long m, Random r) {
        int j = Long.numberOfLeadingZeros(m);
        long k;
        do {
            k = r.nextLong() >>> j;
        } while (k >= m);
        return k;
    }

    /**
     * <p>Generate a random big decimal in the interval [0..m)</p>
     *
     * @param m The big decimal magnitude.
     * @param r The random number generator.
     * @return The random big decimal.
     */
    private static BigDecimal nextBigDecimal(BigDecimal m, Random r) {
        return new BigDecimal(
                nextBigInteger(m.unscaledValue(), r),
                m.scale());
    }

}
