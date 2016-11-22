package jekpro.tools.term;

import jekpro.model.molec.EngineMessage;
import jekpro.model.molec.SkelAtom;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * <p>This class can also be used to wrap strings, numbers and references.
 * The constructor takes an unwrapped atomic and gives a wrapped atomic.
 * There are two advantages of wrapped atomics. Firstly they fit into the
 * Term class hierarchy. Secondly wrapped strings can carry call-site
 * information and the polymorphic cache.
 * </p>
 * <p>This class also provides access, guard, normalization and widening
 * for the Prolog number data type. The Java to Prolog API does not provide
 * specific data types for the number data type. Instead directly the
 * various types in the hierarchy of the Java Number class are used.
 * </p>
 * <p>The Prolog integer data type is represented either as a Java
 * Integer class or as a Java BigInteger class. The methods normBigInteger()
 * allow normalizing into a Prolog integer data type. The method
 * widenBigInteger() allows widening into the Java BigInteger class.
 * </p>
 * <p>The Prolog float data type is represented either as a Java
 * Float class or as a Java Double class. The methods guardFloat()
 * and guardDouble() allow sanitizing a Java float data type into
 * a Prolog float data type.
 * </p>
 * <p>The Prolog decimal data type is represented either as a Java
 * Long class or as a Java BigDecimal class. The methods scale()
 * and unscaledValue() allow accessing the components of a Prolog
 * decimal data type. The methods normBigDecimal() allow normalizing
 * into a Prolog decimal data type. The method widenBigDecimal()
 * allows widening into the Java BigDecimal class.
 * </p>
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
public final class TermAtomic extends Term {
    public final static BigInteger MIN_INTEGER = BigInteger.valueOf(Integer.MIN_VALUE);
    public final static BigInteger MAX_INTEGER = BigInteger.valueOf(Integer.MAX_VALUE);
    public final static Float ZERO_FLOAT = Float.valueOf(0.0f);
    public final static Double ZERO_DOUBLE = Double.valueOf(0.0);
    public final static BigInteger MIN_LONG = BigInteger.valueOf(Long.MIN_VALUE);
    public final static BigInteger MAX_LONG = BigInteger.valueOf(Long.MAX_VALUE);

    final Object skel;

    /**
     * <p>Constructor for internal use only.</p>
     *
     * @param s The skeleton.
     * @param f The flag.
     */
    TermAtomic(Object s, boolean f) {
        skel = s;
    }

    /**
     * <p>Create a wrapped atomic from an unwrapped atomic.</p>
     *
     * @param s The unwrapped atomic.
     */
    public TermAtomic(Object s) {
        this((s instanceof String ? new SkelAtom((String) s) : s), false);
    }

    /**
     * <p>Retrieve the unwrapped atomic from a wrapped atomic.</p>
     *
     * @return The unwrapped atomic.
     */
    public Object getValue() {
        return (skel instanceof SkelAtom ? ((SkelAtom) skel).fun : skel);
    }

    /**
     * <p>Compute the hash code of this term.</p>
     *
     * @return The hash value.
     */
    public int hashCode() {
        return skel.hashCode();
    }

    /**
     * <p>Check the identity of this term with another term.</p>
     *
     * @param o The other object.
     * @return True if the other object is a term and this term
     * is identical to the other term, otherwise false.
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof TermAtomic))
            return false;
        TermAtomic t = (TermAtomic) o;
        return skel.equals(t.skel);
    }

    /**
     * <p>Convert this compound to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        return skel.toString();
    }

    /****************************************************************/
    /* Integer Normalization & Widening                             */
    /****************************************************************/

    /**
     * <p>Return the Prolog integer corresponding to the given long.</p>
     * <p>If the long is between MIN_INTEGER and MAX_INTEGER then
     * it is converted into an integer. Otherwise a big integer
     * is returned.</p>
     *
     * @param val The long.
     * @return The object.
     */
    public static Number normBigInteger(long val) {
        if (Integer.MIN_VALUE <= val && val <= Integer.MAX_VALUE) {
            return Integer.valueOf((int) val);
        } else {
            return BigInteger.valueOf(val);
        }
    }

    /**
     * <p>Return the Prolog integer corresponding to the given big integer.</p>
     * <p>If the big integer is between MIN_INTEGER and MAX_INTEGER
     * then it is converted into an Integer. Otherwise the big integer
     * is returned.</p>
     *
     * @param i The big integer.
     * @return The normalized result.
     */
    public static Number normBigInteger(BigInteger i) {
        if (MIN_INTEGER.compareTo(i) <= 0 &&
                i.compareTo(MAX_INTEGER) <= 0) {
            return Integer.valueOf(i.intValue());
        } else {
            return i;
        }
    }

    /**
     * <p>Denormalize the given number to a big integer.</p>
     * <p>If the number is of type integer then it is converted to a big integer.</p>
     * <p>If the number is of type big integer then it is returned.</p>
     * <p>Should be preceded by check whether the argument is integer number at all.</p>
     *
     * @param m The number.
     * @return The big integer.
     */
    public static BigInteger widenBigInteger(Number m) {
        if (m instanceof Integer) {
            return BigInteger.valueOf(m.intValue());
        } else {
            return (BigInteger) m;
        }
    }

    /****************************************************************/
    /* Float Guard                                                  */
    /****************************************************************/

    /**
     * <p>Guard a float.</p>
     * <p>If the float value is a NaN an exception is thrown.</p>
     * <p>If the float value is infinite an exception is thrown.</p>
     * <p>If the float is negative zero it is converted to zero.</p>
     *
     * @param f The float.
     * @return The guarded float.
     * @throws ArithmeticException Shit happens.
     */
    public static Float guardFloat(Float f) throws ArithmeticException {
        if (f.isNaN())
            throw new ArithmeticException(EngineMessage.OP_EVALUATION_UNDEFINED);
        if (f.floatValue() == Float.POSITIVE_INFINITY)
            throw new ArithmeticException(EngineMessage.OP_EVALUATION_FLOAT_OVERFLOW);
        if (f.floatValue() == Float.NEGATIVE_INFINITY)
            throw new ArithmeticException(EngineMessage.OP_EVALUATION_FLOAT_UNDERFLOW);
        if (f.floatValue() == 0.0f)
            return ZERO_FLOAT;
        return f;
    }

    /**
     * <p>Guard a double.</p>
     * <p>If the double value is a NaN an exception is thrown.</p>
     * <p>If the double value is infinite an exception is thrown.</p>
     * <p>If the double is negative zero it is converted to zero.</p>
     *
     * @param d The double.
     * @return The guarded double.
     * @throws ArithmeticException Shit happens.
     */
    public static Double guardDouble(Double d) throws ArithmeticException {
        if (d.isNaN())
            throw new ArithmeticException(EngineMessage.OP_EVALUATION_UNDEFINED);
        if (d.doubleValue() == Double.POSITIVE_INFINITY)
            throw new ArithmeticException(EngineMessage.OP_EVALUATION_FLOAT_OVERFLOW);
        if (d.doubleValue() == Double.NEGATIVE_INFINITY)
            throw new ArithmeticException(EngineMessage.OP_EVALUATION_FLOAT_UNDERFLOW);
        if (d.doubleValue() == 0.0)
            return ZERO_DOUBLE;
        return d;
    }

    /****************************************************************/
    /* Decimal Accessors & Normalization & Widening                 */
    /****************************************************************/

    /**
     * <p>Retrieve the scale.</p>
     *
     * @param n The decimal.
     * @return The scale.
     */
    public static int scale(Number n) {
        if (n instanceof Long) {
            return 0;
        } else {
            return ((BigDecimal) n).scale();
        }
    }

    /**
     * <p>Retrieve the unscaled value.</p>
     *
     * @param n The decimal.
     * @return The unscaled value.
     */
    public static BigInteger unscaledValue(Number n) {
        if (n instanceof Long) {
            return BigInteger.valueOf(n.longValue());
        } else {
            return ((BigDecimal) n).unscaledValue();
        }
    }

    /**
     * <p>Normalize a big decimal into a Prolog decimal.</p>
     * <p>If the scale is 0 and the mantissa is between MIN_LONG
     * and MAX_LONG then it is converted into a long. Otherwise
     * the original big decimal is returned.</p>
     *
     * @param d The big decimal.
     * @return The normalized result.
     */
    public static Number normBigDecimal(BigDecimal d) {
        if (d.scale() != 0)
            return d;
        BigInteger m = d.unscaledValue();
        if (MIN_LONG.compareTo(m) > 0 || m.compareTo(MAX_LONG) > 0)
            return d;
        return Long.valueOf(m.longValue());
    }

    /**
     * <p>Normalize a big integer into a Prolog decimal.</p>
     * <p>If the scale is 0 and if the mantissa is between MIN_LONG
     * and MAX_LONG then it is converted into an long. Otherwise
     * a big decimal is returned.</p>
     *
     * @param m The unscaled value
     * @param s The scale.
     * @return The normalized result.
     */
    public static Number normBigDecimal(BigInteger m, int s) {
        if (s != 0)
            return new BigDecimal(m, s);
        if (MIN_LONG.compareTo(m) > 0 || m.compareTo(MAX_LONG) > 0)
            return new BigDecimal(m, s);
        return Long.valueOf(m.longValue());
    }

    /**
     * <p>Denormalize the given number to a big decimal.</p>
     * <p>If the number is of type long then it is converted to a big decimal.</p>
     * <p>If the number is of type big decimal then it is returned.</p>
     * <p>Otherwise an exception is thrown.</p>
     *
     * @param m The number.
     * @return The big decimal.
     */
    public static BigDecimal widenBigDecimal(Number m) {
        if (m instanceof Integer) {
            return BigDecimal.valueOf(m.intValue());
        } else if (m instanceof BigInteger) {
            return new BigDecimal((BigInteger) m);
        } else if (m instanceof Long) {
            return BigDecimal.valueOf(m.longValue());
        } else if (m instanceof BigDecimal) {
            return (BigDecimal) m;
        } else if (m instanceof Float) {
            return new BigDecimal(m.floatValue());
        } else {
            return new BigDecimal(m.doubleValue());
        }
    }

}
