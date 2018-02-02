package jekpro.tools.array;

import jekpro.model.inter.Engine;
import jekpro.tools.term.AbstractSkel;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.TermAtomic;
import matula.util.sharik.Livestock;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * <p>Provides mapping between Java data types and the external
 * Prolog data types. The external Prolog data types here only use
 * one pointer, can be used for mapping in predicates and evaluable
 * functions, in class member delegates and proxy argument executors.</p>
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
public final class Types {
    public static final int TYPE_VOID = 0;
    public static final int TYPE_STRING = 1;
    public static final int TYPE_CHARSEQ = 2;
    public static final int TYPE_PRIMBOOL = 3;
    public static final int TYPE_BOOL = 4;
    public static final int TYPE_PRIMBYTE = 5;
    public static final int TYPE_BYTE = 6;
    public static final int TYPE_PRIMCHAR = 7;
    public static final int TYPE_CHAR = 8;
    public static final int TYPE_PRIMSHORT = 9;
    public static final int TYPE_SHORT = 10;
    public static final int TYPE_PRIMINT = 11;
    public static final int TYPE_INTEGER = 12;
    public static final int TYPE_PRIMLONG = 13;
    public static final int TYPE_LONG = 14;
    public static final int TYPE_BIG_INTEGER = 15;
    public static final int TYPE_PRIMFLOAT = 16;
    public static final int TYPE_FLOAT = 17;
    public static final int TYPE_PRIMDOUBLE = 18;
    public static final int TYPE_DOUBLE = 19;
    public static final int TYPE_BIG_DECIMAL = 20;
    public static final int TYPE_NUMBER = 21;
    public static final int TYPE_REF = 22;
    public static final int TYPE_OBJECT = 23;
    public static final int TYPE_TERM = 24;
    public static final int TYPE_INTERPRETER = 25;
    public static final int TYPE_CALLOUT = 26;
    public static final int TYPE_UNSUPPORTED = 27;

    /******************************************************************/
    /* Type Mappings                                                  */
    /******************************************************************/

    /**
     * <p>Normalize a Java type into an external Prolog type.</p>
     *
     * @param typ The Java type.
     * @param res The result object.
     * @return The normalized result object.
     */
    public static Object normJava(int typ, Object res)
            throws EngineMessage {
        try {
            switch (typ) {
                case Types.TYPE_VOID:
                    return AbstractSkel.VOID_OBJ;
                case Types.TYPE_STRING:
                case Types.TYPE_CHARSEQ:
                    return res;
                case Types.TYPE_PRIMBOOL:
                case Types.TYPE_BOOL:
                    if (Boolean.TRUE.equals(res)) {
                        return AbstractSkel.VOID_OBJ;
                    } else {
                        return null;
                    }
                case Types.TYPE_PRIMBYTE:
                case Types.TYPE_BYTE:
                    return Integer.valueOf(((Byte) res).byteValue());
                case Types.TYPE_PRIMCHAR:
                case Types.TYPE_CHAR:
                    return String.valueOf(((Character) res).charValue());
                case Types.TYPE_PRIMSHORT:
                case Types.TYPE_SHORT:
                    return Integer.valueOf(((Short) res).shortValue());
                case Types.TYPE_PRIMINT:
                case Types.TYPE_INTEGER:
                    return res;
                case Types.TYPE_PRIMLONG:
                case Types.TYPE_LONG:
                    if (res != null) {
                        return TermAtomic.normBigInteger(((Long) res).longValue());
                    } else {
                        return null;
                    }
                case Types.TYPE_BIG_INTEGER:
                    if (res != null) {
                        return TermAtomic.normBigInteger((BigInteger) res);
                    } else {
                        return null;
                    }
                case Types.TYPE_PRIMFLOAT:
                case Types.TYPE_FLOAT:
                    if (res != null) {
                        return TermAtomic.guardFloat((Float) res);
                    } else {
                        return null;
                    }
                case Types.TYPE_PRIMDOUBLE:
                case Types.TYPE_DOUBLE:
                    if (res != null) {
                        return TermAtomic.guardDouble((Double) res);
                    } else {
                        return null;
                    }
                case Types.TYPE_BIG_DECIMAL:
                    if (res != null) {
                        return TermAtomic.normBigDecimal((BigDecimal) res);
                    } else {
                        return null;
                    }
                case Types.TYPE_NUMBER:
                case Types.TYPE_REF:
                case Types.TYPE_OBJECT:
                case Types.TYPE_TERM:
                    return res;
                default:
                    throw new IllegalArgumentException("illegal type");
            }
        } catch (ArithmeticException x) {
            throw new EngineMessage(
                    EngineMessage.evaluationError(x.getMessage()));
        }
    }

    /**
     * <p>Denormalize an external Prolog type into a Java type.</p>
     *
     * @param typ The Java type.
     * @param res The argument object, can be null.
     * @return The denormalized argument object, can be null.
     */
    public static Object denormProlog(int typ, Object res)
            throws InterpreterMessage {
        try {
            switch (typ) {
                case Types.TYPE_VOID:
                    return null;
                case Types.TYPE_STRING:
                    if (res != null) {
                        InterpreterMessage.checkInstantiated(res);
                        return InterpreterMessage.castString(res);
                    } else {
                        return null;
                    }
                case Types.TYPE_CHARSEQ:
                    if (res != null) {
                        if (res instanceof String) {
                            return res;
                        } else {
                            InterpreterMessage.checkInstantiated(res);
                            InterpreterMessage.checkRef(res);
                            return res;
                        }
                    } else {
                        return null;
                    }
                case Types.TYPE_PRIMBOOL:
                case Types.TYPE_BOOL:
                    if (res != null) {
                        InterpreterMessage.checkInstantiated(res);
                        String fun = InterpreterMessage.castString(res);
                        return Boolean.valueOf(fun);
                    } else {
                        return null;
                    }
                case Types.TYPE_PRIMBYTE:
                case Types.TYPE_BYTE:
                    if (res != null) {
                        InterpreterMessage.checkInstantiated(res);
                        Number num = InterpreterMessage.castInteger(res);
                        byte val = InterpreterMessage.castByteValue(num);
                        return Byte.valueOf(val);
                    } else {
                        return null;
                    }
                case Types.TYPE_PRIMCHAR:
                case Types.TYPE_CHAR:
                    if (res != null) {
                        InterpreterMessage.checkInstantiated(res);
                        String str = InterpreterMessage.castString(res);
                        char val = InterpreterMessage.castCharValue(str);
                        return Character.valueOf(val);
                    } else {
                        return null;
                    }
                case Types.TYPE_PRIMSHORT:
                case Types.TYPE_SHORT:
                    if (res != null) {
                        InterpreterMessage.checkInstantiated(res);
                        Number num = InterpreterMessage.castInteger(res);
                        short val = InterpreterMessage.castShortValue(num);
                        return Short.valueOf(val);
                    } else {
                        return null;
                    }
                case Types.TYPE_PRIMINT:
                case Types.TYPE_INTEGER:
                    if (res != null) {
                        InterpreterMessage.checkInstantiated(res);
                        Number num = InterpreterMessage.castInteger(res);
                        InterpreterMessage.castIntValue(num);
                        return num;
                    } else {
                        return null;
                    }
                case Types.TYPE_PRIMLONG:
                case Types.TYPE_LONG:
                    if (res != null) {
                        InterpreterMessage.checkInstantiated(res);
                        Number num = InterpreterMessage.castInteger(res);
                        long val = InterpreterMessage.castLongValue(num);
                        return Long.valueOf(val);
                    } else {
                        return null;
                    }
                case Types.TYPE_BIG_INTEGER:
                    if (res != null) {
                        InterpreterMessage.checkInstantiated(res);
                        Number num = InterpreterMessage.castInteger(res);
                        return TermAtomic.widenBigInteger(num);
                    } else {
                        return null;
                    }
                case Types.TYPE_PRIMFLOAT:
                case Types.TYPE_FLOAT:
                    if (res != null) {
                        InterpreterMessage.checkInstantiated(res);
                        Number num = InterpreterMessage.castNumber(res);
                        return (num instanceof Float ? num :
                                TermAtomic.guardFloat(Float.valueOf(num.floatValue())));
                    } else {
                        return null;
                    }
                case Types.TYPE_PRIMDOUBLE:
                case Types.TYPE_DOUBLE:
                    if (res != null) {
                        InterpreterMessage.checkInstantiated(res);
                        Number num = InterpreterMessage.castNumber(res);
                        return (num instanceof Double ? num :
                                TermAtomic.guardDouble(Double.valueOf(num.doubleValue())));
                    } else {
                        return null;
                    }
                case Types.TYPE_BIG_DECIMAL:
                    if (res != null) {
                        InterpreterMessage.checkInstantiated(res);
                        Number num = InterpreterMessage.castNumber(res);
                        return TermAtomic.widenBigDecimal(num);
                    } else {
                        return null;
                    }
                case Types.TYPE_NUMBER:
                    if (res != null) {
                        InterpreterMessage.checkInstantiated(res);
                        return InterpreterMessage.castNumber(res);
                    } else {
                        return null;
                    }
                case Types.TYPE_REF:
                    if (res != null) {
                        InterpreterMessage.checkInstantiated(res);
                        InterpreterMessage.checkRef(res);
                        return res;
                    } else {
                        return null;
                    }
                case Types.TYPE_OBJECT:
                case Types.TYPE_TERM:
                    return res;
                default:
                    throw new IllegalArgumentException("illegal type");
            }
        } catch (ArithmeticException x) {
            throw new InterpreterMessage(
                    InterpreterMessage.evaluationError(x.getMessage()));
        }
    }

    /***********************************************************/
    /* Exception Mapping                                       */
    /***********************************************************/

    /**
     * <p>Check whether the exception class is a mapable throwable.</p>
     *
     * @param ret The exception class.
     * @return True if the exception class is a mappable throwable.
     */
    public static boolean validateThrowable(Class ret) {
        if (InterpreterMessage.class == ret) {
        } else if (IOException.class.isAssignableFrom(ret)) {
        } else if (InterruptedException.class.isAssignableFrom(ret)) {
        } else if (ArithmeticException.class.isAssignableFrom(ret)) {
        } else if (Exception.class.isAssignableFrom(ret)) {
        } else if (Error.class.isAssignableFrom(ret)) {
        } else {
            return false;
        }
        return true;
    }

    /**
     * <p>Map a Java throwable to a Prolog message.</p>
     *
     * @param x The cause.
     * @return The Prolog message.
     */
    public static EngineMessage mapThrowable(Throwable x) {
        if (x instanceof InterpreterMessage) {
            return (EngineMessage) ((InterpreterMessage) x).getException();
        } else if (x instanceof IOException) {
            return EngineMessage.mapIOException((IOException) x);
        } else if (x instanceof InterruptedException) {
            return (EngineMessage) Livestock.sysThreadClear();
        } else if (x instanceof ArithmeticException) {
            String y = x.getMessage();
            if ("/ by zero".equals(y))
                y = EngineMessage.OP_EVALUATION_ZERO_DIVISOR;
            return new EngineMessage(EngineMessage.evaluationError(y));
        } else if (x instanceof Exception) {
            return new EngineMessage(EngineMessage.representationError(
                    x.getMessage()));
        } else if (x instanceof Error) {
            throw (Error) x;
        } else {
            throw new Error("unmappable exception", x);
        }
    }

    /***********************************************************/
    /* Receiver Object                                         */
    /***********************************************************/

    /**
     * <p>Retrieve the receiver.</p>
     *
     * @param t The skeleton.
     * @param d The display.
     * @return The arguments array.
     * @throws EngineMessage Shit happens.
     */
    public static Object castRef(Object t, Display d, Engine en)
            throws EngineMessage {
        en.skel = t;
        en.display = d;
        en.deref();
        if (!(en.skel instanceof AbstractSkel) && !(en.skel instanceof Number)) {
            return en.skel;
        } else {
            EngineMessage.checkInstantiated(en.skel);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_REF,
                    en.skel), en.display);
        }
    }

}
