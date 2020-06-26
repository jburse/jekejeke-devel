package jekpro.tools.array;

import jekpro.frequent.system.ForeignThread;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.reflect.SpecialForeign;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.*;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.HashMap;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
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
    public static final int TYPE_ATOMIC = 25;
    public static final int TYPE_INTERPRETER = 26;
    public static final int TYPE_CALLOUT = 27;

    /******************************************************************/
    /* Evaluable Types                                                */
    /******************************************************************/

    public final static HashMap<Class, Integer> typeeval = new HashMap<Class, Integer>();

    static {
        Types.typeeval.put(Byte.TYPE, Integer.valueOf(Types.TYPE_PRIMBYTE));
        Types.typeeval.put(Byte.class, Integer.valueOf(Types.TYPE_BYTE));
        Types.typeeval.put(Short.TYPE, Integer.valueOf(Types.TYPE_PRIMSHORT));
        Types.typeeval.put(Short.class, Integer.valueOf(Types.TYPE_SHORT));
        Types.typeeval.put(Integer.TYPE, Integer.valueOf(Types.TYPE_PRIMINT));
        Types.typeeval.put(Integer.class, Integer.valueOf(Types.TYPE_INTEGER));
        Types.typeeval.put(Long.TYPE, Integer.valueOf(Types.TYPE_PRIMLONG));
        Types.typeeval.put(Long.class, Integer.valueOf(Types.TYPE_LONG));
        Types.typeeval.put(BigInteger.class, Integer.valueOf(Types.TYPE_BIG_INTEGER));
        Types.typeeval.put(Float.TYPE, Integer.valueOf(Types.TYPE_PRIMFLOAT));
        Types.typeeval.put(Float.class, Integer.valueOf(Types.TYPE_FLOAT));
        Types.typeeval.put(Double.TYPE, Integer.valueOf(Types.TYPE_PRIMDOUBLE));
        Types.typeeval.put(Double.class, Integer.valueOf(Types.TYPE_DOUBLE));
        Types.typeeval.put(BigDecimal.class, Integer.valueOf(Types.TYPE_BIG_DECIMAL));
        Types.typeeval.put(Number.class, Integer.valueOf(Types.TYPE_NUMBER));
        Types.typeeval.put(Interpreter.class, Integer.valueOf(Types.TYPE_INTERPRETER));
    }

    /******************************************************************/
    /* Predicate Types                                                */
    /******************************************************************/

    public final static HashMap<Class, Integer> typepred = new HashMap<Class, Integer>();

    static {
        Types.typepred.put(Void.TYPE, Integer.valueOf(Types.TYPE_VOID));
        Types.typepred.put(String.class, Integer.valueOf(Types.TYPE_STRING));
        Types.typepred.put(CharSequence.class, Integer.valueOf(Types.TYPE_CHARSEQ));
        Types.typepred.put(Boolean.TYPE, Integer.valueOf(Types.TYPE_PRIMBOOL));
        Types.typepred.put(Boolean.class, Integer.valueOf(Types.TYPE_BOOL));
        Types.typepred.put(Byte.TYPE, Integer.valueOf(Types.TYPE_PRIMBYTE));
        Types.typepred.put(Byte.class, Integer.valueOf(Types.TYPE_BYTE));
        Types.typepred.put(Character.TYPE, Integer.valueOf(Types.TYPE_PRIMCHAR));
        Types.typepred.put(Character.class, Integer.valueOf(Types.TYPE_CHAR));
        Types.typepred.put(Short.TYPE, Integer.valueOf(Types.TYPE_PRIMSHORT));
        Types.typepred.put(Short.class, Integer.valueOf(Types.TYPE_SHORT));
        Types.typepred.put(Integer.TYPE, Integer.valueOf(Types.TYPE_PRIMINT));
        Types.typepred.put(Integer.class, Integer.valueOf(Types.TYPE_INTEGER));
        Types.typepred.put(Long.TYPE, Integer.valueOf(Types.TYPE_PRIMLONG));
        Types.typepred.put(Long.class, Integer.valueOf(Types.TYPE_LONG));
        Types.typepred.put(BigInteger.class, Integer.valueOf(Types.TYPE_BIG_INTEGER));
        Types.typepred.put(Float.TYPE, Integer.valueOf(Types.TYPE_PRIMFLOAT));
        Types.typepred.put(Float.class, Integer.valueOf(Types.TYPE_FLOAT));
        Types.typepred.put(Double.TYPE, Integer.valueOf(Types.TYPE_PRIMDOUBLE));
        Types.typepred.put(Double.class, Integer.valueOf(Types.TYPE_DOUBLE));
        Types.typepred.put(BigDecimal.class, Integer.valueOf(Types.TYPE_BIG_DECIMAL));
        Types.typepred.put(Number.class, Integer.valueOf(Types.TYPE_NUMBER));
        Types.typepred.put(Object.class, Integer.valueOf(Types.TYPE_OBJECT));
        Types.typepred.put(TermVar.class, Integer.valueOf(Types.TYPE_OBJECT));
        Types.typepred.put(TermCompound.class, Integer.valueOf(Types.TYPE_OBJECT));
        Types.typepred.put(AbstractTerm.class, Integer.valueOf(Types.TYPE_TERM));
        Types.typepred.put(TermAtomic.class, Integer.valueOf(Types.TYPE_ATOMIC));
        Types.typepred.put(Interpreter.class, Integer.valueOf(Types.TYPE_INTERPRETER));
        Types.typepred.put(CallOut.class, Integer.valueOf(Types.TYPE_CALLOUT));
    }

    /******************************************************************/
    /* Type Mappings                                                  */
    /******************************************************************/

    /**
     * <p>Normalize a Java type into an external Prolog type.</p>
     *
     * @param typ The Java type.
     * @param res The result object.
     * @return The normalized result object.
     * @throws EngineMessage FFI error.
     */
    public static Object normJava(int typ, Object res)
            throws EngineMessage {
        try {
            switch (typ) {
                case Types.TYPE_STRING:
                case Types.TYPE_CHARSEQ:
                    return res;
                case Types.TYPE_PRIMBOOL:
                case Types.TYPE_BOOL:
                    if (res != null) {
                        return Boolean.toString((Boolean) res);
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
                        if (TermAtomic.guardFloat(((Float) res).floatValue())) {
                            return res;
                        } else {
                            return TermAtomic.ZERO_FLOAT;
                        }
                    } else {
                        return null;
                    }
                case Types.TYPE_PRIMDOUBLE:
                case Types.TYPE_DOUBLE:
                    if (res != null) {
                        if (TermAtomic.guardDouble(((Double) res).doubleValue())) {
                            return res;
                        } else {
                            return TermAtomic.ZERO_DOUBLE;
                        }
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
                case Types.TYPE_ATOMIC:
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
     * <p>Generate a indicator value, where Java doesn't provide one.</p>
     *
     * @param typ The Java type.
     * @param res The Java return value.
     * @return The indicator value.
     */
    public static Object noretNormJava(int typ, Object res) {
        return (typ == Types.TYPE_VOID ||
                Boolean.TRUE.equals(res) ? AbstractSkel.VOID_OBJ : null);
    }

    /**
     * <p>Denormalize an external Prolog type into a Java type.</p>
     *
     * @param typ The Java type.
     * @param t   The argument skeleton.
     * @param d   The argument display.
     * @return The denormalized argument.
     * @throws EngineMessage FFI error.
     */
    public static Object denormProlog(int typ, Object t, Display d)
            throws EngineMessage {
        try {
            switch (typ) {
                case Types.TYPE_STRING:
                    return SpecialUniv.derefAndCastString(t, d);
                case Types.TYPE_CHARSEQ:
                    BindUniv b;
                    while (t instanceof SkelVar &&
                            (b = d.bind[((SkelVar) t).id]).display != null) {
                        t = b.skel;
                        d = b.display;
                    }
                    if (t instanceof SkelAtom) {
                        return ((SkelAtom) t).fun;
                    } else if (!(t instanceof AbstractSkel) && !(t instanceof Number)) {
                        return t;
                    } else {
                        EngineMessage.checkInstantiated(t);
                        throw new EngineMessage(EngineMessage.typeError(
                                EngineMessage.OP_TYPE_REF, t), d);
                    }
                case Types.TYPE_PRIMBOOL:
                case Types.TYPE_BOOL:
                    String str = SpecialUniv.derefAndCastString(t, d);
                    return Boolean.valueOf(str);
                case Types.TYPE_PRIMBYTE:
                case Types.TYPE_BYTE:
                    Number num = SpecialEval.derefAndCastInteger(t, d);
                    return Byte.valueOf(SpecialEval.castByteValue(num));
                case Types.TYPE_PRIMCHAR:
                case Types.TYPE_CHAR:
                    str = SpecialUniv.derefAndCastString(t, d);
                    return Character.valueOf(SpecialUniv.castCharValue(str));
                case Types.TYPE_PRIMSHORT:
                case Types.TYPE_SHORT:
                    num = SpecialEval.derefAndCastInteger(t, d);
                    return Short.valueOf(SpecialEval.castShortValue(num));
                case Types.TYPE_PRIMINT:
                case Types.TYPE_INTEGER:
                    num = SpecialEval.derefAndCastInteger(t, d);
                    SpecialEval.castIntValue(num);
                    return num;
                case Types.TYPE_PRIMLONG:
                case Types.TYPE_LONG:
                    num = SpecialEval.derefAndCastInteger(t, d);
                    return Long.valueOf(SpecialEval.castLongValue(num));
                case Types.TYPE_BIG_INTEGER:
                    num = SpecialEval.derefAndCastInteger(t, d);
                    return TermAtomic.widenBigInteger(num);
                case Types.TYPE_PRIMFLOAT:
                case Types.TYPE_FLOAT:
                    num = SpecialEval.derefAndCastNumber(t, d);
                    return (num instanceof Float ? num :
                            TermAtomic.makeFloat(num.floatValue()));
                case Types.TYPE_PRIMDOUBLE:
                case Types.TYPE_DOUBLE:
                    num = SpecialEval.derefAndCastNumber(t, d);
                    return (num instanceof Double ? num :
                            TermAtomic.makeDouble(num.doubleValue()));
                case Types.TYPE_BIG_DECIMAL:
                    num = SpecialEval.derefAndCastNumber(t, d);
                    return TermAtomic.widenBigDecimal(num);
                case Types.TYPE_NUMBER:
                    return SpecialEval.derefAndCastNumber(t, d);
                case Types.TYPE_REF:
                    return SpecialUniv.derefAndCastRef(t, d);
                case Types.TYPE_OBJECT:
                    while (t instanceof SkelVar &&
                            (b = d.bind[((SkelVar) t).id]).display != null) {
                        t = b.skel;
                        d = b.display;
                    }
                    return AbstractTerm.createTerm(t, d);
                case Types.TYPE_TERM:
                    while (t instanceof SkelVar &&
                            (b = d.bind[((SkelVar) t).id]).display != null) {
                        t = b.skel;
                        d = b.display;
                    }
                    return AbstractTerm.createTermWrapped(t, d);
                case Types.TYPE_ATOMIC:
                    while (t instanceof SkelVar &&
                            (b = d.bind[((SkelVar) t).id]).display != null) {
                        t = b.skel;
                        d = b.display;
                    }
                    if (!(t instanceof SkelVar) && !(t instanceof SkelCompound)) {
                        return AbstractTerm.createTermWrapped(t, d);
                    } else {
                        EngineMessage.checkInstantiated(t);
                        throw new EngineMessage(EngineMessage.typeError(
                                EngineMessage.OP_TYPE_ATOMIC, t), d);
                    }
                default:
                    throw new IllegalArgumentException("illegal type");
            }
        } catch (ArithmeticException x) {
            throw new EngineMessage(
                    EngineMessage.evaluationError(x.getMessage()));
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
        }
    }

    /***********************************************************/
    /* Return Types                                            */
    /***********************************************************/

    /**
     * <p>Compute the declared function status.</p>
     *
     * @param typ The type.
     * @return The declared fucntion status.
     */
    public static boolean getRetFlag(int typ) {
        switch (typ) {
            case Types.TYPE_VOID:
            case Types.TYPE_PRIMBOOL:
            case Types.TYPE_BOOL:
                return false;
            case Types.TYPE_STRING:
            case Types.TYPE_CHARSEQ:
            case Types.TYPE_PRIMBYTE:
            case Types.TYPE_BYTE:
            case Types.TYPE_PRIMCHAR:
            case Types.TYPE_CHAR:
            case Types.TYPE_PRIMSHORT:
            case Types.TYPE_SHORT:
            case Types.TYPE_PRIMINT:
            case Types.TYPE_INTEGER:
            case Types.TYPE_PRIMLONG:
            case Types.TYPE_LONG:
            case Types.TYPE_BIG_INTEGER:
            case Types.TYPE_PRIMFLOAT:
            case Types.TYPE_FLOAT:
            case Types.TYPE_PRIMDOUBLE:
            case Types.TYPE_DOUBLE:
            case Types.TYPE_BIG_DECIMAL:
            case Types.TYPE_NUMBER:
            case Types.TYPE_REF:
            case Types.TYPE_OBJECT:
            case Types.TYPE_TERM:
            case Types.TYPE_ATOMIC:
                return true;
            default:
                throw new IllegalArgumentException("illegal return type");
        }
    }

    /***********************************************************/
    /* Throwable Mapping                                       */
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
     * @param x The Java throwable.
     * @return The Prolog message.
     */
    public static EngineMessage mapThrowable(Throwable x) {
        if (x instanceof InterpreterMessage) {
            return (EngineMessage) ((InterpreterMessage) x).getException();
        } else if (x instanceof IOException) {
            return EngineMessage.mapIOException((IOException) x);
        } else if (x instanceof InterruptedException) {
            return (EngineMessage) ForeignThread.sysThreadClear();
        } else if (x instanceof ArithmeticException) {
            String message = x.getMessage();
            if ("/ by zero".equals(message))
                message = EngineMessage.OP_EVALUATION_ZERO_DIVISOR;
            return new EngineMessage(EngineMessage.evaluationError(
                    (message != null ? message : "")));
        } else if (x instanceof Exception) {
            String message = x.getMessage();
            return new EngineMessage(EngineMessage.representationError(
                    (message != null ? message : "")));
        } else if (x instanceof Error) {
            throw (Error) x;
        } else {
            throw new Error("unmappable throwable", x);
        }
    }

    /***********************************************************/
    /* Exception and Error Mapping                             */
    /***********************************************************/

    /**
     * <p>Map a Java exception to a Prolog message.</p>
     * <p>This is used for reflective calls.</p>
     *
     * @param x The Java exception.
     * @param y The Java member.
     * @return The Prolog message.
     */
    public static EngineMessage mapException(Exception x, Member y) {
        if (x instanceof IllegalAccessException) {
            return new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_ACCESS,
                    mapMemberType(y),
                    mapMemberCulprit(y)));
        } else if (x instanceof IllegalArgumentException) {
            return new EngineMessage(EngineMessage.permissionError(
                    AbstractFactory.OP_PERMISSION_APPLY,
                    mapMemberType(y),
                    mapMemberCulprit(y)));
        } else if (x instanceof NullPointerException) {
            return new EngineMessage(EngineMessage.permissionError(
                    AbstractFactory.OP_PERMISSION_LOOKUP,
                    mapMemberType(y),
                    mapMemberCulprit(y)));
        } else if (x instanceof InstantiationException) {
            return new EngineMessage(EngineMessage.permissionError(
                    AbstractFactory.OP_PERMISSION_NEW,
                    mapMemberType(y),
                    mapMemberCulprit(y)));
        } else {
            throw new IllegalArgumentException("illegal exception");
        }
    }

    /**
     * <p>Map a Java member to a Prolog type.</p>
     *
     * @param y The Java member.
     * @return The Prolog type.
     */
    public static String mapMemberType(Member y) {
        if (y instanceof Field) {
            return AbstractFactory.OP_PERMISSION_FIELD;
        } else if (y instanceof Method) {
            return AbstractFactory.OP_PERMISSION_METHOD;
        } else if (y instanceof Constructor) {
            return AbstractFactory.OP_PERMISSION_CONSTRUCTOR;
        } else {
            throw new IllegalArgumentException("illegal member");
        }
    }

    /**
     * <p>Map a Java member to a Prolog culprit.</p>
     *
     * @param y The Java member.
     * @return The Prolog culprit.
     */
    public static Object mapMemberCulprit(Member y) {
        if (y instanceof Field) {
            Field field = (Field) y;
            return new SkelAtom(field.getName());
        } else if (y instanceof Method) {
            Method method = (Method) y;
            return SpecialForeign.methodToCallable(method.getName(),
                    method.getParameterTypes());
        } else if (y instanceof Constructor) {
            Constructor constructor = (Constructor) y;
            return SpecialForeign.constructorToCallable(
                    constructor.getParameterTypes());
        } else {
            throw new IllegalArgumentException("illegal member");
        }
    }

    /**
     * <p>Map a Java exception to a Prolog message.</p>
     * <p>This is used for reflective calls.</p>
     *
     * @param x The Java exception.
     * @return The Prolog message.
     */
    public static EngineMessage mapError(Error x) {
        if (x instanceof ExceptionInInitializerError) {
            return new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_INIT,
                    EngineMessage.OP_PERMISSION_CLASS,
                    new SkelAtom(x.getMessage())));
        } else if (x instanceof NoClassDefFoundError) {
            return new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_LINK,
                    EngineMessage.OP_PERMISSION_CLASS,
                    new SkelAtom(x.getMessage())));
        } else {
            throw new IllegalArgumentException("illegal error");
        }
    }

    /***********************************************************/
    /* CheerpJ Workaround InvocationTargetException            */
    /***********************************************************/

    /**
     * <p>Map a Java exception to a Prolog message.</p>
     * <p>This is used for reflective calls.</p>
     *
     * @param z  The Java exception.
     * @param y  The Java member.
     * @param en The engine.
     */
    /*
    public static Throwable mapException(Exception z, Member y, Engine en) {
        int hint = en.store.foyer.getHint();
        switch (hint) {
            case Foyer.HINT_WEB:
                if (isException(z)) {
                    return Types.mapException(z, y);
                } else {
                    Throwable x = z;
                    if (x instanceof RuntimeWrap)
                        x = x.getCause();
                    if (x instanceof InterpreterException) {
                        return ((InterpreterException) x).getException();
                    } else {
                        return Types.mapThrowable(x);
                    }
                }
            default:
                return Types.mapException(z, y);
        }
    }
    */

    /**
     * <p>CHeck whether an exception is a reflection exception.</p>
     *
     * @param x The exeception.
     * @return True if the exception is a reflection exception, otherwise false.
     */
    /*
    private static boolean isException(Exception x) {
        if (x instanceof IllegalAccessException) {
        } else if (x instanceof IllegalArgumentException) {
        } else if (x instanceof NullPointerException) {
        } else {
            return false;
        }
        return true;
    }
    */

}
