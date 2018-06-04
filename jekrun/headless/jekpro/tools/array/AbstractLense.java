package jekpro.tools.array;

import jekpro.model.builtin.SpecialSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.BindVar;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.HashMap;

/**
 * <p>Base class for the Java array class delegates.</p>
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
public abstract class AbstractLense extends AbstractDelegate {
    public final static int MASK_METH_FUNC = 0x00000100;

    final static int[] VOID_PARAS = new int[0];

    public int encodeobj;
    public int[] encodeparas;
    public int encoderet;

    /******************************************************************/
    /* Evaluable Types                                                */
    /******************************************************************/

    private final static HashMap<Class, Integer> typeeval = new HashMap<Class, Integer>();

    static {
        typeeval.put(Byte.TYPE, Integer.valueOf(Types.TYPE_PRIMBYTE));
        typeeval.put(Byte.class, Integer.valueOf(Types.TYPE_BYTE));
        typeeval.put(Short.TYPE, Integer.valueOf(Types.TYPE_PRIMSHORT));
        typeeval.put(Short.class, Integer.valueOf(Types.TYPE_SHORT));
        typeeval.put(Integer.TYPE, Integer.valueOf(Types.TYPE_PRIMINT));
        typeeval.put(Integer.class, Integer.valueOf(Types.TYPE_INTEGER));
        typeeval.put(Long.TYPE, Integer.valueOf(Types.TYPE_PRIMLONG));
        typeeval.put(Long.class, Integer.valueOf(Types.TYPE_LONG));
        typeeval.put(BigInteger.class, Integer.valueOf(Types.TYPE_BIG_INTEGER));
        typeeval.put(Float.TYPE, Integer.valueOf(Types.TYPE_PRIMFLOAT));
        typeeval.put(Float.class, Integer.valueOf(Types.TYPE_FLOAT));
        typeeval.put(Double.TYPE, Integer.valueOf(Types.TYPE_PRIMDOUBLE));
        typeeval.put(Double.class, Integer.valueOf(Types.TYPE_DOUBLE));
        typeeval.put(BigDecimal.class, Integer.valueOf(Types.TYPE_BIG_DECIMAL));
        typeeval.put(Number.class, Integer.valueOf(Types.TYPE_NUMBER));
        typeeval.put(Interpreter.class, Integer.valueOf(Types.TYPE_INTERPRETER));
    }

    /**
     * <p>Encode the signature of a foreign method.</p>
     *
     * @param en The engine.
     * @return True if the signature is ok, otherwise false.
     * @throws EngineMessage FFI error.
     */
    public boolean encodeSignatureEval(Engine en)
            throws EngineMessage {
        if (!Modifier.isPublic(getModifiers())) {
            en.skel = EngineMessage.domainError(
                    AbstractFactory.OP_DOMAIN_FOREIGN_VISIBILITY,
                    new SkelAtom(Modifier.toString(getModifiers())));
            return false;
        }

        if (!Modifier.isStatic(getModifiers())) {
            subflags |= AbstractDelegate.MASK_DELE_VIRT;
            Class ret = getDeclaringClass();
            Integer encode = typeeval.get(ret);
            if (encode == null ||
                    encode.intValue() == Types.TYPE_INTERPRETER) {
                en.skel = EngineMessage.domainError(
                        AbstractFactory.OP_DOMAIN_FOREIGN_RECEIVER,
                        SpecialSpecial.classToName(ret));
                return false;
            } else {
                encodeobj = encode.intValue();
            }
        }

        Class[] paras = getParameterTypes();
        encodeparas = (paras.length != 0 ? new int[paras.length] :
                VOID_PARAS);
        for (int i = 0; i < paras.length; i++) {
            Class ret = paras[i];
            Integer encode = typeeval.get(ret);
            if (encode == null) {
                en.skel = EngineMessage.domainError(
                        AbstractFactory.OP_DOMAIN_FOREIGN_PARAMETER,
                        SpecialSpecial.classToName(ret));
                return false;
            } else {
                encodeparas[i] = encode.intValue();
            }
        }

        Class ret = getReturnType();
        Integer encode = typeeval.get(ret);
        if (encode == null ||
                encode.intValue() == Types.TYPE_INTERPRETER) {
            en.skel = EngineMessage.domainError(
                    AbstractFactory.OP_DOMAIN_FOREIGN_RETURN,
                    SpecialSpecial.classToName(ret));
            return false;
        } else {
            encoderet = encode.intValue();
        }

        return true;
    }

    /******************************************************************/
    /* Predicate Types                                                */
    /******************************************************************/

    private final static HashMap<Class, Integer> typepred = new HashMap<Class, Integer>();

    static {
        typepred.put(Void.TYPE, Integer.valueOf(Types.TYPE_VOID));
        typepred.put(String.class, Integer.valueOf(Types.TYPE_STRING));
        typepred.put(CharSequence.class, Integer.valueOf(Types.TYPE_CHARSEQ));
        typepred.put(Boolean.TYPE, Integer.valueOf(Types.TYPE_PRIMBOOL));
        typepred.put(Boolean.class, Integer.valueOf(Types.TYPE_BOOL));
        typepred.put(Byte.TYPE, Integer.valueOf(Types.TYPE_PRIMBYTE));
        typepred.put(Byte.class, Integer.valueOf(Types.TYPE_BYTE));
        typepred.put(Character.TYPE, Integer.valueOf(Types.TYPE_PRIMCHAR));
        typepred.put(Character.class, Integer.valueOf(Types.TYPE_CHAR));
        typepred.put(Short.TYPE, Integer.valueOf(Types.TYPE_PRIMSHORT));
        typepred.put(Short.class, Integer.valueOf(Types.TYPE_SHORT));
        typepred.put(Integer.TYPE, Integer.valueOf(Types.TYPE_PRIMINT));
        typepred.put(Integer.class, Integer.valueOf(Types.TYPE_INTEGER));
        typepred.put(Long.TYPE, Integer.valueOf(Types.TYPE_PRIMLONG));
        typepred.put(Long.class, Integer.valueOf(Types.TYPE_LONG));
        typepred.put(BigInteger.class, Integer.valueOf(Types.TYPE_BIG_INTEGER));
        typepred.put(Float.TYPE, Integer.valueOf(Types.TYPE_PRIMFLOAT));
        typepred.put(Float.class, Integer.valueOf(Types.TYPE_FLOAT));
        typepred.put(Double.TYPE, Integer.valueOf(Types.TYPE_PRIMDOUBLE));
        typepred.put(Double.class, Integer.valueOf(Types.TYPE_DOUBLE));
        typepred.put(BigDecimal.class, Integer.valueOf(Types.TYPE_BIG_DECIMAL));
        typepred.put(Number.class, Integer.valueOf(Types.TYPE_NUMBER));
        typepred.put(Object.class, Integer.valueOf(Types.TYPE_OBJECT));
        typepred.put(AbstractTerm.class, Integer.valueOf(Types.TYPE_TERM));
        typepred.put(Interpreter.class, Integer.valueOf(Types.TYPE_INTERPRETER));
        typepred.put(CallOut.class, Integer.valueOf(Types.TYPE_CALLOUT));
    }

    /**
     * <p>Encode the signature of a foreign method.</p>
     * <p>The culprit is returned in the engine skel.</p>
     *
     * @param en The engine.
     * @return True if the signature is ok, otherwise false.
     * @throws EngineMessage FFI error.
     */
    public boolean encodeSignaturePred(Engine en)
            throws EngineMessage {
        if (!Modifier.isPublic(getModifiers())) {
            en.skel = EngineMessage.domainError(
                    AbstractFactory.OP_DOMAIN_FOREIGN_VISIBILITY,
                    new SkelAtom(Modifier.toString(getModifiers())));
            return false;
        }

        if (!Modifier.isStatic(getModifiers())) {
            subflags |= AbstractDelegate.MASK_DELE_VIRT;
            Class ret = getDeclaringClass();
            Integer encode = typepred.get(ret);
            if (encode == null) {
                encodeobj = Types.TYPE_REF;
            } else if (encode.intValue() == Types.TYPE_INTERPRETER ||
                    encode.intValue() == Types.TYPE_CALLOUT ||
                    encode.intValue() == Types.TYPE_UNSUPPORTED) {
                en.skel = EngineMessage.domainError(
                        AbstractFactory.OP_DOMAIN_FOREIGN_RECEIVER,
                        SpecialSpecial.classToName(ret));
                return false;
            } else {
                encodeobj = encode.intValue();
            }
        }

        Class[] paras = getParameterTypes();
        encodeparas = (paras.length != 0 ? new int[paras.length] : VOID_PARAS);
        for (int i = 0; i < paras.length; i++) {
            Class ret = paras[i];
            Integer encode = typepred.get(ret);
            if (encode == null) {
                encodeparas[i] = Types.TYPE_REF;
            } else if (encode.intValue() == Types.TYPE_VOID ||
                    encode.intValue() == Types.TYPE_UNSUPPORTED) {
                en.skel = EngineMessage.domainError(
                        AbstractFactory.OP_DOMAIN_FOREIGN_PARAMETER,
                        SpecialSpecial.classToName(ret));
                return false;
            } else {
                encodeparas[i] = encode.intValue();
            }
        }

        Class ret = getReturnType();
        Integer encode = typepred.get(ret);
        if (encode == null) {
            encoderet = Types.TYPE_REF;
        } else if (encode.intValue() == Types.TYPE_INTERPRETER ||
                encode.intValue() == Types.TYPE_CALLOUT ||
                encode.intValue() == Types.TYPE_UNSUPPORTED) {
            en.skel = EngineMessage.domainError(
                    AbstractFactory.OP_DOMAIN_FOREIGN_RETURN,
                    SpecialSpecial.classToName(ret));
            return false;
        } else {
            encoderet = encode.intValue();
        }

        if (getRetFlag())
            subflags |= MASK_METH_FUNC;

        return true;
    }

    /**
     * <p>Compute the declared function status.</p>
     *
     * @return The declared fucntion status.
     */
    private boolean getRetFlag() {
        switch (encoderet) {
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
                return true;
            default:
                throw new IllegalArgumentException("illegal return type");
        }
    }

    /******************************************************************/
    /* Life-Cycle Services                                            */
    /******************************************************************/

    /**
     * <p>Shrink this predicate from the store for a source.</p>
     *
     * @param pick  The predicate.
     * @param scope The source.
     */
    public final void shrinkPredicate(Predicate pick, AbstractSource scope) {
        /* do noting */
    }

    /**
     * <p>Release this predicate from the store.</p>
     *
     * @param pick The predicate.
     */
    public final void releasePredicate(Predicate pick) {
        /* do noting */
    }

    /******************************************************************/
    /* Accessible Protocol                                            */
    /******************************************************************/

    /**
     * <p>Retrieve the modifier flags as an int.</p>
     *
     * @return The modifier flags as an int.
     */
    public abstract int getModifiers();

    /**
     * <p>Retrieve the return type as Java class.</p>
     *
     * @return The return type as Java class.
     */
    public abstract Class getReturnType();

    /**
     * <p>Retrieve the parameter types as Java classes.</p>
     *
     * @return The parameter types as Java classes.
     */
    public abstract Class[] getParameterTypes();

    /**
     * <p>Retrieve the declaring Java class.</p>
     *
     * @return The declaring Java class.
     */
    public abstract Class getDeclaringClass();

    /***************************************************************/
    /* Foreign Predicates                                          */
    /***************************************************************/

    /**
     * <p>Retrieve an length guess.</p>
     *
     * @return The length guess, or -1.
     */
    public int getArity() {
        int count = getParaCount();
        if ((subflags & AbstractDelegate.MASK_DELE_VIRT) != 0)
            count++;
        if ((subflags & MASK_METH_FUNC) != 0)
            count++;
        return count;
    }

    /**
     * <p>Compute the length.</p>
     *
     * @return The length.
     */
    private int getParaCount() {
        int k = 0;
        for (int i = 0; i < encodeparas.length; i++) {
            switch (encodeparas[i]) {
                case Types.TYPE_STRING:
                case Types.TYPE_CHARSEQ:
                case Types.TYPE_PRIMBOOL:
                case Types.TYPE_BOOL:
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
                    k++;
                    break;
                case Types.TYPE_INTERPRETER:
                case Types.TYPE_CALLOUT:
                    /* */
                    break;
                default:
                    throw new IllegalArgumentException("illegal parameter type");
            }
        }
        return k;
    }

    /***********************************************************/
    /* Foreign Invokation                                      */
    /***********************************************************/

    /**
     * <p>Determine the receiver object. The first argument of
     * the goal is checked and converted if necessary.</p>
     *
     * @param temp The skeleton.
     * @param ref  The display.
     * @param en   The engine.
     * @return The arguments array.
     * @throws EngineMessage FFI error.
     */
    public final Object convertObj(Object temp, Display ref, Engine en)
            throws EngineMessage {
        try {
            if ((subflags & AbstractDelegate.MASK_DELE_VIRT) != 0) {
                en.skel = ((SkelCompound) temp).args[0];
                en.display = ref;
                en.deref();
                Object res = AbstractTerm.createTerm(en.skel, en.display);
                return Types.denormProlog(encodeobj, res);
            } else {
                return null;
            }
        } catch (InterpreterMessage x) {
            throw (EngineMessage) x.getException();
        }
    }

    /**
     * <p>Build an argument.  The arguments of the goal
     * is checked and converted if necessary.</p>
     *
     * @param temp The argument skeleton.
     * @param ref  The argument display.
     * @param typ  The type.
     * @return The argument.
     * @throws EngineMessage FFI error.
     */
    public static Object convertArg(Object temp, Display ref, int typ)
            throws EngineMessage {
        try {
            BindVar b;
            while (temp instanceof SkelVar &&
                    (b = ref.bind[((SkelVar) temp).id]).display != null) {
                temp = b.skel;
                ref = b.display;
            }
            Object res;
            if (typ == Types.TYPE_TERM) {
                res = AbstractTerm.createTermWrapped(temp, ref);
            } else {
                res = AbstractTerm.createTerm(temp, ref);
            }
            return Types.denormProlog(typ, res);
        } catch (InterpreterMessage x) {
            throw (EngineMessage) x.getException();
        }
    }

}
