package jekpro.tools.foreign;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.reference.reflect.SpecialForeign;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.array.AbstractLense;
import jekpro.tools.array.Types;
import jekpro.tools.call.CallOut;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.lang.reflect.Method;

/**
 * <p>Base class for the Java class delegates.</p>
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
public abstract class AbstractMember extends AbstractLense
        implements Comparable<AbstractMember> {
    public final static Object[] VOID_ARGS = new Object[0];

    int scores = -1;

    private static Constructor constr;

    static {
        try {
            Class<?> clazz = Class.forName("java.lang.invoke.MethodHandles$Lookup");
            constr = SpecialForeign.getDeclaredConstructor(clazz, Class.class, Integer.TYPE);
            constr.setAccessible(true);
        } catch (ClassNotFoundException e) {
            constr = null;
        } catch (NoClassDefFoundError x) {
            constr = null;
        } catch (EngineMessage e) {
            constr = null;
        }
    }

    /**
     * <p>Retrieve the proxy that is wrapped.</p>
     *
     * @return The proxy.
     */
    public abstract Member getProxy();

    /**
     * <p>Retrieve the variant that is wrapped.</p>
     *
     * @return The variant.
     */
    public abstract String getVariant();

    /**
     * <p>Retrieve the declaring Java class.</p>
     *
     * @return The declaring Java class.
     */
    public final Class getDeclaringClass() {
        return getProxy().getDeclaringClass();
    }

    /*******************************************************************/
    /* Auto Loader Heuristics                                          */
    /*******************************************************************/

    /**
     * <p>Compare this delegate to another delegate.</p>
     *
     * @param o The other delegate.
     * @return The result.
     */
    public int compareTo(AbstractMember o) {
        int score1 = getScores();
        int score2 = o.getScores();
        return (score1 < score2) ? -1 : ((score1 == score2) ? 0 : 1);
    }

    /**
     * <p>Retrieve the scores.</p>
     *
     * @return The scores.
     */
    private int getScores() {
        int s = scores;
        if (s != -1)
            return s;
        Class[] paras = getParameterTypes();
        s = 0;
        if ((subflags & AbstractDelegate.MASK_DELE_VIRT) != 0)
            s += Score.getScore(encodeobj, getDeclaringClass());
        for (int i = 0; i < encodeparas.length; i++) {
            int typ = encodeparas[i];
            if (typ == Types.TYPE_INTERPRETER) {
                /* do nothing */
            } else if (typ == Types.TYPE_CALLOUT) {
                /* do nothing */
            } else {
                s += Score.getScore(typ, paras[i]);
            }
        }
        scores = s;
        return s;
    }

    /**
     * <p>Retrieve the test closures.</p>
     *
     * @return The test closures.
     */
    public Object[] getTests(AbstractSource scope) {
        Class[] paras = getParameterTypes();
        int n = getArity();
        Object[] t = (n != 0 ? new Object[n] : VOID_ARGS);
        int k = 0;
        if ((subflags & AbstractDelegate.MASK_DELE_VIRT) != 0) {
            t[k] = Score.getTest(encodeobj, getDeclaringClass(), scope);
            k++;
        }
        for (int i = 0; i < encodeparas.length; i++) {
            int typ = encodeparas[i];
            if (typ == Types.TYPE_INTERPRETER) {
                /* do nothing */
            } else if (typ == Types.TYPE_CALLOUT) {
                /* do nothing */
            } else {
                t[k] = Score.getTest(typ, paras[i], scope);
                k++;
            }
        }
        return t;
    }

    /***********************************************************/
    /* Foreign Invokation                                      */
    /***********************************************************/

    /**
     * <p>Convert the receiver, if any.</p>
     *
     * @param temp The arguments skeleton.
     * @param ref  The arguments display.
     * @return The receiver, or null.
     * @throws EngineMessage FFI error.
     */
    final Object convertRecv(Object temp, Display ref)
            throws EngineMessage {
        if ((subflags & AbstractDelegate.MASK_DELE_VIRT) != 0) {
            return Types.denormProlog(encodeobj, ((SkelCompound) temp).args[0], ref);
        } else {
            return null;
        }
    }

    /**
     * <p>Build the arguments array. The arguments of the term
     * are checked and converted if necessary.</p>
     *
     * @param temp The arguments skeleton.
     * @param ref  The arguments display.
     * @param en   The engine.
     * @param co   The call-out.
     * @return The arguments array.
     * @throws EngineMessage FFI error.
     */
    final Object[] convertArgs(Object temp, Display ref,
                               Engine en, CallOut co)
            throws EngineMessage {
        if (encodeparas.length == 0)
            return AbstractMember.VOID_ARGS;
        Object[] args = new Object[encodeparas.length];
        int k = 0;
        if ((subflags & AbstractDelegate.MASK_DELE_VIRT) != 0)
            k++;
        for (int i = 0; i < encodeparas.length; i++) {
            int typ = encodeparas[i];
            if (typ == Types.TYPE_INTERPRETER) {
                args[i] = en.proxy;
            } else if (typ == Types.TYPE_CALLOUT) {
                args[i] = co;
            } else {
                args[i] = Types.denormProlog(typ, ((SkelCompound) temp).args[k], ref);
                k++;
            }
        }
        return args;
    }

    /**
     * <p>Build the arguments array. The arguments of the term
     * are computed, checked and converted if necessary.</p>
     *
     * @param temp The skeleton.
     * @param ref  The display.
     * @param en   The engine.
     * @return The arguments array.
     * @throws EngineMessage   FFI error.
     * @throws EngineException FFI error.
     */
    final Object[] computeAndConvertArgs(Object temp, Display ref,
                                         Engine en)
            throws EngineMessage, EngineException {
        if (encodeparas.length == 0)
            return AbstractMember.VOID_ARGS;
        Object[] args = new Object[encodeparas.length];
        int k = 0;
        if ((subflags & AbstractDelegate.MASK_DELE_VIRT) != 0)
            k++;
        for (int i = 0; i < encodeparas.length; i++) {
            int typ = encodeparas[i];
            if (typ == Types.TYPE_INTERPRETER) {
                args[i] = en.proxy;
            } else {
                en.computeExpr(((SkelCompound) temp).args[k], ref);
                args[i] = Types.denormProlog(typ, en.skel, en.display);
                k++;
            }
        }
        return args;
    }

    /***********************************************************/
    /* CheerpJ Workaround IllegalArgumentException             */
    /***********************************************************/

    /**
     * <p>Check the receiver.</p>
     *
     * @param val The receiver.
     * @throws EngineMessage FFI error.
     */
    final void checkRecv(Object val) throws EngineMessage {
        if ((subflags & AbstractDelegate.MASK_DELE_VIRT) != 0) {
            int typ = encodeobj;
            if (typ == Types.TYPE_CHARSEQ || typ == Types.TYPE_REF) {
                Class clazz = getDeclaringClass();
                if (!clazz.isAssignableFrom(val.getClass())) {
                    Member y = getProxy();
                    throw new EngineMessage(EngineMessage.permissionError(
                            AbstractFactory.OP_PERMISSION_APPLY,
                            Types.mapMemberType(y),
                            Types.mapMemberCulprit(y)));
                }
            } else {
                /* */
            }
        } else {
            /* */
        }
    }

    /**
     * <p>Check the arguments.</p>
     *
     * @param args The arguments.
     * @throws EngineMessage FFI error.
     */
    final void checkArgs(Object[] args)
            throws EngineMessage {
        for (int i = 0; i < encodeparas.length; i++) {
            int typ = encodeparas[i];
            if (typ == Types.TYPE_CHARSEQ || typ == Types.TYPE_REF) {
                Object val = args[i];
                Class clazz = getParameterTypes()[i];
                if (!clazz.isAssignableFrom(val.getClass())) {
                    Member y = getProxy();
                    throw new EngineMessage(EngineMessage.permissionError(
                            AbstractFactory.OP_PERMISSION_APPLY,
                            Types.mapMemberType(y),
                            Types.mapMemberCulprit(y)));
                }
            } else {
                /* */
            }
        }
    }

    /***************************************************************/
    /* Object Protocol                                             */
    /***************************************************************/

    /**
     * <p>Compute a hash code.</p>
     *
     * @return The hash code of this delegate.
     */
    public final int hashCode() {
        return 31 * getProxy().hashCode() + getVariant().hashCode();
    }

    /**
     * <p>Compare with another delegate.</p>
     *
     * @param o The other delegate.
     * @return True if this delegate equals the other delegate, otherwise false.
     */
    public final boolean equals(Object o) {
        if (!(o instanceof AbstractMember))
            return false;
        AbstractMember am = (AbstractMember) o;
        return getProxy().equals(am.getProxy()) &&
                getVariant().equals(am.getVariant());
    }


    /**
     * <p>Generate the spec of this delegate.</p>
     *
     * @param source The source, non null.
     * @return The spec.
     * @throws EngineMessage FFI error.
     */
    public final Object toSpec(AbstractSource source)
            throws EngineMessage {
        return new SkelCompound(new SkelAtom(getVariant()),
                SpecialForeign.classToName(getDeclaringClass(), source),
                mapMemberCulprit(getProxy(), source));
    }

    /**
     * <p>Map a Java member to a Prolog culprit.</p>
     *
     * @param y      The Java member.
     * @param source The source, non null.
     * @return The Prolog culprit.
     * @throws EngineMessage Shit happens.
     */
    public static Object mapMemberCulprit(Member y, AbstractSource source)
            throws EngineMessage {
        if (y instanceof Field) {
            Field field = (Field) y;
            return new SkelAtom(field.getName());
        } else if (y instanceof Method) {
            Method method = (Method) y;
            return SpecialForeign.methodToCallable(method.getName(),
                    method.getParameterTypes(), source);
        } else if (y instanceof Constructor) {
            Constructor constructor = (Constructor) y;
            return SpecialForeign.constructorToCallable(
                    constructor.getParameterTypes(), source);
        } else {
            throw new IllegalArgumentException("illegal member");
        }
    }

    /***************************************************************/
    /* Unreflect Special                                           */
    /***************************************************************/

    /**
     * <p>Encode the special of a foreign method.</p>
     * <p>The culprit is returned in the engine skel.</p>
     *
     * @param en The engine.
     * @return True if the signature is ok, otherwise false.
     */
    boolean encodeSpecial(Engine en) {
        return true;
    }

    /**
     * <p>Encode the special of a foreign method.</p>
     * <p>The culprit is returned in the engine skel.</p>
     *
     * @param method The method.
     * @param en     The engine.
     * @return The method handle or null.
     */
    public static MethodHandle encodeSpecial(Method method, Engine en) {
        MethodHandles.Lookup lookup;
        try {
            lookup = (MethodHandles.Lookup) AutoClass.invokeNew(constr,
                    method.getDeclaringClass(), MethodHandles.Lookup.PRIVATE);
        } catch (EngineException x) {
            en.skel = x;
            return null;
        } catch (EngineMessage x) {
            en.skel = x;
            return null;
        }
        try {
            return lookup.unreflectSpecial(method, method.getDeclaringClass());
        } catch (Exception x) {
            en.skel = Types.mapException(x, method);
            return null;
        } catch (Error x) {
            en.skel = Types.mapError(x);
            return null;
        }
    }

}
