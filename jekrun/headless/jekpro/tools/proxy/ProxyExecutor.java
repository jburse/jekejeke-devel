package jekpro.tools.proxy;

import jekpro.model.inter.Engine;
import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.array.Types;
import jekpro.tools.call.CallIn;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.*;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

/**
 * <p>This class provides the base class for executors of
 * Prolog predicates that are called as result of invoking
 * a proxy class invocation handler.
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
public final class ProxyExecutor {
    public final static int MASK_METH_VIRT = 0x00000001;
    public final static int MASK_METH_FUNC = 0x00000002;

    final static int[] VOID_PARAS = new int[0];
    final static Object[] VOID_PROLOG_ARGS = new Object[0];

    private int subflags;

    private int[] encodeparas;
    private int encoderet;

    private TermAtomic functor;
    private MethodHandle special;

    public static Field impl_lookup;

    static {
        try {
            Class<?> clazz = Class.forName("java.lang.invoke.MethodHandles$Lookup");
            impl_lookup = clazz.getDeclaredField("IMPL_LOOKUP");
            impl_lookup.setAccessible(true);
        } catch (ClassNotFoundException e) {
            impl_lookup = null;
        } catch (NoSuchFieldException e) {
            impl_lookup = null;
        }
    }

    /***********************************************************/
    /* Proxy Executor API                                      */
    /***********************************************************/

    /**
     * <p>Encode the signature of a foreign method.</p>
     * <p>The culprit is returned in the engine skel.</p>
     *
     * @param method The method.
     * @return True if the signature is ok, otherwise false.
     */
    boolean encodeSignature(Method method) {
        Class ret = method.getReturnType();
        Integer encode = Types.typepred.get(ret);
        if (encode == null) {
            encoderet = Types.TYPE_REF;
        } else if (encode.intValue() == Types.TYPE_INTERPRETER ||
                encode.intValue() == Types.TYPE_CALLOUT) {
            return false;
        } else {
            encoderet = encode.intValue();
        }

        Class[] paras = method.getParameterTypes();
        encodeparas = (paras.length != 0 ? new int[paras.length] : VOID_PARAS);
        for (int i = 0; i < paras.length; i++) {
            ret = paras[i];
            encode = Types.typepred.get(ret);
            if (encode == null) {
                encodeparas[i] = Types.TYPE_REF;
            } else if (encode.intValue() == Types.TYPE_VOID ||
                    encode.intValue() == Types.TYPE_INTERPRETER ||
                    encode.intValue() == Types.TYPE_CALLOUT) {
                return false;
            } else {
                encodeparas[i] = encode.intValue();
            }
        }

        if (!Modifier.isStatic(method.getModifiers()))
            subflags |= MASK_METH_VIRT;
        if (Types.getRetFlag(encoderet))
            subflags |= MASK_METH_FUNC;
        return true;
    }

    /**
     * <p>Set the handler.</p>
     *
     * @param method  The method.
     * @param handler The handler.
     * @throws EngineMessage Shit happens.
     */
    void setHandler(Method method, ProxyHandler handler)
            throws EngineMessage {
        SkelAtom val = new SkelAtom(method.getName(), handler.getSource());
        functor = new TermAtomic(val);
        if ((method.getModifiers() & Modifier.ABSTRACT) == 0) {
            try {
                MethodHandles.Lookup lookup = (MethodHandles.Lookup) impl_lookup.get(null);
                special = lookup.unreflectSpecial(method, method.getDeclaringClass());
            } catch (Exception x) {
                throw Types.mapException(x, method);
            } catch (Error x) {
                throw Types.mapError(x);
            }
        } else {
            special = null;
        }
    }

    /**
     * <p>Run the predicate.</p>
     *
     * @param proxy The proxy class instance.
     * @param args  The arguments.
     * @param inter The interpreter.
     * @return The result, can be null.
     * @throws InterpreterMessage   FFI error.
     * @throws InterpreterException FFI error.
     * @throws Throwable            FFI error.
     */
    Object runGoal(Object proxy, Object[] args, Interpreter inter)
            throws InterpreterMessage, InterpreterException, Throwable {
        if (!currentProvable(inter)) {
            if (special != null) {
                return special.bindTo(proxy).invokeWithArguments(args);
            } else {
                throw new InterpreterMessage(existenceProvable(inter));
            }
        } else {
            Object goal = makeGoal(proxy, args, inter);
            return executeGoal(goal, inter);
        }
    }

    /***********************************************************/
    /* Current & Error Functor                                 */
    /***********************************************************/

    /**
     * <p>Check whether the predicate is defined.</p>
     *
     * @param inter The interpreter.
     * @return True if the predicate is defined, otherwise false.
     * @throws InterpreterException Shit happens.
     * @throws InterpreterMessage   Shit happens.
     */
    private boolean currentProvable(Interpreter inter)
            throws InterpreterException, InterpreterMessage {
        int len = encodeparas.length;
        if ((subflags & ProxyExecutor.MASK_METH_VIRT) != 0)
            len++;
        if ((subflags & ProxyExecutor.MASK_METH_FUNC) != 0)
            len++;
        SkelAtom sa = (SkelAtom) functor.getSkel();
        Engine en = inter.getEngine();
        CachePredicate cp;
        try {
            cp = CachePredicate.getPredicateDefined(sa, len, en, 0);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
        if (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0)
            return false;
        return true;
    }

    /**
     * <p>Create a realization missing error message.</p>
     *
     * @param inter The interpreter.
     * @return The error message.
     */
    private EngineMessage existenceProvable(Interpreter inter) {
        int len = encodeparas.length;
        if ((subflags & ProxyExecutor.MASK_METH_VIRT) != 0)
            len++;
        if ((subflags & ProxyExecutor.MASK_METH_FUNC) != 0)
            len++;
        SkelAtom sa = (SkelAtom) functor.getSkel();
        Engine en = inter.getEngine();
        return new EngineMessage(EngineMessage.existenceError(
                EngineMessage.OP_EXISTENCE_PROCEDURE,
                new SkelCompound(en.store.foyer.ATOM_SLASH,
                        sa,
                        Integer.valueOf(len))));
    }

    /***********************************************************/
    /* Make & Execute Goal                                     */
    /***********************************************************/

    /**
     * <p>Create the proxy call.</p>
     *
     * @param proxy The proxy instance.
     * @param args  The arguments.
     * @param inter The interpreter.
     * @return The call.
     * @throws InterpreterMessage Shit happens.
     */
    private Object makeGoal(Object proxy, Object[] args, Interpreter inter)
            throws InterpreterMessage {
        Object[] termargs;
        try {
            termargs = uncompileArgs(proxy, args);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
        if ((subflags & MASK_METH_FUNC) != 0)
            termargs[termargs.length - 1] = new TermVar();
        if (termargs.length != 0) {
            return new TermCompound(inter, functor, termargs);
        } else {
            return functor;
        }
    }

    /**
     * <p>Execute a proxy call.</p>
     *
     * @param goal  The proxy call.
     * @param inter The interpreter.
     * @return The result.
     * @throws InterpreterException Shit happens.
     * @throws InterpreterMessage   Shit happens.
     */
    private Object executeGoal(Object goal, Interpreter inter)
            throws InterpreterException, InterpreterMessage {
        CallIn callin = inter.iterator(goal);
        if (callin.hasNext()) {
            callin.next();
            if ((subflags & MASK_METH_FUNC) != 0) {
                TermCompound tc = (TermCompound) goal;
                goal = AbstractTerm.copyMolec(inter, tc.getArgMolec(tc.getArity() - 1));
                callin.close();
                try {
                    return Types.denormProlog(encoderet, AbstractTerm.getSkel(goal),
                            AbstractTerm.getDisplay(goal));
                } catch (EngineMessage x) {
                    throw new InterpreterMessage(x);
                }
            } else {
                callin.close();
                return noretDenormProlog(true);
            }
        } else {
            if ((subflags & MASK_METH_FUNC) != 0) {
                return null;
            } else {
                return noretDenormProlog(false);
            }
        }
    }

    /***********************************************************/
    /* Parameter & Result Conversion                           */
    /***********************************************************/

    /**
     * <p>Build the arguments.</p>
     *
     * @param args The Java arguments.
     * @return The Prolog arguments.
     * @throws EngineMessage Shit happens.
     */
    private Object[] uncompileArgs(Object proxy, Object[] args)
            throws EngineMessage {
        int len = encodeparas.length;
        if ((subflags & ProxyExecutor.MASK_METH_VIRT) != 0)
            len++;
        if ((subflags & ProxyExecutor.MASK_METH_FUNC) != 0)
            len++;
        Object[] termargs = (len != 0 ?
                new Object[len] : ProxyExecutor.VOID_PROLOG_ARGS);
        int k = 0;
        if ((subflags & ProxyExecutor.MASK_METH_VIRT) != 0) {
            termargs[k] = proxy;
            k++;
        }
        for (int i = 0; i < encodeparas.length; i++) {
            Object res = Types.normJava(encodeparas[i], args[i]);
            if (res == null)
                throw new EngineMessage(EngineMessage.representationError(
                        AbstractFactory.OP_REPRESENTATION_NULL));
            termargs[k] = res;
            k++;
        }
        return termargs;
    }

    /**
     * <p>Generate a Java return value, where Prolog doesn't provide one.</p>
     *
     * @param f The desired value.
     * @return The Java return value.
     */
    private Object noretDenormProlog(boolean f) {
        return (encoderet == Types.TYPE_VOID ? null : Boolean.valueOf(f));
    }

}