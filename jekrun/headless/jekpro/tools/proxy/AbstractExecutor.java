package jekpro.tools.proxy;

import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.array.Types;
import jekpro.tools.call.CallIn;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.*;

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
abstract class AbstractExecutor {
    public final static int MASK_METH_VIRT = 0x00000001;
    public final static int MASK_METH_FUNC = 0x00000002;

    final static int[] VOID_PARAS = new int[0];
    final static Object[] VOID_PROLOG_ARGS = new Object[0];

    int subflags;

    int[] encodeparas;
    int encoderet;

    final Method method;
    TermAtomic functor;

    /**
     * <p>Create an abstract executor.</p>
     *
     * @param m The method.
     */
    AbstractExecutor(Method m) {
        method = m;
    }

    /***********************************************************/
    /* Variation Point                                         */
    /***********************************************************/

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
    abstract Object runGoal(Object proxy, Object[] args, Interpreter inter)
            throws InterpreterMessage, InterpreterException, Throwable;

    /***********************************************************/
    /* Functor & Parameter Compilation                        */
    /***********************************************************/

    /**
     * <p>Set the source.</p>
     *
     * @param src The source.
     */
    void setSource(AbstractSource src) {
        SkelAtom val = new SkelAtom(method.getName(), src);
        functor = (TermAtomic) AbstractTerm.createTermWrapped(val, Display.DISPLAY_CONST);
    }

    /**
     * <p>Encode the signature of a foreign method.</p>
     * <p>The culprit is returned in the engine skel.</p>
     *
     * @return True if the signature is ok, otherwise false.
     */
    boolean encodeSignature() {
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
    protected Object[] uncompileArgs(Object proxy, Object[] args)
            throws EngineMessage {
        int len = encodeparas.length;
        if ((subflags & ExecutorInterface.MASK_METH_VIRT) != 0)
            len++;
        if ((subflags & ExecutorInterface.MASK_METH_FUNC) != 0)
            len++;
        Object[] termargs = (len != 0 ?
                new Object[len] : ExecutorInterface.VOID_PROLOG_ARGS);
        int k = 0;
        if ((subflags & ExecutorInterface.MASK_METH_VIRT) != 0) {
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
    protected Object noretDenormProlog(boolean f) {
        return (encoderet == Types.TYPE_VOID ? null : Boolean.valueOf(f));
    }

    /***********************************************************/
    /* Make & Execute Goal                           */
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
    protected Object makeGoal(Object proxy, Object[] args, Interpreter inter)
            throws InterpreterMessage {
        Object[] termargs;
        try {
            termargs = uncompileArgs(proxy, args);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
        if ((subflags & MASK_METH_FUNC) != 0)
            termargs[termargs.length - 1] = new TermVar();
        Object goal;
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
    protected Object executeGoal(Object goal, Interpreter inter)
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

}