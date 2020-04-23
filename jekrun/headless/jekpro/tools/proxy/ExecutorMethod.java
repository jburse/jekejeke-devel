package jekpro.tools.proxy;

import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.tools.array.Types;
import jekpro.tools.call.CallIn;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.*;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

/**
 * <p>Java code for the instantiation example. This class
 * provides an executor for Prolog predicates that are
 * called as result of invoking a proxy class invocation
 * handler.
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
final class ExecutorMethod extends AbstractExecutor {

    /**
     * <p>Create method predicate.</p>
     *
     * @param m The method.
     */
    ExecutorMethod(Method m) {
        super(m);
    }

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
    public boolean encodeSignature() {
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
        encodeparas = (paras.length != 0 ? new int[paras.length] : ExecutorMethod.VOID_PARAS);
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
    /* Predicate Execution                                     */
    /***********************************************************/

    /**
     * <p>Run the predicate.</p>
     *
     * @param proxy The proxy class instance.
     * @param args  The arguments.
     * @param inter The interpreter.
     * @return The result, can be null.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    Object runGoal(Object proxy, Object[] args, Interpreter inter)
            throws InterpreterMessage, InterpreterException {
        try {
            Object[] termargs = uncompileArgs(proxy, args);
            Object help;
            if ((subflags & AbstractExecutor.MASK_METH_FUNC) != 0) {
                help = new TermVar();
                termargs[termargs.length - 1] = help;
            } else {
                help = null;
            }
            AbstractTerm goal;
            if (termargs.length != 0) {
                goal = new TermCompound(inter, functor, termargs);
            } else {
                goal = functor;
            }

            CallIn callin = inter.iterator(goal);
            if (callin.hasNext()) {
                callin.next();
                help = (help != null ? AbstractTerm.copyMolec(inter, help) : null);
                callin.close();
                help = (help != null ? Types.denormProlog(encoderet, AbstractTerm.getSkel(help),
                        AbstractTerm.getDisplay(help)) : noretDenormProlog(true));
            } else {
                help = (help != null ? null : noretDenormProlog(false));
            }

            return help;
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
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
