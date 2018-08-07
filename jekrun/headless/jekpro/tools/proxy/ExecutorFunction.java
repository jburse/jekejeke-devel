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
 * <p>Java code for the instantiation example. This class
 * provides an executor for Prolog evaluable functions that
 * are called as result of invoking a proxy class invocation
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
final class ExecutorFunction extends AbstractExecutor {
    private TermAtomic is;

    /**
     * <p>Create method evaluable function.</p>
     *
     * @param m The method.
     */
    ExecutorFunction(Method m) {
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
        val = new SkelAtom("is");
        is = (TermAtomic) AbstractTerm.createTermWrapped(val, Display.DISPLAY_CONST);
    }

    /**
     * <p>Encode the signature of a foreign method.</p>
     * <p>The culprit is returned in the engine skel.</p>
     *
     * @return True if the signature is ok, otherwise false.
     */
    public boolean encodeSignature() {
        Class ret = method.getReturnType();
        Integer encode = Types.typeeval.get(ret);
        if (encode == null || encode.intValue() == Types.TYPE_INTERPRETER) {
            return false;
        } else {
            encoderet = encode.intValue();
        }

        Class[] paras = method.getParameterTypes();
        encodeparas = (paras.length != 0 ? new int[paras.length] :
                AbstractExecutor.VOID_PARAS);
        for (int i = 0; i < paras.length; i++) {
            ret = paras[i];
            encode = Types.typeeval.get(ret);
            if (encode == null || encode.intValue() == Types.TYPE_INTERPRETER) {
                return false;
            } else {
                encodeparas[i] = encode.intValue();
            }
        }

        if (!Modifier.isStatic(method.getModifiers()))
            subflags |= MASK_METH_VIRT;
        return true;
    }

    /***********************************************************/
    /* Evaluable Function Execution                            */
    /***********************************************************/

    /**
     * <p>Run the evaluable function.</p>
     *
     * @param proxy The proxy class instance.
     * @param args  The arguments.
     * @param inter The interpreter.
     * @return The result.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    Object runGoal(Object proxy, Object[] args, Interpreter inter)
            throws InterpreterMessage, InterpreterException {
        try {
            Object[] termargs = uncompileArgs(proxy, args);
            Object eval;
            if (termargs.length != 0) {
                eval = new TermCompound(functor, termargs);
            } else {
                eval = functor;
            }
            Object help = new TermVar();
            Object goal = new TermCompound(is, help, eval);

            CallIn callin = inter.iterator(goal);
            callin.next();
            help = AbstractTerm.copyMolec(inter, help);
            callin.close();

            return Types.denormProlog(encoderet, AbstractTerm.getSkel(help),
                    AbstractTerm.getDisplay(help));
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

}
