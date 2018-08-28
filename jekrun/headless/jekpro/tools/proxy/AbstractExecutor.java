package jekpro.tools.proxy;

import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.array.Types;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.TermAtomic;

import java.lang.reflect.Method;

/**
 * <p>The base class for our executors.</p>
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
abstract class AbstractExecutor {
    public final static int MASK_METH_VIRT = 0x00000001;
    public final static int MASK_METH_FUNC = 0x00000002;

    final static int[] VOID_PARAS = new int[0];
    final static Object[] VOID_PROLOG_ARGS = new Object[0];

    protected int subflags;

    protected int[] encodeparas;
    protected int encoderet;

    protected final Method method;
    protected TermAtomic functor;

    /**
     * <p>Create an abstract executor.</p>
     *
     * @param m The method.
     */
    AbstractExecutor(Method m) {
        method = m;
    }

    /**
     * <p>Set the source.</p>
     *
     * @param src The source.
     */
    abstract void setSource(AbstractSource src);

    /**
     * <p>Run the executor.</p>
     *
     * @param proxy The proxy class instance.
     * @param args  The arguments.
     * @param inter The interpreter.
     * @return The result.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    abstract Object runGoal(Object proxy, Object[] args, Interpreter inter)
            throws InterpreterMessage, InterpreterException;

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
    Object[] uncompileArgs(Object proxy, Object[] args)
            throws EngineMessage {
        int len = encodeparas.length;
        if ((subflags & MASK_METH_VIRT) != 0)
            len++;
        if ((subflags & MASK_METH_FUNC) != 0)
            len++;
        Object[] termargs = (len != 0 ?
                new Object[len] : AbstractExecutor.VOID_PROLOG_ARGS);
        int k = 0;
        if ((subflags & MASK_METH_VIRT) != 0) {
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

}
