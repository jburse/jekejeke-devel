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
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.HashMap;

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
    private final static int[] VOID_PARAS = new int[0];
    private final static Object[] VOID_PROLOG_ARGS = new Object[0];

    public final static int MASK_METH_VIRT = 0x00000001;

    private int subflags;

    private int[] encodeparas;
    private int encoderet;

    private final Method method;
    private TermAtomic functor;
    private TermAtomic is;

    /**
     * <p>Create method evaluable function.</p>
     *
     * @param m The method.
     */
    ExecutorFunction(Method m) {
        method = m;
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

    /******************************************************************/
    /* Type Encoding                                                  */
    /******************************************************************/

    private final static HashMap<Class, Integer> typemap = new HashMap<Class, Integer>();

    static {
        typemap.put(Byte.TYPE, Integer.valueOf(Types.TYPE_PRIMBYTE));
        typemap.put(Byte.class, Integer.valueOf(Types.TYPE_BYTE));
        typemap.put(Short.TYPE, Integer.valueOf(Types.TYPE_PRIMSHORT));
        typemap.put(Short.class, Integer.valueOf(Types.TYPE_SHORT));
        typemap.put(Integer.TYPE, Integer.valueOf(Types.TYPE_PRIMINT));
        typemap.put(Integer.class, Integer.valueOf(Types.TYPE_INTEGER));
        typemap.put(Long.TYPE, Integer.valueOf(Types.TYPE_PRIMLONG));
        typemap.put(Long.class, Integer.valueOf(Types.TYPE_LONG));
        typemap.put(BigInteger.class, Integer.valueOf(Types.TYPE_BIG_INTEGER));
        typemap.put(Float.TYPE, Integer.valueOf(Types.TYPE_PRIMFLOAT));
        typemap.put(Float.class, Integer.valueOf(Types.TYPE_FLOAT));
        typemap.put(Double.TYPE, Integer.valueOf(Types.TYPE_PRIMDOUBLE));
        typemap.put(Double.class, Integer.valueOf(Types.TYPE_DOUBLE));
        typemap.put(BigDecimal.class, Integer.valueOf(Types.TYPE_BIG_DECIMAL));
        typemap.put(Number.class, Integer.valueOf(Types.TYPE_NUMBER));
    }

    /**
     * <p>Encode the signature of a foreign method.</p>
     * <p>The culprit is returned in the engine skel.</p>
     *
     * @return True if the signature is ok, otherwise false.
     */
    public boolean encodeSignature() {
        Class ret = method.getReturnType();
        Integer encode = ExecutorFunction.typemap.get(ret);
        if (encode == null) {
            return false;
        } else {
            encoderet = encode.intValue();
        }

        Class[] paras = method.getParameterTypes();
        encodeparas = (paras.length != 0 ? new int[paras.length] :
                ExecutorFunction.VOID_PARAS);
        for (int i = 0; i < paras.length; i++) {
            ret = paras[i];
            encode = ExecutorFunction.typemap.get(ret);
            if (encode == null) {
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
    /* Parameter & Result Conversion                           */
    /***********************************************************/

    /**
     * <p>Build the arguments.</p>
     *
     * @param args The Java arguments.
     * @return The Prolog arguments.
     * @throws InterpreterMessage Shit happens.
     */
    Object[] uncompileArgs(Object proxy, Object[] args)
            throws InterpreterMessage {
        try {
            int len = encodeparas.length;
            if ((subflags & MASK_METH_VIRT) != 0)
                len++;
            Object[] termargs = (len != 0 ?
                    new Object[len] : ExecutorFunction.VOID_PROLOG_ARGS);
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
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
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
        Object[] termargs = uncompileArgs(proxy, args);
        Object eval;
        if (termargs.length != 0) {
            eval = new TermCompound(functor, termargs);
        } else {
            eval = functor;
        }
        TermVar[] vars = TermVar.createVars(1);
        Object goal = new TermCompound(is, vars[0], eval);
        CallIn callin = inter.iterator(vars[0], goal);
        Object res = callin.nextClose();
        return Types.denormProlog(encoderet, res);
    }

}
