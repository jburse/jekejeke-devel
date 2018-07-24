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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
final class ExecutorMethod extends AbstractExecutor {
    private final static int[] VOID_PARAS = new int[0];
    private final static Object[] VOID_PROLOG_ARGS = new Object[0];

    public final static int MASK_METH_VIRT = 0x00000001;
    public final static int MASK_METH_FUNC = 0x00000002;

    private int subflags;

    private int[] encodeparas;
    private int encoderet;

    private final Method method;
    private TermAtomic functor;

    /**
     * <p>Create method predicate.</p>
     *
     * @param m The method.
     */
    ExecutorMethod(Method m) {
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
    Object[] uncompileArgs(Object proxy, Object[] args) throws InterpreterMessage {
        try {
            int len = encodeparas.length;
            if ((subflags & MASK_METH_VIRT) != 0)
                len++;
            if ((subflags & MASK_METH_FUNC) != 0)
                len++;
            Object[] termargs = (len != 0 ?
                    new Object[len] : ExecutorMethod.VOID_PROLOG_ARGS);
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
        Object[] termargs = uncompileArgs(proxy, args);
        if ((subflags & ExecutorMethod.MASK_METH_FUNC) != 0) {
            TermVar[] vars = TermVar.createVars(1);
            termargs[termargs.length - 1] = vars[0];
        }
        Object goal;
        if (termargs.length != 0) {
            goal = new TermCompound(inter, functor, termargs);
        } else {
            goal = functor;
        }
        CallIn callin;
        if ((subflags & ExecutorMethod.MASK_METH_FUNC) != 0) {
            callin = inter.iterator(termargs[termargs.length - 1], goal);
        } else {
            callin = inter.iterator(goal);
        }
        Object res;
        if (encoderet == Types.TYPE_TERM) {
            res = callin.hasNextCloseWrapped();
        } else {
            res = callin.hasNextClose();
        }
        return Types.denormProlog(encoderet, res);
    }

}
