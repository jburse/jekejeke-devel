package jekpro.tools.foreign;

import jekpro.tools.array.*;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.DisplayClause;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Goal;
import jekpro.reference.reflect.SpecialForeign;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.proxy.RuntimeWrap;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

import java.lang.reflect.InvocationTargetException;
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
abstract class AbstractMember extends AbstractLense
        implements Comparable<AbstractMember> {
    public final static Object[] VOID_ARGS = new Object[0];

    int scores = -1;

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
    public Object[] getTests() {
        Class[] paras = getParameterTypes();
        int n = getArity();
        Object[] t = (n != 0 ? new Object[n] : VOID_ARGS);
        int k = 0;
        if ((subflags & AbstractDelegate.MASK_DELE_VIRT) != 0) {
            t[k] = Score.getTest(encodeobj, getDeclaringClass());
            k++;
        }
        for (int i = 0; i < encodeparas.length; i++) {
            int typ = encodeparas[i];
            if (typ == Types.TYPE_INTERPRETER) {
                /* do nothing */
            } else if (typ == Types.TYPE_CALLOUT) {
                /* do nothing */
            } else {
                t[k] = Score.getTest(typ, paras[i]);
                k++;
            }
        }
        return t;
    }

    /***********************************************************/
    /* Foreign Invokation                                      */
    /***********************************************************/

    /**
     * <p>Build the arguments array. The arguments of the goal
     * are computed, checked and converted if necessary.</p>
     *
     * @param temp The skeleton.
     * @param ref  The display.
     * @param en   The engine.
     * @return The arguments array.
     * @throws EngineMessage FFI error.
     * @throws EngineException FFI error.
     */
    final Object[] computeAndConvertArgs(Object temp, Display ref,
                                         Engine en)
            throws EngineMessage, EngineException {
        try {
            Object[] args = (encodeparas.length != 0 ?
                    new Object[encodeparas.length] : AbstractMember.VOID_ARGS);
            int k = 0;
            if ((subflags & AbstractDelegate.MASK_DELE_VIRT) != 0)
                k++;
            for (int i = 0; i < encodeparas.length; i++) {
                int typ = encodeparas[i];
                if (typ == Types.TYPE_INTERPRETER) {
                    args[i] = en.proxy;
                } else {
                    en.computeExpr(((SkelCompound) temp).args[k], ref);
                    k++;
                    Object res = AbstractTerm.createTerm(en.skel, en.display);
                    args[i] = Types.denormProlog(typ, res);
                }
            }
            return args;
        } catch (InterpreterMessage x) {
            throw (EngineMessage) x.getException();
        }
    }

    /**
     * <p>Build the arguments array. The arguments of the goal
     * are checked and converted if necessary.</p>
     *
     * @param temp The skeleton.
     * @param ref  The display.
     * @param en   The engine.
     *             @param co The call-out.
     * @return The arguments array.
     * @throws EngineMessage FFI error.
     */
    final Object[] convertArgs(Object temp, Display ref, Engine en, CallOut co)
            throws EngineMessage {
        try {
            Object[] args = (encodeparas.length != 0 ?
                    new Object[encodeparas.length] : AbstractMember.VOID_ARGS);
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
                    en.skel = ((SkelCompound) temp).args[k];
                    en.display = ref;
                    en.deref();
                    k++;
                    Object res;
                    if (typ == Types.TYPE_TERM) {
                        res = AbstractTerm.createTermWrapped(en.skel, en.display);
                    } else {
                        res = AbstractTerm.createTerm(en.skel, en.display);
                    }
                    args[i] = Types.denormProlog(typ, res);
                }
            }
            return args;
        } catch (InterpreterMessage x) {
            throw (EngineMessage) x.getException();
        }
    }

    /***********************************************************/
    /* Invocation Helpers                                      */
    /***********************************************************/

    /**
     * <p>Invoke the method.</p>
     *
     * @param obj  The receiver.
     * @param args The arguments array.
     * @param en   The engine.
     * @return The invokcation result.
     * @throws EngineException FFI error.
     * @throws EngineMessage   FFI error.
     */
    static Object invokeMethod(Method method, Object obj, Object[] args, Engine en)
            throws EngineException, EngineMessage {
        try {
            return method.invoke(obj, args);
        } catch (InvocationTargetException y) {
            Throwable x = y.getCause();
            if (x instanceof RuntimeWrap)
                x = x.getCause();
            if (x instanceof InterpreterException) {
                throw (EngineException) ((InterpreterException) x).getException();
            } else {
                throw Types.mapThrowable(x);
            }
        } catch (IllegalAccessException x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_ACCESS,
                    AbstractFactory.OP_PERMISSION_METHOD,
                    SpecialForeign.methodToCallable(method.getName(),
                            method.getParameterTypes(), en.store.foyer.SOURCE_SYSTEM, en)));
        } catch (IllegalArgumentException x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    AbstractFactory.OP_PERMISSION_APPLY,
                    AbstractFactory.OP_PERMISSION_METHOD,
                    SpecialForeign.methodToCallable(method.getName(),
                            method.getParameterTypes(), en.store.foyer.SOURCE_SYSTEM, en)));
        } catch (NullPointerException x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    AbstractFactory.OP_PERMISSION_LOOKUP,
                    AbstractFactory.OP_PERMISSION_METHOD,
                    SpecialForeign.methodToCallable(method.getName(),
                            method.getParameterTypes(), en.store.foyer.SOURCE_SYSTEM, en)));
        } catch (NoClassDefFoundError x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_LINK,
                    EngineMessage.OP_PERMISSION_CLASS,
                    new SkelAtom(x.getMessage())));
        }
    }

}
