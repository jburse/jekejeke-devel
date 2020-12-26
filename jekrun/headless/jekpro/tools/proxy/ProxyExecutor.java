package jekpro.tools.proxy;

import jekpro.model.inter.Engine;
import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.array.AbstractLense;
import jekpro.tools.array.Types;
import jekpro.tools.call.CallIn;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.foreign.AbstractMember;
import jekpro.tools.term.*;

import java.lang.invoke.MethodHandle;
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
    private int subflags;

    private int[] encodeparas;
    private int encoderet;

    private TermAtomic functor;
    private int arity;
    private MethodHandle special;

    /***********************************************************/
    /* Proxy Executor API                                      */
    /***********************************************************/

    /**
     * <p>Encode the signature of a foreign method.</p>
     * <p>The culprit is returned in the engine skel.</p>
     *
     * @param method The method.
     * @param en     The engine.
     * @return True if the signature is ok, otherwise false.
     */
    boolean encodeSignature(Method method, Engine en) {
        if (!Modifier.isStatic(method.getModifiers()))
            subflags |= AbstractDelegate.MASK_DELE_VIRT;

        Class ret = method.getReturnType();
        encoderet = AbstractLense.encodeRet(ret, en);
        if (encoderet == -1)
            return false;

        Class[] paras = method.getParameterTypes();
        encodeparas = AbstractLense.encodeParas(paras, en);
        if (encodeparas == null)
            return false;

        if (Types.getRetFlag(encoderet))
            subflags |= AbstractLense.MASK_METH_FUNC;
        return true;
    }

    /**
     * <p>Encode the special of a foreign method.</p>
     * <p>The culprit is returned in the engine skel.</p>
     *
     * @param method The method.
     * @param en     The engine.
     * @return True if the signature is ok, otherwise false.
     */
    boolean encodeSpecial(Method method, Engine en) {
        if ((method.getModifiers() & Modifier.ABSTRACT) == 0) {
            special = AbstractMember.encodeSpecial(method, en);
            if (special == null)
                return false;
        } else {
            special = null;
        }
        return true;
    }

    /**
     * <p>Set the handler.</p>
     *
     * @param method  The method.
     * @param handler The handler.
     */
    void setHandler(Method method, ProxyHandler handler) {
        SkelAtom val = new SkelAtom(method.getName(), handler.getSource());
        functor = new TermAtomic(val);
        arity = AbstractLense.getParaCount(encodeparas);
        if ((subflags & AbstractDelegate.MASK_DELE_VIRT) != 0)
            arity++;
        if ((subflags & AbstractLense.MASK_METH_FUNC) != 0)
            arity++;
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
        SkelAtom sa = (SkelAtom) functor.getSkel();
        Engine en = inter.getEngine();
        CachePredicate cp;
        try {
            cp = CachePredicate.getPredicateDefined(sa, arity, en, 0);
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
        SkelAtom sa = (SkelAtom) functor.getSkel();
        Engine en = inter.getEngine();
        return new EngineMessage(EngineMessage.existenceError(
                EngineMessage.OP_EXISTENCE_PROCEDURE,
                new SkelCompound(en.store.foyer.ATOM_SLASH,
                        sa,
                        Integer.valueOf(arity))));
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
        if ((subflags & AbstractLense.MASK_METH_FUNC) != 0)
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
            if ((subflags & AbstractLense.MASK_METH_FUNC) != 0) {
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
            if ((subflags & AbstractLense.MASK_METH_FUNC) != 0) {
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
        Object[] termargs = (arity != 0 ? new Object[arity] : AbstractMember.VOID_ARGS);
        int k = 0;
        if ((subflags & AbstractDelegate.MASK_DELE_VIRT) != 0) {
            termargs[k] = proxy;
            k++;
        }
        for (int i = 0; i < encodeparas.length; i++) {
            int typ = encodeparas[i];
            if (typ == Types.TYPE_INTERPRETER) {
                /* do nothing */
            } else if (typ == Types.TYPE_CALLOUT) {
                /* do nothing */
            } else {
                Object res = Types.normJava(typ, args[i]);
                if (res == null)
                    throw new EngineMessage(EngineMessage.representationError(
                            AbstractFactory.OP_REPRESENTATION_NULL));
                termargs[k] = res;
                k++;
            }
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