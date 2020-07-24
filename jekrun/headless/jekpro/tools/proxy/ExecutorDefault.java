package jekpro.tools.proxy;

import jekpro.model.inter.Engine;
import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.SkelAtom;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * <p>This class provides an executor for Prolog predicates
 * that are called as result of invoking a proxy class
 * invocation handler and if the method was non-abstract.
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
final class ExecutorDefault extends AbstractExecutor {
    private MethodHandle special;

    private static Field impl_lookup;

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

    /**
     * <p>Create a default executor.</p>
     *
     * @param m The method.
     */
    ExecutorDefault(Method m) {
        super(m);
        try {
            MethodHandles.Lookup lookup = (MethodHandles.Lookup) impl_lookup.get(null);
            special = lookup.unreflectSpecial(method, method.getDeclaringClass());
        } catch (IllegalAccessException e) {
            special = null;
        }
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
     * @throws InterpreterException FFI error.
     * @throws InterpreterMessage   FFI error.
     * @throws Throwable            FFI error.
     */
    Object runGoal(Object proxy, Object[] args, Interpreter inter)
            throws InterpreterException, InterpreterMessage, Throwable {
        if (!currentPredicate(inter)) {
            return special.bindTo(proxy).invokeWithArguments(args);
        } else {
            Object goal = makeGoal(proxy, args, inter);
            return executeGoal(goal, inter);
        }
    }

    /**
     * <p>Check whether the predicate is defined.</p>
     *
     * @param inter The interpreter.
     * @return True if the predicate is defined, otherwise false.
     * @throws InterpreterException Shit happens.
     * @throws InterpreterMessage   Shit happens.
     */
    private boolean currentPredicate(Interpreter inter)
            throws InterpreterException, InterpreterMessage {
        int len = encodeparas.length;
        if ((subflags & ExecutorInterface.MASK_METH_VIRT) != 0)
            len++;
        if ((subflags & ExecutorInterface.MASK_METH_FUNC) != 0)
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

}