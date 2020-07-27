package jekpro.tools.proxy;

import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;

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

    /***********************************************************/
    /* Variation Points                                         */
    /***********************************************************/

    /**
     * <p>Set the handler.</p>
     *
     * @param method  The method.
     * @param handler The handler.
     */
    protected void setHandler(Method method, ProxyHandler handler) {
        super.setHandler(method, handler);
        try {
            MethodHandles.Lookup lookup = (MethodHandles.Lookup) impl_lookup.get(null);
            special = lookup.unreflectSpecial(method, method.getDeclaringClass());
        } catch (IllegalAccessException e) {
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
     * @throws InterpreterException FFI error.
     * @throws InterpreterMessage   FFI error.
     * @throws Throwable            FFI error.
     */
    protected Object runGoal(Object proxy, Object[] args, Interpreter inter)
            throws InterpreterException, InterpreterMessage, Throwable {
        if (!currentProvable(inter)) {
            return special.bindTo(proxy).invokeWithArguments(args);
        } else {
            return super.runGoal(proxy, args, inter);
        }
    }

}