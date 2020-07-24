package jekpro.tools.proxy;

import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;

import java.lang.reflect.Method;

/**
 * <p>This class provides an executor for Prolog predicates
 * that are called as result of invoking a proxy class
 * invocation handler and if the method was abstract.
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
final class ExecutorInterface extends AbstractExecutor {

    /**
     * <p>Create a method executor.</p>
     *
     * @param m The method.
     */
    ExecutorInterface(Method m) {
        super(m);
    }

    /***********************************************************/
    /* Variation Point                                         */
    /***********************************************************/

    /**
     * <p>Run the predicate.</p>
     *
     * @param proxy The proxy instance.
     * @param args  The arguments.
     * @param inter The interpreter.
     * @return The result, can be null.
     * @throws InterpreterMessage   FFI error.
     * @throws InterpreterException FFI error.
     */
    Object runGoal(Object proxy, Object[] args, Interpreter inter)
            throws InterpreterMessage, InterpreterException {
        Object goal = makeGoal(proxy, args, inter);
        return executeGoal(goal, inter);
    }

}
