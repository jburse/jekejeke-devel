package jekpro.frequent.advanced;

import jekpro.tools.call.CallIn;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractTerm;
import matula.util.misc.InterfacePipe;
import matula.util.misc.Pivot;

/**
 * <p>Provides built-in predicates for the module micro.</p>
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
public final class ForeignMicro {

    /**
     * <p>Create a new micro engine.</p>
     *
     * @param inter The interpreter.
     * @param goal  The goal.
     * @return The micro engine.
     */
    public static CallIn sysMicroNew(Interpreter inter, AbstractTerm goal) {
        Object obj = AbstractTerm.copyMolec(inter, goal);
        return inter.iterable().iterator(obj);
    }

    /**
     * <p>Continue with micro engine.</p>
     *
     * @param callin The micro engine.
     * @return True if the continuation succeeded, otherwise false.
     * @throws InterpreterException Shit happens.
     * @throws InterpreterMessage   No such element.
     */
    public static boolean sysMicroCont(CallIn callin)
            throws InterpreterException, InterpreterMessage {
        if (callin.hasNext()) {
            callin.next();
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Place a copy into the pivot.</p>
     *
     * @param inter The interpreter.
     * @param q     The pivot.
     * @param t     The term.
     */
    public static void sysPivotPut(Interpreter inter, Pivot q, AbstractTerm t) {
        Object obj = AbstractTerm.copyMolec(inter, t);
        q.put(obj);
    }

}