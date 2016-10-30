package jekpro.reference.bootload;

import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;

import java.math.BigInteger;
import java.util.ArrayList;

/**
 * <p>The foreign predicates for the module engine.</p>
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
public final class ForeignEngine {
    private final static Integer MAX_UNSIGNED_BYTE = Integer.valueOf(255);

    /**
     * <p>Retrieve the list of flags.</p>
     *
     * @param inter The call-in.
     * @return The list of flags.
     */
    public static Object sysListFlags(Interpreter inter) {
        ArrayList<String> flags = inter.getProperties();
        Object res = Knowledgebase.OP_NIL;
        for (int i = flags.size() - 1; i >= 0; i--)
            res = new TermCompound(Knowledgebase.OP_CONS,
                    flags.get(i), res);
        return res;
    }

    /**
     * <p>Retrieve a flag.</p>
     *
     * @param inter The call-in.
     * @param flag  The flag.
     * @return The value.
     * @throws InterpreterMessage Flag undefined.
     */
    public static Object sysGetFlag(Interpreter inter, String flag)
            throws InterpreterMessage {
        Object val = inter.getProperty(flag);
        if (val == null)
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    "prolog_flag", flag));
        return val;
    }

    /**
     * <p>Set a flag.</p>
     *
     * @param inter The call-in.
     * @param flag  The flag.
     * @param val   The value.
     * @throws InterpreterMessage Flag undefined.
     */
    public static void sysSetFlag(Interpreter inter, String flag, Object val)
            throws InterpreterMessage {
        inter.setProperty(flag, val);
    }

    /**
     * <p>Halt the system.</p>
     *
     * @param val The exit code.
     * @throws InterpreterMessage Illegal exit code.
     */
    public static void sysHalt(Object val) throws InterpreterMessage {
        InterpreterMessage.checkInstantiated(val);
        Number num = InterpreterMessage.castInteger(val);
        InterpreterMessage.checkNotLessThanZero(num);
        if (num instanceof BigInteger || ((Integer) num).compareTo(MAX_UNSIGNED_BYTE) > 0) {
            throw new InterpreterMessage(InterpreterMessage.representationError(
                    "max_status"));
        }
        System.exit(num.intValue());
    }

}