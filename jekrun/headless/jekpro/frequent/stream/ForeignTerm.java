package jekpro.frequent.stream;

import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractTerm;

import java.io.Reader;
import java.io.Writer;

/**
 * The foreign predicates for the module term.
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
public final class ForeignTerm {

    /****************************************************************/
    /* AbstractTerm I/O                                             */
    /****************************************************************/

    /**
     * <p>Write a term respecting options to a stream.</p>
     *
     * @param inter   The interpreter.
     * @param para    The write stream.
     * @param val     The write term.
     * @param opt     The write options.
     * @throws InterpreterMessage   Validation problem or option problem.
     * @throws InterpreterException Shit happens.
     */
    public static void sysWriteTerm(Interpreter inter,
                                    Writer para, AbstractTerm val,
                                    Object opt)
            throws InterpreterMessage, InterpreterException {
        inter.unparseTerm(para, val, opt);
    }

    /**
     * <p>Read a term and options from a stream.</p>
     *
     * @param inter   The interpreter.
     * @param para    The read stream.
     * @param opt     The read options.
     * @return The read term.
     * @throws InterpreterMessage   Validation problem or option problem.
     * @throws InterpreterException Validation problem or option problem.
     */
    public static AbstractTerm sysReadTerm(Interpreter inter,
                                           Reader para,
                                           Object opt)
            throws InterpreterMessage, InterpreterException {
        return inter.parseTerm(para, opt);
    }

}
