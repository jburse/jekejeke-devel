package jekpro.frequent.stream;

import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractTerm;

import java.io.IOException;
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class ForeignTerm {

    /****************************************************************/
    /* AbstractTerm I/O                                             */
    /****************************************************************/

    /**
     * <p>Write a term to a stream.</p>
     *
     * @param inter   The interpreter.
     * @param callout Marker for frame.
     * @param para    The stream.
     * @param val     The term.
     * @throws IOException          IO problem.
     * @throws InterpreterMessage   Validation problem.
     * @throws InterpreterException Shit happens.
     */
    public static void sysWrite(Interpreter inter, CallOut callout,
                                Writer para, Object val)
            throws IOException, InterpreterMessage, InterpreterException {
        inter.unparseTerm(para, Interpreter.FLAG_NUMBERVARS, val);
    }

    /**
     * <p>Write a term quoted if necessary to a stream.</p>
     *
     * @param inter   The interpreter.
     * @param callout Marker for frame.
     * @param para    The stream.
     * @param val     The term.
     * @throws IOException        IO problem.
     * @throws InterpreterMessage Validation problem.
     * @throws InterpreterMessage Shit happens.
     */
    public static void sysWriteq(Interpreter inter, CallOut callout,
                                 Writer para, Object val)
            throws IOException, InterpreterMessage, InterpreterException {
        inter.unparseTerm(para, Interpreter.FLAG_NUMBERVARS | Interpreter.FLAG_QUOTED, val);
    }

    /**
     * <p>Write a term canonical to a stream.</p>
     *
     * @param inter   The interpreter.
     * @param callout Marker for frame.
     * @param para    The stream.
     * @param val     The term.
     * @throws IOException          IO problem.
     * @throws InterpreterMessage   Validation problem.
     * @throws InterpreterException Shit happens.
     */
    public static void sysWriteCanonical(Interpreter inter, CallOut callout,
                                         Writer para, Object val)
            throws IOException, InterpreterMessage, InterpreterException {
        inter.unparseTerm(para, Interpreter.FLAG_QUOTED | Interpreter.FLAG_IGNORE_OPS, val);
    }

    /**
     * <p>Write a term respecting options to a stream.</p>
     *
     * @param inter   The interpreter.
     * @param callout Marker for frame.
     * @param para    The stream.
     * @param val     The term.
     * @param opt     The options.
     * @throws IOException          IO problem
     * @throws InterpreterMessage   Validation problem or option problem.
     * @throws InterpreterException Shit happens.
     */
    public static void sysWriteTerm(Interpreter inter, CallOut callout,
                                    Writer para, AbstractTerm val, Object opt)
            throws IOException, InterpreterMessage, InterpreterException {
        inter.unparseTerm(para, opt, val);
    }

    /**
     * <p>Read a term from a stream.</p>
     *
     * @param inter   The interpreter.
     * @param callout Marker for frame.
     * @param para    The stream.
     * @return The read term.
     * @throws IOException          IO problem
     * @throws InterpreterMessage   Validation problem.
     * @throws InterpreterException Validation problem.
     */
    public static Object sysRead(Interpreter inter, CallOut callout,
                                 Reader para)
            throws IOException, InterpreterMessage, InterpreterException {
        return inter.parseTerm(para);
    }

    /**
     * <p>Read a term and options from a stream.</p>
     *
     * @param inter   The interpreter.
     * @param callout Marker for frame.
     * @param para    The stream.
     * @param opt     The options.
     * @return The read term.
     * @throws IOException          IO problem
     * @throws InterpreterMessage   Validation problem or option problem.
     * @throws InterpreterException Validation problem or option problem.
     */
    public static AbstractTerm sysReadTerm(Interpreter inter, CallOut callout,
                                           Reader para, Object opt)
            throws IOException, InterpreterMessage, InterpreterException {
        return inter.parseTermWrapped(para, opt);
    }

}
