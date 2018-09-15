package jekpro.frequent.misc;

import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import matula.util.regex.AbstractCompiler;
import matula.util.regex.AbstractPattern;
import matula.util.regex.AbstractSpecimen;
import matula.util.regex.ScannerError;
import matula.util.system.ConnectionReader;
import matula.util.system.OpenOpts;

import java.io.IOException;
import java.io.StringReader;

/**
 * Provides the methods for the module misc/text.
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
public final class ForeignText {

    /**
     * <p>Creata a specimen from a string.</p>
     *
     * @param inter The interpreter.
     * @param comp  The compiler.
     * @param pat   The string.
     * @return The specimen.
     * @throws InterpreterException Problem creating specimen.
     */
    public static AbstractSpecimen sysCreateSpecimen(Interpreter inter,
                                                     AbstractCompiler comp,
                                                     String pat)
            throws InterpreterException {
        AbstractSpecimen spec;
        try {
            spec = comp.createSpecimen(pat);
        } catch (ScannerError y) {
            String line = ScannerError.linePosition(pat, y.getPos());
            InterpreterMessage x = new InterpreterMessage(
                    InterpreterMessage.syntaxError(y.getError()));
            throw new InterpreterException(x,
                    InterpreterException.fetchPos(
                            InterpreterException.fetchStack(inter), line, inter));
        }
        return spec;
    }

    /**
     * <p>Make a pattern.</p>
     *
     * @param inter The interpreter.
     * @param comp  The compiler.
     * @param pat   The string.
     * @param flag  The style and the features.
     * @return The pattern.
     * @throws InterpreterException Problem making pattern.
     * @throws IOException          IO error.
     */
    public static AbstractPattern sysMakePattern(Interpreter inter,
                                                 AbstractCompiler comp,
                                                 String pat, int flag)
            throws InterpreterException, IOException {
        switch (flag & AbstractSpecimen.MATCH_STLE) {
            case AbstractSpecimen.MATCH_CRTE:
                AbstractSpecimen spec;
                try {
                    spec = comp.createSpecimen(pat, flag);
                } catch (ScannerError y) {
                    String line = ScannerError.linePosition(pat, y.getPos());
                    InterpreterMessage x = new InterpreterMessage(
                            InterpreterMessage.syntaxError(y.getError()));
                    throw new InterpreterException(x,
                            InterpreterException.fetchPos(
                                    InterpreterException.fetchStack(inter), line, inter));
                }
                return spec;
            case AbstractSpecimen.MATCH_PRSE:
                ConnectionReader cr = new ConnectionReader(new StringReader(pat));
                cr.setLineNumber(1);
                try {
                    spec = comp.parseSpecimen(cr, flag);
                } catch (ScannerError y) {
                    String line = ScannerError.linePosition(OpenOpts.getLine(cr), y.getPos());
                    InterpreterMessage x = new InterpreterMessage(
                            InterpreterMessage.syntaxError(y.getError()));
                    throw new InterpreterException(x,
                            InterpreterException.fetchPos(
                                    InterpreterException.fetchStack(inter), line, inter));
                }
                return spec;
            default:
                throw new IllegalArgumentException("illegal style");
        }
    }

}
