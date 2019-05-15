package jekdev.reference.notebook;

import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.PositionKey;
import matula.text.format.AbstractDom;
import matula.text.format.AbstractReader;
import matula.text.format.AbstractWriter;
import matula.util.regex.ScannerError;
import matula.util.system.OpenOpts;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

/**
 * <p>The foreign predicates for the module system/json.</p>
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
public final class ForeignSerialize {

    /*******************************************************************/
    /* Node Read & Write                                               */
    /*******************************************************************/

    /**
     * <p>Read a DOM node.</p>
     *
     * @param inter  The interpreter.
     * @param reader The reader.
     * @param opts   The DOM options.
     * @param node   The DOM node.
     * @throws InterpreterMessage   Validation error.
     * @throws IOException          IO error.
     * @throws InterpreterException Syntax error.
     */
    public static void sysNodeRead(Interpreter inter,
                                   Reader reader,
                                   Object opts,
                                   AbstractDom node)
            throws InterpreterMessage, IOException, InterpreterException {
        SerializeOpts res = SerializeOpts.decodeSerializeOpts(opts);
        try {
            AbstractReader.load(reader, node,
                    res.getMask(), res.getControl());
        } catch (ScannerError y) {
            String line = ScannerError.linePosition(OpenOpts.getLine(reader), y.getErrorOffset());
            InterpreterMessage x = new InterpreterMessage(
                    InterpreterMessage.syntaxError(y.getMessage()));
            PositionKey pos = (OpenOpts.getPath(reader) != null ?
                    new PositionKey(OpenOpts.getPath(reader), OpenOpts.getLineNumber(reader)) : null);
            throw new InterpreterException(x,
                    InterpreterException.fetchPos(
                            InterpreterException.fetchLoc(
                                    InterpreterException.fetchStack(inter),
                                    pos, inter), line, inter));
        }
    }

    /**
     * <p>Write a DOM node.</p>
     *
     * @param writer The writer.
     * @param opts   The DOM options.
     * @param node   The DOM node.
     * @throws InterpreterMessage Validation error.
     * @throws IOException        IO error.
     */
    public static void sysNodeWrite(Writer writer,
                                    Object opts,
                                    AbstractDom node)
            throws InterpreterMessage, IOException {
        SerializeOpts res = SerializeOpts.decodeSerializeOpts(opts);
        AbstractWriter.store(writer, node, res.getComment(),
                res.getMask(), res.getControl());
    }

}
