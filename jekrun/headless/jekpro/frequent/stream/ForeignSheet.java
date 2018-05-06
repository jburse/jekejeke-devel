package jekpro.frequent.stream;

import jekpro.frequent.system.DomOpts;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import matula.util.format.DomElement;
import matula.util.format.AbstractDom;
import matula.util.format.DomWriter;
import matula.util.regex.ScannerError;
import matula.util.system.OpenOpts;
import matula.util.transform.*;

import java.io.IOException;
import java.io.Writer;
import java.text.ParseException;

/**
 * <p>The foreign predicates for the module stream/xsl.</p>
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
public final class ForeignSheet {

    /**
     * <p>Digest an XSD schema.</p>
     *
     * @param inter   The interpreter.
     * @param xs The XSD schema.
     * @param de The DOM element.
     * @throws InterpreterMessage Validation error.
     * @throws InterpreterException Syntax error.
     * @throws IOException IO error.
     */
    public static void sysXsdDigest(Interpreter inter,
                                    XSDSchema xs, DomElement de)
            throws InterpreterMessage, IOException, InterpreterException, ParseException {
        try {
            xs.digestElements(de);
        } catch (ScannerError y) {
            String line = ScannerError.linePosition(y.getLine(), y.getPos());
            InterpreterMessage x = new InterpreterMessage(
                    InterpreterMessage.syntaxError(y.getError()));
            throw new InterpreterException(x,
                    InterpreterException.fetchPos(
                            InterpreterException.fetchStack(inter), line, inter));
        } catch (ValidationError y) {
            throw new InterpreterMessage(
                    InterpreterMessage.domainError(y.getError(), y.getCulprit()));
        }
    }

    /**
     * <p>Check a XML node.</p>
     *
     * @param dn   The XML node.
     * @param xs   The XSD schema.
     * @param opts The DOM options.
     * @throws InterpreterMessage Validation error.
     */
    public static void sysXMLCheck(AbstractDom dn, XSDSchema xs, Object opts)
            throws InterpreterMessage {
        DomOpts res = DomOpts.decodeDomOpts(opts);
        XMLCheck xc = new XMLCheck();
        xc.setContext(res.getContext());
        xc.setMask(res.getMask());
        xc.setSchema(xs);
        try {
            xc.check(dn);
        } catch (ValidationError y) {
            throw new InterpreterMessage(
                    InterpreterMessage.domainError(y.getError(), y.getCulprit()));
        }
    }

    /**
     * <p>Transform an XSL node.</p>
     *
     * @param inter   The interpreter.
     * @param dn      The XSL node.
     * @param writer  The writer.
     * @param comment The comment.
     * @param opts    The sheet options.
     * @throws InterpreterMessage   Validation error.
     * @throws IOException          IO error.
     * @throws InterpreterException Syntax error.
     */
    public static void sysXSLTransform(Interpreter inter,
                                       AbstractDom dn, Writer writer,
                                       String comment, Object opts)
            throws InterpreterMessage, IOException, InterpreterException, ParseException {
        SheetOpts res = SheetOpts.decodeSheetOpts(opts);
        DomWriter dw = new DomWriter();
        dw.setWriter(writer);
        dw.setMask(res.getMask());
        dw.setControl(res.getControl());

        XSLSheetTransform xt = new XSLSheetTransform();
        xt.setWriter(dw);
        xt.setVariables(res.getVariables());
        try {
            xt.xslt(dn, comment);
        } catch (ScannerError y) {
            String line = ScannerError.linePosition(y.getLine(), y.getPos());
            InterpreterMessage x = new InterpreterMessage(
                    InterpreterMessage.syntaxError(y.getError()));
            throw new InterpreterException(x,
                    InterpreterException.fetchPos(
                            InterpreterException.fetchStack(inter), line, inter));
        } catch (ValidationError y) {
            throw new InterpreterMessage(
                    InterpreterMessage.domainError(y.getError(), y.getCulprit()));
        }
    }

    /**
     * <p>Check an XSL node.</p>
     *
     * @param dn   The XSL node.
     * @param opts The sheet options.
     * @throws InterpreterMessage   Validation error.
     * @throws IOException          IO error.
     * @throws InterpreterException Syntax error.
     */
    public static void sysXSLCheck(Interpreter inter,
                                   AbstractDom dn, Object opts)
            throws InterpreterMessage, IOException,
            InterpreterException, ParseException {
        SheetOpts res = SheetOpts.decodeSheetOpts(opts);
        XSLSheetCheck xc = new XSLSheetCheck();
        xc.setMask(res.getMask());
        try {
            xc.check(dn);
        } catch (ScannerError y) {
            String line = ScannerError.linePosition(y.getLine(), y.getPos());
            InterpreterMessage x = new InterpreterMessage(
                    InterpreterMessage.syntaxError(y.getError()));
            throw new InterpreterException(x,
                    InterpreterException.fetchPos(
                            InterpreterException.fetchStack(inter), line, inter));
        } catch (ValidationError y) {
            throw new InterpreterMessage(
                    InterpreterMessage.domainError(y.getError(), y.getCulprit()));
        }
    }

}