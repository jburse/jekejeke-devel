package jekpro.frequent.stream;

import jekpro.frequent.system.DomOpts;
import jekpro.tools.call.InterpreterMessage;
import matula.util.format.DomNode;
import matula.util.regex.ScannerError;
import matula.util.transform.XMLCheck;
import matula.util.transform.XSDSchema;
import matula.util.transform.XSLCheck;
import matula.util.transform.XSLTransform;

import java.io.IOException;
import java.io.Writer;

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
     * <p>Check a XML node.</p>
     *
     * @param dn   The XML node.
     * @param xs   The XSD schema.
     * @param opts The DOM options.
     * @throws ScannerError Validation error.
     */
    public static void sysXmlCheck(DomNode dn, XSDSchema xs, Object opts)
            throws ScannerError, InterpreterMessage {
        DomOpts res = DomOpts.decodeDomOpts(opts);
        XMLCheck xc = new XMLCheck();
        xc.setSchema(xs);
        xc.setMask(res.getMask());
        xc.check(dn);
    }

    /**
     * <p>Check an XSL node.</p>
     *
     * @param dn   The XSL node.
     * @param opts The sheet options.
     * @throws ScannerError Validation error.
     * @throws IOException IO error.
     */
    public static void sysXslCheck(DomNode dn, Object opts)
            throws IOException, ScannerError, InterpreterMessage {
        SheetOpts res = SheetOpts.decodeSheetOpts(opts);
        XSLCheck xc = new XSLCheck();
        xc.setMask(res.getMask());
        xc.check(dn);
    }

    /**
     * <p>Transform an XSL node.</p>
     *
     * @param dn      The XSL node.
     * @param writer  The writer.
     * @param comment The comment.
     * @param opts    The sheet options.
     * @throws ScannerError Validation error.
     * @throws IOException IO error.
     */
    public static void sysXslTransform(DomNode dn, Writer writer,
                                       String comment, Object opts)
            throws InterpreterMessage, IOException, ScannerError {
        SheetOpts res = SheetOpts.decodeSheetOpts(opts);
        XSLTransform xt = new XSLTransform();
        xt.setWriter(writer);
        xt.setVariables(res.getVariables());
        xt.setMask(res.getMask());
        xt.xslt(dn, comment);
    }

}