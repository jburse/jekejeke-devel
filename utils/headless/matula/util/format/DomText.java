package matula.util.format;

import matula.util.regex.ScannerError;
import matula.util.system.ForeignXml;
import matula.util.system.OpenOpts;

import java.io.IOException;

/**
 * <p>This class provides a dom element.</p>
 * </p>
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
public final class DomText extends DomNode {
    static final String DOM_MISSING_TEXT = "dom_missing_text";

    private String data = "";

    /**
     * <p>Retrieve the data.</p>
     *
     * @return The data.
     */
    public String getData() {
        return data;
    }

    /**
     * <p>Set the data.</p>
     *
     * @param d The data.
     */
    public void setData(String d) {
        if (data == null)
            throw new NullPointerException("data missing");
        data = d;
    }

    /**
     * <p>Load a dom text.</p>
     *
     * @param dr The dom reader.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    void loadNode(DomReader dr) throws IOException, ScannerError {
        switch (dr.getRes()) {
            case XmlMachine.RES_TEXT:
                data = ForeignXml.sysTextUnescape(dr.getText());
                dr.nextTagOrText();
                break;
            case XmlMachine.RES_TAG:
                throw new ScannerError(DOM_MISSING_TEXT, OpenOpts.getOffset(dr.getReader()));
            case XmlMachine.RES_EOF:
                throw new ScannerError(DOM_MISSING_TEXT, OpenOpts.getOffset(dr.getReader()));
            default:
                throw new IllegalArgumentException("illegal res");
        }
    }

    /**
     * <p>Store this dom text.</p>
     *
     * @param dw The dom writer.
     * @throws IOException Shit happens.
     */
    void storeNode(DomWriter dw) throws IOException {
        dw.write(ForeignXml.sysTextEscape(data));
    }

}
