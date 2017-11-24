package matula.util.format;

import matula.util.data.MapHash;
import matula.util.regex.ScannerError;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;

/**
 * <p>This class provides a dom reader.</p>
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
final class DomReader extends XmlScanner<XmlMachine> {
    static final String STRING_SLASH = "/";

    static final String DOM_START_MISSING = "dom_start_missing";
    static final String DOM_ATTRIBUTES_UNEXPECTED = "dom_attributes_unexpected";
    static final String DOM_END_MISSING = "dom_end_missing";
    static final String DOM_TYPE_MISMATCH = "dom_type_mismatch";
    static final String DOM_DUPLICATE_ATTRIBUTE = "dom_duplicate_attribute";
    static final String DOM_SUPERFLOUS_TAG = "dom_superflous_tag";
    static final String DOM_NONE_WHITESPACE = "dom_none_whitespace";
    static final String DOM_UNBALANCED_COMMENT = "dom_unbalanced_comment";

    private int mask;
    private MapHash<String, Integer> control;

    /**
     * <p>Set the return mask.</p>
     *
     * @param m The return mask.
     */
    void setMask(int m) {
        mask = m;
    }

    /**
     * <p>Set the tag control.</p>
     *
     * @param c The tag control.
     */
    void setControl(MapHash<String, Integer> c) {
        control = c;
    }

    /**
     * <p>Retrieve the return mask.</p>
     *
     * @return The return mask.
     */
    int getMask() {
        return mask;
    }

    /**
     * <p>Retrieve the tag control.</p>
     *
     * @return The tag control.
     */
    MapHash<String, Integer> getControl() {
        return control;
    }

    /**
     * <p>Creates a new dom reader.</p>
     */
    DomReader() {
        super(new XmlMachine());
    }

    /**
     * <p>Get the next tag.</p>
     *
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    public void nextTagOrText() throws IOException, ScannerError {
        super.nextTagOrText();
        for (; ; ) {
            switch (getRes()) {
                case XmlMachine.RES_TEXT:
                    if ((mask & DomNode.MASK_TEXT) != 0)
                        return;
                    checkWhitespace();
                    super.nextTagOrText();
                    break;
                case XmlMachine.RES_TAG:
                    if (!getType().startsWith("!--"))
                        return;
                    checkComment();
                    super.nextTagOrText();
                    break;
                case XmlMachine.RES_EOF:
                    return;
                default:
                    throw new IllegalArgumentException("illegal res");

            }
        }
    }

    /**
     * <p>Check whether the text is a white space text.</p>
     *
     * @throws ScannerError Shit happens.
     */
    private void checkWhitespace() throws ScannerError {
        char[] buf = getTextBuf();
        int len = getTextLen();
        for (int i = 0; i < len; i++) {
            char ch = buf[i];
            if (ch <= XmlMachine.CHAR_SPACE || ch == XmlMachine.CHAR_BOM) {
                // do nothing
            } else {
                throw new ScannerError(DOM_NONE_WHITESPACE);
            }
        }
    }

    /**
     * <p>Check whether the tag is a comment tag.</p>
     *
     * @throws ScannerError Shit happens.
     */
    private void checkComment() throws ScannerError {
        int n = getAttrCount();
        if (n != 0 &&
                "".equals(getValueAt(n - 1)) &&
                getAttr(n - 1).equals("--")) {
            /* do nothing */
        } else {
            throw new ScannerError(DOM_UNBALANCED_COMMENT);
        }
    }

    /**
     * <p>Check whether the dom reader is at eof.</p>
     *
     * @throws ScannerError Shit happens.
     */
    void checkEof() throws ScannerError, IOException {
        for (; ; ) {
            switch (getRes()) {
                case XmlMachine.RES_TEXT:
                    checkWhitespace();
                    super.nextTagOrText();
                    break;
                case XmlMachine.RES_TAG:
                    throw new ScannerError(DOM_SUPERFLOUS_TAG);
                case XmlMachine.RES_EOF:
                    return;
                default:
                    throw new IllegalArgumentException("illegal res");
            }
        }
    }

    /**
     * <p>Some test cases.</p>
     *
     * @param args The arguments.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    public static void main(String[] args) throws IOException, ScannerError {
        String text = "<p>The quick brown fox <img/> jumps over the lazy dog.</p>";
        StringReader sr = new StringReader(text);
        DomElement de = new DomElement();
        de.load(sr, DomNode.MASK_TEXT);
        PrintWriter pw = new PrintWriter(System.out);
        de.store(pw, null, DomNode.MASK_TEXT);
        pw.println();

        text = "<p>The quick brown fox <img> jumps over the lazy dog.</p>";
        MapHash<String, Integer> control = new MapHash<String, Integer>();
        control.add("img", Integer.valueOf(DomNode.CONTROL_EMPTY));
        sr = new StringReader(text);
        de = new DomElement();
        de.load(sr, DomNode.MASK_TEXT, control);
        de.store(pw, null, DomNode.MASK_TEXT, control);
        pw.println();
    }

}
