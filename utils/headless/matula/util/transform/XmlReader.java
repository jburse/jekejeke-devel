package matula.util.transform;

import matula.util.data.ListArray;
import matula.util.format.*;
import matula.util.regex.ScannerError;
import matula.util.system.ForeignXml;
import matula.util.system.OpenOpts;

import java.io.IOException;
import java.io.Reader;

/**
 * <p>This class provides a XML reader.</p>
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class XmlReader extends AbstractReader {
    private static final String DOM_MISSING_TEXT = "dom_missing_text";

    private static final String DOM_MISSING_ELEM = "dom_missing_elem";
    private static final String DOM_ILLEGAL_VALUE = "dom_illegal_value";
    private static final String DOM_CLOSED_EMPTY = "dom_closed_empty";
    private static final String DOM_MISSING_END = "dom_missing_end";
    private static final String DOM_UNEXPECTED_ATTR = "dom_unexpected_attr";
    private static final String DOM_MISMATCHED_END = "dom_mismatched_end";

    private static final String DOM_NONE_WHITESPACE = "dom_none_whitespace";
    private static final String DOM_UNBALANCED_COMMENT = "dom_unbalanced_comment";
    private static final String DOM_UNBALANCED_PROCINSTR = "dom_unbalanced_procinstr";
    private static final String DOM_SUPERFLOUS_TAG = "dom_superflous_tag";

    private static final String STRING_BANG_DASH_DASH = "!--";
    private static final String STRING_DASH_DASH = "--";

    private XmlScanner scanner;

    /**
     * <p>Set the reader.</p>
     *
     * @param r The reader.
     * @throws IOException I/O Error.
     */
    public void setReader(Reader r)
            throws IOException {
        scanner = new XmlScanner(new XmlMachine());
        scanner.setReader(r);
    }

    /**********************************************************/
    /* Load Methods                                           */
    /**********************************************************/

    /**
     * <p>Load the children.</p>
     *
     * @return The children.
     * @throws IOException  I/O Error..
     * @throws ScannerError Syntax error.
     */
    public ListArray<AbstractDom> loadNodes()
            throws IOException, ScannerError {
        nextTagOrText();
        ListArray<AbstractDom> res = null;
        for (; ; ) {
            switch (scanner.getRes()) {
                case XmlMachine.RES_TEXT:
                    DomText dt = new DomText();
                    loadNode(dt);
                    if (res == null)
                        res = new ListArray<AbstractDom>();
                    res.add(dt);
                    break;
                case XmlMachine.RES_TAG:
                    String temp = scanner.getType();
                    if (temp.length() > 0 &&
                            temp.charAt(0) == XmlMachine.CHAR_SLASH)
                        return res;
                    DomElement dh = new DomElement();
                    loadNode(dh);
                    if (res == null)
                        res = new ListArray<AbstractDom>();
                    res.add(dh);
                    break;
                case XmlMachine.RES_EOF:
                    return res;
                default:
                    throw new IllegalArgumentException("illegal res");
            }
        }
    }

    /**
     * <p>Load a dom node.</p>
     * <p>Not synchronized, uses cut-over.</p>
     *
     * @param node The dom node.
     * @throws IOException  I/O Error..
     * @throws ScannerError Syntax error.
     */
    public void loadNode(AbstractDom node)
            throws IOException, ScannerError {
        if (node instanceof DomText) {
            DomText dt = (DomText) node;
            switch (scanner.getRes()) {
                case XmlMachine.RES_TEXT:
                    dt.setData(ForeignXml.sysTextUnescape(getTextStrip()));
                    nextTagOrText();
                    break;
                case XmlMachine.RES_TAG:
                    throw new ScannerError(DOM_MISSING_TEXT,
                            OpenOpts.getOffset(scanner.getReader()));
                case XmlMachine.RES_EOF:
                    throw new ScannerError(DOM_MISSING_TEXT,
                            OpenOpts.getOffset(scanner.getReader()));
                default:
                    throw new IllegalArgumentException("illegal res");
            }
        } else {
            DomElement de = (DomElement) node;
            switch (scanner.getRes()) {
                case XmlMachine.RES_TEXT:
                    throw new ScannerError(DOM_MISSING_ELEM,
                            OpenOpts.getOffset(scanner.getReader()));
                case XmlMachine.RES_TAG:
                    boolean lastspace = ((getMask() & AbstractDom.MASK_LTSP) != 0);
                    String type = scanner.getType();
                    if (type.length() > 0 &&
                            type.charAt(0) == XmlMachine.CHAR_SLASH)
                        throw new ScannerError(DOM_MISSING_ELEM,
                                OpenOpts.getOffset(scanner.getReader()));
                    boolean closed = checkClosed();
                    ListArray<AbstractDom> newattrs = null;
                    for (int i = 0; i < scanner.getAttrCount(); i++) {
                        String valstr = getValueStripAt(i);
                        Object val;
                        if (XmlMachine.isQuoted(valstr) || "".equals(valstr)) {
                            val = ForeignXml.sysTextUnescape(XmlMachine.stripValue(valstr));
                        } else if (XmlMachine.isNumber(valstr)) {
                            try {
                                if (valstr.indexOf('.') != -1) {
                                    val = Double.valueOf(valstr);
                                } else {
                                    val = Long.valueOf(valstr);
                                }
                            } catch (NumberFormatException x) {
                                throw new ScannerError(DOM_ILLEGAL_VALUE,
                                        OpenOpts.getOffset(scanner.getReader()));
                            }
                        } else {
                            throw new ScannerError(DOM_ILLEGAL_VALUE,
                                    OpenOpts.getOffset(scanner.getReader()));
                        }
                        if (newattrs == null)
                            newattrs = new ListArray<AbstractDom>();
                        DomText dt = new DomText();
                        dt.setKey(scanner.getAttr(i));
                        dt.setDataObj(val);
                        newattrs.add(dt);
                    }
                    boolean empty = (AbstractDom.getControl(getControl(), type) == AbstractDom.TYPE_EMPTY);
                    ListArray<AbstractDom> newchildren;
                    if (!closed && !empty) {
                        int backmask = getMask();
                        if (((backmask & AbstractDom.MASK_TEXT) == 0 ||
                                (backmask & AbstractDom.MASK_STRP) != 0) &&
                                AbstractDom.getControl(getControl(), type) == AbstractDom.TYPE_ANY) {
                            int mask = backmask;
                            mask |= AbstractDom.MASK_TEXT;
                            mask &= ~AbstractDom.MASK_STRP;
                            setMask(mask);
                            try {
                                newchildren = loadNodes();
                                setMask(backmask);
                            } catch (IOException x) {
                                setMask(backmask);
                                throw x;
                            } catch (ScannerError x) {
                                setMask(backmask);
                                throw x;
                            }
                        } else if (((backmask & AbstractDom.MASK_TEXT) == 0 ||
                                (backmask & AbstractDom.MASK_STRP) == 0) &&
                                AbstractDom.getControl(getControl(), type) == AbstractDom.TYPE_TEXT) {
                            int mask = backmask;
                            mask |= AbstractDom.MASK_TEXT;
                            mask |= AbstractDom.MASK_STRP;
                            setMask(mask);
                            try {
                                newchildren = loadNodes();
                                setMask(backmask);
                            } catch (IOException x) {
                                setMask(backmask);
                                throw x;
                            } catch (ScannerError x) {
                                setMask(backmask);
                                throw x;
                            }
                        } else {
                            newchildren = loadNodes();
                        }
                        checkEnd(type);
                    } else {
                        if (closed && empty)
                            throw new ScannerError(DOM_CLOSED_EMPTY, -1);
                        newchildren = null;
                        nextTagOrText();
                    }
                    de.setName(type);
                    de.setAttrsFast(newattrs);
                    de.setChildrenFast(newchildren);
                    if (lastspace) {
                        setMask(getMask() | AbstractDom.MASK_LTSP);
                    } else {
                        setMask(getMask() & ~AbstractDom.MASK_LTSP);
                    }
                    break;
                case XmlMachine.RES_EOF:
                    throw new ScannerError(DOM_MISSING_ELEM,
                            OpenOpts.getOffset(scanner.getReader()));
                default:
                    throw new IllegalArgumentException("illegal res");
            }
        }
    }

    /**
     * <p>Check whether the actual tag is closed.</p>
     * <p>As a side effect the tag is validated.</p>
     * <p>As a side effect the tag is made open.</p
     *
     * @return True of the tag is closed, otherwise false.
     */
    private boolean checkClosed() {
        int n = scanner.getAttrCount();
        if (n != 0 &&
                "".equals(scanner.getValueAt(n - 1)) &&
                scanner.getAttr(n - 1).length() == 1 &&
                scanner.getAttr(n - 1).charAt(0) == XmlMachine.CHAR_SLASH) {
            scanner.removeAttrValue(n - 1);
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Check whether the actual tag is an end tag.</p>
     * <p>As a side effect a correct tag is consumed.</p>
     *
     * @param type The type.
     * @throws IOException  I/O Error..
     * @throws ScannerError Syntax error.
     */
    public void checkEnd(String type)
            throws ScannerError, IOException {
        switch (scanner.getRes()) {
            case XmlMachine.RES_TEXT:
                throw new ScannerError(DOM_MISSING_END,
                        OpenOpts.getOffset(scanner.getReader()));
            case XmlMachine.RES_TAG:
                String temp = scanner.getType();
                if (temp.length() == 0 ||
                        temp.charAt(0) != XmlMachine.CHAR_SLASH)
                    throw new ScannerError(DOM_MISSING_END,
                            OpenOpts.getOffset(scanner.getReader()));
                if (scanner.getAttrCount() != 0)
                    throw new ScannerError(DOM_UNEXPECTED_ATTR,
                            OpenOpts.getOffset(scanner.getReader()));
                temp = temp.substring(1);
                if (!type.equals(temp))
                    throw new ScannerError(DOM_MISMATCHED_END,
                            OpenOpts.getOffset(scanner.getReader()));
                nextTagOrText();
                break;
            case XmlMachine.RES_EOF:
                throw new ScannerError(DOM_MISSING_END,
                        OpenOpts.getOffset(scanner.getReader()));
            default:
                throw new IllegalArgumentException("illegal res");
        }
    }

    /**********************************************************/
    /* Text & Element Reading                                 */
    /**********************************************************/

    /**
     * <p>Get the next tag.</p>
     *
     * @throws IOException  I/O Error..
     * @throws ScannerError Syntax error.
     */
    public void nextTagOrText() throws IOException, ScannerError {
        scanner.nextTagOrText();
        for (; ; ) {
            switch (scanner.getRes()) {
                case XmlMachine.RES_TEXT:
                    if ((getMask() & AbstractDom.MASK_TEXT) != 0)
                        return;
                    checkWhitespace();
                    scanner.nextTagOrText();
                    break;
                case XmlMachine.RES_TAG:
                    String temp = scanner.getType();
                    if (temp.equals(STRING_BANG_DASH_DASH)) {
                        checkComment();
                        scanner.nextTagOrText();
                        break;
                    } else if (temp.length() > 0 &&
                            temp.charAt(0) == XmlMachine.CHAR_QUESTION) {
                        checkProcInstr();
                        scanner.nextTagOrText();
                        break;
                    } else {
                        return;
                    }
                case XmlMachine.RES_EOF:
                    return;
                default:
                    throw new IllegalArgumentException("illegal res");

            }
        }
    }

    /**
     * <p>Check whether the dom reader is at eof.</p>
     *
     * @throws IOException  I/O Error..
     * @throws ScannerError Syntax error.
     */
    public void checkEof() throws IOException, ScannerError {
        for (; ; ) {
            switch (scanner.getRes()) {
                case XmlMachine.RES_TEXT:
                    if ((getMask() & AbstractDom.MASK_TEXT) != 0)
                        throw new ScannerError(DOM_SUPERFLOUS_TAG,
                                OpenOpts.getOffset(scanner.getReader()));
                    checkWhitespace();
                    scanner.nextTagOrText();
                    break;
                case XmlMachine.RES_TAG:
                    String temp = scanner.getType();
                    if (temp.equals(STRING_BANG_DASH_DASH)) {
                        checkComment();
                        scanner.nextTagOrText();
                        break;
                    } else if (temp.length() > 0 &&
                            temp.charAt(0) == XmlMachine.CHAR_QUESTION) {
                        checkProcInstr();
                        scanner.nextTagOrText();
                        break;
                    } else {
                        throw new ScannerError(DOM_SUPERFLOUS_TAG, OpenOpts.getOffset(scanner.getReader()));
                    }
                case XmlMachine.RES_EOF:
                    return;
                default:
                    throw new IllegalArgumentException("illegal res");
            }
        }
    }

    /**
     * <p>Check whether the tag is a comment tag.</p>
     *
     * @throws ScannerError Syntax error.
     */
    private void checkComment() throws ScannerError {
        int n = scanner.getAttrCount();
        if (n != 0 &&
                "".equals(scanner.getValueAt(n - 1)) &&
                scanner.getAttr(n - 1).equals(STRING_DASH_DASH)) {
            /* do nothing */
        } else {
            throw new ScannerError(DOM_UNBALANCED_COMMENT, OpenOpts.getOffset(scanner.getReader()));
        }
    }

    /**
     * <p>Check whether the tag is a processing instruction tag.</p>
     *
     * @throws ScannerError Syntax error.
     */
    private void checkProcInstr() throws ScannerError {
        int n = scanner.getAttrCount();
        if (n != 0 &&
                "".equals(scanner.getValueAt(n - 1)) &&
                scanner.getAttr(n - 1).length() == 1 &&
                scanner.getAttr(n - 1).charAt(0) == XmlMachine.CHAR_QUESTION) {
            /* do nothing */
        } else {
            throw new ScannerError(DOM_UNBALANCED_PROCINSTR, OpenOpts.getOffset(scanner.getReader()));
        }
    }

    /**
     * <p>Check whether the text is a white space text.</p>
     *
     * @throws ScannerError Syntax error.
     */
    private void checkWhitespace() throws ScannerError {
        char[] buf = scanner.getTextBuf();
        int len = scanner.getTextLen();
        for (int i = 0; i < len; i++) {
            char ch = buf[i];
            if (ch <= XmlMachine.CHAR_SPACE || ch == XmlMachine.CHAR_BOM) {
                // do nothing
            } else {
                throw new ScannerError(DOM_NONE_WHITESPACE, OpenOpts.getOffset(scanner.getReader()));
            }
        }
    }

    /**********************************************************/
    /* Texts & Attribute Values                               */
    /**********************************************************/

    /**
     * <p>Retrieve the actual text.</p>
     * <p>Can be retrieved when the result type is RES_TEXT.</p>
     * <p>It can also be used for result type RES_TAG, and it will
     * then return the full tag.</p>
     *
     * @return The actual text.
     */
    String getTextStrip() {
        if ((getMask() & AbstractDom.MASK_STRP) == 0)
            return scanner.getText();
        char[] buf = scanner.getTextBuf();
        int len = scanner.getTextLen();
        len = stripWhitespace(buf, 0, len);
        return new String(buf, 0, len);
    }

    /**
     * Retrieve the nth attribute value. Can be retrieved
     * when the result type is RES_TAG.</p>
     *
     * @param i The index.
     * @return The attribute value.
     */
    String getValueStripAt(int i) {
        if ((getMask() & AbstractDom.MASK_STRP) == 0)
            return scanner.getValueAt(i);
        char[] buf = scanner.getTextBuf();
        XmlMachineRange xmr = scanner.getValueRange(i);
        int off = xmr.getOff();
        int len = xmr.getLen();
        len = stripWhitespace(buf, off, len);
        return new String(buf, off, len);
    }

    /**
     * <p>Strip superflous white space from text.</p>
     *
     * @param buf The text.
     * @param off The offset.
     * @param len The length.
     * @return The new length;
     */
    private int stripWhitespace(char[] buf, int off, int len) {
        int k = 0;
        boolean lastspace = ((getMask() & AbstractDom.MASK_LTSP) != 0);
        for (int i = 0; i < len; i++) {
            char ch = buf[off + i];
            if (ch <= XmlMachine.CHAR_SPACE ||
                    ch == XmlMachine.CHAR_BOM) {
                if (!lastspace) {
                    buf[off + k] = XmlMachine.CHAR_SPACE;
                    k++;
                    lastspace = true;
                }
            } else {
                buf[off + k] = ch;
                k++;
                lastspace = false;
            }
        }
        if (lastspace) {
            setMask(getMask() | AbstractDom.MASK_LTSP);
        } else {
            setMask(getMask() & ~AbstractDom.MASK_LTSP);
        }
        return k;
    }

}