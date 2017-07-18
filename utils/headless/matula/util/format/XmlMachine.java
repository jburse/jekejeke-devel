package matula.util.format;

import matula.util.data.ListArray;
import matula.util.regex.ScannerError;
import matula.util.system.ForeignXml;

/**
 * <p>The base class for a xml state machines.</p>
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
public class XmlMachine {
    public static final int MAX_JUNK = 8192;

    public static final int RES_EOF = 0;
    public static final int RES_TEXT = 1;
    public static final int RES_TAG = 2;
    public static final int RES_NONE = 3;

    public static final int STATE_NONE = 0;
    public static final int STATE_TEXT = 1;
    public static final int STATE_ENTITY = 2;
    public static final int STATE_TYPE = 3;
    public static final int STATE_SNIPPET = 4;
    public static final int STATE_PRE = 5;
    public static final int STATE_BLANK = 6;
    public static final int STATE_ATTR = 7;
    public static final int STATE_VALUE = 8;
    public static final int STATE_DOUBLE = 9;
    public static final int STATE_SINGLE = 10;
    public static final int STATE_AFTER = 11;
    public static final int STATE_BEFORE = 12;

    public static final String XML_BUFFER_OVERFLOW = "xml_buffer_overflow";
    public static final String XML_PREMATURE_END = "xml_premature_end";

    public static final String VALUE_EMPTY = "";

    private char[] text = new char[MAX_JUNK];
    private int top;
    private String type;
    private int off;
    private ListArray<String> attr = new ListArray<String>();
    private ListArray<String> value = new ListArray<String>();
    private int res = XmlMachine.RES_NONE;
    private int state = XmlMachine.STATE_NONE;

    /**
     * Reset the state machine.
     */
    public void startTagOrText() {
        state = XmlMachine.STATE_NONE;
        res = XmlMachine.RES_NONE;
        top = 0;
    }

    /**
     * <p>Fill the character into the local buffer.</p>
     *
     * @param ch The character.
     * @throws RuntimeException When the buffer is overrun.
     */
    private void fill(int ch) {
        if (top < MAX_JUNK) {
            text[top++] = (char) ch;
        } else {
            throw new RuntimeException(XML_BUFFER_OVERFLOW);
        }
    }

    /**
     * Advance the state machine.
     *
     * @param ch The next character.
     * @return True if the character was consumed, otherwise false.
     * @throws ScannerError When the buffer is overrun.
     */
    public boolean consume(int ch) throws ScannerError {
        switch (state) {
            case XmlMachine.STATE_NONE:
                if (ch == '<') {
                    state = XmlMachine.STATE_TYPE;
                    off = top + 1;
                    attr.clear();
                    value.clear();
                } else if (ch == -1) {
                    res = XmlMachine.RES_EOF;
                    return false;
                } else if (ch == '&') {
                    state = XmlMachine.STATE_ENTITY;
                } else {
                    state = XmlMachine.STATE_TEXT;
                }
                break;
            case XmlMachine.STATE_TEXT:
                if (ch == '<') {
                    res = XmlMachine.RES_TEXT;
                    return false;
                } else if (ch == -1) {
                    res = XmlMachine.RES_TEXT;
                    return false;
                } else if (ch == '&') {
                    if (top >= MAX_JUNK - ForeignXml.MAX_ENTITY) {
                        res = XmlMachine.RES_TEXT;
                        return false;
                    } else {
                        off = top;
                        state = XmlMachine.STATE_ENTITY;
                    }
                } else if (!Character.isHighSurrogate((char) ch)) {
                    if (top >= MAX_JUNK - ForeignXml.MAX_ENTITY) {
                        res = XmlMachine.RES_TEXT;
                        return false;
                    }
                }
                break;
            case XmlMachine.STATE_ENTITY:
                if (ch == '<') {
                    res = XmlMachine.RES_TEXT;
                    return false;
                } else if (ch == -1) {
                    res = XmlMachine.RES_TEXT;
                    return false;
                } else if (ch == '&') {
                    if (top >= MAX_JUNK - ForeignXml.MAX_ENTITY) {
                        res = XmlMachine.RES_TEXT;
                        return false;
                    } else {
                        off = top;
                    }
                } else if (top - off >= ForeignXml.MAX_ENTITY ||
                        ch == ';' || ch <= ' ') {
                    state = XmlMachine.STATE_TEXT;
                }
                break;
            case XmlMachine.STATE_TYPE:
                if (ch == -1) {
                    if (top == off) {
                        res = XmlMachine.RES_TEXT;
                        return false;
                    } else {
                        throw new ScannerError(XML_PREMATURE_END);
                    }
                } else if (ch <= ' ') {
                    if (top == off) {
                        state = XmlMachine.STATE_TEXT;
                    } else {
                        type = new String(text, off, top - off);
                        state = XmlMachine.STATE_BLANK;
                    }
                } else if (ch == '/') {
                    if (top == off) {
                        /* */
                    } else {
                        type = new String(text, off, top - off);
                        off = top;
                        state = XmlMachine.STATE_ATTR;
                    }
                } else if (ch == '>') {
                    if (top == off) {
                        state = XmlMachine.STATE_TEXT;
                    } else {
                        type = new String(text, off, top - off);
                        fill(ch);
                        res = XmlMachine.RES_TAG;
                        return true;
                    }
                } else if (off == top && ch == '%') {
                    state = XmlMachine.STATE_SNIPPET;
                } else {
                    /* */
                }
                break;
            case XmlMachine.STATE_SNIPPET:
                if (ch == -1) {
                    throw new ScannerError(XML_PREMATURE_END);
                } else if (ch == '%') {
                    state = XmlMachine.STATE_PRE;
                } else {
                    /* */
                }
                break;
            case XmlMachine.STATE_PRE:
                if (ch == -1) {
                    throw new ScannerError(XML_PREMATURE_END);
                } else if (ch == '>') {
                    type = new String(text, off, top - off);
                    fill(ch);
                    res = XmlMachine.RES_TAG;
                    return true;
                } else if (ch == '%') {
                    /* */
                } else {
                    state = XmlMachine.STATE_SNIPPET;
                }
                break;
            case XmlMachine.STATE_BLANK:
                if (ch == -1) {
                    throw new ScannerError(XML_PREMATURE_END);
                } else if (ch <= ' ') {
                    /* */
                } else if (ch == '>') {
                    fill(ch);
                    res = XmlMachine.RES_TAG;
                    return true;
                } else {
                    off = top;
                    state = XmlMachine.STATE_ATTR;
                }
                break;
            case XmlMachine.STATE_ATTR:
                if (ch == -1) {
                    throw new ScannerError(XML_PREMATURE_END);
                } else if (ch <= ' ') {
                    String temp = new String(text, off, top - off);
                    attr.add(temp);
                    state = XmlMachine.STATE_BEFORE;
                } else if (ch == '/') {
                    if (top == off) {
                        /* */
                    } else {
                        String temp = new String(text, off, top - off);
                        attr.add(temp);
                        state = XmlMachine.STATE_BEFORE;
                    }
                } else if (ch == '=') {
                    String temp = new String(text, off, top - off);
                    attr.add(temp);
                    state = XmlMachine.STATE_AFTER;
                } else if (ch == '>') {
                    String temp = new String(text, off, top - off);
                    attr.add(temp);
                    value.add(VALUE_EMPTY);
                    fill(ch);
                    res = XmlMachine.RES_TAG;
                    return true;
                } else {
                    /* */
                }
                break;
            case XmlMachine.STATE_BEFORE:
                if (ch == -1) {
                    throw new ScannerError(XML_PREMATURE_END);
                } else if (ch <= ' ') {
                    /* */
                } else if (ch == '>') {
                    value.add(VALUE_EMPTY);
                    fill(ch);
                    res = XmlMachine.RES_TAG;
                    return true;
                } else if (ch == '=') {
                    state = XmlMachine.STATE_AFTER;
                } else {
                    value.add(VALUE_EMPTY);
                    off = top;
                    state = XmlMachine.STATE_ATTR;
                }
                break;
            case XmlMachine.STATE_AFTER:
                if (ch == -1) {
                    throw new ScannerError(XML_PREMATURE_END);
                } else if (ch <= ' ') {
                    /* */
                } else if (ch == '>') {
                    value.add(VALUE_EMPTY);
                    fill(ch);
                    res = XmlMachine.RES_TAG;
                    return true;
                } else if (ch == '"') {
                    off = top;
                    state = XmlMachine.STATE_DOUBLE;
                } else if (ch == '\'') {
                    off = top;
                    state = XmlMachine.STATE_SINGLE;
                } else {
                    off = top;
                    state = XmlMachine.STATE_VALUE;
                }
                break;
            case XmlMachine.STATE_VALUE:
                if (ch == -1) {
                    throw new ScannerError(XML_PREMATURE_END);
                } else if (ch <= ' ') {
                    String temp = new String(text, off, top - off);
                    value.add(temp);
                    state = XmlMachine.STATE_BLANK;
                } else if (ch == '>') {
                    String temp = new String(text, off, top - off);
                    value.add(temp);
                    fill(ch);
                    res = XmlMachine.RES_TAG;
                    return true;
                } else {
                    /* */
                }
                break;
            case XmlMachine.STATE_DOUBLE:
                if (ch == -1) {
                    throw new ScannerError(XML_PREMATURE_END);
                } else if (ch == '"') {
                    fill(ch);
                    String temp = new String(text, off, top - off);
                    value.add(temp);
                    state = XmlMachine.STATE_BLANK;
                    return true;
                } else {
                    /* */
                }
                break;
            case XmlMachine.STATE_SINGLE:
                if (ch == -1) {
                    throw new ScannerError(XML_PREMATURE_END);
                } else if (ch == '\'') {
                    fill(ch);
                    String temp = new String(text, off, top - off);
                    value.add(temp);
                    state = XmlMachine.STATE_BLANK;
                    return true;
                } else {
                    /* */
                }
                break;
            default:
                throw new IllegalArgumentException("illegal state");
        }
        fill(ch);
        return true;
    }

    /**
     * Retrieve the actual result type. Result type can be
     * either RES_EOF, RES_TEXT or RES_TAG.
     *
     * @return The result type.
     */
    public int getRes() {
        return res;
    }

    /**
     * <p>Retrieve the actual text.</p>
     * <p>Can be retrieved when the result type is RES_TEXT.</p>
     * <p>It can also be used for result type RES_TAG, and it will the return the full tag.</p>
     *
     * @return The actual text.
     */
    public String getText() {
        return new String(text, 0, top);
    }

    /**
     * For efficiency, we allow to return the text buf.
     *
     * @return The text buf.
     */
    public char[] getTextBuf() {
        return text;
    }

    /**
     * For efficiency, we allow to return the text len.
     *
     * @return The text len.
     */
    public int getTextLen() {
        return top;
    }

    /**
     * <p>Retrieve the actual tag type. can be retrieved when
     * the result type is RES_TAG.</p>
     *
     * @return The actual tag type.
     */
    String getType() {
        return type;
    }

    /**
     * <p>Set the tag type.</p>
     *
     * @param t The tag type.
     */
    public void setType(String t) {
        type = t;
    }

    /**
     * <p>Convenience method to check the actual tag type.</p>
     * <p>Will check the actual tag type ignoring case.</p>
     *
     * @param t The tag type.
     * @return true if actual tag type matches parameter.
     */
    public boolean isType(String t) {
        return t.equalsIgnoreCase(type);
    }

    /**
     * <p>Retrieve the number of attributes. Can be retrieved when the
     * result type is RES_TAG.</p>
     *
     * @return The number of attributes.
     */
    public int getAttrCount() {
        return attr.size();
    }

    /**
     * <p>Retrieve the nth attribute name. Can be retrieved when the
     * result type is RES_TAG.</p>
     *
     * @param i The index.
     * @return The attribute name.
     */
    public String getAttr(int i) {
        return attr.get(i);
    }

    /**
     * <p>Retrieve the nth attribute value. Can be retrieved when the
     * result type is RES_TAG.</p>
     *
     * @param i The index.
     * @return The attribute value.
     */
    public String getValueAt(int i) {
        return value.get(i);
    }

    /**
     * <p>Find the index of an attribute.</p>
     * <p>The case of the attribute is ignored.</p>
     *
     * @param a The attribute.
     * @return The index, or -1.
     */
    public int indexAttr(String a) {
        int m = attr.size();
        for (int i = 0; i < m; i++) {
            String n = attr.get(i);
            if (n.equalsIgnoreCase(a))
                return i;
        }
        return -1;
    }

    /**
     * <p>Remove an attribute value pair.</p>
     *
     * @param i The index.
     */
    public void removeAttrValue(int i) {
        attr.remove(i);
        value.remove(i);
    }

    /**
     * <p>Strip the value by single or double quotes.</P>
     *
     * @param v The value.
     * @return The stripped value.
     */
    public static String stripValue(String v) {
        if (v.length() > 1 && v.charAt(0) == '"' &&
                v.charAt(v.length() - 1) == '"') {
            return v.substring(1, v.length() - 1);
        } else if (v.length() > 1 && v.charAt(0) == '\'' &&
                v.charAt(v.length() - 1) == '\'') {
            return v.substring(1, v.length() - 1);
        } else {
            return v;
        }
    }

    /**
     * <p>Retrieve an actual tag argument.</p>
     * <p>Can be retrieved when the result type is RES_TAG.</p>
     * <p>The attribute name will be matched ignore case.</p>
     *
     * @param a The attribute name
     * @return The value or null if attribute is not present.
     */
    public String getValue(String a) {
        int k = indexAttr(a);
        if (k == -1) return null;
        return ForeignXml.sysTextUnescape(stripValue(getValueAt(k)));
    }

    /**
     * <p>Retrieve the an attribute value.</p>
     *
     * @param a The attribute name.
     * @param d The default attribute value.
     * @return The attribute value.
     */
    public String getValue(String a, String d) {
        String v = getValue(a);
        if (v == null) v = d;
        return v;
    }

}
