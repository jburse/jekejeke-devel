package matula.util.format;

import matula.util.data.AssocArray;
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

    public static final int STATE_NONE = 0; /* nothing yet */
    public static final int STATE_TEXT = 1; /* normal text */
    public static final int STATE_ENTITY = 2; /* entity text */
    public static final int STATE_TYPE = 3; /* inside element name */
    public static final int STATE_SNIPPET = 4; /* inside Java snippet */
    public static final int STATE_PRE = 5; /* before Java snippet end */
    public static final int STATE_BLANK = 6; /* before attribute */
    public static final int STATE_ATTR = 7; /* inside attribute name */
    public static final int STATE_ATTR_DOUBLE = 8; /* inside double quote attribute name */
    public static final int STATE_ATTR_SINGLE = 9; /* inside single quote attribute name */
    public static final int STATE_VALUE = 10; /* inside name token */
    public static final int STATE_VALUE_DOUBLE = 11; /* inside double quote literal */
    public static final int STATE_VALUE_SINGLE = 12; /* inside single quote literal */
    public static final int STATE_BEFORE = 13; /* before equal sign */
    public static final int STATE_AFTER = 14; /* after equal sign */

    private static final String XML_ILLEGAL_ATTR = "xml_illegal_attr";
    private static final String XML_DUPLICATE_ATTR = "xml_duplicate_attr";
    private static final String XML_BUFFER_OVERFLOW = "xml_buffer_overflow";
    private static final String XML_PREMATURE_END = "xml_premature_end";

    public static final String VALUE_EMPTY = "";

    public static final char CHAR_AMPER = '&';
    private static final char CHAR_OPEN = '<';
    public static final char CHAR_SPACE = ' ';
    public static final char CHAR_BOM = 0xFEFF;
    public static final char CHAR_DOUBLE = '"';
    public static final char CHAR_SINGLE = '\'';
    public static final char CHAR_SLASH = '/';
    private static final char CHAR_CLOSE = '>';
    private static final int CHAR_EOF = -1;
    private static final char CHAR_PERCENT = '%';
    private static final char CHAR_EQ = '=';
    public static final char CHAR_SEMI = ';';
    public static final char CHAR_BANG = '!';
    public static final char CHAR_QUESTION = '?';

    private char[] text = new char[MAX_JUNK];
    private int top;
    private int off;
    private int res = XmlMachine.RES_NONE;
    private int state = XmlMachine.STATE_NONE;
    private boolean ignore;
    private String type = VALUE_EMPTY;
    private String key;
    private AssocArray<String, String> kvs = new AssocArray<String, String>();

    /**
     * <p>Type call-back.</p>
     *
     * @param t The type.
     */
    private void saxType(String t) {
        if (t.length() > 0 && t.charAt(0) == CHAR_BANG) {
            ignore = true;
        } else {
            ignore = false;
        }
        type = t;
    }

    /**
     * <p>Key call-back.</p>
     *
     * @param k The key.
     * @throws ScannerError Syntax error,
     */
    private void saxKey(String k) throws ScannerError {
        if (!ignore) {
            if (XmlMachine.isQuoted(k))
                throw new ScannerError(XML_ILLEGAL_ATTR, -1);
            if (XmlMachine.isNumber(k))
                throw new ScannerError(XML_ILLEGAL_ATTR, -1);
            if (XmlMachine.indexAttr(kvs, k) != -1)
                throw new ScannerError(XML_DUPLICATE_ATTR, -1);
        }
        key = k;
    }

    /**
     * <p>Value call-back.</p>
     *
     * @param v The value.
     */
    private void saxValue(String v) throws ScannerError {
        kvs.add(key, v);
    }


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
    private void fill(int ch) throws ScannerError {
        if (top < MAX_JUNK) {
            text[top++] = (char) ch;
        } else {
            throw new ScannerError(XML_BUFFER_OVERFLOW, -1);
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
                if (ch == CHAR_OPEN) {
                    state = XmlMachine.STATE_TYPE;
                    off = top + 1;
                    kvs.clear();
                } else if (ch == CHAR_EOF) {
                    res = XmlMachine.RES_EOF;
                    return false;
                } else if (ch == CHAR_AMPER) {
                    state = XmlMachine.STATE_ENTITY;
                } else {
                    state = XmlMachine.STATE_TEXT;
                }
                break;
            case XmlMachine.STATE_TEXT:
                if (ch == CHAR_OPEN) {
                    res = XmlMachine.RES_TEXT;
                    return false;
                } else if (ch == CHAR_EOF) {
                    res = XmlMachine.RES_TEXT;
                    return false;
                } else if (ch == CHAR_AMPER) {
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
                if (ch == CHAR_OPEN) {
                    res = XmlMachine.RES_TEXT;
                    return false;
                } else if (ch == CHAR_EOF) {
                    res = XmlMachine.RES_TEXT;
                    return false;
                } else if (ch == CHAR_AMPER) {
                    if (top >= MAX_JUNK - ForeignXml.MAX_ENTITY) {
                        res = XmlMachine.RES_TEXT;
                        return false;
                    } else {
                        off = top;
                    }
                } else if (top - off >= ForeignXml.MAX_ENTITY ||
                        ch == CHAR_SEMI || ch <= CHAR_SPACE ||
                        ch == CHAR_BOM) {
                    state = XmlMachine.STATE_TEXT;
                }
                break;
            case XmlMachine.STATE_TYPE:
                if (ch == CHAR_EOF) {
                    if (top == off) {
                        res = XmlMachine.RES_TEXT;
                        return false;
                    } else {
                        throw new ScannerError(XML_PREMATURE_END, -1);
                    }
                } else if (ch <= CHAR_SPACE || ch == CHAR_BOM) {
                    if (top == off) {
                        state = XmlMachine.STATE_TEXT;
                    } else {
                        saxType(new String(text, off, top - off));
                        state = XmlMachine.STATE_BLANK;
                    }
                } else if (ch == CHAR_SLASH || ch == CHAR_QUESTION) {
                    if (top == off) {
                        /* */
                    } else {
                        saxType(new String(text, off, top - off));
                        off = top;
                        state = XmlMachine.STATE_ATTR;
                    }
                } else if (ch == CHAR_DOUBLE) {
                    saxType(new String(text, off, top - off));
                    off = top;
                    state = XmlMachine.STATE_ATTR_DOUBLE;
                } else if (ch == CHAR_SINGLE) {
                    saxType(new String(text, off, top - off));
                    off = top;
                    state = XmlMachine.STATE_ATTR_SINGLE;
                } else if (ch == CHAR_CLOSE) {
                    if (top == off) {
                        state = XmlMachine.STATE_TEXT;
                    } else {
                        saxType(new String(text, off, top - off));
                        fill(ch);
                        res = XmlMachine.RES_TAG;
                        return true;
                    }
                } else if (off == top && ch == CHAR_PERCENT) {
                    state = XmlMachine.STATE_SNIPPET;
                } else {
                    /* */
                }
                break;
            case XmlMachine.STATE_SNIPPET:
                if (ch == CHAR_EOF) {
                    throw new ScannerError(XML_PREMATURE_END, -1);
                } else if (ch == CHAR_PERCENT) {
                    state = XmlMachine.STATE_PRE;
                } else {
                    /* */
                }
                break;
            case XmlMachine.STATE_PRE:
                if (ch == CHAR_EOF) {
                    throw new ScannerError(XML_PREMATURE_END, -1);
                } else if (ch == CHAR_CLOSE) {
                    type = new String(text, off, top - off);
                    fill(ch);
                    res = XmlMachine.RES_TAG;
                    return true;
                } else if (ch == CHAR_PERCENT) {
                    /* */
                } else {
                    state = XmlMachine.STATE_SNIPPET;
                }
                break;
            case XmlMachine.STATE_BLANK:
                if (ch == CHAR_EOF) {
                    throw new ScannerError(XML_PREMATURE_END, -1);
                } else if (ch <= CHAR_SPACE || ch == CHAR_BOM) {
                    /* */
                } else if (ch == CHAR_DOUBLE) {
                    off = top;
                    state = XmlMachine.STATE_ATTR_DOUBLE;
                } else if (ch == CHAR_SINGLE) {
                    off = top;
                    state = XmlMachine.STATE_ATTR_SINGLE;
                } else if (ch == CHAR_CLOSE) {
                    fill(ch);
                    res = XmlMachine.RES_TAG;
                    return true;
                } else {
                    off = top;
                    state = XmlMachine.STATE_ATTR;
                }
                break;
            case XmlMachine.STATE_ATTR:
                if (ch == CHAR_EOF) {
                    throw new ScannerError(XML_PREMATURE_END, -1);
                } else if (ch <= CHAR_SPACE || ch == CHAR_BOM) {
                    saxKey(new String(text, off, top - off));
                    state = XmlMachine.STATE_BEFORE;
                } else if (ch == CHAR_SLASH || ch == CHAR_QUESTION) {
                    if (top == off) {
                        /* */
                    } else {
                        saxKey(new String(text, off, top - off));
                        saxValue(VALUE_EMPTY);
                        key = null;
                        off = top;
                    }
                } else if (ch == CHAR_EQ) {
                    saxKey(new String(text, off, top - off));
                    state = XmlMachine.STATE_AFTER;
                } else if (ch == CHAR_DOUBLE) {
                    saxKey(new String(text, off, top - off));
                    saxValue(VALUE_EMPTY);
                    key = null;
                    off = top;
                    state = XmlMachine.STATE_ATTR_DOUBLE;
                } else if (ch == CHAR_SINGLE) {
                    saxKey(new String(text, off, top - off));
                    saxValue(VALUE_EMPTY);
                    key = null;
                    off = top;
                    state = XmlMachine.STATE_ATTR_SINGLE;
                } else if (ch == CHAR_CLOSE) {
                    saxKey(new String(text, off, top - off));
                    saxValue(VALUE_EMPTY);
                    key = null;
                    fill(ch);
                    res = XmlMachine.RES_TAG;
                    return true;
                } else {
                    /* */
                }
                break;
            case XmlMachine.STATE_ATTR_DOUBLE:
                if (ch == CHAR_EOF) {
                    throw new ScannerError(XML_PREMATURE_END, -1);
                } else if (ch == CHAR_DOUBLE) {
                    fill(ch);
                    saxKey(new String(text, off, top - off));
                    state = XmlMachine.STATE_BEFORE;
                    return true;
                } else {
                    /* */
                }
                break;
            case XmlMachine.STATE_ATTR_SINGLE:
                if (ch == CHAR_EOF) {
                    throw new ScannerError(XML_PREMATURE_END, -1);
                } else if (ch == CHAR_SINGLE) {
                    fill(ch);
                    saxKey(new String(text, off, top - off));
                    state = XmlMachine.STATE_BEFORE;
                    return true;
                } else {
                    /* */
                }
                break;
            case XmlMachine.STATE_VALUE:
                if (ch == CHAR_EOF) {
                    throw new ScannerError(XML_PREMATURE_END, -1);
                } else if (ch <= CHAR_SPACE || ch == CHAR_BOM) {
                    String temp = new String(text, off, top - off);
                    saxValue(temp);
                    key = null;
                    state = XmlMachine.STATE_BLANK;
                } else if (ch == CHAR_SLASH || ch == CHAR_QUESTION) {
                    String temp = new String(text, off, top - off);
                    saxValue(temp);
                    key = null;
                    off = top;
                    state = XmlMachine.STATE_ATTR;
                } else if (ch == CHAR_DOUBLE) {
                    String temp = new String(text, off, top - off);
                    saxValue(temp);
                    key = null;
                    off = top;
                    state = XmlMachine.STATE_ATTR_DOUBLE;
                } else if (ch == CHAR_SINGLE) {
                    String temp = new String(text, off, top - off);
                    saxValue(temp);
                    key = null;
                    off = top;
                    state = XmlMachine.STATE_ATTR_SINGLE;
                } else if (ch == CHAR_CLOSE) {
                    String temp = new String(text, off, top - off);
                    saxValue(temp);
                    key = null;
                    fill(ch);
                    res = XmlMachine.RES_TAG;
                    return true;
                } else {
                    /* */
                }
                break;
            case XmlMachine.STATE_VALUE_DOUBLE:
                if (ch == CHAR_EOF) {
                    throw new ScannerError(XML_PREMATURE_END, -1);
                } else if (ch == CHAR_DOUBLE) {
                    fill(ch);
                    String temp = new String(text, off, top - off);
                    saxValue(temp);
                    key = null;
                    state = XmlMachine.STATE_BLANK;
                    return true;
                } else {
                    /* */
                }
                break;
            case XmlMachine.STATE_VALUE_SINGLE:
                if (ch == CHAR_EOF) {
                    throw new ScannerError(XML_PREMATURE_END, -1);
                } else if (ch == CHAR_SINGLE) {
                    fill(ch);
                    String temp = new String(text, off, top - off);
                    saxValue(temp);
                    key = null;
                    state = XmlMachine.STATE_BLANK;
                    return true;
                } else {
                    /* */
                }
                break;
            case XmlMachine.STATE_BEFORE:
                if (ch == CHAR_EOF) {
                    throw new ScannerError(XML_PREMATURE_END, -1);
                } else if (ch <= CHAR_SPACE || ch == CHAR_BOM) {
                    /* */
                } else if (ch == CHAR_EQ) {
                    state = XmlMachine.STATE_AFTER;
                } else if (ch == CHAR_DOUBLE) {
                    saxValue(VALUE_EMPTY);
                    key = null;
                    off = top;
                    state = XmlMachine.STATE_ATTR_DOUBLE;
                } else if (ch == CHAR_SINGLE) {
                    saxValue(VALUE_EMPTY);
                    key = null;
                    off = top;
                    state = XmlMachine.STATE_ATTR_SINGLE;
                } else if (ch == CHAR_CLOSE) {
                    saxValue(VALUE_EMPTY);
                    key = null;
                    fill(ch);
                    res = XmlMachine.RES_TAG;
                    return true;
                } else {
                    saxValue(VALUE_EMPTY);
                    key = null;
                    off = top;
                    state = XmlMachine.STATE_ATTR;
                }
                break;
            case XmlMachine.STATE_AFTER:
                if (ch == CHAR_EOF) {
                    throw new ScannerError(XML_PREMATURE_END, -1);
                } else if (ch <= CHAR_SPACE || ch == CHAR_BOM) {
                    /* */
                } else if (ch == CHAR_SLASH || ch == CHAR_QUESTION) {
                    saxValue(VALUE_EMPTY);
                    key = null;
                    off = top;
                    state = XmlMachine.STATE_ATTR;
                } else if (ch == CHAR_DOUBLE) {
                    off = top;
                    state = XmlMachine.STATE_VALUE_DOUBLE;
                } else if (ch == CHAR_SINGLE) {
                    off = top;
                    state = XmlMachine.STATE_VALUE_SINGLE;
                } else if (ch == CHAR_CLOSE) {
                    saxValue(VALUE_EMPTY);
                    key = null;
                    fill(ch);
                    res = XmlMachine.RES_TAG;
                    return true;
                } else {
                    off = top;
                    state = XmlMachine.STATE_VALUE;
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
        return type.equalsIgnoreCase(t);
    }

    /**
     * <p>Retrieve the number of attributes. Can be retrieved when the
     * result type is RES_TAG.</p>
     *
     * @return The number of attributes.
     */
    public int getAttrCount() {
        return kvs.size();
    }

    /**
     * <p>Retrieve the nth attribute name. Can be retrieved when the
     * result type is RES_TAG.</p>
     *
     * @param i The index.
     * @return The attribute name.
     */
    public String getAttr(int i) {
        return kvs.getKey(i);
    }

    /**
     * <p>Retrieve the nth attribute value. Can be retrieved when the
     * result type is RES_TAG.</p>
     *
     * @param i The index.
     * @return The attribute value.
     */
    public String getValueAt(int i) {
        return kvs.getValue(i);
    }

    /**
     * <p>Find the index of an attribute.</p>
     * <p>The case of the attribute is ignored.</p>
     *
     * @param a The attribute.
     * @return The index, or -1.
     */
    public int indexAttr(String a) {
        return indexAttr(kvs, a);
    }

    /**
     * <p>Remove an attribute value pair.</p>
     *
     * @param i The index.
     */
    public void removeAttrValue(int i) {
        kvs.removeEntry(i);
        kvs.resize();
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
        int k = indexAttr(kvs, a);
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

    /**************************************************************/
    /* Attribute Helper                                           */
    /**************************************************************/

    /**
     * <p>Find the index of an attribute.</p>
     * <p>The case of the attribute is ignored.</p>
     *
     * @param a The attribute.
     * @return The index, or -1.
     */
    public static <V> int indexAttr(AssocArray<String, V> l, String a) {
        int m = l.size();
        for (int i = 0; i < m; i++) {
            String n = l.getKey(i);
            if (n.equalsIgnoreCase(a))
                return i;
        }
        return -1;
    }

    /**
     * <p>Check whether a string is quoted.</p>
     *
     * @param v The string.
     * @return True if its quoted, otherwise false.
     */
    public static boolean isQuoted(String v) {
        return (v.length() > 1 &&
                (v.charAt(0) == CHAR_DOUBLE || v.charAt(0) == CHAR_SINGLE) &&
                v.charAt(v.length() - 1) == v.charAt(0));
    }

    /**
     * <p>Strip the value by single or double quotes.</P>
     *
     * @param v The value.
     * @return The stripped value.
     */
    public static String stripValue(String v) {
        if (isQuoted(v)) {
            return v.substring(1, v.length() - 1);
        } else {
            return v;
        }
    }

    /**
     * <p>Check whether the value starts as a number.</p>
     *
     * @param v The value.
     * @return True if the value starts as a number, otherwise false.
     */
    public static boolean isNumber(String v) {
        return (v.length() > 0 && (Character.isDigit(v.codePointAt(0)) ||
                (v.charAt(0) == '-' && v.length() > 1 &&
                        Character.isDigit(v.codePointAt(1)))));
    }

}
