package matula.util.wire;

import matula.util.data.AssocArray;
import matula.util.data.ListArray;
import matula.util.regex.ScannerError;
import matula.util.system.MimeHeader;

import java.io.IOException;
import java.io.StreamTokenizer;
import java.io.StringReader;

/**
 * <p>The class supports style attributes.</p>
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
public final class StyleAttribute {
    private static final String ERROR_NAME_MISSING = "name missing";
    private static final String ERROR_COLON_MISSING = ": missing";
    private static final String ERROR_VALUE_MISSING = "value missing";
    private static final String ERROR_SUPERFLOUS_TOKEN = "superflous token";

    private AssocArray<String, String> kvs = new AssocArray<String, String>();

    /**
     * <p>Create a style attribute from a string.</p>
     *
     * @param s The string.
     * @throws ScannerError Shit happens.
     */
    public StyleAttribute(String s) throws ScannerError {
        try {
            parse(s);
        } catch (IOException x) {
            throw new RuntimeException("internal error", x);
        }
    }

    /**
     * <p>Retrieve an attribute value.</p>
     * <p>The search is done with ignore case. The first value is returned.</p>
     *
     * @param a The attribute.
     * @return The value, or null.
     */
    public String getValue(String a) {
        for (int i = 0; i < kvs.size(); i++) {
            if (kvs.getKey(i).equalsIgnoreCase(a))
                return kvs.getValue(i);
        }
        return null;
    }

    /**
     * <p>Retrieve an attribute value.</p>
     * <p>The search is done with ignore case. The first value is returned.</p>
     * <p>If no attribute value is found, the default value is returned.</p>
     *
     * @param a The attribute.
     * @param d The default value.
     * @return The value, or null.
     */
    public String getValue(String a, String d) {
        String v = getValue(a);
        if (v == null)
            return d;
        return v;
    }

    /*******************************************************************/
    /* Decode Style Attribute                                          */
    /*******************************************************************/

    /**
     * <p>Parse a style attribute.</p>
     * <p>Quote translation is automatically done by the stream tokenizer.</p>
     *
     * @param s The style attribute.
     * @throws IOException  IO error.
     * @throws ScannerError Shit happens.
     */
    private void parse(String s) throws IOException, ScannerError {
        StreamTokenizer st = new StreamTokenizer(new StringReader(s));
        st.resetSyntax();
        st.wordChars('a', 'z');
        st.wordChars('A', 'Z');
        st.wordChars('0', '9');
        st.wordChars('-', '-');
        st.wordChars('.', '.');
        st.wordChars('*', '*');
        st.wordChars('+', '+');
        st.wordChars('!', '!');
        st.whitespaceChars(0, ' ');
        st.quoteChar('\'');
        st.nextToken();
        for (;;) {
            String a;
            if (st.ttype != StreamTokenizer.TT_WORD)
                throw new ScannerError(ERROR_NAME_MISSING, -1);
            a = st.sval;
            st.nextToken();
            if (st.ttype != ':')
                throw new ScannerError(ERROR_COLON_MISSING, -1);
            st.nextToken();
            String v;
            if (st.ttype == StreamTokenizer.TT_WORD) {
                v = st.sval;
                st.nextToken();
            } else if (st.ttype == '\'') {
                v = st.sval;
                st.nextToken();
            } else {
                throw new ScannerError(ERROR_VALUE_MISSING, -1);
            }
            kvs.add(a, v);
            if (st.ttype == ';') {
                st.nextToken();
            } else {
                break;
            }
        }
        if (st.ttype != StreamTokenizer.TT_EOF)
            throw new ScannerError(ERROR_SUPERFLOUS_TOKEN, -1);
    }

    /*******************************************************************/
    /* Encode Mime Type                                                */
    /*******************************************************************/

    /**
     * <p>Convert a style attribute back to a string.</p>
     * <p>The order of the attribute value pairs is preserved.</p>
     * <p>The case of the attributes and the values is preserved.</p>
     *
     * @return The unparse mime header.
     */
    public String toString() {
        StringBuilder buf = new StringBuilder();
        for (int i = 0; i < kvs.size(); i++) {
            if (i!=0)
                buf.append("; ");
            buf.append(kvs.getKey(i));
            buf.append(": ");
            String v = kvs.getValue(i);
            if (needsQuote(v)) {
                buf.append('\'');
                buf.append(quoteString(v));
                buf.append('\'');
            } else {
                buf.append(v);
            }
        }
        return buf.toString();
    }

    /**
     * <p>Check whether a value needs quotes.</p>
     *
     * @param s The value.
     * @return True if quotes are needed.
     */
    private boolean needsQuote(String s) {
        int n = s.length();
        for (int i = 0; i < n; i++) {
            char ch = s.charAt(i);
            if (ch == '\\' || ch == '\'' ||
                    ch == ':' || ch == ';')
                return true;
        }
        return false;
    }

    /**
     * <p>Quote a value.</p>
     *
     * @param s The value.
     * @return The quoted value.
     */
    private String quoteString(String s) {
        StringBuilder buf = null;
        int n = s.length();
        for (int i = 0; i < n; i++) {
            char ch = s.charAt(i);
            if (ch == '\\' || ch == '\'') {
                if (buf == null)
                    buf = new StringBuilder(s.substring(0, i));
                buf.append('\\');
                buf.append(ch);
            } else {
                if (buf != null)
                    buf.append(ch);
            }
        }
        if (buf != null)
            return buf.toString();
        return s;
    }

    /**
     * <p>Some test cases.</p>
     *
     * @param args Not used.
     */
    /*
    public static void main(String[] args) throws ScannerError {
        String str = "height: 3.7em; width: 12.8em";
        System.out.println("str=" + str);

        StyleAttribute sa = new StyleAttribute(str);
        System.out.println("sa(str)=" + sa);
    }
    */

}