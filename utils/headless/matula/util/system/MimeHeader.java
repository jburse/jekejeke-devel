package matula.util.system;

import matula.util.data.ListArray;
import matula.util.regex.ScannerError;

import java.io.IOException;
import java.io.StreamTokenizer;
import java.io.StringReader;

/**
 * <p>The class supports mime headers.</p>
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
public final class MimeHeader {
    private static final String ERROR_NAME_MISSING = "name missing";
    private static final String ERROR_SLASH_MISSING = "/ missing";
    private static final String ERROR_EQUAL_MISSING = "= missing";
    private static final String ERROR_VALUE_MISSING = "value missing";
    private static final String ERROR_SUPERFLOUS_TOKEN = "superflous token";

    public static final String MIME_CHARSET = "charset";

    private String type = "";
    private String subtype = "";
    private ListArray<String> attr = new ListArray<String>();
    private ListArray<String> val = new ListArray<String>();

    /**
     * <p>Create a mime header from a string.</p>
     *
     * @param s The string.
     * @throws ScannerError Shit happens.
     */
    public MimeHeader(String s) throws ScannerError {
        try {
            parse(s);
        } catch (IOException x) {
            throw new RuntimeException("internal error", x);
        }
    }

    /**
     * <p>Retrieve the type.</p>
     *
     * @return The type.
     */
    public String getType() {
        return type;
    }

    /**
     * <p>Retrieve the sub type.</p>
     *
     * @return The sub type.
     */
    public String getSubType() {
        return subtype;
    }

    /**
     * <p>Retrieve an attribute value.</p>
     * <p>The search is done with ignore case. The first value is returned.</p>
     *
     * @param a The attribute.
     * @return The value, or null.
     */
    public String getValue(String a) {
        for (int i = 0; i < attr.size(); i++) {
            if (attr.get(i).equalsIgnoreCase(a))
                return val.get(i);
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

    /**
     * <p>Parse a mime header.</p>
     * <p>Quante translation is automatically done by the stream tokenizer.</p>
     *
     * @param s The mime header.
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
        st.quoteChar('"');
        st.nextToken();
        if (st.ttype != StreamTokenizer.TT_WORD)
            throw new ScannerError(ERROR_NAME_MISSING, -1);
        type = st.sval;
        st.nextToken();
        if (st.ttype != '/')
            throw new ScannerError(ERROR_SLASH_MISSING, -1);
        st.nextToken();
        if (st.ttype != StreamTokenizer.TT_WORD)
            throw new ScannerError(ERROR_NAME_MISSING, -1);
        subtype = st.sval;
        st.nextToken();
        while (st.ttype == ';') {
            st.nextToken();
            String a;
            if (st.ttype != StreamTokenizer.TT_WORD)
                throw new ScannerError(ERROR_NAME_MISSING, -1);
            a = st.sval;
            st.nextToken();
            if (st.ttype != '=')
                throw new ScannerError(ERROR_EQUAL_MISSING, -1);
            st.nextToken();
            String v;
            if (st.ttype == StreamTokenizer.TT_WORD) {
                v = st.sval;
                st.nextToken();
            } else if (st.ttype == '"') {
                v = st.sval;
                st.nextToken();
            } else {
                throw new ScannerError(ERROR_VALUE_MISSING, -1);
            }
            attr.add(a);
            val.add(v);
        }
        if (st.ttype != StreamTokenizer.TT_EOF)
            throw new ScannerError(ERROR_SUPERFLOUS_TOKEN, -1);
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
            if (ch == '\\' || ch == '"' ||
                    ch == '/' || ch == '=' || ch == ';')
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
            if (ch == '\\' || ch == '"') {
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
     * <p>Convert a mime header back to a string.</p>
     * <p>The order of the attribute value pairs is preserved.</p>
     * <p>The case of the attributes and the values is preserved.</p>
     *
     * @return The unparse mime header.
     */
    public String toString() {
        StringBuilder buf = new StringBuilder();
        buf.append(type);
        buf.append("/");
        buf.append(subtype);
        for (int i = 0; i < attr.size(); i++) {
            buf.append("; ");
            buf.append(attr.get(i));
            buf.append("=");
            String v = val.get(i);
            if (needsQuote(v)) {
                buf.append('"');
                buf.append(quoteString(v));
                buf.append('"');
            } else {
                buf.append(v);
            }
        }
        return buf.toString();
    }

    /**
     * <p>Some test cases.</p>
     *
     * @param args Not used.
     */
    public static void main(String[] args) throws ScannerError {
        String str = "text/plain; charset=UTF-8";
        System.out.println("str=" + str);

        MimeHeader mime = new MimeHeader(str);
        System.out.println("mime(str)=" + mime);
    }

}
