package matula.util.system;

import matula.util.data.MapHash;
import matula.util.format.XmlMachine;

import java.text.ParseException;

/**
 * The foreign predicates for the module system/xml.
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
public final class ForeignXml {
    public static final int GROUPS_PER_LINE = 10;

    public static final int MAX_ENTITY = 8; /* including & and ; */
    public static final String PREFIX_HTTP = "http:";

    private static final MapHash<String, Integer> entity = new MapHash<String, Integer>();
    private static final String[] entityrev = new String[255];

    static {
        addEntity("quot", XmlMachine.CHAR_DOUBLE);
//        addEntity("apos", XmlMachine.CHAR_SINGLE);
        addEntity("lt", '<');
        addEntity("gt", '>');
        addEntity("amp", '&');
        addEntity("nbsp", 0xA0);
    }

    /**
     * <p>Add a small character name.</p>
     *
     * @param str The name.
     * @param i   The character, less than or equal 255.
     */
    private static void addEntity(String str, int i) {
        Integer ival = Integer.valueOf(i);
        entity.add(str, ival);
        entityrev[i] = str;
    }

    /**
     * <p>Retrieve an enity.</p>
     *
     * @param str The name
     * @return The character or -1,
     */
    public static int getEntity(String str) {
        if (str.startsWith("#x")) {
            try {
                int ival = Integer.parseInt(str.substring(2), 16);
                return (0 <= ival && ival <= Character.MAX_CODE_POINT ? ival : -1);
            } catch (NumberFormatException x) {
                return -1;
            }
        } else if (str.startsWith("#")) {
            try {
                int ival = Integer.parseInt(str.substring(1));
                return (0 <= ival && ival <= Character.MAX_CODE_POINT ? ival : -1);
            } catch (NumberFormatException x) {
                return -1;
            }
        } else {
            Integer ival = entity.get(str);
            return (ival != null ? ival.intValue() : -1);
        }
    }

    /**
     * <p>Retrieve an entity rev.</p>
     *
     * @param ch The character.
     * @return The name or null.
     */
    public static String getEntityRev(int ch) {
        return (ch <= 255 ? entityrev[ch] : null);
    }

    /*******************************************************************/
    /* Text Escaping/Unescaping                                        */
    /*******************************************************************/

    /**
     * <p>Text escape a string.</p>
     *
     * @param s The string.
     * @return The text escaped string.
     */
    public static String sysTextEscape(String s) {
        return sysTextEscape(s, 0, s.length());
    }

    /**
     * <p>Text escape a string.</p>
     *
     * @param s     The string.
     * @param begin the beginning index.
     * @param end   the ending index.
     * @return The text escaped string, or null.
     */
    public static String sysTextEscape(String s, int begin, int end) {
        /* we keep buf = null as long as no character was escaped */
        int back = begin;
        StringBuilder buf = null;
        while (begin < end) {
            int ch = s.codePointAt(begin);
            String help = getEntityRev(ch);
            if (help != null) {
                if (buf == null)
                    buf = new StringBuilder(s.substring(back, begin));
                buf.appendCodePoint(XmlMachine.CHAR_AMPER);
                buf.append(help);
                buf.appendCodePoint(XmlMachine.CHAR_SEMI);
            } else {
                if (buf != null)
                    buf.appendCodePoint(ch);
            }
            begin += Character.charCount(ch);
        }
        if (buf == null)
            return s.substring(back, end);
        return buf.toString();
    }

    /**
     * <p>Text unescape a string.</p>
     * <p>Incomplete entities are tolerated.</p>
     *
     * @param str The string.
     * @return The text unescaped string.
     */
    public static String sysTextUnescape(String str) {
        /* we keep buf = null as long as no entity was unescaped */
        StringBuilder temp = null;
        StringBuilder buf = null;
        int n = str.length();
        int pos = 0;
        while (pos < n) {
            int ch = str.codePointAt(pos);
            if (ch == XmlMachine.CHAR_AMPER) {
                int k = pos;
                pos += Character.charCount(ch);
                if (temp == null)
                    temp = new StringBuilder();
                while (pos < n && pos - k < MAX_ENTITY &&
                        (ch = str.codePointAt(pos)) != XmlMachine.CHAR_SEMI &&
                        ch != XmlMachine.CHAR_AMPER &&
                        ch > XmlMachine.CHAR_SPACE &&
                        ch != XmlMachine.CHAR_BOM) {
                    temp.appendCodePoint(ch);
                    pos += Character.charCount(ch);
                }
                if (pos < n && pos - k < MAX_ENTITY && ch == XmlMachine.CHAR_SEMI) {
                    String help = temp.toString();
                    int ival = getEntity(help);
                    if (ival != -1) {
                        if (buf == null)
                            buf = new StringBuilder(str.substring(0, k));
                        buf.appendCodePoint(ival);
                    } else {
                        if (buf != null) {
                            buf.appendCodePoint(XmlMachine.CHAR_AMPER);
                            buf.append(help);
                            buf.appendCodePoint(XmlMachine.CHAR_SEMI);
                        }
                    }
                    pos += Character.charCount(ch);
                } else {
                    if (buf != null) {
                        String help = temp.toString();
                        buf.appendCodePoint(XmlMachine.CHAR_AMPER);
                        buf.append(help);
                    }
                }
                temp.setLength(0);
            } else {
                if (buf != null)
                    buf.appendCodePoint(ch);
                pos += Character.charCount(ch);
            }
        }
        if (buf != null)
            return buf.toString();
        return str;
    }

    /**
     * <p>Translate a plain position back to its escape position.</p>
     *
     * @param str The escaped text.
     * @param pos The plain position.
     * @return The escape position.
     */
    public static int plainToEscape(String str, int pos) {
        StringBuilder buf = null;
        int n = str.length();
        int i = 0;
        for (; i < n && 0 < pos; i++) {
            char ch = str.charAt(i);
            if (ch == XmlMachine.CHAR_AMPER) {
                int k = i;
                i++;
                if (buf == null)
                    buf = new StringBuilder();
                while (i < n && i - k < MAX_ENTITY &&
                        (ch = str.charAt(i)) != XmlMachine.CHAR_SEMI &&
                        ch != XmlMachine.CHAR_AMPER &&
                        ch > XmlMachine.CHAR_SPACE &&
                        ch != XmlMachine.CHAR_BOM) {
                    buf.append(ch);
                    i++;
                }
                if (i < n && i - k < MAX_ENTITY && ch == XmlMachine.CHAR_SEMI) {
                    String help = buf.toString();
                    int ival = getEntity(help);
                    if (ival != -1) {
                        pos--;
                    } else {
                        if (pos < buf.length() + 2)
                            return pos + k;
                        pos -= buf.length() + 2;
                    }
                } else {
                    if (pos < buf.length() + 1)
                        return pos + k;
                    pos -= buf.length() + 1;
                    i--;
                }
                buf.setLength(0);
            } else {
                pos--;
            }
        }
        return i;
    }

    /**
     * <p>Automatically create links for http: in the text.</p>
     *
     * @param str The text.
     * @param tar The target.
     * @return The rewritten text.
     */
    public static String linkAutomation(String str, String tar) {
        StringBuilder buf = null;
        int n = str.length();
        for (int i = 0; i < n; i++) {
            int ch = str.codePointAt(i);
            if (ch == 'h' && str.startsWith(PREFIX_HTTP, i)) {
                int k = i + PREFIX_HTTP.length();
                while (k < n && (Character.isLetterOrDigit(ch = str.codePointAt(k))
                        || "/.-?&=:;".indexOf(ch) != -1)) {
                    k += Character.charCount(ch);
                }
                if (buf == null)
                    buf = new StringBuilder(str.substring(0, i));
                buf.append("<a href='");
                buf.append(str.substring(i, k));
                if (tar != null) {
                    buf.append("' target='");
                    buf.append(tar);
                }
                buf.append("'>");
                buf.append(str.substring(i, k));
                buf.append("</a>");
                i = k - 1;
            } else {
                if (buf != null)
                    buf.appendCodePoint(ch);
                i += Character.charCount(ch) - 1;
            }
        }
        if (buf != null)
            return buf.toString();
        return str;
    }

    /*******************************************************************/
    /* Base64 Decode/Encode                                            */
    /*******************************************************************/

    /**
     * <p>Decode a string.</p>
     *
     * @param s The string.
     * @return The bytes.
     * @throws ParseException Shit happens.
     */
    public static byte[] sysBase64Decode(String s) throws ParseException {
        int n = s.length();
        int k = 0;
        for (int i = 0; i < n; i++) {
            char ch = s.charAt(i);
            if (ch == '\n' || ch == '\r') k++;
        }
        if ((n - k) % 4 != 0)
            throw new ParseException("length problem", n);
        int m = (n - k) / 4 * 3;
        if (s.endsWith("==")) {
            m -= 2;
        } else if (s.endsWith("=")) {
            m -= 1;
        }
        byte[] res = new byte[m];
        int j = 0;
        k = 0;
        int v = 0;
        for (int i = 0; i < m; i++) {
            switch (k) {
                case 0:
                    char ch = s.charAt(j);
                    while (ch == '\n' || ch == '\r') {
                        j++;
                        ch = s.charAt(j);
                    }
                    j++;
                    v = charToByte(ch);
                    if (v == -1)
                        throw new ParseException("decoding problem", j);
                    res[i] = (byte) (v << 2);
                    ch = s.charAt(j);
                    j++;
                    if (i == m - 1 && ch == '=') {
                        v = 0;
                    } else {
                        v = charToByte(ch);
                        if (v == -1)
                            throw new ParseException("decoding problem", j);
                    }
                    res[i] |= (byte) (v >> 4);
                    k = 1;
                    break;
                case 1:
                    res[i] = (byte) (v << 4);
                    ch = s.charAt(j);
                    j++;
                    v = charToByte(ch);
                    if (v == -1)
                        throw new ParseException("decoding problem", j);
                    res[i] |= (byte) (v >> 2);
                    k = 2;
                    break;
                case 2:
                    res[i] = (byte) (v << 6);
                    ch = s.charAt(j);
                    j++;
                    v = charToByte(ch);
                    if (v == -1)
                        throw new ParseException("decoding problem", j);
                    res[i] |= v;
                    k = 0;
                    break;
            }
        }
        return res;
    }

    /**
     * <p>Decode a character to a 6-bit value.</p>
     *
     * @param ch The character.
     * @return The 6-bit value or -1.
     */
    private static int charToByte(int ch) {
        if (('A' <= ch) && (ch <= 'Z')) {
            return ch - 'A';
        } else if (('a' <= ch) && (ch <= 'z')) {
            return ch - 'a' + 26;
        } else if (('0' <= ch) && (ch <= '9')) {
            return ch - '0' + 52;
        } else if (ch == '+') {
            return 62;
        } else if (ch == '/') {
            return 63;
        } else {
            return -1;
        }
    }

    /**
     * <p>Encode bytes.</p>
     *
     * @param b The bytes.
     * @return The string.
     */
    public static String sysBase64Encode(byte[] b) {
        int k = b.length / (GROUPS_PER_LINE * 3);
        int m = (b.length + 2) / 3 * 4 + k;
        char[] res = new char[m];
        int j = 0;
        k = 0;
        int v = 0;
        int l = -1;
        for (int i = 0; i < m; i++) {
            switch (k) {
                case 0:
                    l++;
                    if (l == GROUPS_PER_LINE) {
                        res[i] = '\n';
                        i++;
                        l = 0;
                    }
                    v = b[j];
                    j++;
                    res[i] = (char) byteToChar((v >>> 2) & 0x3F);
                    k = 1;
                    break;
                case 1:
                    int w = v;
                    if (j < b.length) {
                        v = b[j];
                    } else {
                        v = 0;
                    }
                    j++;
                    res[i] = (char) byteToChar(((w << 4) & 0x30) | ((v >>> 4) & 0x0F));
                    k = 2;
                    break;
                case 2:
                    if (j <= b.length) {
                        w = v;
                        if (j < b.length) {
                            v = b[j];
                        } else {
                            v = 0;
                        }
                        j++;
                        res[i] = (char) byteToChar(((w << 2) & 0x3C) | ((v >>> 6) & 0x03));
                    } else {
                        res[i] = '=';
                    }
                    k = 3;
                    break;
                case 3:
                    if (j <= b.length) {
                        res[i] = (char) byteToChar(v & 0x3F);
                    } else {
                        res[i] = '=';
                    }
                    k = 0;
                    break;
            }
        }
        return new String(res);
    }

    /**
     * <p>Encode a 6-bit value to a character.</p>
     *
     * @param b The 6-bit value.
     * @return The character.
     */
    private static int byteToChar(int b) {
        if (b < 26) {
            return 'A' + b;
        } else if (b < 52) {
            return 'a' + b - 26;
        } else if (b < 62) {
            return '0' + b - 52;
        } else if (b == 62) {
            return '+';
        } else if (b == 63) {
            return '/';
        } else {
            throw new IllegalArgumentException("problem encoding");
        }
    }

    /**
     * <p>Some tests.</p>
     *
     * @param args Not used.
     */
    /*
    public static void main(String[] args) {
        String str = "This is an \"experiment\"";
        System.out.println("str=" + str);
        System.out.println("sysTextEscape(str)=" + sysTextEscape(str));

        System.out.println();

        str = "This is an &quot;experiment&quot;";
        System.out.println("str=" + str);
        System.out.println("sysTextUnescape(str)=" + sysTextUnescape(str));

        System.out.println();

        str = "This is an &#x22;experiment&#x22;";
        System.out.println("str=" + str);
        System.out.println("sysTextUnescape(str)=" + sysTextUnescape(str));

        System.out.println();

        str = "This is an &#34;experiment&#34;";
        System.out.println("str=" + str);
        System.out.println("sysTextUnescape(str)=" + sysTextUnescape(str));
    }
    */

}
