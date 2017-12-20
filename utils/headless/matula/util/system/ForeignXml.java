package matula.util.system;

import matula.util.data.MapHash;
import matula.util.format.XmlMachine;

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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class ForeignXml {
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
     * @return The text escaped string.
     */
    public static String sysTextEscape(String s, int begin, int end) {
        /* we keep buf = null as long as no character was escaped */
        int back = begin;
        StringBuilder buf = null;
        while (begin < end) {
            int ch = s.codePointAt(begin);
            String help = (ch <= 255 ? entityrev[ch] : null);
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
                        ch != XmlMachine.CHAR_AMPER && ch > ' ') {
                    temp.appendCodePoint(ch);
                    pos += Character.charCount(ch);
                }
                if (pos < n && pos - k < MAX_ENTITY && ch == XmlMachine.CHAR_SEMI) {
                    String help = temp.toString();
                    Integer ival = entity.get(help);
                    if (ival != null) {
                        if (buf == null)
                            buf = new StringBuilder(str.substring(0, k));
                        buf.appendCodePoint(ival.intValue());
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
                        ch != XmlMachine.CHAR_AMPER && ch > ' ') {
                    buf.append(ch);
                    i++;
                }
                if (i < n && i - k < MAX_ENTITY && ch == XmlMachine.CHAR_SEMI) {
                    String help = buf.toString();
                    Integer ival = entity.get(help);
                    if (ival != null) {
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

    /**
     * <p>Some tests.</p>
     * @param args Not used.
     */
    /*
    public static void main(String[] args) {
        String str="I don"+(char)39+"t get it";
        System.out.println("str="+str);
        System.out.println("sysTextEscape(str)="+sysTextEscape(str));

        System.out.println();

        str="I don&apos;t get it";
        System.out.println("str="+str);
        System.out.println("sysTextUnescape(str)="+sysTextUnescape(str));
    }
    */

}
