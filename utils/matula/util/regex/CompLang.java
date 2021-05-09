package matula.util.regex;

/**
 * <p>Classify computer languages.</p>
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
public final class CompLang {
    public final static CompLang ISO_COMPLANG = new CompLang();
    public final static CompLang JSON_COMPLANG = new CompLang();

    public static final char CHAR_AMPER = '&';
    public static final char CHAR_SEMI = ';';
    public static final char CHAR_BOM = 0xFEFF;
    public static final char CHAR_SPACE = ' ';

    public static final String OP_SYNTAX_ILLEGAL_ESCAPE = "illegal_escape";
    public static final String OP_SYNTAX_ILLEGAL_LAYOUT = "illegal_layout";
    public static final String OP_SYNTAX_ILLEGAL_UNICODE = "illegal_unicode";

    private String linecomment;
    private String blockcommentstart;
    private String blockcommentend;
    private String codeescapes;

    static {
        ISO_COMPLANG.setLineComment("%");
        ISO_COMPLANG.setBlockCommentStart("/*");
        ISO_COMPLANG.setBlockCommentEnd("*/");
        ISO_COMPLANG.setCodeEscapes("x0");

        CompLang.JSON_COMPLANG.setLineComment("//");
        CompLang.JSON_COMPLANG.setBlockCommentStart("/*");
        CompLang.JSON_COMPLANG.setBlockCommentEnd("*/");
        CompLang.JSON_COMPLANG.setCodeEscapes("u/");
    }

    /**
     * <p>Set the line comment.</p>
     *
     * @param s The line comment.
     */
    public void setLineComment(String s) {
        linecomment = s;
    }

    /**
     * <p>Retrieve the line comment.</p>
     *
     * @return The line comment.
     */
    public String getLineComment() {
        return linecomment;
    }

    /**
     * <p>Set the block comment start.</p>
     *
     * @param s The block comment start.
     */
    public void setBlockCommentStart(String s) {
        blockcommentstart = s;
    }

    /**
     * <p>Retrieve the block comment start.</p>
     *
     * @return The block comment start, can be null.
     */
    public String getBlockCommentStart() {
        return blockcommentstart;
    }

    /**
     * <p>Set the block comment end.</p>
     *
     * @param s The block comment end, can be null.
     */
    public void setBlockCommentEnd(String s) {
        blockcommentend = s;
    }

    /**
     * <p>Retrieve the block comment end.</p>
     *
     * @return The block comment end, can be null.
     */
    public String getBlockCommentEnd() {
        return blockcommentend;
    }

    /**
     * <p>Set the code escapes.</p>
     *
     * @param c The code escapes, can be null.
     */
    public void setCodeEscapes(String c) {
        codeescapes = c;
    }

    /**
     * <p>Retrieve the code escapes.</p>
     *
     * @return The code escapes.
     */
    public String getCodeEscapes() {
        return codeescapes;
    }

    /**
     * <p>Check whether the string is a relevant token.</p>
     * <p>The check consists of:</p>
     * <ul>
     * <li>Does not start with a line comment.</li>
     * <li>Does not start with a block comment.</li>
     * </ul>
     * <p>Version that can handle >16 bit Unicode.</p>
     *
     * @param str The token to check.
     * @return True if the string is a relevant token, otherwise false.
     */
    public boolean relevantToken(String str) {
        if (str.startsWith(linecomment))
            return false;
        if (str.startsWith(blockcommentstart))
            return false;
        return true;
    }

    /***********************************************************************/
    /* String Normalization                                                */
    /***********************************************************************/

    /**
     * <p>Resolve the escapes in the string.</p>
     * <p>Version that can handle >16 bit Unicode.</p>
     *
     * @param str    The token.
     * @param quote  The quote.
     * @param cont   The continuation escape sequence flag.
     * @param offset The error offset.
     * @param d      The delemiter.
     * @return The resolved token.
     * @throws ScannerError Parsing problem.
     */
    public String resolveEscape(String str, int quote, boolean cont,
                                int offset, CodeType d)
            throws ScannerError {
        StringBuilder buf = null;
        int n = str.length();
        for (int i = 0; i < n; i++) {
            int k = str.codePointAt(i);
            if (k == quote) {
                if (buf == null)
                    buf = new StringBuilder(str.substring(0, i));
                i += Character.charCount(k);
                if (!(i < n) || (k = str.codePointAt(i)) != quote)
                    throw new ScannerError(CodeType.OP_SYNTAX_DOUBLING_MISSING, offset + i);
                buf.appendCodePoint(k);
            } else if (k == CodeType.LINE_BACKSLASH) {
                if (buf == null)
                    buf = new StringBuilder(str.substring(0, i));
                i += Character.charCount(k);
                if (i < n) {
                    k = str.codePointAt(i);
                    switch (k) {
                        case 'a':
                            buf.appendCodePoint('\u0007');
                            break;
                        case 'b':
                            buf.appendCodePoint('\u0008');
                            break;
                        case 'f':
                            buf.appendCodePoint('\u000C');
                            break;
                        case 'n':
                            buf.appendCodePoint(CodeType.LINE_EOL);
                            break;
                        case 'r':
                            buf.appendCodePoint(CodeType.LINE_WIN);
                            break;
                        case 't':
                            buf.appendCodePoint('\u0009');
                            break;
                        case 'v':
                            buf.appendCodePoint('\u000B');
                            break;
                        case 'x':
                            i += Character.charCount(k);
                            int i2 = i;
                            while (i < n && d.isAlfanum(k = str.codePointAt(i)))
                                i += Character.charCount(k);
                            if (i < n && (k = str.codePointAt(i)) == CodeType.LINE_BACKSLASH) {
                                try {
                                    i2 = Integer.parseInt(str.substring(i2, i), 16);
                                } catch (NumberFormatException x) {
                                    throw new ScannerError(OP_SYNTAX_ILLEGAL_ESCAPE, offset + i);
                                }
                                if (i2 < 0 || i2 > Character.MAX_CODE_POINT)
                                    throw new ScannerError(OP_SYNTAX_ILLEGAL_ESCAPE, offset + i);
                                buf.appendCodePoint(i2);
                            } else {
                                throw new ScannerError(OP_SYNTAX_ILLEGAL_ESCAPE, offset + i);
                            }
                            break;
                        case 'u':
                            i += Character.charCount(k);
                            i2 = i;
                            for (int j = 0; j < 4; j++) {
                                if (!(i < n) || Character.digit(k = str.codePointAt(i), 16) == -1)
                                    throw new ScannerError(OP_SYNTAX_ILLEGAL_ESCAPE, offset + i);
                                if (j != 3)
                                    i += Character.charCount(k);
                            }
                            try {
                                i2 = Integer.parseInt(str.substring(i2, i + Character.charCount(k)), 16);
                            } catch (NumberFormatException x) {
                                throw new ScannerError(OP_SYNTAX_ILLEGAL_ESCAPE, offset + i);
                            }
                            if (i2 < 0 || i2 > Character.MAX_CODE_POINT)
                                throw new ScannerError(OP_SYNTAX_ILLEGAL_ESCAPE, offset + i);
                            buf.appendCodePoint(i2);
                            break;
                        case CodeType.LINE_SINGLE:
                        case CodeType.LINE_DOUBLE:
                        case CodeType.LINE_BACK:
                        case CodeType.LINE_BACKSLASH:
                        case CodeType.LINE_SLASH:
                            buf.appendCodePoint(k);
                            break;
                        case CodeType.LINE_EOL:
                            if (!cont)
                                throw new ScannerError(ScannerToken.OP_SYNTAX_CONT_ESC_IN_CHARACTER, offset + i);
                            break;
                        default:
                            if (Character.digit(k, 8) != -1) {
                                i2 = i;
                                while (i < n && d.isAlfanum(k = str.codePointAt(i)))
                                    i += Character.charCount(k);
                                if (i < n && (k = str.codePointAt(i)) == CodeType.LINE_BACKSLASH) {
                                    try {
                                        i2 = Integer.parseInt(str.substring(i2, i), 8);
                                    } catch (NumberFormatException x) {
                                        throw new ScannerError(OP_SYNTAX_ILLEGAL_ESCAPE, offset + i);
                                    }
                                    if (i2 < 0 || i2 > Character.MAX_CODE_POINT)
                                        throw new ScannerError(OP_SYNTAX_ILLEGAL_ESCAPE, offset + i);
                                    buf.appendCodePoint(i2);
                                } else {
                                    throw new ScannerError(OP_SYNTAX_ILLEGAL_ESCAPE, offset + i);
                                }
                                break;
                            } else {
                                throw new ScannerError(OP_SYNTAX_ILLEGAL_ESCAPE, offset + i);
                            }
                    }
                } else {
                    throw new ScannerError(OP_SYNTAX_ILLEGAL_ESCAPE, offset + i);
                }
            } else if (k == CodeType.LINE_EOL) {
                if (cont) {
                    throw new ScannerError(ScannerToken.OP_SYNTAX_END_OF_LINE_IN_STRING, offset + i);
                } else {
                    throw new ScannerError(ScannerToken.OP_SYNTAX_END_OF_LINE_IN_CHARACTER, offset + i);
                }
            } else if (k != ' ' && d.isLayout(k)) {
                throw new ScannerError(OP_SYNTAX_ILLEGAL_LAYOUT, offset + i);
            } else if (!d.isValid(k)) {
                throw new ScannerError(OP_SYNTAX_ILLEGAL_UNICODE, offset + i);
            } else {
                if (buf != null)
                    buf.appendCodePoint(k);
            }
            i += Character.charCount(k) - 1;
        }
        if (buf != null)
            return buf.toString();
        return str;
    }

    /**
     * <p>Resolve the escapes in the string.</p>
     * <p>Version that can handle >16 bit Unicode.</p>
     *
     * @param str   The unresolved string.
     * @param d     The delemiter.
     * @param quote The quote.
     * @return The resolved string.
     */
    public String escapeControl(String str, CodeType d, int quote) {
        StringBuilder buf = null;
        int n = str.length();
        for (int i = 0; i < n; i++) {
            int k = str.codePointAt(i);
            if (k == quote) {
                if (buf == null)
                    buf = new StringBuilder(str.substring(0, i));
                if (getCodeEscapes().indexOf('u') != -1) {
                    buf.appendCodePoint(CodeType.LINE_BACKSLASH);
                } else {
                    buf.appendCodePoint(k);
                }
                buf.appendCodePoint(k);
            } else if (k == CodeType.LINE_BACKSLASH
                    || (k == CodeType.LINE_SLASH &&
                    getCodeEscapes().indexOf(CodeType.LINE_SLASH) != -1)
                    || (k != ' ' && d.isLayout(k))
                    || !d.isValid(k)) {
                if (buf == null)
                    buf = new StringBuilder(str.substring(0, i));
                buf.appendCodePoint(CodeType.LINE_BACKSLASH);
                switch (k) {
                    case '\u0007':
                        buf.appendCodePoint('a');
                        break;
                    case '\u0008':
                        buf.appendCodePoint('b');
                        break;
                    case '\u000C':
                        buf.appendCodePoint('f');
                        break;
                    case CodeType.LINE_EOL:
                        buf.appendCodePoint('n');
                        break;
                    case CodeType.LINE_WIN:
                        buf.appendCodePoint('r');
                        break;
                    case '\u0009':
                        buf.appendCodePoint('t');
                        break;
                    case '\u000B':
                        buf.appendCodePoint('v');
                        break;
                    case CodeType.LINE_SINGLE:
                    case CodeType.LINE_DOUBLE:
                    case CodeType.LINE_BACK:
                    case CodeType.LINE_BACKSLASH:
                    case CodeType.LINE_SLASH:
                        buf.appendCodePoint(k);
                        break;
                    default:
                        if (k <= 0x1FF && getCodeEscapes().indexOf('0') != -1) {
                            buf.append(Integer.toOctalString(k));
                            buf.appendCodePoint(CodeType.LINE_BACKSLASH);
                        } else if (getCodeEscapes().indexOf('u') != -1) {
                            char[] res = Character.toChars(k);
                            for (int j = 0; j < res.length; j++) {
                                if (j != 0)
                                    buf.appendCodePoint(CodeType.LINE_BACKSLASH);
                                buf.appendCodePoint('u');
                                String t = Integer.toHexString(k).toUpperCase();
                                for (int i2 = t.length(); i2 < 4; i2++)
                                    buf.appendCodePoint('0');
                                buf.append(t);
                            }
                        } else {
                            buf.appendCodePoint('x');
                            buf.append(Integer.toHexString(k).toUpperCase());
                            buf.appendCodePoint(CodeType.LINE_BACKSLASH);
                        }
                        break;
                }
            } else {
                if (buf != null)
                    buf.appendCodePoint(k);
            }
            i += Character.charCount(k) - 1;
        }
        if (buf != null)
            return buf.toString();
        return str;
    }

    /**
     * <p>Some tests.</p>
     *
     * @param args Not used.
     */
    /*
    public static void main(String[] args) throws ScannerError {
        String foo = "abc\\adef";
        String res = resolveEscape(foo, '\'', false, 0, CodeType.ISO_CODETYPE);
        System.out.println("res=" + res);

        String res2 = escapeControl(res, CodeType.ISO_CODETYPE);
        System.out.println("res=" + res2);
    }
    */

}