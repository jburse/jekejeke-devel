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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class CompLang {
    public final static CompLang ISO_COMPLANG = new CompLang();

    public static final String OP_SYNTAX_ILLEGAL_ESCAPE = "illegal_escape";
    public static final String OP_SYNTAX_ILLEGAL_EOL = "illegal_eol";
    public static final String OP_SYNTAX_ILLEGAL_LAYOUT = "illegal_layout";
    public static final String OP_SYNTAX_ILLEGAL_UNICODE = "illegal_unicode";

    private String linecomment = "//";
    private String blockcommentstart = "/*";
    private String blockcommentend = "*/";
    private int end = '.';

    static {
        ISO_COMPLANG.setLineComment("%");
        ISO_COMPLANG.setBlockCommentStart("/*");
        ISO_COMPLANG.setBlockCommentEnd("*/");
        ISO_COMPLANG.setEnd('.');
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
     * @return The block comment start.
     */
    public String getBlockCommentStart() {
        return blockcommentstart;
    }

    /**
     * <p>Set the block comment end.</p>
     *
     * @param s The block comment end.
     */
    public void setBlockCommentEnd(String s) {
        blockcommentend = s;
    }

    /**
     * <p>Retrieve the block comment end.</p>
     *
     * @return The block comment end.
     */
    public String getBlockCommentEnd() {
        return blockcommentend;
    }

    /**
     * <p>Retrieve the line end character.</p>
     *
     * @return The line end character.
     */
    public int getEnd() {
        return end;
    }

    /**
     * <p>Set the line end character.</p>
     *
     * @param e The line end character.
     */
    public void setEnd(int e) {
        end = e;
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
     * @param offset The error offset.
     * @param cont   The cont flag.
     * @param d      The delemiter.
     * @return The resolved token.
     * @throws ScannerError Parsing problem.
     */
    public static String resolveEscape(String str, int quote, int offset,
                                       boolean cont, CodeType d)
            throws ScannerError {
        StringBuilder buf = null;
        int n = str.length();
        for (int i = 0; i < n; i++) {
            int k = str.codePointAt(i);
            if (k == quote) {
                offset++;
                if (buf != null)
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
                            if (i < n && str.codePointAt(i) == CodeType.LINE_BACKSLASH) {
                                int val;
                                try {
                                    val = Integer.parseInt(str.substring(i2, i), 16);
                                } catch (NumberFormatException x) {
                                    throw new ScannerError(OP_SYNTAX_ILLEGAL_ESCAPE, offset + i);
                                }
                                if (val < 0 || val > Character.MAX_CODE_POINT)
                                    throw new ScannerError(OP_SYNTAX_ILLEGAL_ESCAPE, offset + i);
                                buf.appendCodePoint(val);
                                k = CodeType.LINE_BACKSLASH;
                            } else {
                                throw new ScannerError(OP_SYNTAX_ILLEGAL_ESCAPE, offset + i);
                            }
                            break;
                        case CodeType.LINE_EOL:
                            if (!cont)
                                throw new ScannerError(OP_SYNTAX_ILLEGAL_EOL, offset + i);
                            break;
                        case CodeType.LINE_SINGLE:
                        case CodeType.LINE_DOUBLE:
                        case CodeType.LINE_BACK:
                        case CodeType.LINE_BACKSLASH:
                            buf.appendCodePoint(k);
                            break;
                        default:
                            if (Character.digit(k, 8) != -1) {
                                i2 = i;
                                while (i < n && d.isAlfanum(k = str.codePointAt(i)))
                                    i += Character.charCount(k);
                                if (i < n && str.codePointAt(i) == CodeType.LINE_BACKSLASH) {
                                    int val;
                                    try {
                                        val = Integer.parseInt(str.substring(i2, i), 8);
                                    } catch (NumberFormatException x) {
                                        throw new ScannerError(OP_SYNTAX_ILLEGAL_ESCAPE, offset + i);
                                    }
                                    if (val < 0 || val > Character.MAX_CODE_POINT)
                                        throw new ScannerError(OP_SYNTAX_ILLEGAL_ESCAPE, offset + i);
                                    buf.appendCodePoint(val);
                                    k = CodeType.LINE_BACKSLASH;
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
                throw new ScannerError(OP_SYNTAX_ILLEGAL_EOL, offset + i);
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
     * @param str The unresolved string.
     * @param d   The delemiter.
     * @return The resolved string.
     */
    public static String escapeControl(String str, CodeType d) {
        StringBuilder buf = null;
        int n = str.length();
        for (int i = 0; i < n; i++) {
            int k = str.codePointAt(i);
            if (k == CodeType.LINE_BACKSLASH
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
                        buf.appendCodePoint(k);
                        break;
                    default:
                        if (k < 512) {
                            buf.append(Integer.toOctalString(k));
                        } else {
                            buf.appendCodePoint('x');
                            buf.append(Integer.toHexString(k).toUpperCase());
                        }
                        buf.appendCodePoint(CodeType.LINE_BACKSLASH);
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

}