package matula.util.regex;

import matula.util.system.OpenOpts;
import matula.util.text.LangProperties;

import java.io.IOException;
import java.io.Reader;
import java.util.Locale;
import java.util.Properties;

/**
 * <p>This class provides a token scanner.</p>.
 * <p>The folllowing token syntax is supported.</p>
 * <pre>
 *    token'' --> filler
 *              | linecomment
 *              | blockcomment
 *              | string
 *              | solo
 *              | name
 *              | number
 *              | relator
 *    name    --> alfa { alfanum }.
 *    relator --> graphic { graphic }.
 * </pre>
 * <p>Warning: The pre-allocated string buffer keeps using an
 * internal buffer of size >max of the encountered tokens.</p>
 * <p>For graphic and solo see CodeType.</p>
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
public final class ScannerToken {
    /* code in scanner error */
    public static final String OP_SYNTAX_ERROR = "syntax_error";
    public static final String OP_SYNTAX_END_OF_FILE_IN_BLOCK_COMMENT = "end_of_file_in_block_comment"; /* e303 */
    public static final String OP_SYNTAX_END_OF_FILE_IN_NAME = "end_of_file_in_name"; /* e304 */
    public static final String OP_SYNTAX_END_OF_FILE_IN_VARIABLE = "end_of_file_in_variable"; /* e304 */
    public static final String OP_SYNTAX_END_OF_FILE_IN_STRING = "end_of_file_in_string"; /* e305 */
    public static final String OP_SYNTAX_END_OF_FILE_IN_CHARACTER = "end_of_file_in_character"; /* e304 */
    public static final String OP_SYNTAX_END_OF_LINE_IN_STRING = "end_of_line_in_string";
    public static final String OP_SYNTAX_END_OF_LINE_IN_CHARACTER = "end_of_line_in_character";
    public static final String OP_SYNTAX_CONT_ESC_IN_CHARACTER = "cont_esc_in_character";

    public static final char PREFIX_REFERENCE = 'r';
    public static final char PREFIX_DECIMAL = 'd';
    public static final char PREFIX_FLOAT32 = 'f';
    public static final char PREFIX_BINARY = 'b';
    public static final char PREFIX_OCTAL = 'o';
    public static final char PREFIX_HEX = 'x';

    public static final char SCAN_PERIOD = '.';
    public static final char SCAN_NEG = '-';
    public static final char SCAN_POS = '+';
    public static final char SCAN_EXPLOW = 'e';
    public static final char SCAN_EXPCAP = 'E';

    public static final int MASK_RTRN_LAYT = 0x00000001;
    public static final int MASK_RTRN_LINE = 0x00000002;
    public static final int MASK_RTRN_BLCK = 0x00000004;

    public static final int MASK_RTRN_ALL = MASK_RTRN_LAYT | MASK_RTRN_LINE | MASK_RTRN_BLCK;

    private Reader reader;
    private int ch;
    private String token;
    private StringBuilder buf = new StringBuilder();
    private CodeType delemiter;
    private CompLang remark;
    private int flags = MASK_RTRN_LINE | MASK_RTRN_BLCK;
    private String origin;

    /**
     * <p>Set the delemiter.</p>
     *
     * @param d The delemiter.
     */
    public void setDelemiter(CodeType d) {
        delemiter = d;
    }

    /**
     * <p>Retrieve the delemiter.</p>
     *
     * @return The delemiter.
     */
    public CodeType getDelemiter() {
        return delemiter;
    }

    /**
     * <p>Set the remark.</p>
     *
     * @param r The remark.
     */
    public void setRemark(CompLang r) {
        remark = r;
    }

    /**
     * <p>Retrieve the remark.</p>
     *
     * @return The remark.
     */
    public CompLang getRemark() {
        return remark;
    }

    /**
     * <p>Set the line counter.</p>
     *
     * @param l The line counter.
     */
    public void setLineNo(int l) {
        OpenOpts.setLineNumber(reader, l);
    }

    /**
     * <p>Retrieve the line counter.</p>
     *
     * @return The line counter.
     */
    public int getLineNo() {
        return OpenOpts.getLineNumber(reader);
    }

    /**
     * <p>Set the origin.</p>
     *
     * @param o The origin.
     */
    public void setOrigin(String o) {
        origin = o;
    }

    /**
     * <p>Retrieve the origin.</p>
     *
     * @return The origin.
     */
    public String getOrigin() {
        return origin;
    }

    /**
     * <p>Retrieve the current token.</p>
     *
     * @return The current token.
     */
    public String getToken() {
        return token;
    }

    /**
     * <p>Retrieve the current token offset.</p>
     *
     * @return The current token offset.
     */
    public int getTokenOffset() {
        int k = (ch != CodeType.LINE_EOF ? Character.charCount(ch) : 0);
        int ch2;
        if (token.length() > 0 &&
                delemiter.getQuotes().indexOf(ch2 = token.codePointAt(0)) != -1)
            k += Character.charCount(ch2);
        return OpenOpts.getOffset(reader) - k - token.length();
    }

    /**
     * <p>Retrieve the current line.</p>
     *
     * @return The current line.
     */
    public String getLine() {
        return OpenOpts.getLine(reader);
    }

    /**
     * <p>Retrieve the flags.</p>
     *
     * @return The flags.
     */
    public int getFlags() {
        return flags;
    }

    /**
     * <p>Set the flags.</p>
     *
     * @param f The flags.
     */
    public void setFlags(int f) {
        flags = f;
    }

    /**
     * <p>Set the reader.</p>
     *
     * @param r The reader.
     */
    public void setReader(Reader r) {
        reader = r;
    }

    /**
     * <p>Retrieve the reader.</p>
     *
     * @return The reader.
     */
    public Reader getReader() {
        return reader;
    }

    /*************************************************************/
    /* String Scanning                                           */
    /*************************************************************/

    /**
     * <p>Parse a string.</p>
     * <p>The following string syntax is supported:</p>
     * <pre>
     *      string' --> quote { quote quote | strchar } ( eol | quote ).
     * </pre>
     *
     * @throws ScannerError Scanning problem.
     */
    private void nextString() throws ScannerError, IOException {
        int quote = ch;
        buf.appendCodePoint(ch);
        ch = getCode();
        while (ch != CodeType.LINE_EOF) {
            if (ch == quote) {
                ch = getCode();
                if (ch == quote) {
                    buf.appendCodePoint(ch);
                    buf.appendCodePoint(ch);
                    ch = getCode();
                } else {
                    token = buf.toString();
                    return;
                }
            } else {
                nextChar(true);
            }
        }
        token = buf.toString();
        if (quote == CodeType.LINE_SINGLE) {
            throw new ScannerError(OP_SYNTAX_END_OF_FILE_IN_NAME, OpenOpts.getOffset(reader));
        } else if (quote == CodeType.LINE_DOUBLE) {
            throw new ScannerError(OP_SYNTAX_END_OF_FILE_IN_STRING, OpenOpts.getOffset(reader));
        } else {
            throw new ScannerError(OP_SYNTAX_END_OF_FILE_IN_VARIABLE, OpenOpts.getOffset(reader));
        }
    }

    /**
     * <p>Parse a string character.</p>
     * <p>The followng string character syntax is supported:</p>
     * <pre>
     *      strchar --> "/" super { alfanum } "/"
     *                | "/" char
     *                | char.
     *      super   --> "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "x"
     *      alfan   --> underscore
     *                | lower
     *                | upper
     *                | other.
     *      alfanum --> alfa
     *                | digit.
     * </pre>
     * <p>For underscore, lower, upper, other and digit see class CodeType.</p>
     *
     * @param cont The continuation escape sequence flag.
     * @throws ScannerError Scanning problem.
     */
    private void nextChar(boolean cont) throws ScannerError, IOException {
        if (ch == CodeType.LINE_BACKSLASH) {
            buf.appendCodePoint(ch);
            ch = getCode();
            if (ch != CodeType.LINE_EOF) {
                if (!cont && ch == CodeType.LINE_EOL)
                    throw new ScannerError(OP_SYNTAX_CONT_ESC_IN_CHARACTER, OpenOpts.getOffset(reader));
                if (Character.digit(ch, 8) != -1
                        || ch == 'x') {
                    while (ch != CodeType.LINE_EOF && delemiter.isAlfanum(ch)) {
                        buf.appendCodePoint(ch);
                        ch = getCode();
                    }
                    if (ch == CodeType.LINE_BACKSLASH) {
                        buf.appendCodePoint(ch);
                        ch = getCode();
                    }
                } else {
                    buf.appendCodePoint(ch);
                    ch = getCode();
                }
            }
        } else if (ch == CodeType.LINE_EOL) {
            if (cont) {
                throw new ScannerError(OP_SYNTAX_END_OF_LINE_IN_STRING, OpenOpts.getOffset(reader));
            } else {
                throw new ScannerError(OP_SYNTAX_END_OF_LINE_IN_CHARACTER, OpenOpts.getOffset(reader));
            }
        } else {
            buf.appendCodePoint(ch);
            ch = getCode();
        }
    }

    /*************************************************************/
    /* Number Scanning                                           */
    /*************************************************************/

    /**
     * <p>Parse a number.</p>
     * <p>The following number syntax is supported:</p>
     * <pre>
     *      number   --> "0" "b" { binary | underscore }
     *                 | "0" "o" { octal | underscore }
     *                 | "0" "x" { hexadec | underscore }
     *                 | "0" "r" { hexadec | underscore }
     *                 | "0" "d" [ mantissa ] [ fraction ] [ exponent ]
     *                 | "0" "f" [ mantissa ] [ fraction ] [ exponent ]
     *                 | mantissa [ fraction [ exponent ]].
     *      mantissa --> { digit | underscore }
     *      fraction --> "."  digit { digit | underscore }
     *      binary   --> binary_digit_number.
     *      octal    --> octal_digit_number.
     *      hexadec  --> hexdecimal_digit_number.
     * </pre>
     * <p>For underscore and digit see class CodeType.</p>
     * <p>binary_digit_number, octal_digit_number and hexdecimal_digit_number
     * is based on the Java Character.isDigit(int,int) method.</p>
     *
     * @throws ScannerError Scanning problem.
     */
    private void nextNumber() throws ScannerError, IOException {
        if (ch == CodeType.LINE_ZERO) {
            buf.appendCodePoint(ch);
            ch = getCode();
            if ((ch == PREFIX_OCTAL || ch == PREFIX_BINARY ||
                    ch == PREFIX_HEX || ch == PREFIX_REFERENCE)) {
                int radix;
                if (ch == PREFIX_OCTAL) {
                    radix = 8;
                } else if (ch == PREFIX_BINARY) {
                    radix = 2;
                } else {
                    radix = 16;
                }
                buf.appendCodePoint(ch);
                ch = getCode();
                while (ch != CodeType.LINE_EOF) {
                    if (Character.digit(ch, radix) != -1 ||
                            delemiter.isUnderscore(ch)) {
                        buf.appendCodePoint(ch);
                        ch = getCode();
                    } else {
                        token = buf.toString();
                        return;
                    }
                }
                token = buf.toString();
                return;
            } else if (ch == CodeType.LINE_SINGLE) {
                buf.appendCodePoint(ch);
                ch = getCode();
                if (ch == CodeType.LINE_SINGLE) {
                    ch = getCode();
                    if (ch == CodeType.LINE_SINGLE) {
                        buf.appendCodePoint(ch);
                        buf.appendCodePoint(ch);
                        ch = getCode();
                    }
                } else if (ch != CodeType.LINE_EOF) {
                    nextChar(false);
                } else {
                    token = buf.toString();
                    throw new ScannerError(OP_SYNTAX_END_OF_FILE_IN_CHARACTER, OpenOpts.getOffset(reader));
                }
                token = buf.toString();
                return;
            } else if (ch == PREFIX_DECIMAL || ch == PREFIX_FLOAT32) {
                buf.appendCodePoint(ch);
                ch = getCode();
                /* mantiassa */
                while (ch != CodeType.LINE_EOF && isDigitOrUnderscore(ch)) {
                    buf.appendCodePoint(ch);
                    ch = getCode();
                }
                if (ch == SCAN_PERIOD && Character.isDigit(peekCode())) {
                    /* fraction */
                    buf.appendCodePoint(ch);
                    ch = getCode();
                    buf.appendCodePoint(ch);
                    ch = getCode();
                    while (ch != CodeType.LINE_EOF && isDigitOrUnderscore(ch)) {
                        buf.appendCodePoint(ch);
                        ch = getCode();
                    }
                }
                nextExponent();
                token = buf.toString();
                return;
            }
        }
        /* mantiassa */
        while (ch != CodeType.LINE_EOF && isDigitOrUnderscore(ch)) {
            buf.appendCodePoint(ch);
            ch = getCode();
        }
        if (ch == SCAN_PERIOD && Character.isDigit(peekCode())) {
            /* fraction */
            buf.appendCodePoint(ch);
            ch = getCode();
            buf.appendCodePoint(ch);
            ch = getCode();
            while (ch != CodeType.LINE_EOF && isDigitOrUnderscore(ch)) {
                buf.appendCodePoint(ch);
                ch = getCode();
            }
            /* exponent */
            nextExponent();
        }
        token = buf.toString();
    }

    /**
     * <p>Parse an exponent.</p>
     * <p>The following exponent syntax is supported:</p>
     * <pre>
     *    exponent --> ( "e" | "E" ) ( "-" | "+" ) digit { digit | underscore }.
     * </pre>
     * <p>For underscore and digit see class CodeType.</p>
     *
     * @throws ScannerError Scanning problem.
     */
    private void nextExponent() throws ScannerError, IOException {
        if ((ch == SCAN_EXPLOW || ch == SCAN_EXPCAP) &&
                (Character.isDigit(peekCode()) ||
                        peekCode() == SCAN_NEG ||
                        peekCode() == SCAN_POS)) {
            /* exponent character */
            buf.appendCodePoint(ch);
            ch = getCode();
            /* exponent sign */
            if (ch == SCAN_NEG || ch == SCAN_POS) {
                buf.appendCodePoint(ch);
                ch = getCode();
            }
            /* exponent mantissa */
            if (Character.isDigit(ch)) {
                buf.appendCodePoint(ch);
                ch = getCode();
                while (ch != CodeType.LINE_EOF && isDigitOrUnderscore(ch)) {
                    buf.appendCodePoint(ch);
                    ch = getCode();
                }
            }
        }
    }

    /**
     * <p>Check whether the code point is a digit or an underscore.</p>
     *
     * @param cp The code point.
     * @return True if the code point is a digit or an underscore.
     */
    private boolean isDigitOrUnderscore(int cp) {
        if (Character.isDigit(cp))
            return true;
        return delemiter.isUnderscore(cp);
    }

    /*************************************************************/
    /* Comment Scanning                                          */
    /*************************************************************/

    /**
     * <p>Read the current line comment.</p>
     * <p>The following line comment syntax is upported.</p>
     * <pre>
     *     linecomment --> lcstart { char } eol.
     * </pre>
     *
     * @throws ScannerError Scanning problem.
     */
    private void nextLineComment() throws ScannerError, IOException {
        if ((flags & MASK_RTRN_LINE) != 0) {
            String lc = remark.getLineComment();
            consumeStr(lc);
            while (ch != CodeType.LINE_EOF && ch != CodeType.LINE_EOL) {
                buf.appendCodePoint(ch);
                ch = getCode();
            }
            if (ch != CodeType.LINE_EOF) {
                buf.appendCodePoint(ch);
                ch = getCode();
            }
            token = buf.toString();
        } else {
            String lc = remark.getLineComment();
            skipStr(lc);
            while (ch != CodeType.LINE_EOF && ch != CodeType.LINE_EOL)
                ch = getCode();
            if (ch != CodeType.LINE_EOF)
                ch = getCode();
        }
    }

    /**
     * <p>Read the current block comment.</p>
     * <p>The following block comment syntax is supported.</p>
     * <pre>
     *     blockcomment --> bcstart { char } bcend.
     * </pre>
     *
     * @throws ScannerError Scanning problem.
     */
    private void nextBlockComment() throws ScannerError, IOException {
        if ((flags & MASK_RTRN_BLCK) != 0) {
            String bc = remark.getBlockCommentStart();
            consumeStr(bc);
            while (ch != CodeType.LINE_EOF) {
                bc = remark.getBlockCommentEnd();
                if (bc != null && startsWith(bc)) {
                    consumeStr(bc);
                    token = buf.toString();
                    return;
                }
                buf.appendCodePoint(ch);
                ch = getCode();
            }
        } else {
            String lc = remark.getBlockCommentStart();
            skipStr(lc);
            while (ch != CodeType.LINE_EOF) {
                lc = remark.getBlockCommentEnd();
                if (lc != null && startsWith(lc)) {
                    skipStr(lc);
                    return;
                }
                ch = getCode();
            }
        }
        token = buf.toString();
        throw new ScannerError(OP_SYNTAX_END_OF_FILE_IN_BLOCK_COMMENT, OpenOpts.getOffset(reader));
    }

    /**
     * <p>Read the filler.</p>
     * <p>The following filler syntax is supported.</p>
     * <pre>
     *     filler --> layout { layout }.
     *     layout --> whitespace
     *              | control.
     * </pre>
     * <p>For whitespace and control see class CodeType.</p>
     *
     * @throws ScannerError Scanning problem.
     */
    private void nextFiller() throws ScannerError, IOException {
        if ((flags & MASK_RTRN_LAYT) != 0) {
            buf.appendCodePoint(ch);
            ch = getCode();
            while (ch != CodeType.LINE_EOF &&
                    delemiter.isLayout(ch)) {
                buf.appendCodePoint(ch);
                ch = getCode();
            }
            token = buf.toString();
        } else {
            ch = getCode();
            while (ch != CodeType.LINE_EOF &&
                    delemiter.isLayout(ch)) {
                ch = getCode();
            }
        }
    }

    /*************************************************************/
    /* Scanning Cursor                                           */
    /*************************************************************/

    /**
     * <p>Retrieve the first token.</p>
     *
     * @throws ScannerError Scanning problem.
     */
    public void firstToken() throws ScannerError, IOException {
        ch = getCode();
        nextToken();
    }

    /**
     * <p>Advance to next token.</p>
     *
     * @throws ScannerError Scanning problem.
     */
    public void nextToken() throws ScannerError, IOException {
        buf.setLength(0);
        while (ch != CodeType.LINE_EOF) {
            switch (delemiter.classOf(ch)) {
                case CodeType.SUB_CLASS_WHITESPACE:
                case CodeType.SUB_CLASS_CONTROL:
                    nextFiller();
                    if ((flags & MASK_RTRN_LAYT) != 0)
                        return;
                    continue;
                case CodeType.SUB_CLASS_INVALID:
                case CodeType.SUB_CLASS_SOLO:
                    String lc = remark.getLineComment();
                    if (lc != null && startsWith(lc)) {
                        nextLineComment();
                        if ((flags & MASK_RTRN_LINE) != 0)
                            return;
                        continue;
                    }
                    if (delemiter.getQuotes().indexOf(ch) != -1) {
                        nextString();
                        return;
                    }
                    buf.appendCodePoint(ch);
                    ch = getCode();
                    token = buf.toString();
                    return;
                case CodeType.SUB_CLASS_UNDERSCORE:
                case CodeType.SUB_CLASS_LOWER:
                case CodeType.SUB_CLASS_UPPER:
                case CodeType.SUB_CLASS_OTHER:
                    buf.appendCodePoint(ch);
                    ch = getCode();
                    while (ch != CodeType.LINE_EOF && delemiter.isAlfanum(ch)) {
                        buf.appendCodePoint(ch);
                        ch = getCode();
                    }
                    token = buf.toString();
                    return;
                case CodeType.SUB_CLASS_DIGIT:
                    nextNumber();
                    return;
                case CodeType.SUB_CLASS_GRAPHIC:
                    lc = remark.getLineComment();
                    if (lc != null && startsWith(lc)) {
                        nextLineComment();
                        if ((flags & MASK_RTRN_LINE) != 0)
                            return;
                        continue;
                    }
                    lc = remark.getBlockCommentStart();
                    if (lc != null && startsWith(lc)) {
                        nextBlockComment();
                        if ((flags & MASK_RTRN_BLCK) != 0)
                            return;
                        continue;
                    }
                    buf.appendCodePoint(ch);
                    ch = getCode();
                    while (ch != CodeType.LINE_EOF && delemiter.isGraphic(ch)) {
                        buf.appendCodePoint(ch);
                        ch = getCode();
                    }
                    token = buf.toString();
                    return;
                default:
                    throw new IllegalArgumentException("illegal subclass");
            }
        }
        token = buf.toString();
    }

    /**
     * <p>Retriev the look ahead character.</p>
     *
     * @return The look ahead code point.
     */
    public int lookAhead() {
        return ch;
    }

    /**************************************************************/
    /* Terminal Point                                             */
    /**************************************************************/

    /**
     * <p>Check whether we are at second of a terminal point.</p>
     *
     * @return True if we are at second of a terminal point, otherwise false.
     * @throws ScannerError Scanning problem.
     */
    public boolean isTerminalSuffix() throws ScannerError, IOException {
        if (ch == CodeType.LINE_EOF)
            return true;
        if (delemiter.isLayout(ch))
            return true;
        String lc = remark.getLineComment();
        if (lc != null && startsWith(lc))
            return true;
        return false;
    }

    /**
     * <p>Read the spaces.</p>
     * <p>The following spaces syntax is supported.</p>
     * <pre>
     *     spaces --> space { space }.
     *     space  --> ~eol whitespace
     *              | control.
     * </pre>
     * <p>For whitespace and control see class CodeType.</p>
     *
     * @throws ScannerError Scanning problem.
     */
    private void nextSpaces() throws ScannerError, IOException {
        if ((flags & MASK_RTRN_LAYT) != 0) {
            buf.appendCodePoint(ch);
            ch = getCode();
            while (ch != CodeType.LINE_EOF &&
                    ch != CodeType.LINE_EOL &&
                    delemiter.isLayout(ch)) {
                buf.appendCodePoint(ch);
                ch = getCode();
            }
            token = buf.toString();
        } else {
            ch = getCode();
            while (ch != CodeType.LINE_EOF &&
                    ch != CodeType.LINE_EOL &&
                    delemiter.isLayout(ch)) {
                ch = getCode();
            }
        }
    }

    /**
     * <p>Advance to next terminal suffix.</p>
     *
     * @throws ScannerError Scanning problem.
     */
    public void nextTerminalSuffix() throws ScannerError, IOException {
        buf.setLength(0);
        while (ch != CodeType.LINE_EOF) {
            switch (delemiter.classOf(ch)) {
                case CodeType.SUB_CLASS_WHITESPACE:
                case CodeType.SUB_CLASS_CONTROL:
                    if (ch != CodeType.LINE_EOL) {
                        nextSpaces();
                        if ((flags & MASK_RTRN_LAYT) != 0)
                            return;
                        continue;
                    }
                    buf.appendCodePoint(ch);
                    token = buf.toString();
                    return;
                case CodeType.SUB_CLASS_INVALID:
                case CodeType.SUB_CLASS_SOLO:
                    String lc = remark.getLineComment();
                    if (lc != null && startsWith(lc)) {
                        nextLineComment();
                        if ((flags & MASK_RTRN_LINE) != 0)
                            return;
                        continue;
                    }
                    token = buf.toString();
                    return;
                case CodeType.SUB_CLASS_UNDERSCORE:
                case CodeType.SUB_CLASS_LOWER:
                case CodeType.SUB_CLASS_UPPER:
                case CodeType.SUB_CLASS_OTHER:
                case CodeType.SUB_CLASS_DIGIT:
                case CodeType.SUB_CLASS_GRAPHIC:
                    token = buf.toString();
                    return;
                default:
                    throw new IllegalArgumentException("illegal subclass");
            }
        }
        token = buf.toString();
    }

    /*************************************************************/
    /* Surrogate Pair Helpers                                    */
    /*************************************************************/

    /**
     * <p>Get a code from a text stream.</p>
     *
     * @return The read code point or -1.
     * @throws ScannerError Scanning problem.
     */
    private int getCode() throws ScannerError, IOException {
        int ch = reader.read();
        if (Character.isHighSurrogate((char) ch)) {
            reader.mark(1);
            int ch2;
            try {
                ch2 = reader.read();
            } catch (IOException x) {
                reader.reset();
                throw x;
            }
            reader.reset();
            if (Character.isLowSurrogate((char) ch2)) {
                ch = Character.toCodePoint((char) ch, (char) ch2);
                reader.read();
            }
        }
        return ch;
    }

    /**
     * <p>Peek a code from a text stream.</p>
     *
     * @return The peeked code point or -1.
     * @throws ScannerError Scanning problem.
     */
    private int peekCode() throws ScannerError, IOException {
        reader.mark(2);
        int ch;
        try {
            ch = reader.read();
            if (Character.isHighSurrogate((char) ch)) {
                int ch2 = reader.read();
                if (Character.isLowSurrogate((char) ch2))
                    ch = Character.toCodePoint((char) ch, (char) ch2);
            }
        } catch (IOException x) {
            reader.reset();
            throw x;
        }
        reader.reset();
        return ch;
    }

    /**
     * <p>Push back the lock ahead character.</p>
     *
     * @throws IOException Scanning problem.
     */
    public void pushBack() throws IOException {
        int k = (ch != CodeType.LINE_EOF ? Character.charCount(ch) : 0);
        reader.skip(-k);
    }

    /**********************************************************/
    /* String Matching                                        */
    /**********************************************************/

    /**
     * <p>Check whether the stream starts with a string.</p>
     *
     * @param s The string.
     * @return True if the stream starts with, otherwise false.
     * @throws ScannerError Scanning problem.
     */
    private boolean startsWith(String s) throws ScannerError, IOException {
        if (s.length() == 1) {
            return (ch == s.charAt(0));
        } else if (s.length() == 2) {
            return (ch == s.charAt(0) && peekCode() == s.charAt(1));
        } else {
            throw new IllegalArgumentException("illegal config");
        }
    }

    /**
     * <p>Consume the stream started with a string.</p>
     *
     * @param s The string.
     * @throws ScannerError Scanning problem.
     */
    private void consumeStr(String s) throws ScannerError, IOException {
        if (s.length() == 1) {
            buf.appendCodePoint(ch);
            ch = getCode();
        } else if (s.length() == 2) {
            buf.appendCodePoint(ch);
            ch = getCode();
            buf.appendCodePoint(ch);
            ch = getCode();
        } else {
            throw new IllegalArgumentException("illegal config");
        }
    }

    /**
     * <p>Skip the stream started with a strimng.</p>
     *
     * @param s The string.
     * @throws ScannerError Scanning problem.
     */
    private void skipStr(String s) throws ScannerError, IOException {
        if (s.length() == 1) {
            ch = getCode();
        } else if (s.length() == 2) {
            getCode();
            ch = getCode();
        } else {
            throw new IllegalArgumentException("illegal config");
        }
    }

    /**
     * <p>Retrieve the text bundle.</p>
     *
     * @param locale The locale.
     * @return The text bundle.
     */
    public static Properties getLang(Locale locale) {
        return LangProperties.getLang(ScannerToken.class, "scanner", locale);
    }

}
