package matula.util.regex;

/**
 * <p>Classify code points.</p>
 * <p>The following character classifications are used:</p>
 * <pre>
 *     whitespace -->  space_separator | line_separator | paragraph_separator
 *     control    -->  ~hints (control | format).
 *     invalid    -->  unassigned | surrogate | private_use | invalid.
 *     solo       -->  start_punctuation | end_punctuation | initial_quote_punctuation |
 *                     final_quote_punctuation | delemiter | quote.
 *     underscore -->  connector_punctuation.
 *     lower      -->  lowercase_letter | modifier_letter | other_letter.
 *     upper      -->  uppercase_letter | titlecase_letter.
 *     other      -->  non_spacing_mark | enclosing_mark |
 *                     combining_spacing_mark | letter_number |
 *                     other_number | joiner | hints.
 *     digit      -->  decimal_digit_number.
 *     graphic    -->  ~delemiter ~quote ~invalid ~joiner (dash_punctuation |
 *                     other_punctuation
 *                     math_symbol |
 *                     currency_symbol |
 *                     modifier_symbol |
 *                     other_symbol).
 * </pre>
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
public final class CodeType {
    public static final CodeType ISO_CODETYPE = new CodeType();
    public static final CodeType ISO_PAT_CODETYPE = new CodeType();

    public static final String OP_SYNTAX_ILLEGAL_UNDERSCORE = "illegal_underscore";
    public static final String OP_SYNTAX_DOUBLING_MISSING = "doubling_missing";

    public static final int LINE_EOF = -1;
    public static final char LINE_EOL = '\n';
    public static final char LINE_WIN = '\r';
    public static final char LINE_BACKSLASH = '\\';
    public static final char LINE_ZERO = '0';
    public static final char LINE_SINGLE = '\'';
    public static final char LINE_DOUBLE = '\"';
    public static final char LINE_BACK = '`';

    public static final int SUB_CLASS_WHITESPACE = 0;
    public static final int SUB_CLASS_CONTROL = 1;

    public static final int SUB_CLASS_INVALID = 2;
    public static final int SUB_CLASS_SOLO = 3;

    public static final int SUB_CLASS_UNDERSCORE = 4;
    public static final int SUB_CLASS_UPPER = 5;
    public static final int SUB_CLASS_LOWER = 6;
    public static final int SUB_CLASS_OTHER = 7;
    public static final int SUB_CLASS_DIGIT = 8;

    public static final int SUB_CLASS_GRAPHIC = 9;

    private String hints = "\u200C\u200D";
    private String delemiters = ",;!|";
    private String quotes = "\'\"`";
    private String invalids = "\uFFFD";
    private String joiners = "";

    static {
        ISO_CODETYPE.setHints("\u200C\u200D");
        ISO_CODETYPE.setDelemiters(",;!|%");
        ISO_CODETYPE.setQuotes("\'\"`");
        ISO_CODETYPE.setInvalids("\uFFFD\b\f\r");
        ISO_CODETYPE.setJoiners("");

        ISO_PAT_CODETYPE.setHints("\u200C\u200D");
        ISO_PAT_CODETYPE.setDelemiters(",;!|%");
        ISO_PAT_CODETYPE.setQuotes("\'\"`");
        ISO_PAT_CODETYPE.setInvalids("\uFFFD\b\f\r");
        ISO_PAT_CODETYPE.setJoiners("");
        patternDelemiter(ISO_PAT_CODETYPE);
    }

    /**
     * <p>Modifiy a delemiter to a pattern delemiter.</p>
     *
     * @param ct The delemiter.
     */
    public static void patternDelemiter(CodeType ct) {
        ct.setDelemiters(remove(ct.getDelemiters(), "*?<~>^"));
        ct.setJoiners(add(ct.getJoiners(), "*?<~>^"));
    }

    /**
     * <p>Set the hints.</p>
     *
     * @param h The hints.
     */
    public void setHints(String h) {
        hints = h;
    }

    /**
     * <p>Retrieve the hints.</p>
     *
     * @return The hints.
     */
    public String getHints() {
        return hints;
    }

    /**
     * <p>Set the delemiters.</p>
     *
     * @param d The delemiters.
     */
    public void setDelemiters(String d) {
        delemiters = d;
    }

    /**
     * <p>Retrieve the delemiters.</p>
     *
     * @return The delemiters.
     */
    public String getDelemiters() {
        return delemiters;
    }

    /**
     * <p>Set the quotes.</p>
     *
     * @param s The quotes.
     */
    public void setQuotes(String s) {
        quotes = s;
    }

    /**
     * <p>Retrieve the quotes.</p>
     *
     * @return The quotes.
     */
    public String getQuotes() {
        return quotes;
    }

    /**
     * <p>Set the invalids.</p>
     *
     * @param i The invalids.
     */
    public void setInvalids(String i) {
        invalids = i;
    }

    /**
     * <p>Retrieve the invalids.</p>
     *
     * @return The invalids.
     */
    public String getInvalids() {
        return invalids;
    }

    /**
     * <p>Set the joiners.</p>
     *
     * @param j The joiners.
     */
    public void setJoiners(String j) {
        joiners = j;
    }

    /**
     * <p>Get the joiners.</p>
     *
     * @return The joiners.
     */
    public String getJoiners() {
        return joiners;
    }

    /**
     * <p>Remove some characters from a string.</p<
     *
     * @param s The string.
     * @param d The characters to remove.
     * @return The result.
     */
    public static String remove(String s, String d) {
        StringBuilder buf = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            char ch = s.charAt(i);
            if (d.indexOf(ch) == -1)
                buf.append(ch);
        }
        return buf.toString();
    }

    /**
     * <p>Add some characters to a string.</p>
     *
     * @param s The string.
     * @param d The characters to add.
     * @return The result.
     */
    public static String add(String s, String d) {
        StringBuilder buf = new StringBuilder(s);
        for (int i = 0; i < d.length(); i++) {
            char ch = d.charAt(i);
            if (s.indexOf(ch) == -1)
                buf.append(ch);
        }
        return buf.toString();
    }

    /*******************************************************/
    /* Classify Code Points                                */
    /*******************************************************/

    /**
     * <p>Sub classify a code point.</p>
     *
     * @param cp The code point to classify.
     * @return The sub classification.
     */
    public int classOf(int cp) {
        int type = Character.getType(cp);
        switch (type) {
            case Character.SPACE_SEPARATOR:
            case Character.LINE_SEPARATOR:
            case Character.PARAGRAPH_SEPARATOR:
                return CodeType.SUB_CLASS_WHITESPACE;
            case Character.CONTROL:
            case Character.FORMAT:
                if (hints.indexOf(cp) != -1) {
                    return CodeType.SUB_CLASS_OTHER;
                } else if (invalids.indexOf(cp) != -1) {
                    return CodeType.SUB_CLASS_INVALID;
                } else {
                    return CodeType.SUB_CLASS_CONTROL;
                }
            case Character.UNASSIGNED:
            case Character.SURROGATE:
            case Character.PRIVATE_USE:
                return CodeType.SUB_CLASS_INVALID;
            case Character.START_PUNCTUATION:
            case Character.END_PUNCTUATION:
            case Character.INITIAL_QUOTE_PUNCTUATION:
            case Character.FINAL_QUOTE_PUNCTUATION:
                return CodeType.SUB_CLASS_SOLO;
            case Character.UPPERCASE_LETTER:
                return CodeType.SUB_CLASS_UPPER;
            case Character.LOWERCASE_LETTER:
                return CodeType.SUB_CLASS_LOWER;
            case Character.TITLECASE_LETTER:
                return CodeType.SUB_CLASS_UPPER;
            case Character.MODIFIER_LETTER:
            case Character.OTHER_LETTER:
                return CodeType.SUB_CLASS_LOWER;
            case Character.NON_SPACING_MARK:
            case Character.ENCLOSING_MARK:
            case Character.COMBINING_SPACING_MARK:
                return CodeType.SUB_CLASS_OTHER;
            case Character.DECIMAL_DIGIT_NUMBER:
                return CodeType.SUB_CLASS_DIGIT;
            case Character.LETTER_NUMBER:
            case Character.OTHER_NUMBER:
                return CodeType.SUB_CLASS_OTHER;
            case Character.CONNECTOR_PUNCTUATION:
                return CodeType.SUB_CLASS_UNDERSCORE;
            default:
                if (delemiters.indexOf(cp) != -1) {
                    return CodeType.SUB_CLASS_SOLO;
                } else if (quotes.indexOf(cp) != -1) {
                    return CodeType.SUB_CLASS_SOLO;
                } else if (invalids.indexOf(cp) != -1) {
                    return CodeType.SUB_CLASS_INVALID;
                } else if (joiners.indexOf(cp) != -1) {
                    return CodeType.SUB_CLASS_OTHER;
                } else {
                    return CodeType.SUB_CLASS_GRAPHIC;
                }
        }
    }

    /**
     * <p>Check whether the code point is a layout character.</p>
     *
     * @param cp The code point.
     * @return True if the character is a layout character, otherwise false.
     */
    public boolean isLayout(int cp) {
        int subtype = classOf(cp);
        return (subtype == CodeType.SUB_CLASS_WHITESPACE ||
                subtype == CodeType.SUB_CLASS_CONTROL);
    }

    /**
     * <p>Check whether the code point is a solo character.</p>
     *
     * @param cp The code point.
     * @return True if the character is a solo character, otherwise false.
     */
    public boolean isSolo(int cp) {
        int subtype = classOf(cp);
        return (subtype == CodeType.SUB_CLASS_INVALID ||
                subtype == CodeType.SUB_CLASS_SOLO);
    }

    /**
     * <p>Check whether the code point is an alfanum character.</p>
     *
     * @param cp The code point.
     * @return True if the character is a alfanum character, otherwise false.
     */
    public boolean isAlfanum(int cp) {
        int subtype = classOf(cp);
        return (CodeType.SUB_CLASS_UNDERSCORE <= subtype &&
                subtype <= CodeType.SUB_CLASS_DIGIT);
    }

    /**
     * <p>Check whether the code point is a graphic character.</p>
     *
     * @param cp The code point.
     * @return True if the character is a graphic character, otherwise false.
     */
    public boolean isGraphic(int cp) {
        return classOf(cp) == CodeType.SUB_CLASS_GRAPHIC;
    }

    /**
     * <p>Check whether the code point is an upper case letter.</p>
     *
     * @param cp The code point.
     * @return True if the character an upper case letter, otherwise false.
     */
    public boolean isUpper(int cp) {
        return (classOf(cp) == CodeType.SUB_CLASS_UPPER);
    }

    /**
     * <p>Check whether the code point is an underscore.</p>
     *
     * @param cp The code point.
     * @return True if the code point is an underscore.
     */
    public boolean isUnderscore(int cp) {
        return (classOf(cp) == CodeType.SUB_CLASS_UNDERSCORE);
    }

    /**
     * <p>Check whether the code point is valid.</p>
     *
     * @param cp code point.
     * @return True if the character is valid, otherwise false.
     */
    public boolean isValid(int cp) {
        return (classOf(cp) != CodeType.SUB_CLASS_INVALID);
    }

    /*******************************************************/
    /* Classify Word Breaks                                */
    /*******************************************************/

    /**
     * @see #wordBreak1
     */
    private static boolean[][] wordbreak1 =
            {{false, false, false, false, false, false, false, false, false, false},
                    {false, false, false, false, false, false, false, false, false, false},
                    {true, true, true, true, true, true, true, true, true, true},
                    {true, true, true, true, true, true, true, true, true, true},
                    {true, true, true, true, false, false, false, false, false, true},
                    {true, true, true, true, false, false, false, false, false, true},
                    {true, true, true, true, false, false, false, false, false, true},
                    {true, true, true, true, false, false, false, false, false, true},
                    {true, true, true, true, false, false, false, false, false, true},
                    {true, true, true, true, true, true, true, true, true, false}};

    /**
     * @see #wordBreak2
     */
    private static boolean[][] wordbreak2 =
            {{false, false, true, true, true, true, true, true, true, true},
                    {false, false, true, true, true, true, true, true, true, true},
                    {false, false, true, true, true, true, true, true, true, true},
                    {false, false, true, true, true, true, true, true, true, true},
                    {false, false, true, true, false, false, false, false, false, true},
                    {false, false, true, true, false, false, false, false, false, true},
                    {false, false, true, true, false, false, false, false, false, true},
                    {false, false, true, true, false, false, false, false, false, true},
                    {false, false, true, true, false, false, false, false, false, true},
                    {false, false, true, true, true, true, true, true, true, false}};

    /**
     * <p>Determine the end of a word break.</p>
     *
     * @param ch1 The last code point.
     * @param ch2 The last code point.
     * @return True if the position is the beginning of a word break, otherwise false.
     */
    public boolean wordBreak1(int ch1, int ch2) {
        return wordbreak1[ch1 == LINE_EOF ? CodeType.SUB_CLASS_INVALID : classOf(ch1)]
                [ch2 == LINE_EOF ? CodeType.SUB_CLASS_INVALID : classOf(ch2)];
    }

    /**
     * <p>Determine the beginning of a word break.</p>
     *
     * @param ch1 The last code point.
     * @param ch2 The last code point.
     * @return True if the position is the beginning of a word break, otherwise false.
     */
    public boolean wordBreak2(int ch1, int ch2) {
        return wordbreak2[ch1 == LINE_EOF ? CodeType.SUB_CLASS_INVALID : classOf(ch1)]
                [ch2 == LINE_EOF ? CodeType.SUB_CLASS_INVALID : classOf(ch2)];
    }

    /**
     * <p>Determine the end of a word break.</p>
     *
     * @param t The string to check.
     * @param k The position to check.
     * @return true if the position is the beginning of a word break, otherwise false.
     */
    public boolean wordBreak1(int k, String t) {
        return wordBreak1(k == 0 ? LINE_EOF : t.codePointBefore(k),
                k == t.length() ? LINE_EOF : t.codePointAt(k));
    }

    /**
     * <p>Determine the beginnig of a word break.</p>
     *
     * @param t The string to check.
     * @param k The position to check.
     * @return true if the position is the end of a word break, otherwise false.
     */
    public boolean wordBreak2(int k, String t) {
        return wordBreak2(k == 0 ? LINE_EOF : t.codePointBefore(k),
                    k == t.length() ? LINE_EOF : t.codePointAt(k));
    }

    /**
     * <p>Check whether the string is a single token.</p>
     * <p>The check consists of:</p>
     * <ul>
     * <li>Is not empty.</li>
     * <li>Does not start with a quote character.</li>
     * <li>Does not start with an invalid character.</li>
     * <li>Does not start with a layout character.</li>
     * <li>Does not contain a word end.</li>
     * </ul>
     * <p>Version that can handle >16 bit Unicode.</p>
     *
     * @param str The token to check.
     * @return True if the string is a single token, otherwise false.
     */
    public boolean singleToken(String str) {
        if (str.length() == 0)
            return false;
        int ch = str.codePointAt(0);
        if (getQuotes().indexOf(ch) != -1)
            return false;
        if (!isValid(ch))
            return false;
        if (isLayout(ch))
            return false;
        int n = str.length();
        int pos = 0;
        while (pos < n) {
            if (pos != 0 && wordBreak1(pos, str))
                return false;
            pos += Character.charCount(ch);
        }
        return true;
    }

    /***********************************************************************/
    /* Number Normalization                                                */
    /***********************************************************************/

    /**
     * <p>Strip the underscores from a string.</p>
     *
     * @param str The string.
     * @return The stripped string.
     */
    public String stripUnderscore(String str) {
        StringBuilder buf = null;
        int n = str.length();
        int pos = 0;
        while (pos < n) {
            int k = str.codePointAt(pos);
            if (isUnderscore(k)) {
                if (buf == null)
                    buf = new StringBuilder(str.substring(0, pos));
                /* skip */
            } else {
                if (buf != null)
                    buf.appendCodePoint(k);
            }
            pos += Character.charCount(k);
        }
        if (buf != null)
            return buf.toString();
        return str;
    }

    /**
     * <p>Check whether a string does neither starts nor ends
     * with an underscore, nor has a twin underscore.</p>
     * <p>We have to check again, since we strip underscores and toNumber()
     * is also called from number_codes/2.</p>
     *
     * @param pos    The index.
     * @param pos2   The second index.
     * @param str    The string.
     * @param offset The offset.
     * @throws ScannerError If the string starts with a sign.
     */
    public void checkUnderscore(int pos, int pos2, String str,
                                int offset)
            throws ScannerError {
        if (!(pos < pos2))
            return;
        int ch = str.codePointAt(pos);
        if (isUnderscore(ch))
            throw new ScannerError(OP_SYNTAX_ILLEGAL_UNDERSCORE, pos + offset);
        pos += Character.charCount(ch);

        if (!(pos < pos2))
            return;
        ch = str.codePointBefore(pos2);
        pos2 -= Character.charCount(ch);
        if (isUnderscore(ch))
            throw new ScannerError(OP_SYNTAX_ILLEGAL_UNDERSCORE, pos2 + offset);

        int k = indexTwin(pos, pos2, str);
        if (k != -1)
            throw new ScannerError(OP_SYNTAX_ILLEGAL_UNDERSCORE, k + offset);
    }

    /**
     * <p>Find an underscore twin.</p>
     *
     * @param pos  The start of the string region.
     * @param pos2 The end of the string region.
     * @param str  The string.
     * @return The first index, or -1.
     */
    private int indexTwin(int pos, int pos2, String str) {
        boolean last = false;
        while (pos < pos2) {
            int ch = str.codePointAt(pos);
            if (isUnderscore(ch)) {
                if (!last) {
                    last = true;
                } else {
                    return pos;
                }
            } else {
                if (last)
                    last = false;
            }
            pos += Character.charCount(ch);
        }
        return -1;
    }

    /***********************************************************************/
    /* String Normalization                                                */
    /***********************************************************************/

    /**
     * <p>Resolve the doubles.</p>
     * <p>Unbalanced escapes are tolerated.</p>
     * <p>Version that can handle >16 bit Unicode.</p>
     *
     * @param str    The token.
     * @param quote  The quote.
     * @param offset The error offset.
     * @return The resolved token.
     * @throws ScannerError Parsing problem.
     */
    public String resolveDouble(String str, int quote, int offset)
            throws ScannerError {
        StringBuilder buf = null;
        int n = str.length();
        for (int i = 0; i < n; i++) {
            int k = str.codePointAt(i);
            if (k == quote) {
                if (i + Character.charCount(k) < n &&
                        str.codePointAt(i + Character.charCount(k)) == quote) {
                    if (buf == null)
                        buf = new StringBuilder(str.substring(0, i));
                    i += Character.charCount(k);
                    buf.appendCodePoint(k);
                } else {
                    throw new ScannerError(OP_SYNTAX_DOUBLING_MISSING, offset + i);
                }
            } else if (k == LINE_BACKSLASH) {
                if (buf != null)
                    buf.appendCodePoint(k);
                if (i + Character.charCount(k) < n) {
                    i += Character.charCount(k);
                    k = str.codePointAt(i);
                    if (buf != null)
                        buf.appendCodePoint(k);
                    if (Character.digit(k, 8) != -1 || k == 'x') {
                        int k2;
                        while (i + Character.charCount(k) < n &&
                                isAlfanum(k2 = str.codePointAt(i + Character.charCount(k)))) {
                            i += Character.charCount(k);
                            k = k2;
                            if (buf != null)
                                buf.appendCodePoint(k);
                        }
                        if (i + Character.charCount(k) < n &&
                                str.codePointAt(i + Character.charCount(k)) == LINE_BACKSLASH) {
                            i += Character.charCount(k);
                            k = LINE_BACKSLASH;
                            if (buf != null)
                                buf.appendCodePoint(k);
                        }
                    }
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
     * <p>Double the quotes.</p>
     * <p>Unbalanced escapes are tolerated.</p>
     * <p>Version that can handle >16 bit Unicode.</p>
     *
     * @param str   The token.
     * @param quote The quote.
     * @return The doubled token.
     */
    public String doubleQuote(String str, int quote) {
        StringBuilder buf = null;
        int n = str.length();
        for (int i = 0; i < n; i++) {
            int k = str.codePointAt(i);
            if (k == quote) {
                if (buf == null)
                    buf = new StringBuilder(str.substring(0, i));
                buf.appendCodePoint(quote);
                buf.appendCodePoint(quote);
            } else if (k == LINE_BACKSLASH) {
                if (buf != null)
                    buf.appendCodePoint(LINE_BACKSLASH);
                if (i + Character.charCount(k) < n) {
                    i += Character.charCount(k);
                    k = str.codePointAt(i);
                    if (buf != null)
                        buf.appendCodePoint(k);
                    if (Character.digit(k, 8) != -1 || k == 'x') {
                        int k2;
                        while (i + Character.charCount(k) < n &&
                                isAlfanum(k2 = str.codePointAt(i + Character.charCount(k)))) {
                            i += Character.charCount(k);
                            k = k2;
                            if (buf != null)
                                buf.appendCodePoint(k);
                        }
                        if (i + Character.charCount(k) < n &&
                                str.codePointAt(i + Character.charCount(k)) == LINE_BACKSLASH) {
                            i += Character.charCount(k);
                            k = LINE_BACKSLASH;
                            if (buf != null)
                                buf.appendCodePoint(k);
                        }
                    }
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
