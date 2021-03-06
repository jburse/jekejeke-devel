package matula.util.misc;

/**
 * <p>Classify code points.</p>
 * <p>The following character classifications are used:</p>
 * <pre>
 *     whitespace -->  space_separator | line_separator | paragraph_separator
 *     layout -->      ~hints (control | format).
 *     invalid -->     unassigned | surrogate | private_use | invalid.
 *     solo -->        start_punctuation | end_punctuation | initial_quote_punctuation |
 *                     final_quote_punctuation | delemiter | quote.
 *     underscore -->  connector_punctuation.
 *     lower -->       lowercase_letter | modifier_letter | other_letter.
 *     upper -->       uppercase_letter | titlecase_letter.
 *     alfanum -->     non_spacing_mark | enclosing_mark |
 *                     combining_spacing_mark | letter_number |
 *                     other_number | joiner | hints.
 *     digit -->       decimal_digit_number.
 *     graphic -->     ~delemiter ~quote ~invalid ~joiner (dash_punctuation |
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
public class CodeType {
    public final static CodeType ISO_CODETYPE = new CodeType();

    public static final int LINE_EOF = -1;
    public static final char LINE_EOL = '\n';

    public static final int SUB_CLASS_WHITESPACE = 0;
    public static final int SUB_CLASS_LAYOUT = 1;

    public static final int SUB_CLASS_INVALID = 2;
    public static final int SUB_CLASS_SOLO = 3;

    public static final int SUB_CLASS_UNDERSCORE = 4;
    public static final int SUB_CLASS_UPPER = 5;
    public static final int SUB_CLASS_LOWER = 6;
    public static final int SUB_CLASS_ALFANUM = 7;
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
        ISO_CODETYPE.setInvalids("\uFFFD");
        ISO_CODETYPE.setJoiners("");
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
                    return CodeType.SUB_CLASS_ALFANUM;
                } else {
                    return CodeType.SUB_CLASS_LAYOUT;
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
                return CodeType.SUB_CLASS_ALFANUM;
            case Character.DECIMAL_DIGIT_NUMBER:
                return CodeType.SUB_CLASS_DIGIT;
            case Character.LETTER_NUMBER:
            case Character.OTHER_NUMBER:
                return CodeType.SUB_CLASS_ALFANUM;
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
                    return CodeType.SUB_CLASS_ALFANUM;
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
                subtype == CodeType.SUB_CLASS_LAYOUT);
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

}
