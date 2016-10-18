package matula.util.regex;

/**
 * <p>This class describes the Prolog language.</p>
 *
 * @author CopyRight 2003-2014, XLOG Technology GmbH, Jan Burse
 * @version QA (Quality Assurance) v1.0
 */
public class CompLang {
    public final static CompLang ISO_COMPLANG = new CompLang();

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
}
