package matula.util.regex;

import matula.util.system.ConnectionReader;

import java.io.IOException;
import java.io.StringReader;

/**
 * <p>Base class for a pattern compiler.</p>
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
public abstract class AbstractCompiler {
    public static final CodeType SHELL_CODETYPE = new CodeType();
    public static final CodeType SHELL_PAT_CODETYPE = new CodeType();
    public static final CompLang SHELL_COMPLANG = new CompLang();

    public static final int EXPRESSION_SINGLEQUOTE = 0x00000001;
    public static final int EXPRESSION_EQUALS = 0x00000002;

    public static final String ERROR_SYNTAX_SUPERFLUOUS_TOKEN = "superfluous_token";

    static {
        SHELL_CODETYPE.setHints("\u200C\u200D");
        SHELL_CODETYPE.setDelemiters("!");
        SHELL_CODETYPE.setQuotes("\'\"`");
        SHELL_CODETYPE.setInvalids("\uFFFD");
        SHELL_CODETYPE.setJoiners(".$-");

        SHELL_PAT_CODETYPE.setHints("\u200C\u200D");
        SHELL_PAT_CODETYPE.setDelemiters("!");
        SHELL_PAT_CODETYPE.setQuotes("\'\"`");
        SHELL_PAT_CODETYPE.setInvalids("\uFFFD");
        SHELL_PAT_CODETYPE.setJoiners(".$-");
        CodeType.patternDelemiter(SHELL_PAT_CODETYPE);

        SHELL_COMPLANG.setLineComment(null);
        SHELL_COMPLANG.setBlockCommentStart(null);
        SHELL_COMPLANG.setBlockCommentEnd(null);
        SHELL_COMPLANG.setEnd(-1);
    }

    /**
     * <p>Parse a pattern.</p>
     *
     * @param st   The scanner token.
     * @param expr The flags.
     * @param md   The replace code type.
     * @return The pattern.
     * @throws ScannerError Scanner error.
     * @throws IOException  IO error.
     */
    public abstract AbstractSpecimen parseMatcher(ScannerToken st, int expr,
                                                  CodeType md)
            throws ScannerError, IOException;

    /**
     * <p>Creates a pattern matcher.</p>
     *
     * @param s    The string to create the pattern matcher from.
     * @param pd   The pattern delemiter.
     * @param r    The pattern remark.
     * @param expr The expression features to use.
     * @param md   The match delemiter.
     * @return The pattern matcher.
     * @throws ScannerError Shit happens.
     */
    public AbstractSpecimen createSpecimen(String s, CodeType pd,
                                           CompLang r, int expr,
                                           CodeType md)
            throws ScannerError {
        try {
            ScannerToken st = new ScannerToken();
            ConnectionReader cr = new ConnectionReader(new StringReader(s));
            st.setReader(cr);
            st.setDelemiter(pd);
            st.setRemark(r);
            st.firstToken();
            AbstractSpecimen matcher = parseMatcher(st, expr, md);
            if (!"".equals(st.getToken()))
                throw new ScannerError(ERROR_SYNTAX_SUPERFLUOUS_TOKEN,
                        st.getTokenOffset());
            return matcher;
        } catch (IOException x) {
            throw new RuntimeException("shouldn't happen", x);
        }
    }

    /**
     * <p>Creates a pattern matcher.</p>
     *
     * @param s  The string to create the pattern matcher from.
     * @param pd The pattern delemiter.
     * @param r  The pattern remark.
     * @param md The match delemiter.
     * @return The pattern matcher.
     * @throws ScannerError Shit happens.
     */
    public AbstractSpecimen createSpecimen(String s, CodeType pd,
                                           CompLang r,
                                           CodeType md)
            throws ScannerError {
        return createSpecimen(s, pd, r,
                EXPRESSION_EQUALS | EXPRESSION_SINGLEQUOTE, md);
    }

    /**
     * <p>Creates a pattern matcher.</p>
     *
     * @param s The string to create the pattern matcher from.
     * @return The pattern matcher.
     * @throws ScannerError Shit happens.
     */
    public AbstractSpecimen createSpecimen(String s)
            throws ScannerError {
        return createSpecimen(s, SHELL_PAT_CODETYPE,
                SHELL_COMPLANG, SHELL_CODETYPE);
    }

}
