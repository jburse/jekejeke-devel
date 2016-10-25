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
    public static final CodeType TEXT_CODETYPE = new CodeType();
    public static final CodeType TEXT_PAT_CODETYPE = new CodeType();
    public static final CompLang TEXT_COMPLANG = new CompLang();
    public static final CompilerSimple TEXT_COMPILERSIMPLE = new CompilerSimple();

    public static final int EXPRESSION_SINGLEQUOTE = 0x00000001;
    public static final int EXPRESSION_EQUALS = 0x00000002;

    public static final String ERROR_SYNTAX_SUPERFLUOUS_TOKEN = "superfluous_token";

    protected CodeType patdelemiter;
    protected CompLang remark;
    protected CodeType matchdelemiter;

    static {
        TEXT_CODETYPE.setHints("\u200C\u200D");
        TEXT_CODETYPE.setDelemiters("!");
        TEXT_CODETYPE.setQuotes("\'\"`");
        TEXT_CODETYPE.setInvalids("\uFFFD");
        TEXT_CODETYPE.setJoiners(".$");

        TEXT_PAT_CODETYPE.setHints("\u200C\u200D");
        TEXT_PAT_CODETYPE.setDelemiters("!");
        TEXT_PAT_CODETYPE.setQuotes("\'\"`");
        TEXT_PAT_CODETYPE.setInvalids("\uFFFD");
        TEXT_PAT_CODETYPE.setJoiners(".$");
        CodeType.patternDelemiter(TEXT_PAT_CODETYPE);

        TEXT_COMPLANG.setLineComment(null);
        TEXT_COMPLANG.setBlockCommentStart(null);
        TEXT_COMPLANG.setBlockCommentEnd(null);
        TEXT_COMPLANG.setEnd(-1);

        TEXT_COMPILERSIMPLE.setPatDelemiter(TEXT_PAT_CODETYPE);
        TEXT_COMPILERSIMPLE.setRemark(TEXT_COMPLANG);
        TEXT_COMPILERSIMPLE.setMatchDelemiter(TEXT_CODETYPE);
    }

    /**
     * <p>Set the pattern delemiter.</p>
     *
     * @param pd The pattrn delemiter.
     */
    public void setPatDelemiter(CodeType pd) {
        patdelemiter = pd;
    }

    /**
     * <p>Retrieve the pattern delemiter.</p>
     *
     * @return The pattern delemiter.
     */
    public CodeType getPatDelemiter() {
        return patdelemiter;
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
     * <p>Set the remark.</p>
     *
     * @param r The remark.
     */
    public void setRemark(CompLang r) {
        remark = r;
    }

    /**
     * <p>Set the match delemiter.</p>
     * @param md The match delemiter.
     */
    public void setMatchDelemiter(CodeType md) {
        matchdelemiter = md;
    }

    /**
     * <p>Retrieve the match delemiter.</p>
     *
     * @return The match delemiter.
     */
    public CodeType getMatchDelemiter() {
        return matchdelemiter;
    }

    /**
     * <p>Parse a pattern.</p>
     *
     * @param st   The scanner token.
     * @param expr The flags.
     * @return The pattern.
     * @throws ScannerError Scanner error.
     * @throws IOException  IO error.
     */
    public abstract AbstractSpecimen parseMatcher(ScannerToken st, int expr)
            throws ScannerError, IOException;

    /**
     * <p>Creates a pattern matcher.</p>
     *
     * @param s    The string to create the pattern matcher from.
     * @param expr The expression features to use.
     * @return The pattern matcher.
     * @throws ScannerError Shit happens.
     */
    public AbstractSpecimen createSpecimen(String s,
                                           int expr)
            throws ScannerError {
        try {
            ScannerToken st = new ScannerToken();
            ConnectionReader cr = new ConnectionReader(new StringReader(s));
            st.setReader(cr);
            st.setDelemiter(getPatDelemiter());
            st.setRemark(getRemark());
            st.firstToken();
            AbstractSpecimen matcher = parseMatcher(st, expr);
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
     * @return The pattern matcher.
     * @throws ScannerError Shit happens.
     */
    public AbstractSpecimen createSpecimen(String s)
            throws ScannerError {
        return createSpecimen(s,
                EXPRESSION_EQUALS | EXPRESSION_SINGLEQUOTE);
    }

}
