package matula.util.regex;

import matula.util.system.ConnectionReader;
import matula.util.text.LangProperties;
import matula.util.text.PropertiesUnion;
import util.regex.CompilerAdvanced;

import java.io.IOException;
import java.io.StringReader;
import java.util.Locale;
import java.util.Properties;

/**
 * <p>This class provides the base class for string specimen.</p>
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

public abstract class AbstractSpecimen extends AbstractPattern {
    public static final int MATCH_NEGATIV = 0x00000001;

    public static final int MASK_PAT_BOUNCE = 0x00000001;

    protected String pattern = "*";
    protected int flag;

    /**
     * <p>Set the pattern.</p>
     *
     * @param p The pattern.
     */
    public void setPattern(String p) {
        pattern = p;
    }

    /**
     * <p>Retrieve the pattern.</p>
     *
     * @return The pattern.
     */
    public String getPattern() {
        return pattern;
    }

    /**
     * <p>Set the flags.</p>
     *
     * @param f The flags.
     */
    public final void setFlag(int f) {
        flag = f;
    }

    /**
     * <p>Retrieve the flags.</p>
     *
     * @return The flags.
     */
    public final int getFlag() {
        return flag;
    }

    /******************************************************************/
    /* Preparation                                                    */
    /******************************************************************/

    /**
     * <p>Repare the pattern.</p>
     *
     * @throws ScannerError Shit happens.
     */
    public abstract void prepareMatch()
            throws ScannerError;

    /**
     * <p>Move the pattern to the replace target.</p>
     *
     * @param match The pattern to move.
     */
    public abstract void replaceTo(AbstractSpecimen match);

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
    public static AbstractSpecimen createPatternWord(String s, CodeType pd,
                                                CompLang r, int expr,
                                                AbstractCompiler comp,
                                                CodeType md)
            throws ScannerError {
        try {
            ScannerToken st = new ScannerToken();
            ConnectionReader cr = new ConnectionReader(new StringReader(s));
            st.setReader(cr);
            st.setDelemiter(pd);
            st.setRemark(r);
            st.firstToken();
            AbstractSpecimen matcher = comp.parseMatcher(st, expr, md);
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
    public static AbstractSpecimen createPatternWord(String s, CodeType pd,
                                                CompLang r,
                                                CodeType md)
            throws ScannerError {
        return createPatternWord(s, pd, r,
                EXPRESSION_EQUALS | EXPRESSION_SINGLEQUOTE,
                CompilerAdvanced.DEFAULT, md);
    }

    /**
     * <p>Creates a pattern matcher.</p>
     *
     * @param s The string to create the pattern matcher from.
     * @return The pattern matcher.
     * @throws ScannerError Shit happens.
     */
    public static AbstractSpecimen createPatternWord(String s)
            throws ScannerError {
        return createPatternWord(s, CodeType.ISO_PAT_CODETYPE,
                CompLang.ISO_COMPLANG, CodeType.ISO_CODETYPE);
    }

    /**
     * <p>Return a copy of the matcher.</p>
     *
     * @return The copy.
     */
    public abstract AbstractSpecimen copyMatcher();

    /**
     * <p>Retrieve the text bundle.</p>
     *
     * @param locale The locale.
     * @return The text bundle.
     */
    public static Properties getLang(Locale locale) {
        Properties[] props = new Properties[2];
        props[0] = LangProperties.getLang(AbstractSpecimen.class, "pattern", locale);
        props[1] = ScannerToken.getLang(locale);
        return new PropertiesUnion(props);
    }

}
