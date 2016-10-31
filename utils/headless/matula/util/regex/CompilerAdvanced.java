package matula.util.regex;

import java.io.IOException;

/**
 * <p>Compiler for an advanced patterns.</p>
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
public final class CompilerAdvanced extends AbstractCompiler {

    /**
     * <p>Creata a specimen from a string.</p>
     *
     * @param pattern The string.
     * @param flag    The specimen features to use.
     * @return The specimen.
     * @throws ScannerError Parsing problem.
     */
    public AbstractSpecimen createSpecimen(String pattern, int flag)
            throws ScannerError {
        SpecimenAdvanced pm = new SpecimenAdvanced();
        pm.setPatDelemiter(getPatDelemiter());
        pm.setMatchDelemiter(getMatchDelemiter());
        pm.setPattern(pattern);
        pm.setFlag(flag);
        pm.prepareMatch();
        return pm;
    }

    /**
     * <p>Parse a pattern.</p>
     *
     * @param st   The tokenizer.
     * @param expr The parse features to use.
     * @return The pattern.
     * @throws ScannerError Parsing problem.
     * @throws IOException  IO error.
     */
    public AbstractSpecimen parseSpecimen(ScannerToken st, int expr)
            throws ScannerError, IOException {
        int flag = 0;
        if ((expr & AbstractSpecimen.MATCH_IGCS) != 0) {
            flag |= SpecimenAdvanced.MATCH_EQSN;
            if ("=".equals(st.getToken())) {
                st.nextToken();
            } else {
                flag |= SpecimenAdvanced.MATCH_IGCS;
            }
        }
        if ("".equals(st.getToken()) ||
                "(".equals(st.getToken()) ||
                ")".equals(st.getToken()) ||
                "!".equals(st.getToken()))
            throw new ScannerError(ERROR_SYNTAX_PHRASE_MISSING,
                    st.getTokenOffset());
        String pattern;
        if ((expr & AbstractSpecimen.MATCH_WORD) != 0) {
            flag |= AbstractSpecimen.MATCH_SQTE;
            int ch = st.getToken().codePointAt(0);
            if (st.getDelemiter().getQuotes().indexOf(ch) != -1) {
                pattern = st.getDelemiter().resolveDouble(st.getToken().substring(1),
                        ch, st.getTokenOffset() + 1);
                if (ch == CodeType.LINE_DOUBLE) {
                    flag |= SpecimenAdvanced.MATCH_WORD;
                } else if (ch == CodeType.LINE_SINGLE) {
                    flag |= SpecimenAdvanced.MATCH_PART;
                } else {
                    throw new ScannerError(ERROR_SYNTAX_QUOTED_SINGLE,
                            st.getTokenOffset());
                }
            } else {
                flag |= SpecimenAdvanced.MATCH_WORD;
                pattern = st.getToken();
            }
        } else {
            int ch = st.getToken().codePointAt(0);
            if (st.getDelemiter().getQuotes().indexOf(ch) != -1) {
                pattern = st.getDelemiter().resolveDouble(st.getToken().substring(1),
                        ch, st.getTokenOffset() + 1);
                if (ch == CodeType.LINE_DOUBLE) {
                    /* */
                } else {
                    throw new ScannerError(ERROR_SYNTAX_QUOTED_SINGLE,
                            st.getTokenOffset());
                }
            } else {
                pattern = st.getToken();
            }
        }
        int pos = st.getTokenOffset();
        st.nextToken();
        SpecimenAdvanced pm = new SpecimenAdvanced();
        pm.setPatDelemiter(getPatDelemiter());
        pm.setMatchDelemiter(getMatchDelemiter());
        pm.setFlag(flag);
        pm.setPattern(pattern);
        try {
            pm.prepareMatch();
        } catch (ScannerError x) {
            x.setPos(x.getPos() + pos);
            throw x;
        }
        return pm;
    }

}
