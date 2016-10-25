package matula.util.regex;

/**
 * <p>This class provides the base class for bounded string patterns.</p>
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
public abstract class AbstractPattern {
    protected CodeType patdelemiter;
    protected CodeType matchdelemiter;

    /**
     * <p>Set the pattern delemiter.</p>
     *
     * @param d The pattern delemiter.
     */
    public void setPatDelemiter(CodeType d) {
        patdelemiter = d;
    }

    /**
     * Returns the pattern delemiter.
     *
     * @return The pattern delemiter.
     */
    public CodeType getPatDelemiter() {
        return patdelemiter;
    }

    /**
     * <p>Set the match delemiter.</p>
     *
     * @param d The match delemiter.
     */
    public void setMatchDelemiter(CodeType d) {
        matchdelemiter = d;
    }

    /**
     * Returns the match delemiter.
     *
     * @return The match delemiter.
     */
    public CodeType getMatchDelemiter() {
        return matchdelemiter;
    }

    /******************************************************************/
    /* Preparation                                                    */
    /******************************************************************/

    /**
     * Compute an upper bound based on the number of wildcards.
     *
     * @return The bound.
     */
    public abstract int getMatchBound();

    /******************************************************************/
    /* Find                                                           */
    /******************************************************************/

    /**
     * <p>Match the pattern from a given position in a given string.</p>
     *
     * @param k     The start position to try the match from.
     * @param t     The string to match the pattern with.
     * @param flags The flags.
     * @return True if a match has been found.
     */
    public abstract boolean matchPattern(int k, String t, int flags);

    /**
     * <p>Match the pattern from a given position in a given string.</p>
     * <p>Backward search version.</p>
     *
     * @param k     The start position to try the match from.
     * @param t     The string to match the pattern with.
     * @param flags The flags.
     * @return True if a match has been found.
     */
    public abstract boolean matchLastPattern(int k, String t, int flags);

    /**
     * <p>Return the match start position.</p>
     *
     * @return The match start.
     */
    public abstract int getMatchStart();

    /**
     * <p>Return the match end position.</p>
     *
     * @return The match end.
     */
    public abstract int getMatchEnd();

    /******************************************************************/
    /* Replace                                                        */
    /******************************************************************/

    /**
     * Find all matches of the given string and replace them.
     *
     * @param str The string to match and replace.
     * @return The result.
     */
    public String patternReplaceAll(String str) {
        StringBuilder buf = new StringBuilder();
        int k = 0;
        while (matchPattern(k, str, 0)) {
            buf.append(patternReplace(true));
            int u = getMatchEnd();
            if (u == k)
                throw new IllegalArgumentException("problem with matcher");
            k = u;
        }
        buf.append(str.substring(k));
        return buf.toString();
    }

    /**
     * <p>Replace the match by the substitution defined by the pattern.</p>
     *
     * @param strip Do not include the ending blanks.
     * @return The replacement.
     */
    public abstract String patternReplace(boolean strip);

}
