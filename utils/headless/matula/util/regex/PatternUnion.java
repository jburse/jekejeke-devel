package matula.util.regex;

import matula.util.data.ListArray;
import matula.util.system.ConnectionReader;

import java.io.IOException;
import java.io.StringReader;

/**
 * <p>This class provides a disjunction of pattern inter.</p>
 * <p>The following syntax is used:</p>
 * <pre>
 *     findunion = find | "(" { find } [ "!" { find } ] ")".
 * </pre>
 * <p/>
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class PatternUnion extends AbstractPattern {
    public static final AbstractSpecimen[] EMPTY_MATCHERS = new AbstractSpecimen[0];

    public static final int UNION_NEGATIV = 1;

    private AbstractSpecimen found;
    private int flag;
    private AbstractSpecimen[] matchers = EMPTY_MATCHERS;
    private final ListArray<Integer> exclude = new ListArray<>();

    /**
     * <p>Retrieve the matchers.</p>
     *
     * @return The matchers.
     */
    public AbstractSpecimen[] getMatchers() {
        return matchers;
    }

    /**
     * <p>Set the matchers.</p>
     *
     * @param v The matchers.
     */
    public void setMatchers(ListArray<AbstractSpecimen> v) {
        matchers = new AbstractSpecimen[v.size()];
        v.toArray(matchers);
    }

    /**
     * <p>Retrieve the flags.</p>
     *
     * @return The flags.
     */
    public int getFlag() {
        return flag;
    }

    /**
     * <p>Set the flags.</p>
     *
     * @param f The flags.
     */
    public void setFlag(int f) {
        flag = f;
    }

    /**
     * <p>Count the positive patterns.</p>
     *
     * @return The number of positive patterns.
     */
    public int getCountPositive() {
        int i = 0;
        for (; i < matchers.length &&
                (matchers[i].getFlag() & AbstractSpecimen.MATCH_NEGT) == 0; i++)
            ;
        return i;
    }

    /**
     * <p>Retrieve the found pattern matcher.</p>
     *
     * @return The found pattern matcher.
     */
    public AbstractSpecimen getFound() {
        return found;
    }

    /******************************************************************/
    /* Parse                                                          */
    /******************************************************************/

    /**
     * Create a union pattern from string.
     *
     * @param s    The external representation of the pattern.
     * @param expr The flags that control the parsing.
     * @param comp The specimen compiler.
     * @return The parse pattern.
     * @throws ScannerError Parsing problem.
     */
    public static PatternUnion parseUnion(String s, int expr,
                                          AbstractCompiler comp)
            throws ScannerError {
        try {
            ScannerToken st = new ScannerToken();
            ConnectionReader cr = new ConnectionReader(new StringReader(s));
            st.setReader(cr);
            st.setDelemiter(comp.getPatDelemiter());
            st.setRemark(comp.getRemark());
            st.firstToken();
            PatternUnion union = parseUnion(st, expr, comp);
            if (st.getHint() != 0 || !"".equals(st.getData()))
                throw new ScannerError(AbstractCompiler.ERROR_SYNTAX_END_OF_CLAUSE_EXPECTED,
                        st.getTokenOffset());
            return union;
        } catch (IOException x) {
            throw new RuntimeException("shouldn't happen", x);
        }
    }

    /**
     * Create a union pattern from string.
     *
     * @param s    The external representation of the pattern.
     * @param comp The specimen compiler.
     * @return The parse pattern.
     * @throws ScannerError Parsing problem.
     */
    public static PatternUnion parseUnion(String s, AbstractCompiler comp)
            throws ScannerError {
        return parseUnion(s,
                AbstractSpecimen.MATCH_IGCS | AbstractSpecimen.MATCH_WORD,
                comp);
    }


    /**
     * @param st   The scanner for the external representation.
     * @param expr The flags that control the parsing.
     * @param comp The specimen compiler.
     * @return The parse pattern.
     * @throws ScannerError Parsing problem.
     */
    static PatternUnion parseUnion(ScannerToken st, int expr,
                                   AbstractCompiler comp)
            throws ScannerError, IOException {
        ListArray<AbstractSpecimen> vec = new ListArray<>();
        while (st.getHint() != 0 || (!"".equals(st.getData()) &&
                !")".equals(st.getData()) &&
                !"!".equals(st.getData()))) {
            vec.add(comp.parseSpecimen(st, expr));
        }
        if (st.getHint() == 0 && "!".equals(st.getData())) {
            st.nextToken();
            while (st.getHint() != 0 || (!"".equals(st.getData()) &&
                    !")".equals(st.getData()) &&
                    !"!".equals(st.getData()))) {
                AbstractSpecimen temp = comp.parseSpecimen(st, expr);
                temp.setFlag(temp.getFlag() | AbstractSpecimen.MATCH_NEGT);
                vec.add(temp);
            }
        }
        PatternUnion pu = new PatternUnion();
        pu.setMatchers(vec);
        pu.setPatDelemiter(st.getDelemiter());
        pu.setMatchDelemiter(comp.getMatchDelemiter());
        return pu;
    }

    /******************************************************************/
    /* Preparation                                                    */
    /******************************************************************/

    /**
     * @return The bound.
     */
    public int getMatchBound() {
        int max = 0;
        for (int i = 0; i < matchers.length; i++)
            max = Math.max(max, matchers[i].getMatchBound());
        return max;
    }

    /**
     * <p>Set the replacement target.</p>
     *
     * @param pat The replacement target.
     */
    public void replaceTo(AbstractPattern pat) {
        PatternUnion union = (PatternUnion) pat;
        for (int i = 0; i < matchers.length &&
                (matchers[i].getFlag() & AbstractSpecimen.MATCH_NEGT) == 0; i++) {
            AbstractSpecimen matcher = union.getMatchers()[i];
            if ((matcher.getFlag() & AbstractSpecimen.MATCH_NEGT) != 0)
                throw new IllegalArgumentException("negative replace");
            matchers[i].replaceTo(matcher);
        }
    }

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
    public boolean matchPattern(int k, String t, int flags) {
        int best = 0;
        found = null;
        exclude.clear();
        for (int i = matchers.length - 1; i >= 0; i--) {
            AbstractSpecimen matcher = matchers[i];
            if (matcher.matchPattern(k, t, flags)) {
                if ((matcher.getFlag() & AbstractSpecimen.MATCH_NEGT) == 0) {
                    int current = matcher.getMatchStart();
                    if (!exclude.contains(Integer.valueOf(current))) {
                        if (found == null || current < best) {
                            found = matcher;
                            best = current;
                        }
                    }
                } else {
                    exclude.add(Integer.valueOf(matcher.getMatchStart()));
                }
            }
        }
        return (found != null);
    }

    /**
     * <p>Match the pattern from a given position in a given string.</p>
     * <p>Backward search version.</p>
     *
     * @param k     The start position to try the match from.
     * @param t     The string to match the pattern with.
     * @param flags The flags.
     * @return True if a match has been found.
     */
    public boolean matchLastPattern(int k, String t, int flags) {
        int best = 0;
        found = null;
        exclude.clear();
        for (int i = matchers.length - 1; i >= 0; i--) {
            AbstractSpecimen matcher = matchers[i];
            if (matcher.matchLastPattern(k, t, flags)) {
                if ((matcher.getFlag() & AbstractSpecimen.MATCH_NEGT) == 0) {
                    int current = matcher.getMatchEnd();
                    if (!exclude.contains(Integer.valueOf(current))) {
                        if (found == null || current > best) {
                            found = matcher;
                            best = current;
                        }
                    }
                } else {
                    exclude.add(Integer.valueOf(matcher.getMatchEnd()));
                }
            }
        }
        return (found != null);
    }

    /**
     * <p>Return the match start position.</p>
     *
     * @return The match start.
     */
    public int getMatchStart() {
        return found.getMatchStart();
    }

    /**
     * <p>Return the match end position.</p>
     *
     * @return The match end.
     */
    public int getMatchEnd() {
        return found.getMatchEnd();
    }

    /******************************************************************/
    /* Replace                                                        */
    /******************************************************************/

    /**
     * <p>Replace the match according to the replace pattern.</p>
     * <p>For word and part only the region that matched is replaced.</p>
     *
     * @param buf The string builder.
     */
    public void patternReplace(StringBuilder buf) {
        found.patternReplace(buf);
    }

    /***********************************************************************/
    /* Some Utlities                                                       */
    /***********************************************************************/

    /**
     * <p>Delete the exclusion.</p>
     */
    public void deleteExclusion() {
        int k = getCountPositive();
        if (k != matchers.length) {
            AbstractSpecimen[] newmatchers = new AbstractSpecimen[k];
            System.arraycopy(matchers, 0, newmatchers, 0, k);
            matchers = newmatchers;
        }
    }

}
