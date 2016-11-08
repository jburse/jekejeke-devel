package matula.util.regex;

import matula.util.data.ListArray;
import matula.util.system.ConnectionReader;

import java.io.IOException;
import java.io.StringReader;

/**
 * <p>Die Klasse PatternUnion erlaubt es eine Disjunktion
 * von Stringmuster zu erkennen und zu ersetzten. Die
 * Syntax einer Disjunktion lautet:
 * <pre>
 *     findunion = find | "(" { find } [ "!" { find } ] ")".
 * </pre>
 * <p>Mittels dem Konstruktor PatternUnion kann ein entsprechendes
 * Objekt erzeugt werden. Die einzelnen Stringmuster können
 * als Liste übergeben werden. Das Flag MATCH_NEGT
 * zeigt an ob es sich um einen Teil des Ausschluss handelt, als
 * nach dem Ausrufezeichen (!) steht. Die Methode patternMatch()
 * erlaubt es ein AbstractPattern zu suchen. Ist die Suche erfolgreich
 * so gibt getFound() das gefundene AbstractPattern zurück. Die Methode
 * getMatchStart() und getMatchEnd() gibt wieder den Startpunkt und
 * Endpunkt des gefundenen Patterns an.
 * <p>Die Methode getCountPositiv() zählt die AbstractPattern die nicht
 * aussschliessend sind. Die Methode patternToSQL() erzeugt mehrere
 * Strings für ein LIKE Statement wobei nur die positiven AbstractPattern
 * enthalten sind. Die Methode getMatchBound() gibt die maximale
 * Länge die ein Match ausfüllen kann.
 * <p>Mittels der Methode parse() und toString() kann eine Disjunktion
 * ein- und ausgelesen werden, wobei das Format maschinenlesbar
 * ist. Das menschenlesbare Format wird mittels der Methoden
 * parseUnion() und pretty() ein- und ausgelesen. Dabei zeigen
 * die Flags die erlaubten Syntaxoptionen an. Die Option EXPRESSION_PARENTHESIS
 * zeigt an, dass die Klammern nötig sind.
 * <p>Mittels der Methode replaceTo() kann ein anderes AbstractPattern als
 * Target spezifiziert werden. Das Target wird dann in das vorliegende
 * AbstractPattern eingebaut. Mittels der Methode deleteExclusion() können die
 * Ausschlüsse gestrichen werden. Die Methode main() enthält ein paar
 * Testfälle.
 *
 * @author CopyRight 2003-2012, XLOG Technology GmbH, Jan Burse
 * @version QA (Quality Assurance) v1.0
 */
public final class PatternUnion extends AbstractPattern {
    public static final AbstractSpecimen[] EMPTY_MATCHERS = new AbstractSpecimen[0];

    public static final int UNION_NEGATIV = 1;

    private AbstractSpecimen found;
    private int flag;
    private AbstractSpecimen[] matchers = EMPTY_MATCHERS;
    private final ListArray<Integer> exclude = new ListArray<Integer>();

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
            if (!"".equals(st.getToken()))
                throw new ScannerError(AbstractCompiler.ERROR_SYNTAX_SUPERFLUOUS_TOKEN,
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
        ListArray<AbstractSpecimen> vec = new ListArray<AbstractSpecimen>();
        while (!"".equals(st.getToken()) &&
                !")".equals(st.getToken()) &&
                !"!".equals(st.getToken())) {
            vec.add(comp.parseSpecimen(st, expr));
        }
        if ("!".equals(st.getToken())) {
            st.nextToken();
            while (!"".equals(st.getToken()) &&
                    !")".equals(st.getToken()) &&
                    !"!".equals(st.getToken())) {
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
        PatternUnion union = (PatternUnion)pat;
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
