package matula.util.regex;

import matula.util.data.ListArray;
import matula.util.system.ConnectionReader;

import java.io.IOException;
import java.io.StringReader;

/**
 * <p>Die Klasse PatternInter erlaubt es eine Konjunktion
 * von Stringmuster zu erkennen und zu ersetzten. Die
 * Syntax einer Konjunktion lautet:
 * <pre>
 *    findinter = { findunion } [ "!" { findunion } ].
 * </pre>
 * <p>Mittels dem Konstruktor PatternInter kann ein entsprechendes
 * Objekt erzeugt werden. Die einzelnen Disjunktionen können
 * als Liste übergeben werden. Das Flag INTER_NEGATIV
 * zeigt an ob es sich um einen Teil des Ausschluss handelt, also
 * nach dem Ausrufezeichen (!) steht. Die Methode patternMatch()
 * erlaubt es ein AbstractPattern zu suchen. Ist die Suche erfolgreich
 * so gibt getFound() die gefundene Disjunktionen zurück. Die Methode
 * getMatchStart() und getMatchEnd() geben wieder den Startpunkt und
 * Endpunkt des gefundenen Patterns an.
 * <p>Die Methode getCountPositiv() zählt die Disjunktionen die nicht
 * aussschliessend sind. Die Methode patternToSQL() erzeugt mehrere
 * Stringlisten für ein LIKE Statement wobei nur die positiven Disjunktionen
 * enthalten sind. Die Methode getMatchBound() gibt die maximale
 * Länge die ein Match ausfüllen kann.
 * <p>Mittels der Methode parse() und toString() kann eine Conjunktion
 * ein- und ausgelesen werden, wobei das Format maschinenlesbar
 * ist. Das menschenlesbare Format wird mittels der Methoden
 * parseInter() und pretty() ein- und ausgelesen. Dabei zeigen
 * die Flags die erlaubten Syntaxoptionen an.
 * <p>Mittels der Methode replaceTo() kann ein anderes AbstractPattern als
 * Target spezifiziert werden. Das Target wird dann in das vorliegende
 * AbstractPattern eingebaut. Mittels der Methode deleteExclusion() können die
 * Ausschlüsse gestrichen werden. Die Methode main() enthält ein paar
 * Testfälle.
 *
 * @author CopyRight 2003-2012, XLOG Technology GmbH, Jan Burse
 * @version QA (Quality Assurance) v1.0
 */
public final class PatternInter extends AbstractPattern {
    public static final PatternInter EMPTY = new PatternInter();
    public static final PatternUnion[] EMPTY_UNIONS = new PatternUnion[0];

    public static final String ERROR_SYNTAX_RIGHT_PARENTHESIS = "right_parenthesis";

    public static int MAX_MATCH_INTER = 196;

    private PatternUnion[] unions = EMPTY_UNIONS;
    private final ListArray<PatternUnion> pos = new ListArray<>();
    private final ListArray<AbstractSpecimen> matcher = new ListArray<>();
    private final ListArray<PatternUnion> neg = new ListArray<>();

    /**
     * <p>Retrieve the unions.</p>
     *
     * @return The unions.
     */
    public PatternUnion[] getUnions() {
        return unions;
    }

    /**
     * <p>Set the unions.</p>
     *
     * @param v The unions.
     */
    public void setUnions(ListArray<PatternUnion> v) {
        unions = new PatternUnion[v.size()];
        v.toArray(unions);
    }

    /**
     * <p>Return the copies of positive union matchers.</p>
     *
     * @return The positive union matchers.
     */
    public ListArray<AbstractSpecimen> getMatcher() {
        return matcher;
    }

    /**
     * Counts the postive pattern unions.
     *
     * @return The number of positive pattern unions.
     */
    public int getCountPositive() {
        int i = 0;
        for (; i < unions.length &&
                (unions[i].getFlag() & PatternUnion.UNION_NEGATIV) == 0; i++)
            ;
        return i;
    }

    /******************************************************************/
    /* Parse                                                          */
    /******************************************************************/

    /**
     * <p>Create an inter pattern from string.</p>
     *
     * @param s    The string.
     * @param expr The expression features.
     * @param comp The specimen compiler.
     * @return The inter pattern.
     * @throws ScannerError Parsing problem.
     */
    public static PatternInter parseInter(String s, int expr,
                                          AbstractCompiler comp)
            throws ScannerError {
        try {
            ScannerToken st = new ScannerToken();
            ConnectionReader cr = new ConnectionReader(new StringReader(s));
            st.setReader(cr);
            st.setDelemiter(comp.getPatDelemiter());
            st.setRemark(comp.getRemark());
            st.firstToken();
            PatternInter inter = parseInter(st, expr, comp);
            if (st.getHint() != 0 || !"".equals(st.getData()))
                throw new ScannerError(AbstractCompiler.ERROR_SYNTAX_END_OF_CLAUSE_EXPECTED,
                        st.getTokenOffset());
            return inter;
        } catch (IOException x) {
            throw new RuntimeException("shouldn't happen", x);
        }
    }

    /**
     * <p>Create a inter pattern from string.</p>
     *
     * @param s    The string.
     * @param comp The specimen compiler.
     * @return The inter pattern.
     * @throws ScannerError Parsing problem.
     */
    public static PatternInter parseInter(String s, AbstractCompiler comp)
            throws ScannerError {
        return parseInter(s,
                AbstractSpecimen.MATCH_IGCS | AbstractSpecimen.MATCH_WORD,
                comp);
    }

    /**
     * <p>Parse the external representation of a pattern inter.</p>
     *
     * @param st   The scanner.
     * @param expr The parse feature flags.
     * @param comp The specimen compiler.
     * @return The pattern inter.
     * @throws ScannerError Parsing problem.
     */
    private static PatternInter parseInter(ScannerToken st, int expr,
                                           AbstractCompiler comp)
            throws ScannerError, IOException {
        ListArray<PatternUnion> vec = new ListArray<>();
        while (st.getHint() != 0 || (!"".equals(st.getData()) &&
                !")".equals(st.getData()) &&
                !"!".equals(st.getData()))) {
            vec.add(parseUnion(st, expr, comp));
        }
        if (st.getHint() == 0 && "!".equals(st.getData())) {
            st.nextToken();
            while (st.getHint() != 0 || (!"".equals(st.getData()) &&
                    !")".equals(st.getData()) &&
                    !"!".equals(st.getData()))) {
                PatternUnion temp = parseUnion(st, expr, comp);
                temp.setFlag(temp.getFlag() | PatternUnion.UNION_NEGATIV);
                vec.add(temp);
            }
        }
        PatternInter pi = new PatternInter();
        pi.setUnions(vec);
        pi.setPatDelemiter(st.getDelemiter());
        pi.setMatchDelemiter(comp.getMatchDelemiter());
        return pi;
    }


    /**
     * <p>Parse a pattern union with parenthesis.</p>
     *
     * @param st   The scanner.
     * @param expr The parse feature flags.
     * @param comp The specimen compiler.
     * @return The pattern union.
     * @throws ScannerError Parsing problem.
     */
    private static PatternUnion parseUnion(ScannerToken st, int expr,
                                           AbstractCompiler comp)
            throws ScannerError, IOException {
        if (st.getHint() == 0 && "(".equals(st.getData())) {
            st.nextToken();
            PatternUnion pu = PatternUnion.parseUnion(st, expr, comp);
            if (st.getHint() != 0 || !")".equals(st.getData()))
                throw new ScannerError(ERROR_SYNTAX_RIGHT_PARENTHESIS,
                        st.getTokenOffset());
            st.nextToken();
            return pu;
        } else {
            AbstractSpecimen pm = comp.parseSpecimen(st, expr);
            ListArray<AbstractSpecimen> vec = new ListArray<>();
            vec.add(pm);
            PatternUnion pu = new PatternUnion();
            pu.setMatchers(vec);
            pu.setPatDelemiter(st.getDelemiter());
            pu.setMatchDelemiter(comp.getMatchDelemiter());
            return pu;
        }
    }

    /******************************************************************/
    /* Preparation                                                    */
    /******************************************************************/

    /**
     * Compute the match bound as the max of all match bounds.
     *
     * @return The match bound.
     */
    public int getMatchBound() {
        int sum = 0;
        for (int i = 0; i < unions.length; i++) {
            sum += unions[i].getMatchBound();
            if (i != 0)
                sum += MAX_MATCH_INTER;
        }
        return sum;
    }

    /**
     * <p>Set the replacement target.</p>
     *
     * @param pat The replacement target.
     */
    public void replaceTo(AbstractPattern pat) {
        PatternInter inter = (PatternInter) pat;
        for (int i = 0; i < unions.length &&
                (unions[i].getFlag() & PatternUnion.UNION_NEGATIV) == 0; i++) {
            PatternUnion union = inter.getUnions()[i];
            if ((union.getFlag() & PatternUnion.UNION_NEGATIV) != 0)
                throw new IllegalArgumentException("negative replace");
            unions[i].replaceTo(union);
        }
    }

    /******************************************************************/
    /* Find                                                           */
    /******************************************************************/

    /**
     * <p>Match the pattern from a given position in a given string.</p>
     *
     * @param k     The start position to tty the match from.
     * @param t     The string to match the pattern with.
     * @param flags The flags.
     * @return True if a match has been found.
     */
    public boolean matchPattern(int k, String t, int flags) {
        pos.clear();
        matcher.clear();
        neg.clear();
        int positive = getCountPositive();
        for (; ; ) {
            PatternUnion min = matchMinUnion(k, t, flags);
            if (min == null)
                return false;
            if (min.getMatchStart() - k > MAX_MATCH_INTER) {
                pos.clear();
                matcher.clear();
                neg.clear();
            }
            if ((min.getFlag() & PatternUnion.UNION_NEGATIV) == 0) {
                int u;
                if ((u = pos.indexOf(min)) >= 0) {
                    pos.remove(u);
                    matcher.remove(u);
                }
                pos.add(min);
                matcher.add(min.getFound().copyMatcher());
            } else {
                if (!neg.contains(min))
                    neg.add(min);
            }
            if (positive == pos.size() &&
                    (positive == unions.length ||
                            neg.size() != unions.length - positive))
                return true;
            k = min.getMatchEnd();
        }
    }

    /**
     * <p>Find the minimum matching union.</p>
     *
     * @param k     The position to start the match from.
     * @param t     The string to search inside.
     * @param flags The flags.
     * @return True if a match was possible, otherwise false.
     */
    private PatternUnion matchMinUnion(int k, String t, int flags) {
        int best = 0;
        PatternUnion found = null;
        for (int i = unions.length - 1; i >= 0; i--) {
            PatternUnion union = unions[i];
            if (union.matchPattern(k, t, flags)) {
                int current = union.getMatchStart();
                if (found == null || current < best) {
                    found = union;
                    best = current;
                } else if (current == best &&
                        ((current - k > MAX_MATCH_INTER) ||
                                (!pos.contains(union) && !neg.contains(union)))) {
                    found = union;
                    best = current;
                }
            }
        }
        return found;
    }

    /**
     * <p>Match the pattern from a given position in a given string.</p>
     * <p>Backward search version.</p>
     *
     * @param k     The start position to tty the match from.
     * @param t     The string to match the pattern with.
     * @param flags The flags.
     * @return True if a match has been found.
     */
    public boolean matchLastPattern(int k, String t, int flags) {
        pos.clear();
        matcher.clear();
        neg.clear();
        int positive = getCountPositive();
        for (; ; ) {
            PatternUnion max = matchMaxUnion(k, t, flags);
            if (max == null)
                return false;
            if (k - max.getMatchEnd() > MAX_MATCH_INTER) {
                pos.clear();
                matcher.clear();
                neg.clear();
            }
            if ((max.getFlag() & PatternUnion.UNION_NEGATIV) == 0) {
                int u;
                if ((u = pos.indexOf(max)) >= 0) {
                    pos.remove(u);
                    matcher.remove(u);
                }
                pos.add(max);
                matcher.add(0, max.getFound().copyMatcher());
            } else {
                if (!neg.contains(max))
                    neg.add(max);
            }
            if (positive == pos.size() &&
                    (positive == unions.length ||
                            neg.size() != unions.length - positive))
                return true;
            k = max.getMatchStart();
        }
    }

    /**
     * <p>Find the maximum matching union.</p>
     *
     * @param k     The position to start the match from.
     * @param t     The string to search inside.
     * @param flags The flags.
     * @return True if a match was possible, otherwise false.
     */
    private PatternUnion matchMaxUnion(int k, String t, int flags) {
        int best = 0;
        PatternUnion found = null;
        for (int i = unions.length - 1; i >= 0; i--) {
            PatternUnion union = unions[i];
            if (union.matchLastPattern(k, t, flags)) {
                int current = union.getMatchEnd();
                if (found == null || current > best) {
                    found = union;
                    best = current;
                } else if (current == best &&
                        ((k - current > MAX_MATCH_INTER) ||
                                (!pos.contains(union) && !neg.contains(union)))) {
                    found = union;
                    best = current;
                }
            }
        }
        return found;
    }

    /**
     * <p>Return the match start position.</p>
     *
     * @return The match start.
     */
    public int getMatchStart() {
        return matcher.get(0).getMatchStart();
    }

    /**
     * <p>Return the match end position.</p>
     *
     * @return The match end.
     */
    public int getMatchEnd() {
        return matcher.get(matcher.size() - 1).getMatchEnd();
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
        for (int i = 0; i < matcher.size(); i++) {
            AbstractSpecimen pu = matcher.get(i);
            if (i != 0) {
                AbstractSpecimen pu2 = matcher.get(i - 1);
                buf.append(pu.getMatchStr(), pu2.getMatchEnd(), pu.getMatchStart());
            }
            pu.patternReplace(buf);
        }
    }

    /***********************************************************************/
    /* Some Utlities                                                       */
    /***********************************************************************/

    /**
     * <p>Delete the exclusion.</p>
     */
    public void deleteExclusion() {
        int k = getCountPositive();
        if (k != unions.length) {
            PatternUnion[] newunions = new PatternUnion[k];
            System.arraycopy(unions, 0, newunions, 0, k);
            unions = newunions;
        }
        for (int i = 0; i < k; i++)
            unions[i].deleteExclusion();
    }

}
