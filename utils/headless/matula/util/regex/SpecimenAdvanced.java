package matula.util.regex;

import matula.util.data.ListArray;

/**
 * <p>This class provides a bounded advanced pattern.</p>
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
public final class SpecimenAdvanced extends AbstractSpecimen {
    public static final String ERROR_SYNTAX_RIGHT_BRACE = "right_brace";
    public static final String ERROR_SYNTAX_PREMATURE_END = "premature_end";
    public static final String ERROR_SYNTAX_CHARSET_FROM = "charset_from";
    public static final String ERROR_SYNTAX_CHARSET_END = "charset_end";
    public static final String ERROR_SYNTAX_CHARSET_TO = "charset_to";
    public static final String ERROR_SYNTAX_NAMED_ARROW = "named_arrow";

    public static final char CH_CHARSET_START = '<';
    public static final char CH_CHARSET_END = '>';
    public static final char CH_CHARSET_RANGE = '~';
    public static final char CH_NAMED_MULTI = '^';
    public static final char CH_REPEAT_START = '{';
    public static final char CH_REPEAT_END = '}';

    private static final int MAX_MATCH_REPEAT = 45;

    private static final int MASK_PAT_BLOCK = 0x00000100;

    private Match[] match;
    private String matchstr;
    private int searchpos;
    private int matchbound;
    private int patstart;
    private int patend;
    private Match[] tarmatch;

    /**
     * <p>Retreive the matches.</p>
     *
     * @return The matches.
     */
    public Match[] getMatch() {
        return match;
    }

    /**
     * <p>Retrieve the match string.</p>
     *
     * @return The match string.
     */
    public String getMatchStr() {
        return matchstr;
    }

    /**
     * <p>Retrieve the patend.</p>
     *
     * @return The patend.
     */
    public int getPatEnd() {
        return patend;
    }

    /******************************************************************/
    /* Preparation                                                    */
    /******************************************************************/

    /**
     * Count the wild cards. There are implicit and explicit
     * wildcards. The implicit wildcards are between word
     * breaks. The explicit wildcards are *, ? and ^name^.
     * Loops are between { and }.
     *
     * @param i The index where to start from.
     * @throws ScannerError Parsing problem.
     */
    private void prepareMatch(int i) throws ScannerError {
        ListArray<Match> vec = new ListArray<Match>();
        patstart = i;
        matchbound = 0;
        for (; ; ) {
            if ((flag & MATCH_WORD) != 0 && patdelemiter.wordBreak1(i, pattern)) {
                int j = i;

                // skip the word break
                while (!patdelemiter.wordBreak2(i, pattern)) {
                    int ch = pattern.codePointAt(i);
                    i += Character.charCount(ch);
                }

                // true word break?
                if (handleBreak(j, pattern)) {
                    // add an implicit wildcard
                    Match match = new Match();
                    match.origstart = j;
                    match.origend = i;
                    vec.add(match);

                    // inside blanks are bounded
                    if (j != 0 && i != pattern.length())
                        matchbound += MAX_MATCH_BLANK;
                }
            } else if ((flag & MATCH_PART) != 0 && i == 0) {
                // add an implicit wildcard
                Match match = new Match();
                match.origstart = i;
                match.origend = i;
                vec.add(match);
            }
            if (i < pattern.length()) {
                int ch = pattern.codePointAt(i);
                if (ch == CH_REPEAT_END) {
                    break;
                } else if (ch == CH_ANNONYM_MULT) {
                    // add an explicit wildcard
                    Match match = new Match();
                    match.origstart = i;
                    match.origend = i + Character.charCount(ch);
                    vec.add(match);
                    matchbound += MAX_MATCH_WILDCARD;
                } else if (ch == CH_ANNONYM_SINGLE) {
                    // add an explicit wildcard
                    Match match = new Match();
                    match.origstart = i;
                    match.origend = i + Character.charCount(ch);
                    vec.add(match);
                    matchbound += Character.charCount(ch);
                } else if (ch == CH_NAMED_MULTI) {
                    int j = i;
                    StringBuilder buf = new StringBuilder();

                    // skip the name
                    i += Character.charCount(ch);
                    if (!(i < pattern.length()))
                        throw new ScannerError(ERROR_SYNTAX_NAMED_ARROW, i);
                    ch = pattern.codePointAt(i);
                    while (ch != CH_NAMED_MULTI) {
                        buf.appendCodePoint(ch);
                        i += Character.charCount(ch);
                        if (!(i < pattern.length()))
                            throw new ScannerError(ERROR_SYNTAX_NAMED_ARROW, i);
                        ch = pattern.codePointAt(i);
                    }

                    // add a named match
                    MatchName match = new MatchName();
                    match.origstart = j;
                    match.origend = i + Character.charCount(ch);
                    match.name = buf.toString();
                    match.index = indexOf(vec, match.name);
                    vec.add(match);
                    matchbound += MAX_MATCH_WILDCARD;
                } else if (ch == CH_CHARSET_START) {
                    int j = i;
                    int k = 0;

                    // skip the charset
                    i += Character.charCount(ch);
                    if (!(i < pattern.length()))
                        throw new ScannerError(ERROR_SYNTAX_CHARSET_FROM, i);
                    ch = pattern.codePointAt(i);
                    for (; ; ) {
                        k = Math.max(k, Character.charCount(ch));
                        i += Character.charCount(ch);
                        if (!(i < pattern.length()))
                            throw new ScannerError(ERROR_SYNTAX_CHARSET_END, i);
                        ch = pattern.codePointAt(i);
                        if (ch == CH_CHARSET_RANGE) {
                            i += Character.charCount(ch);
                            if (!(i < pattern.length()))
                                throw new ScannerError(ERROR_SYNTAX_CHARSET_TO, i);
                            ch = pattern.codePointAt(i);
                            k = Math.max(k, Character.charCount(ch));
                            i += Character.charCount(ch);
                            if (!(i < pattern.length()))
                                throw new ScannerError(ERROR_SYNTAX_CHARSET_END, i);
                            ch = pattern.codePointAt(i);
                        }
                        if (ch == CH_CHARSET_END)
                            break;
                    }
                    // add an unnamed match
                    Match match = new Match();
                    match.origstart = j;
                    match.origend = i + Character.charCount(ch);
                    vec.add(match);
                    matchbound += k;
                } else if (ch == CH_REPEAT_START) {
                    int j = i;
                    // recursively call prepare
                    i += Character.charCount(ch);
                    SpecimenAdvanced pat = new SpecimenAdvanced();
                    pat.setPatDelemiter(patdelemiter);
                    pat.setMatchDelemiter(matchdelemiter);
                    pat.setPattern(pattern);
                    pat.setFlag(flag);
                    pat.prepareMatch(i);
                    i = pat.patend;
                    if (!(i < pattern.length()))
                        throw new ScannerError(ERROR_SYNTAX_RIGHT_BRACE, i);
                    ch = pattern.codePointAt(i);
                    if (ch != CH_REPEAT_END)
                        throw new ScannerError(ERROR_SYNTAX_RIGHT_BRACE, i);

                    // add a repeat match
                    MatchRepeat match = new MatchRepeat();
                    match.origstart = j;
                    match.origend = i + Character.charCount(i);
                    match.pat = pat;
                    vec.add(match);
                    matchbound += MAX_MATCH_REPEAT * pat.getMatchBound();
                } else {
                    matchbound += Character.charCount(ch);
                }
                i += Character.charCount(ch);
            } else {
                break;
            }
        }
        if ((flag & MATCH_WORD) != 0) {
            // end was already handled in loop by word break */
        } else if ((flag & MATCH_PART) != 0 && i == pattern.length()) {
            // add an implicit wildcard
            Match match = new Match();
            match.origstart = i;
            match.origend = i;
            vec.add(match);
        }
        match = new Match[vec.size()];
        vec.toArray(match);
        patend = i;
    }

    /**
     * Count the wild cards. There are implicit and explicit
     * wildcards. The implicit wildcards are between word
     * breaks. The explicit wildcards are *, ? and ^name^.
     * Loops are between { and }.
     *
     * @throws ScannerError Parsing problem.
     */
    public void prepareMatch() throws ScannerError {
        prepareMatch(0);
        if (patend != pattern.length())
            throw new ScannerError(ERROR_SYNTAX_PREMATURE_END, patend);
    }

    /**
     * <p>Find a named match position with the given name.</p>
     *
     * @param name The name to find.
     * @param vec  The vector to search.
     * @return The position or -1.
     */
    private static int indexOf(ListArray<Match> vec, String name) {
        for (int i = 0; i < vec.size(); i++) {
            Match m = vec.get(i);
            if (m instanceof MatchName &&
                    name.equals(((MatchName) m).name))
                return i;
        }
        return -1;
    }

    /**
     * Compute an upper bound based on the number of wildcards.
     *
     * @return The bound.
     */
    public int getMatchBound() {
        return matchbound;
    }

    /**
     * <p>Set the replacement target.</p>
     *
     * @param pat The replacement target.
     */
    public void replaceTo(AbstractPattern pat) {
        SpecimenAdvanced sa = (SpecimenAdvanced) pat;
        target = sa.pattern;
        if (match.length < sa.match.length)
            throw new IllegalArgumentException("incompatible target");
        tarmatch = sa.match;
    }

    /**
     * <p>Find a named match position with the given name.</p>
     *
     * @param name The name to find.
     * @return The position or -1.
     */
    private int indexOf(String name) {
        for (int i = 0; i < match.length; i++) {
            Match m = match[i];
            if (m instanceof MatchName &&
                    name.equals(((MatchName) m).name))
                return i;
        }
        return -1;
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
        matchstr = t;
        searchpos = k;
        return matchPattern(0, k, 0, flags);
    }

    /**
     * <p>The working horse of the matcher.</p>
     *
     * @param i     The pattern index.
     * @param k     The match string index.
     * @param z     The index of the wildcard.
     * @param flags The flags.
     * @return True if there is a match, otherwise false.
     */
    private boolean matchPattern(int i, int k, int z, int flags) {
        for (; ; ) {
            if ((flags & MASK_PAT_BLOCK) == 0) {
                if ((flag & MATCH_WORD) != 0 && patdelemiter.wordBreak1(i, pattern)) {
                    int j = i;

                    // skip the word break
                    while (!patdelemiter.wordBreak2(i, pattern)) {
                        int ch = pattern.codePointAt(i);
                        i += Character.charCount(ch);
                    }

                    // true word break?
                    if (handleBreak(j, pattern)) {
                        // assure that previous word end was reached
                        if (j != 0 && !matchdelemiter.wordBreak1(k, matchstr))
                            return false;

                        int u = k;
                        for (; ; ) {
                            if (!matchdelemiter.wordBreak2(u, matchstr)) {
                                // don't try
                            } else if ((flags & MASK_PAT_BOUNCE) != 0 && u == matchstr.length()) {
                                // don't try
                            } else {
                                // try match
                                if (matchPattern(i, u, z + 1, flags | MASK_PAT_BLOCK)) {
                                    match[z].start = k;
                                    match[z].end = u;
                                    return true;
                                }

                                // assure that only single break is skipped
                                if (j != 0)
                                    return false;
                            }

                            if (u < matchstr.length()) {
                                int ch = matchstr.codePointAt(u);
                                u += Character.charCount(ch);
                            } else {
                                break;
                            }

                            // inside blanks are bounded
                            if (j != 0 && i != pattern.length()
                                    && u - k >= MAX_MATCH_BLANK)
                                break;
                        }
                        return false;
                    } else {
                        flags |= MASK_PAT_BLOCK;
                    }
                } else if ((flag & MATCH_PART) != 0 && i == 0) {
                    int u = k;
                    for (; ; ) {
                        // try match
                        if (matchPattern(i, u, z + 1, flags | MASK_PAT_BLOCK)) {
                            match[z].start = k;
                            match[z].end = u;
                            return true;
                        }

                        if (u < matchstr.length()) {
                            int ch = matchstr.codePointAt(u);
                            u += Character.charCount(ch);
                        } else {
                            break;
                        }
                    }
                    return false;
                }
            }
            if (i < pattern.length()) {
                int ch = pattern.codePointAt(i);
                if (ch == CH_REPEAT_END) {
                    break;
                } else if (ch == CH_ANNONYM_MULT || ch == CH_NAMED_MULTI) {
                    int j = i;
                    if (ch == CH_NAMED_MULTI) {
                        i += Character.charCount(ch);
                        ch = pattern.codePointAt(i);
                        while (ch != CH_NAMED_MULTI) {
                            i += Character.charCount(ch);
                            ch = pattern.codePointAt(i);
                        }
                    } else {
                        i += Character.charCount(ch);
                    }
                    if ((flag & MATCH_WORD) != 0) {
                        boolean f1 = patdelemiter.wordBreak2(j, pattern) &&
                                patdelemiter.wordBreak1(i, pattern);
                        boolean f2 = matchdelemiter.wordBreak2(k, matchstr);
                        int u = k;
                        for (; ; ) {
                            if (f1) {
                                // don't try
                                f1 = false;
                            } else {
                                // try match
                                if (sameContent(match[z], k, u) &&
                                        matchPattern(i, u, z + 1, flags & ~MASK_PAT_BLOCK)) {
                                    match[z].start = k;
                                    match[z].end = u;
                                    return true;
                                }
                                // assure that only single break is skipped
                                if ((!f2 || u != k) && matchdelemiter.wordBreak1(u, matchstr))
                                    return false;
                            }

                            if (u < matchstr.length()) {
                                ch = matchstr.codePointAt(u);
                                u += Character.charCount(ch);
                            } else {
                                break;
                            }

                            // explicit wildcard multi is bound
                            if (u - k >= MAX_MATCH_WILDCARD)
                                break;
                        }
                    } else {
                        int u = k;
                        for (; ; ) {
                            // try match
                            if (sameContent(match[z], k, u) &&
                                    matchPattern(i, u, z + 1, flags & ~MASK_PAT_BLOCK)) {
                                match[z].start = k;
                                match[z].end = u;
                                return true;
                            }

                            if (u < matchstr.length()) {
                                ch = matchstr.codePointAt(u);
                                u += Character.charCount(ch);
                            } else {
                                break;
                            }

                            // explicit wildcard multi is bound
                            if (u - k >= MAX_MATCH_WILDCARD)
                                break;
                        }
                    }
                    return false;
                } else if (ch == CH_REPEAT_START) {
                    SpecimenAdvanced pat = ((MatchRepeat) match[z]).pat;
                    i += Character.charCount(ch);
                    ch = pattern.codePointAt(pat.patend);
                    int j = k;
                    int u = 0;
                    for (; ; ) {
                        // try match
                        if (matchPattern(pat.patend + Character.charCount(ch),
                                j, z + 1, flags & ~MASK_PAT_BLOCK)) {
                            match[z].start = k;
                            match[z].end = j;
                            return true;
                        }
                        pat.matchstr = matchstr;
                        pat.searchpos = j;
                        if (u < MAX_MATCH_REPEAT &&
                                pat.matchPattern(i, j, 0,
                                        (flags & ~MASK_PAT_BLOCK) & ~MASK_PAT_BOUNCE)) {
                            j = pat.getMatchEnd(false);
                            u++;
                        } else {
                            return false;
                        }
                    }
                } else if (ch == CH_ANNONYM_SINGLE) {
                    // explicit wildcard single
                    if (k < matchstr.length()) {
                        int ch2 = matchstr.codePointAt(k);
                        match[z].start = k;
                        match[z].end = k + Character.charCount(ch2);
                        z++;
                        k += Character.charCount(ch2);
                        flags &= ~MASK_PAT_BLOCK;
                    } else {
                        return false;
                    }
                    i += Character.charCount(ch);
                } else if (ch == CH_CHARSET_START) {
                    if (k < matchstr.length()) {
                        int ch2 = matchstr.codePointAt(k);
                        i += Character.charCount(ch);
                        ch = pattern.codePointAt(i);
                        if ((flag & MATCH_IGCS) == 0) {
                            for (; ; ) {
                                int chlow = ch;
                                i += Character.charCount(ch);
                                ch = pattern.codePointAt(i);
                                if (ch == CH_CHARSET_RANGE) {
                                    i += Character.charCount(ch);
                                    ch = pattern.codePointAt(i);
                                    int chhigh = ch;
                                    i += Character.charCount(ch);
                                    ch = pattern.codePointAt(i);
                                    if (chlow <= ch2 && ch2 <= chhigh) break;
                                } else {
                                    if (chlow == ch2) break;
                                }
                                if (ch == CH_CHARSET_END)
                                    return false;
                            }
                        } else {
                            int chtst = Character.toLowerCase(ch2);
                            for (; ; ) {
                                int chlow = Character.toLowerCase(ch);
                                i += Character.charCount(ch);
                                ch = pattern.codePointAt(i);
                                if (ch == CH_CHARSET_RANGE) {
                                    i += Character.charCount(ch);
                                    ch = pattern.codePointAt(i);
                                    int chhigh = Character.toLowerCase(ch);
                                    i += Character.charCount(ch);
                                    ch = pattern.codePointAt(i);
                                    if (chlow <= chtst && chtst <= chhigh)
                                        break;
                                } else {
                                    if (chlow == chtst)
                                        break;
                                }
                                if (ch == CH_CHARSET_END)
                                    return false;
                            }
                        }
                        while (ch != CH_CHARSET_END) {
                            i += Character.charCount(ch);
                            ch = pattern.codePointAt(i);
                            if (ch == CH_CHARSET_RANGE) {
                                i += Character.charCount(ch);
                                ch = pattern.codePointAt(i);
                                i += Character.charCount(ch);
                                ch = pattern.codePointAt(i);
                            }
                        }
                        match[z].start = k;
                        match[z].end = k + Character.charCount(ch2);
                        z++;
                        k += Character.charCount(ch2);
                        flags &= ~MASK_PAT_BLOCK;
                    } else {
                        return false;
                    }
                    i += Character.charCount(ch);
                } else {
                    // compare characters
                    if (k < matchstr.length()) {
                        int ch2 = matchstr.codePointAt(k);
                        if ((flag & MATCH_IGCS) == 0) {
                            if (ch != ch2)
                                return false;
                        } else {
                            if (Character.toLowerCase(ch) !=
                                    Character.toLowerCase(ch2))
                                return false;
                        }
                        k += Character.charCount(ch2);
                        flags &= ~MASK_PAT_BLOCK;
                    } else {
                        return false;
                    }
                    i += Character.charCount(ch);
                }
            } else {
                break;
            }
        }
        if ((flag & MATCH_WORD) != 0) {
            // already handled in loop
            return true;
        } else if ((flag & MATCH_PART) != 0 && i == pattern.length()) {
            // set end of part
            match[z].start = k;
            match[z].end = matchstr.length();
            return true;
        }
        return (k == matchstr.length());
    }

    /**
     * <p>Match the pattern from a given position in a given string.</p>
     * <p>Backward search version.</p>
     *
     * @param k     The start position to try the match from plus one.
     * @param t     The string to match the pattern with.
     * @param flags The flags.
     * @return True if a match has been found.
     */
    public boolean matchLastPattern(int k, String t, int flags) {
        matchstr = t.substring(0, k);
        searchpos = 0;
        return matchLastPattern(pattern.length(), k, match.length - 1, flags);
    }

    /**
     * <p>The working horse of the matcher.</p>
     * <p>Backward search version.</p>
     *
     * @param i     The pattern index plus one.
     * @param k     The match string index plus one.
     * @param z     The index of the wildcard.
     * @param flags The flags.
     * @return True if there is a match, otherwise false.
     */
    private boolean matchLastPattern(int i, int k, int z, int flags) {
        for (; ; ) {
            if ((flags & MASK_PAT_BLOCK) == 0) {
                if ((flag & MATCH_WORD) != 0 && patdelemiter.wordBreak2(i, pattern)) {
                    int j = i;

                    // skip the word break
                    while (!patdelemiter.wordBreak1(i, pattern)) {
                        int ch = pattern.codePointBefore(i);
                        i -= Character.charCount(ch);
                    }

                    // true word break?
                    if (handleLastBreak(j, pattern)) {
                        // assure that previous word start was reached
                        if (j != pattern.length() && !matchdelemiter.wordBreak2(k, matchstr))
                            return false;

                        int u = k;
                        for (; ; ) {
                            if (!matchdelemiter.wordBreak1(u, matchstr)) {
                                // don't try
                            }
                            if ((flags & MASK_PAT_BOUNCE) != 0 && u == 0) {
                                // don't try
                            } else {
                                // try match
                                if (matchLastPattern(i, u, z - 1, flags | MASK_PAT_BLOCK)) {
                                    match[z].start = u;
                                    match[z].end = k;
                                    return true;
                                }
                                // assure that only single break is skipped
                                if (j != pattern.length())
                                    return false;
                            }

                            if (u > 0) {
                                int ch = matchstr.codePointBefore(u);
                                u -= Character.charCount(ch);
                            } else {
                                break;
                            }

                            // inside blanks are bounded
                            if (j != pattern.length() && i != 0
                                    && k - u >= MAX_MATCH_BLANK)
                                break;
                        }
                        return false;
                    } else {
                        flags |= MASK_PAT_BLOCK;
                    }
                } else if ((flag & MATCH_PART) != 0 && i == pattern.length()) {
                    int u = k;
                    for (; ; ) {
                        // try match
                        if (matchLastPattern(i, u, z - 1, flags | MASK_PAT_BLOCK)) {
                            match[z].start = u;
                            match[z].end = k;
                            return true;
                        }

                        if (u > 0) {
                            int ch = matchstr.codePointBefore(u);
                            u -= Character.charCount(ch);
                        } else {
                            break;
                        }
                    }
                    return false;
                }
            }
            if (i > 0) {
                int ch = pattern.codePointBefore(i);
                if (ch == CH_REPEAT_START) {
                    break;
                } else if (ch == CH_ANNONYM_MULT || ch == CH_NAMED_MULTI) {
                    int j = i;
                    if (ch == CH_NAMED_MULTI) {
                        i -= Character.charCount(ch);
                        ch = pattern.codePointBefore(i);
                        while (ch == CH_NAMED_MULTI) {
                            i -= Character.charCount(ch);
                            ch = pattern.codePointBefore(i);
                        }
                    } else {
                        i -= Character.charCount(ch);
                    }
                    if ((flag & MATCH_WORD) != 0) {
                        boolean f1 = patdelemiter.wordBreak2(i, pattern) &&
                                patdelemiter.wordBreak1(j, pattern);
                        boolean f2 = matchdelemiter.wordBreak1(k, matchstr);
                        int u = k;
                        for (; ; ) {
                            if (f1) {
                                // don't try
                                f1 = false;
                            } else {
                                // try match
                                if (sameContent(match[z], u, k) &&
                                        matchLastPattern(i, u, z - 1, flags & ~MASK_PAT_BLOCK)) {
                                    match[z].start = u;
                                    match[z].end = k;
                                    return true;
                                }
                                // assure that only single break is skipped
                                if ((!f2 || u != k) && matchdelemiter.wordBreak2(u, matchstr))
                                    return false;
                            }

                            if (u > 0) {
                                ch = matchstr.codePointBefore(u);
                                u -= Character.charCount(ch);
                            } else {
                                break;
                            }

                            // explicit wildcard multi is bound
                            if (k - u >= MAX_MATCH_WILDCARD)
                                break;
                        }
                    } else {
                        int u = k;
                        for (; ; ) {
                            // try match
                            if (sameContent(match[z], u, k) &&
                                    matchLastPattern(i, u, z - 1, flags & ~MASK_PAT_BLOCK)) {
                                match[z].start = u;
                                match[z].end = k;
                                return true;
                            }

                            if (u > 0) {
                                ch = matchstr.codePointBefore(u);
                                u -= Character.charCount(ch);
                            } else {
                                break;
                            }

                            // explicit wildcard multi is bound
                            if (k - u >= MAX_MATCH_WILDCARD)
                                break;
                        }
                    }
                    return false;
                } else if (ch == CH_REPEAT_END) {
                    SpecimenAdvanced pat = ((MatchRepeat) match[z]).pat;
                    i -= Character.charCount(ch);
                    ch = pattern.codePointBefore(pat.patstart);
                    int j = k;
                    int u = 0;
                    for (; ; ) {
                        // try match
                        if (matchLastPattern(pat.patstart - Character.charCount(ch),
                                j, z - 1, flags & ~MASK_PAT_BLOCK)) {
                            match[z].start = j;
                            match[z].end = k;
                            return true;
                        }
                        pat.matchstr = matchstr.substring(0, j);
                        pat.searchpos = 0;
                        if (u < MAX_MATCH_REPEAT &&
                                pat.matchLastPattern(i, j, pat.match.length - 1,
                                        (flags & ~MASK_PAT_BLOCK) & ~MASK_PAT_BOUNCE)) {
                            j = pat.getMatchStart(false);
                            u++;
                        } else {
                            return false;
                        }
                    }
                } else if (ch == CH_ANNONYM_SINGLE) {
                    // explicit wildcard single
                    if (k > 0) {
                        int ch2 = matchstr.codePointBefore(k);
                        match[z].start = k - Character.charCount(ch2);
                        match[z].end = k;
                        z--;
                        k -= Character.charCount(ch2);
                        flags &= ~MASK_PAT_BLOCK;
                    } else {
                        return false;
                    }
                    i -= Character.charCount(ch);
                } else if (ch == CH_CHARSET_END) {
                    if (k > 0) {
                        int ch2 = matchstr.codePointBefore(k);
                        i -= Character.charCount(ch);
                        ch = pattern.codePointBefore(i);
                        if ((flag & MATCH_IGCS) == 0) {
                            for (; ; ) {
                                int chhigh = ch;
                                i -= Character.charCount(ch);
                                ch = pattern.codePointBefore(i);
                                if (ch == CH_CHARSET_RANGE) {
                                    i -= Character.charCount(ch);
                                    ch = pattern.codePointBefore(i);
                                    int chlow = ch;
                                    i -= Character.charCount(ch);
                                    ch = pattern.codePointBefore(i);
                                    if (chlow <= ch2 && ch2 <= chhigh) break;
                                } else {
                                    if (chhigh == ch2) break;
                                }
                                if (ch == CH_CHARSET_START)
                                    return false;
                            }
                        } else {
                            int chtst = Character.toLowerCase(ch2);
                            for (; ; ) {
                                int chhigh = Character.toLowerCase(ch);
                                i -= Character.charCount(ch);
                                ch = pattern.codePointBefore(i);
                                if (ch == CH_CHARSET_RANGE) {
                                    i -= Character.charCount(ch);
                                    ch = pattern.codePointBefore(i);
                                    int chlow = Character.toLowerCase(ch);
                                    i -= Character.charCount(ch);
                                    ch = pattern.codePointBefore(i);
                                    if (chlow <= chtst && chtst <= chhigh)
                                        break;
                                } else {
                                    if (chhigh == chtst)
                                        break;
                                }
                                if (ch == CH_CHARSET_START)
                                    return false;
                            }
                        }
                        while (ch != CH_CHARSET_START) {
                            i -= Character.charCount(ch);
                            ch = pattern.codePointBefore(i);
                            if (ch == CH_CHARSET_RANGE) {
                                i -= Character.charCount(ch);
                                ch = pattern.codePointBefore(i);
                                i -= Character.charCount(ch);
                                ch = pattern.codePointBefore(i);
                            }
                        }
                        match[z].start = k - Character.charCount(ch2);
                        match[z].end = k;
                        z--;
                        k -= Character.charCount(ch2);
                        flags &= ~MASK_PAT_BLOCK;
                    } else {
                        return false;
                    }
                } else {
                    // compare characters
                    if (k > 0) {
                        int ch2 = matchstr.codePointBefore(k);
                        if ((flag & MATCH_IGCS) == 0) {
                            if (ch != ch2)
                                return false;
                        } else {
                            if (Character.toLowerCase(ch) !=
                                    Character.toLowerCase(ch2))
                                return false;
                        }
                        k -= Character.charCount(ch2);
                        flags &= ~MASK_PAT_BLOCK;
                    } else {
                        return false;
                    }
                    i -= Character.charCount(ch);
                }
            } else {
                break;
            }
        }
        if ((flag & MATCH_WORD) != 0) {
            // already handled in loop
            return true;
        } else if ((flag & MATCH_PART) != 0 && i == 0) {
            // set end of part
            match[z].start = 0;
            match[z].end = k;
            return true;
        }
        return (k == 0);
    }

    /**
     * <p>Return the match start position.</p>
     *
     * @return The match start.
     */
    public int getMatchStart() {
        return getMatchStart(true);
    }

    /**
     * <p>Return the match start position.</p>
     *
     * @param strip True if exclude first word break match, otherwise false.
     * @return The match start.
     */
    private int getMatchStart(boolean strip) {
        if ((flag & MATCH_WORD) != 0) {
            int k = 0;
            while (k < match.length && match[k] instanceof MatchRepeat)
                k++;
            if (strip) {
                return match[k].end;
            } else {
                return match[k].start;
            }
        } else if ((flag & MATCH_PART) != 0) {
            return match[0].end;
        } else {
            return searchpos;
        }
    }

    /**
     * Return the end of the match.
     *
     * @param strip True if exclude last word break match, otherwise false.
     * @return The end.
     */
    private int getMatchEnd(boolean strip) {
        if ((flag & MATCH_WORD) != 0) {
            int k = match.length - 1;
            while (k > 0 && match[k] instanceof MatchRepeat)
                k--;
            if (strip) {
                return match[k].start;
            } else {
                return match[k].end;
            }
        } else if ((flag & MATCH_PART) != 0) {
            return match[match.length - 1].start;
        } else {
            return matchstr.length();
        }
    }

    /**
     * <p>Return the match end position.</p>
     *
     * @return The match end.
     */
    public int getMatchEnd() {
        return getMatchEnd(true);
    }

    /**
     * <p>Return a copy of the matcher.</p>
     *
     * @return The copy.
     */
    public AbstractSpecimen copyMatcher() {
        try {
            /* the pattern */
            SpecimenAdvanced pm = new SpecimenAdvanced();
            pm.setPatDelemiter(patdelemiter);
            pm.setMatchDelemiter(matchdelemiter);
            pm.setPattern(pattern);
            pm.setFlag(flag);
            pm.prepareMatch();

            /* the target */
            if (target != null) {
                SpecimenAdvanced pr = new SpecimenAdvanced();
                pr.setPatDelemiter(patdelemiter);
                pr.setPattern(target);
                pr.setFlag(flag);
                pr.prepareMatch();
                pm.replaceTo(pr);
            }

            /* the actual search */
            for (int i = 0; i < match.length; i++) {
                pm.match[i].start = match[i].start;
                pm.match[i].end = match[i].end;
            }
            pm.searchpos = searchpos;
            pm.matchstr = matchstr;

            return pm;
        } catch (ScannerError x) {
            throw new RuntimeException(x);
        }
    }

    /******************************************************************/
    /* Replace                                                        */
    /******************************************************************/

    /**
     * Replace the match according to the replace pattern.
     *
     * @param strip True if last wildcard shouldn't be applied.
     * @return The replace.
     */
    public String patternReplace(boolean strip) {
        StringBuilder buf = new StringBuilder();
        int z = 0;
        if ((flag & MATCH_IGCS) != 0) {
            int wildp = 0;
            int wilde = z < match.length ? match[z].origstart : pattern.length();
            int matchp = 0;
            int matche = z < match.length ? match[z].start : matchstr.length();
            int i = 0;
            for (; ; ) {
                if ((flag & MATCH_WORD) != 0 && patdelemiter.wordBreak1(i, target)) {
                    while (!patdelemiter.wordBreak2(i, target)) {
                        int ch = target.codePointAt(i);
                        i += Character.charCount(ch);
                    }
                    if (i < target.length() || !strip) {
                        buf.append(matchstr, match[z].start, match[z].end);
                        wildp = match[z].origend;
                        matchp = match[z].end;
                        z++;
                        wilde = z < match.length ? match[z].origstart : pattern.length();
                        matche = z < match.length ? match[z].start : matchstr.length();
                    }
                } else if ((flag & MATCH_PART) != 0 && i == 0) {
                    buf.append(matchstr, match[z].start, match[z].end);
                    wildp = match[z].origend;
                    matchp = match[z].end;
                    z++;
                    wilde = z < match.length ? match[z].origstart : pattern.length();
                    matche = z < match.length ? match[z].start : matchstr.length();
                }
                if (i < target.length()) {
                    int ch = target.codePointAt(i);
                    if (ch == CH_ANNONYM_MULT || ch == CH_ANNONYM_SINGLE) {
                        buf.append(matchstr, match[z].start, match[z].end);
                        wildp = match[z].origend;
                        matchp = match[z].end;
                        z++;
                        wilde = z < match.length ? match[z].origstart : pattern.length();
                        matche = z < match.length ? match[z].start : matchstr.length();
                    } else if (ch == CH_NAMED_MULTI) {
                        i += Character.charCount(ch);
                        ch = target.codePointAt(i);
                        while (ch != CH_NAMED_MULTI) {
                            i += Character.charCount(ch);
                            ch = target.codePointAt(i);
                        }
                        buf.append(matchstr, match[z].start, match[z].end);
                        wildp = match[z].origend;
                        matchp = match[z].end;
                        z++;
                        wilde = z < match.length ? match[z].origstart : pattern.length();
                        matche = z < match.length ? match[z].start : matchstr.length();
                    } else if (ch == CH_CHARSET_START) {
                        i += Character.charCount(ch);
                        ch = target.codePointAt(i);
                        for (; ; ) {
                            i += Character.charCount(ch);
                            ch = target.codePointAt(i);
                            if (target.charAt(i) == CH_CHARSET_RANGE) {
                                i += Character.charCount(ch);
                                ch = target.codePointAt(i);
                                i += Character.charCount(ch);
                                ch = target.codePointAt(i);
                            }
                            if (target.charAt(i) == CH_CHARSET_END)
                                break;
                        }
                        buf.append(matchstr, match[z].start, match[z].end);
                        wildp = match[z].origend;
                        matchp = match[z].end;
                        z++;
                        wilde = z < match.length ? match[z].origstart : pattern.length();
                        matche = z < match.length ? match[z].start : matchstr.length();
                    } else if (ch == CH_REPEAT_START) {
                        SpecimenAdvanced pat = ((MatchRepeat) tarmatch[z]).pat;
                        i = pat.patend;
                        ch = target.codePointAt(i);
                        buf.append(matchstr, match[z].start, match[z].end);
                        wildp = match[z].origend;
                        matchp = match[z].end;
                        z++;
                        wilde = z < match.length ? match[z].origstart : pattern.length();
                        matche = z < match.length ? match[z].start : matchstr.length();
                    } else {
                        if (wildp < wilde && matchp < matche &&
                                pattern.codePointAt(wildp) != matchstr.codePointAt(matchp)) {
                            if (Character.isLowerCase(ch)) {
                                buf.append(Character.toUpperCase(ch));
                            } else if (Character.isUpperCase(ch)) {
                                buf.append(Character.toLowerCase(ch));
                            } else {
                                buf.append(ch);
                            }
                        } else {
                            buf.append(ch);
                        }
                        if (wildp < wilde) {
                            int ch2 = pattern.codePointAt(wildp);
                            wildp += Character.charCount(ch2);
                        }
                        if (matchp < matche) {
                            int ch2 = matchstr.codePointAt(matchp);
                            matchp += Character.charCount(ch2);
                        }
                    }
                    i += Character.charCount(ch);
                } else {
                    break;
                }
            }
            if ((flag & MATCH_WORD) != 0) {
                if (!strip)
                    buf.append(matchstr, match[z - 1].end, matchstr.length());
            } else if ((flag & MATCH_PART) != 0) {
                if (!strip)
                    buf.append(matchstr, match[z].start, match[z].end);
            }
        } else {
            int i = 0;
            for (; ; ) {
                if ((flag & MATCH_WORD) != 0 && patdelemiter.wordBreak1(i, target)) {
                    while (!patdelemiter.wordBreak2(i, target)) {
                        int ch = target.codePointAt(i);
                        i += Character.charCount(ch);
                    }
                    if (i < target.length() || !strip) {
                        buf.append(matchstr, match[z].start, match[z].end);
                        z++;
                    }
                } else if ((flag & MATCH_PART) != 0 && i == 0) {
                    buf.append(matchstr, match[z].start, match[z].end);
                    z++;
                }
                if (i < target.length()) {
                    int ch = target.codePointAt(i);
                    if (ch == CH_ANNONYM_MULT || ch == CH_ANNONYM_SINGLE) {
                        buf.append(matchstr, match[z].start, match[z].end);
                        z++;
                    } else if (ch == CH_NAMED_MULTI) {
                        i += Character.charCount(ch);
                        ch = target.codePointAt(i);
                        while (ch != CH_NAMED_MULTI) {
                            i += Character.charCount(ch);
                            ch = target.codePointAt(i);
                        }
                        buf.append(matchstr, match[z].start, match[z].end);
                        z++;
                    } else if (ch == CH_CHARSET_START) {
                        i += Character.charCount(ch);
                        ch = target.codePointAt(i);
                        for (; ; ) {
                            i += Character.charCount(ch);
                            ch = target.codePointAt(i);
                            if (target.charAt(i) == CH_CHARSET_RANGE) {
                                i += Character.charCount(ch);
                                ch = target.codePointAt(i);
                                i += Character.charCount(ch);
                                ch = target.codePointAt(i);
                            }
                            if (target.charAt(i) == CH_CHARSET_END)
                                break;
                        }
                        buf.append(matchstr, match[z].start, match[z].end);
                        z++;
                    } else if (ch == CH_REPEAT_START) {
                        SpecimenAdvanced pat = ((MatchRepeat) tarmatch[z]).pat;
                        i = pat.patend;
                        ch = target.codePointAt(i);
                        buf.append(matchstr, match[z].start, match[z].end);
                        z++;
                    } else {
                        buf.appendCodePoint(ch);
                    }
                    i += Character.charCount(ch);
                } else {
                    break;
                }
            }
            if ((flag & MATCH_WORD) != 0) {
                if (!strip)
                    buf.append(matchstr, match[z - 1].end, matchstr.length());
            } else if ((flag & MATCH_PART) != 0) {
                if (!strip)
                    buf.append(matchstr, match[z].start, match[z].end);
            }
        }
        return buf.toString();
    }

    /***********************************************************************/
    /* Some Utlities                                                       */
    /***********************************************************************/

    /**
     * <p>Check whether the wildcard has an eventual previous
     * value and whether this value is the same.</p>
     *
     * @param m The wildcard.
     * @param k The start of the current wildcard.
     * @param u The end of the current wildcard.
     * @return True if condition holdes, false otherwise.
     */
    private boolean sameContent(Match m, int k, int u) {
        if (!(m instanceof MatchName))
            return true;
        MatchName mn = (MatchName) m;
        int w = mn.index;
        if (w == -1)
            return true;
        int len = u - k;
        MatchName mn2 = (MatchName) match[w];
        if (len != mn2.end - mn2.start)
            return false;
        return matchstr.regionMatches(mn2.start, matchstr, k, len);
    }

    /**
     * <p>Determine whether word break should be handled.</p>
     *
     * @param k The position to check.
     * @param t The string to check.
     * @return true if word break should be handled at the position, otherwise false.
     */
    public static boolean handleBreak(int k, String t) {
        return (k == 0 || (t.charAt(k - 1) != CH_REPEAT_START && t.charAt(k - 1) != CH_REPEAT_END));
    }

    /**
     * <p>Determine whether word break should be handled.</p>
     *
     * @param k The position to check.
     * @param t The string to check.
     * @return true if word break should be handled at the position, otherwise false.
     */
    private static boolean handleLastBreak(int k, String t) {
        return (k == t.length() || (t.charAt(k) != CH_REPEAT_START && t.charAt(k) != CH_REPEAT_END));
    }

}
