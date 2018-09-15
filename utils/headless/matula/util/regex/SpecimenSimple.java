package matula.util.regex;

import matula.util.data.ListArray;

/**
 * <p>This class provides a bounded simple pattern.</p>
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
public final class SpecimenSimple extends AbstractSpecimen {
    private static final int MASK_PAT_BLOCK = 0x00000100;

    private Match[] match;
    private int matchbound;

    /**
     * <p>Retreive the matches.</p>
     *
     * @return The matches.
     */
    public Match[] getMatch() {
        return match;
    }

    /******************************************************************/
    /* Preparation                                                    */
    /******************************************************************/

    /**
     * Count the wild cards. There are implicit and explicit
     * wildcards. The implicit wildcards are between word
     * breaks. The explicit wildcards are * and ?.
     */
    public void prepareMatch() {
        ListArray<Match> vec = new ListArray<Match>();
        int i = 0;
        matchbound = 0;
        for (; ; ) {
            if ((flag & MATCH_WORD) != 0 && patdelemiter.wordBreak1(i, pattern)) {
                int j = i;

                // skip the word break
                while (!patdelemiter.wordBreak2(i, pattern)) {
                    int ch = pattern.codePointAt(i);
                    i += Character.charCount(ch);
                }
                // add an implicit wildcard
                Match match = new Match();
                match.origstart = j;
                match.origend = i;
                vec.add(match);

                // inside blanks are bounded
                if (j != 0 && i != pattern.length())
                    matchbound += MAX_MATCH_BLANK;
            } else if ((flag & MATCH_PART) != 0 && i == 0) {
                // add an implicit wildcard
                Match match = new Match();
                match.origstart = i;
                match.origend = i;
                vec.add(match);
            }
            if (i < pattern.length()) {
                int ch = pattern.codePointAt(i);
                if (ch == CH_ANNONYM_MULT) {
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
        } else if ((flag & MATCH_PART) != 0) {
            // add an implicit wildcard
            Match match = new Match();
            match.origstart = i;
            match.origend = i;
            vec.add(match);
        }
        match = new Match[vec.size()];
        vec.toArray(match);
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
        SpecimenSimple ss = (SpecimenSimple) pat;
        target = ss.pattern;
        if (match.length != ss.match.length)
            throw new IllegalArgumentException("incompatible_target");
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
                if (ch == CH_ANNONYM_MULT) {
                    int j = i;
                    i += Character.charCount(ch);
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
                                if (matchPattern(i, u, z + 1, flags & ~MASK_PAT_BLOCK)) {
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
                            if (matchPattern(i, u, z + 1, flags & ~MASK_PAT_BLOCK)) {
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
        } else if ((flag & MATCH_PART) != 0) {
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

                    // assure that previous word start was reached
                    if (j != pattern.length() && !matchdelemiter.wordBreak2(k, matchstr))
                        return false;

                    int u = k;
                    for (; ; ) {
                        if (!matchdelemiter.wordBreak1(u, matchstr)) {
                            // don't try
                        } else if ((flags & MASK_PAT_BOUNCE) != 0 && u == 0) {
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
                if (ch == CH_ANNONYM_MULT) {
                    int j = i;
                    i -= Character.charCount(ch);
                    if ((flag & MATCH_WORD) != 0) {
                        boolean f1 = patdelemiter.wordBreak1(j, pattern) &&
                                patdelemiter.wordBreak2(i, pattern);
                        boolean f2 = matchdelemiter.wordBreak1(k, matchstr);
                        int u = k;
                        for (; ; ) {
                            if (f1) {
                                // don't try
                                f1 = false;
                            } else {
                                // try match
                                if (matchLastPattern(i, u, z - 1, flags & ~MASK_PAT_BLOCK)) {
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
                            if (matchLastPattern(i, u, z - 1, flags & ~MASK_PAT_BLOCK)) {
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
        } else if ((flag & MATCH_PART) != 0) {
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
        if ((flag & MATCH_WORD) != 0) {
            return match[0].end;
        } else if ((flag & MATCH_PART) != 0) {
            return match[0].end;
        } else {
            return searchpos;
        }
    }

    /**
     * <p>Return the match end position.</p>
     *
     * @return The match end.
     */
    public int getMatchEnd() {
        if ((flag & MATCH_WORD) != 0) {
            return match[match.length - 1].start;
        } else if ((flag & MATCH_PART) != 0) {
            return match[match.length - 1].start;
        } else {
            return matchstr.length();
        }
    }

    /**
     * <p>Return a copy of the matcher.</p>
     *
     * @return The copy.
     */
    public AbstractSpecimen copyMatcher() {
            /* the pattern */
        SpecimenSimple pm = new SpecimenSimple();
        pm.setPatDelemiter(patdelemiter);
        pm.setMatchDelemiter(matchdelemiter);
        pm.setPattern(pattern);
        pm.setFlag(flag);
        pm.prepareMatch();

            /* the target */
        if (target != null) {
            SpecimenSimple pr = new SpecimenSimple();
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
        int z = 0;
        int i = 0;
        if ((flag & MATCH_IGCS) != 0) {
            int wildp = 0;
            int matchp = 0;
            int wilde = z < match.length ? match[z].origstart : pattern.length();
            int matche = z < match.length ? match[z].start : matchstr.length();
            for (; ; ) {
                if ((flag & MATCH_WORD) != 0 && patdelemiter.wordBreak1(i, target)) {
                    while (!patdelemiter.wordBreak2(i, target)) {
                        int ch = target.codePointAt(i);
                        i += Character.charCount(ch);
                    }
                    if (z != 0 && z != match.length - 1)
                        buf.append(matchstr, match[z].start, match[z].end);
                    wildp = match[z].origend;
                    matchp = match[z].end;
                    z++;
                    wilde = z < match.length ? match[z].origstart : pattern.length();
                    matche = z < match.length ? match[z].start : matchstr.length();
                } else if ((flag & MATCH_PART) != 0 && i == 0) {
                    /* do nothing */
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
                    } else {
                        int chw;
                        if (wildp < wilde) {
                            chw = pattern.codePointAt(wildp);
                            wildp += Character.charCount(chw);
                        } else {
                            chw = -1;
                        }
                        int chm;
                        if (matchp < matche) {
                            chm = matchstr.codePointAt(matchp);
                            matchp += Character.charCount(chm);
                        } else {
                            chm = -1;
                        }
                        if (chw != -1 && chm != -1 && chw != chm) {
                            if (Character.isLowerCase(ch)) {
                                buf.appendCodePoint(Character.toUpperCase(ch));
                            } else if (Character.isUpperCase(ch)) {
                                buf.appendCodePoint(Character.toLowerCase(ch));
                            } else {
                                buf.appendCodePoint(ch);
                            }
                        } else {
                            buf.appendCodePoint(ch);
                        }
                    }
                    i += Character.charCount(ch);
                } else {
                    break;
                }
            }
            if ((flag & MATCH_WORD) != 0) {
                /* already handled */
            } else if ((flag & MATCH_PART) != 0) {
                /* do nothing */
            }
        } else {
            for (; ; ) {
                if ((flag & MATCH_WORD) != 0 && patdelemiter.wordBreak1(i, target)) {
                    while (!patdelemiter.wordBreak2(i, target)) {
                        int ch = target.codePointAt(i);
                        i += Character.charCount(ch);
                    }
                    if (z != 0 && z != match.length - 1)
                        buf.append(matchstr, match[z].start, match[z].end);
                    z++;
                } else if ((flag & MATCH_PART) != 0 && i == 0) {
                    /* do nothing */
                    z++;
                }
                if (i < target.length()) {
                    int ch = target.codePointAt(i);
                    if (ch == CH_ANNONYM_MULT || ch == CH_ANNONYM_SINGLE) {
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
                /* already handled */
            } else if ((flag & MATCH_PART) != 0) {
                /* do nothing */
            }
        }
    }

}
