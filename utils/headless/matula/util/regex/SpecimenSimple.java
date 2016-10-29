package matula.util.regex;

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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SpecimenSimple extends AbstractSpecimen {
    public static final char CH_ANNONYM_MULT = '*';
    public static final char CH_ANNONYM_SINGLE = '?';

    private static final int MAX_MATCH_BLANK = 15;
    private static final int MAX_MATCH_WILDCARD = 150;

    private static final int MASK_PAT_BLOCK = 0x00000100;

    private int[] wildstart;
    private int[] wildend;
    private int[] matchstart;
    private int[] matchend;
    private String matchstr;
    private int searchpos;
    private int matchbound;

    /**
     * Count the wild cards. There are implicit and explicit
     * wildcards. The implicit wildcards are between word
     * breaks. The explicit wildcards are * and ?.
     *
     * @throws ScannerError Parsing problem.
     */
    public void prepareMatch() throws ScannerError {
        int i = 0;
        int k = 0;
        matchbound = 0;
        for (; ; ) {
            if ((flag & MATCH_WORD) != 0 && patdelemiter.wordBreak1(i, pattern)) {
                int j = i;

                // skip the word break
                while (!patdelemiter.wordBreak2(i, pattern)) {
                    int ch = pattern.codePointAt(i);
                    i += Character.charCount(ch);
                }
                // add a implicit wildcard
                k++;

                // inside blanks are bounded
                if (j != 0 && i != pattern.length())
                    matchbound += MAX_MATCH_BLANK;
            } else if ((flag & MATCH_PART) != 0 && i == 0) {
                // add a implicit wildcard
                k++;
            }
            if (i < pattern.length()) {
                int ch = pattern.codePointAt(i);
                if (ch == CH_ANNONYM_MULT) {
                    // add an explicit wildcard
                    k++;
                    matchbound += MAX_MATCH_WILDCARD;
                } else if (ch == CH_ANNONYM_SINGLE) {
                    // add an explicit wildcard
                    k++;
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
            // add a implicit wildcard
            k++;
        }
        matchstart = new int[k];
        matchend = new int[k];
        // now that we know the number of matches we populate wild start and end
        wildstart = new int[k];
        wildend = new int[k];
        i = 0;
        k = 0;
        for (; ; ) {
            if ((flag & MATCH_WORD) != 0 && patdelemiter.wordBreak1(i, pattern)) {
                int j = i;

                // skip the word break
                while (!patdelemiter.wordBreak2(i, pattern)) {
                    int ch = pattern.codePointAt(i);
                    i += Character.charCount(ch);
                }

                // add a implicit wildcard
                wildstart[k] = j;
                wildend[k] = i;
                k++;
            } else if ((flag & MATCH_PART) != 0 && i == 0) {
                // add a implicit wildcard
                wildstart[k] = i;
                wildend[k] = i;
                k++;
            }
            if (i < pattern.length()) {
                int ch = pattern.codePointAt(i);
                if (ch == CH_ANNONYM_MULT || ch == CH_ANNONYM_SINGLE) {
                    // add an explicit wildcard
                    wildstart[k] = i;
                    wildend[k] = i + Character.charCount(ch);
                    k++;
                } else {
                    // do nothing
                }
                i += Character.charCount(ch);
            } else {
                break;
            }
        }
        if ((flag & MATCH_WORD) != 0) {
            // end was already handled in loop by word break
        } else if ((flag & MATCH_PART) != 0) {
            // add a implicit wildcard
            wildstart[k] = i;
            wildend[k] = i;
            k++;
        }
        if (k != wildstart.length)
            throw new IllegalArgumentException("prepare problem");
    }

    /**
     * Compute an upper bound based on the number of wildcards.
     *
     * @return The bound.
     */
    public int getMatchBound() {
        return matchbound;
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

                    // assure that previous word end was reached
                    if (j != 0 && !matchdelemiter.wordBreak1(k, matchstr))
                        return false;

                    // skip the word break
                    while (!patdelemiter.wordBreak2(i, pattern)) {
                        int ch = pattern.codePointAt(i);
                        i += Character.charCount(ch);
                    }

                    int u = k;
                    for (; ; ) {
                        if (!matchdelemiter.wordBreak2(u, matchstr)) {
                            // don't try
                        } else if ((flags & MASK_PAT_BOUNCE) != 0 && u == matchstr.length()) {
                            // don't try
                        } else {
                            // try match
                            if (matchPattern(i, u, z + 1, flags | MASK_PAT_BLOCK)) {
                                matchstart[z] = k;
                                matchend[z] = u;
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
                            matchstart[z] = k;
                            matchend[z] = u;
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
                                    matchstart[z] = k;
                                    matchend[z] = u;
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
                                matchstart[z] = k;
                                matchend[z] = u;
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
                        matchstart[z] = k;
                        matchend[z] = k + Character.charCount(ch2);
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
            matchstart[z] = k;
            matchend[z] = matchstr.length();
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
        return matchLastPattern(pattern.length(), k, matchstart.length - 1, flags);
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

                    // assure that previous word start was reached
                    if (j != pattern.length() && !matchdelemiter.wordBreak2(k, matchstr))
                        return false;

                    // skip the word break
                    while (!patdelemiter.wordBreak1(i, pattern)) {
                        int ch = pattern.codePointBefore(i);
                        i -= Character.charCount(ch);
                    }

                    int u = k;
                    for (; ; ) {
                        if (!matchdelemiter.wordBreak1(u, matchstr)) {
                            // don't try
                        } else if ((flags & MASK_PAT_BOUNCE) != 0 && u == 0) {
                            // don't try
                        } else {
                            // try match
                            if (matchLastPattern(i, u, z - 1, flags | MASK_PAT_BLOCK)) {
                                matchstart[z] = u;
                                matchend[z] = k;
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
                            matchstart[z] = u;
                            matchend[z] = k;
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
                                    matchstart[z] = u;
                                    matchend[z] = k;
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
                                matchstart[z] = u;
                                matchend[z] = k;
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
                        matchstart[z] = k - Character.charCount(ch2);
                        matchend[z] = k;
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
            matchstart[z] = 0;
            matchend[z] = k;
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
            int z = 0;
            if (strip) {
                return matchend[z];
            } else {
                return matchstart[z];
            }
        } else if ((flag & MATCH_PART) != 0) {
            return matchend[0];
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
        return getMatchEnd(true);
    }

    /**
     * Return the end of the match.
     *
     * @param strip True if exclude last word break match, otherwise false.
     * @return The end.
     */
    private int getMatchEnd(boolean strip) {
        if ((flag & MATCH_WORD) != 0) {
            int z = matchstart.length - 1;
            if (strip) {
                return matchstart[z];
            } else {
                return matchend[z];
            }
        } else if ((flag & MATCH_PART) != 0) {
            return matchstart[matchstart.length - 1];
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
        try {
            SpecimenSimple pm = new SpecimenSimple();
            pm.setPatDelemiter(patdelemiter);
            pm.setMatchDelemiter(matchdelemiter);
            pm.setPattern(pattern);
            pm.setFlag(flag);
            pm.prepareMatch();
            for (int i = 0; i < matchstart.length; i++) {
                pm.matchstart[i] = matchstart[i];
                pm.matchend[i] = matchend[i];
            }
            pm.searchpos = searchpos;
            pm.matchstr = matchstr;
            pm.target = target;
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
            int wilde = z < wildstart.length ? wildstart[z] : pattern.length();
            int matchp = 0;
            int matche = z < matchstart.length ? matchstart[z] : matchstr.length();
            int i = 0;
            for (; ; i++) {
                if ((flag & MATCH_WORD) != 0 && patdelemiter.wordBreak1(i, target)) {
                    while (!patdelemiter.wordBreak2(i, target)) {
                        int ch = target.codePointAt(i);
                        i += Character.charCount(ch);
                    }
                    if (i < target.length() || !strip) {
                        buf.append(matchstr, matchstart[z], matchend[z]);
                        wildp = wildend[z];
                        matchp = matchend[z];
                        z++;
                        wilde = z < wildstart.length ? wildstart[z] : pattern.length();
                        matche = z < matchstart.length ? matchstart[z] : matchstr.length();
                    }
                } else if ((flag & MATCH_PART) != 0 && i == 0) {
                    buf.append(matchstr, matchstart[z], matchend[z]);
                    wildp = wildend[z];
                    matchp = matchend[z];
                    z++;
                    wilde = z < wildstart.length ? wildstart[z] : pattern.length();
                    matche = z < matchstart.length ? matchstart[z] : matchstr.length();
                }
                if (i < target.length()) {
                    int ch = target.codePointAt(i);
                    if (ch == CH_ANNONYM_MULT || ch == CH_ANNONYM_SINGLE) {
                        buf.append(matchstr, matchstart[z], matchend[z]);
                        wildp = wildend[z];
                        matchp = matchend[z];
                        z++;
                        wilde = z < wildstart.length ? wildstart[z] : pattern.length();
                        matche = z < matchstart.length ? matchstart[z] : matchstr.length();
                    } else {
                        if (wildp < wilde && matchp < matche &&
                                pattern.codePointAt(wildp) != matchstr.codePointAt(matchp)) {
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
                    if (wildp < wilde) {
                        ch = pattern.codePointAt(wildp);
                        wildp += Character.charCount(ch);
                    }
                    if (matchp < matche) {
                        ch = pattern.codePointAt(matchp);
                        matchp += Character.charCount(ch);
                    }
                } else {
                    break;
                }
            }
            if ((flag & MATCH_WORD) != 0) {
                if (!strip)
                    buf.append(matchstr, matchend[z - 1], matchstr.length());
            } else if ((flag & MATCH_PART) != 0) {
                if (!strip)
                    buf.append(matchstr, matchstart[z], matchend[z]);
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
                        buf.append(matchstr, matchstart[z], matchend[z]);
                        z++;
                    }
                } else if ((flag & MATCH_PART) != 0 && i == 0) {
                    buf.append(matchstr, matchstart[z], matchend[z]);
                    z++;
                }
                if (i < target.length()) {
                    int ch = target.codePointAt(i);
                    if (ch == CH_ANNONYM_MULT || ch == CH_ANNONYM_SINGLE) {
                        buf.append(matchstr, matchstart[z], matchend[z]);
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
                    buf.append(matchstr, matchend[z - 1], matchstr.length());
            } else if ((flag & MATCH_PART) != 0) {
                if (!strip)
                    buf.append(matchstr, matchstart[z], matchend[z]);
            }
        }
        return buf.toString();
    }

}
