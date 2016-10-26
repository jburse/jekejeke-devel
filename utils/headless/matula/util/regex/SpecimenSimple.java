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
    public static final String ERROR_SYNTAX_PHRASE_MISSING = "phrase_missing";
    public static final String ERROR_SYNTAX_QUOTED_SINGLE = "quoted_single";
    public static final String ERROR_SYNTAX_NOT_SUPPORTED = "not_supported";

    public static final char CH_ANNONYM_MULT = '*';
    public static final char CH_ANNONYM_SINGLE = '?';

    public static final int MATCH_WORD = 0x00000200;
    public static final int MATCH_PART = 0x00000400;
    public static final int MATCH_CASE = 0x00000800;
    public static final int MATCH_QUOTE = 0x00001000;

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
        prepareMatch(0);
    }

    /**
     * Count the wild cards. There are implicit and explicit
     * wildcards. The implicit wildcards are between word
     * breaks. The explicit wildcards are * and ?.
     *
     * @param i The index where to start from.
     * @throws ScannerError Parsing problem.
     */
    private void prepareMatch(int i) throws ScannerError {
        int vecsize = 0;
        matchbound = 0;
        int patstart = i;
        for (; ; i++) {
            if ((flag & MATCH_WORD) != 0 && patdelemiter.wordBreak1(i, pattern)) {
                // skip the word break
                while (!patdelemiter.wordBreak2(i, pattern))
                    i++;
                // if necessary add a unnamed match
                vecsize++;
                matchbound += MAX_MATCH_BLANK;
            } else if ((flag & MATCH_PART) != 0 && i == 0) {
                // if at beginng add a unnamed match
                vecsize++;
            }
            if (i < pattern.length()) {
                char ch = pattern.charAt(i);
                if (ch == CH_ANNONYM_MULT) {
                    // add a unnamed match
                    vecsize++;
                    matchbound += MAX_MATCH_WILDCARD;
                } else if (ch == CH_ANNONYM_SINGLE) {
                    // add a unnamed match
                    vecsize++;
                    matchbound++;
                } else {
                    matchbound++;
                }
            } else {
                break;
            }
        }
        if ((flag & MATCH_WORD) != 0) {
            // end was already handled in loop by word break */
        } else if ((flag & MATCH_PART) != 0) {
            // at end add a unnamed match */
            vecsize++;
        }
        int patend = i;
        matchstart = new int[vecsize];
        matchend = new int[vecsize];
        // now that we know the number of matches we populate wild start and end
        wildstart = new int[vecsize];
        wildend = new int[vecsize];
        //if ((flag & MATCH_SENSITIV) != 0) {
        int k = 0;
        for (i = patstart; ; i++) {
            if ((flag & MATCH_WORD) != 0 && patdelemiter.wordBreak1(i, pattern)) {
                // skip the word break
                int j = i;
                while (!patdelemiter.wordBreak2(i, pattern))
                    i++;
                // if necessary we had a wild
                wildstart[k] = j;
                wildend[k] = i;
                k++;
            } else if ((flag & MATCH_PART) != 0 && i == 0) {
                // if at beginng we had a wild
                wildstart[k] = i;
                wildend[k] = i;
                k++;
            }
            if (i < pattern.length()) {
                char ch = pattern.charAt(i);
                if (ch == CH_ANNONYM_MULT || ch == CH_ANNONYM_SINGLE) {
                    // add a wild
                    wildstart[k] = i;
                    wildend[k] = i + 1;
                    k++;
                } else {
                    /* do nothing */
                }
            } else {
                break;
            }
        }
        if ((flag & MATCH_WORD) != 0) {
            // end was already handled in loop by word break
        } else if ((flag & MATCH_PART) != 0) {
            // at end add a wild
            wildstart[k] = i;
            wildend[k] = i;
            k++;
        }
        if (k != vecsize)
            throw new IllegalArgumentException("problem prepare pat");
        if (i != patend)
            throw new IllegalArgumentException("problem prepare pat");
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
            char ch;
            if ((flags & MASK_PAT_BLOCK) == 0 &&
                    (flag & MATCH_WORD) != 0 && patdelemiter.wordBreak1(i, pattern)) {
                // skip the word break
                int j = i;
                while (!patdelemiter.wordBreak2(i, pattern))
                    i++;
                // assure that previous word end was reached
                if (j != 0 && !matchdelemiter.wordBreak1(k, matchstr))
                    return false;
                // try match
                for (int u = k; u <= matchstr.length() &&
                        (j == 0 || i == pattern.length()
                                || u - k < MAX_MATCH_BLANK); u++) {
                    if (!matchdelemiter.wordBreak2(u, matchstr))
                        continue;
                    if ((flags & MASK_PAT_BOUNCE) != 0 && u == matchstr.length())
                        continue;
                    if (matchPattern(i, u, z + 1, flags | MASK_PAT_BLOCK)) {
                        matchstart[z] = k;
                        matchend[z] = u;
                        return true;
                    }
                    // assure that only single break is skipped
                    if (j != 0)
                        return false;
                }
                return false;
            } else if ((flags & MASK_PAT_BLOCK) == 0 &&
                    (flag & MATCH_PART) != 0 && i == 0) {
                for (int u = k; u <= matchstr.length(); u++) {
                    // try match
                    if (matchPattern(i, u, z + 1, flags | MASK_PAT_BLOCK)) {
                        matchstart[z] = k;
                        matchend[z] = u;
                        return true;
                    }
                }
                return false;
            } else if (i == pattern.length()) {
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
            } else if ((ch = pattern.charAt(i)) == CH_ANNONYM_MULT) {
                int j = i;
                i++;
                if ((flag & MATCH_WORD) != 0) {
                    boolean flagstart = patdelemiter.wordBreak2(j, pattern);
                    boolean flagend = patdelemiter.wordBreak1(i, pattern);
                    int u = k;
                    if (flagstart && flagend)
                        u++;
                    boolean break2 = matchdelemiter.wordBreak2(k, matchstr);
                    // try match
                    for (; u <= matchstr.length() && u - k < MAX_MATCH_WILDCARD; u++) {
                        if (matchPattern(i, u, z + 1, flags & ~MASK_PAT_BLOCK)) {
                            matchstart[z] = k;
                            matchend[z] = u;
                            return true;
                        }
                        // assure that only single break is skipped
                        if ((!break2 || u != k) && matchdelemiter.wordBreak1(u, matchstr))
                            return false;
                    }
                } else {
                    // try match
                    for (int u = k; u <= matchstr.length() &&
                            u - k < MAX_MATCH_WILDCARD; u++) {
                        if (matchPattern(i, u, z + 1, flags & ~MASK_PAT_BLOCK)) {
                            matchstart[z] = k;
                            matchend[z] = u;
                            return true;
                        }
                    }
                }
                return false;
            } else if (ch == CH_ANNONYM_SINGLE) {
                if (k == matchstr.length()) return false;
                matchstart[z] = k;
                k++;
                matchend[z] = k;
                z++;
                i++;
                flags &= ~MASK_PAT_BLOCK;
            } else {
                // single character
                if (k == matchstr.length()) return false;
                if ((flag & MATCH_SENSITIV) != 0) {
                    if (pattern.charAt(i) != matchstr.charAt(k))
                        return false;
                } else {
                    if (Character.toLowerCase(pattern.charAt(i)) !=
                            Character.toLowerCase(matchstr.charAt(k)))
                        return false;
                }
                k++;
                i++;
                flags &= ~MASK_PAT_BLOCK;
            }
        }
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
            char ch;
            if ((flags & MASK_PAT_BLOCK) == 0 &&
                    (flag & MATCH_WORD) != 0 && patdelemiter.wordBreak2(i, pattern)) {
                // skip the word break
                int j = i;
                while (!patdelemiter.wordBreak1(i, pattern))
                    i--;
                if (j != pattern.length() && !matchdelemiter.wordBreak2(k, matchstr))
                    return false;
                // try match
                for (int u = k; u >= 0 && (j == pattern.length()
                        || i == 0
                        || k - u < MAX_MATCH_BLANK); u--) {
                    if (!matchdelemiter.wordBreak1(u, matchstr))
                        continue;
                    if ((flags & MASK_PAT_BOUNCE) != 0 && u == 0)
                        continue;
                    if (matchLastPattern(i, u, z - 1, flags | MASK_PAT_BLOCK)) {
                        matchstart[z] = u;
                        matchend[z] = k;
                        return true;
                    }
                    // assure that only single break is skipped
                    if (j != pattern.length())
                        return false;
                }
                return false;
            } else if ((flags & MASK_PAT_BLOCK) == 0 &&
                    (flag & MATCH_PART) != 0 && i == pattern.length()) {
                for (int u = k; u >= 0; u--) {
                    // try match
                    if (matchLastPattern(i, u, z - 1, flags | MASK_PAT_BLOCK)) {
                        matchstart[z] = u;
                        matchend[z] = k;
                        return true;
                    }
                }
                return false;
            } else if (i == 0) {
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
            } else if ((ch = pattern.charAt(i - 1)) == CH_ANNONYM_MULT) {
                int j = i;
                i--;
                if ((flag & MATCH_WORD) != 0) {
                    boolean flagstart = patdelemiter.wordBreak2(i, pattern);
                    boolean flagend = patdelemiter.wordBreak1(j, pattern);
                    int u = k;
                    if (flagstart && flagend)
                        u--;
                    boolean break2 = matchdelemiter.wordBreak1(k, matchstr);
                    // try match
                    for (; u >= 0 && k - u < MAX_MATCH_WILDCARD; u--) {
                        if (matchLastPattern(i, u, z - 1, flags & ~MASK_PAT_BLOCK)) {
                            matchstart[z] = u;
                            matchend[z] = k;
                            return true;
                        }
                        // assure that only single break is skipped
                        if ((!break2 || u != k) && matchdelemiter.wordBreak2(u, matchstr))
                            return false;
                    }
                } else {
                    // try match
                    for (int u = k; u >= 0 && k - u < MAX_MATCH_WILDCARD; u--) {
                        if (matchLastPattern(i, u, z - 1, flags & ~MASK_PAT_BLOCK)) {
                            matchstart[z] = u;
                            matchend[z] = k;
                            return true;
                        }
                    }
                }
                return false;
            } else if (ch == CH_ANNONYM_SINGLE) {
                if (k == 0) return false;
                matchend[z] = k;
                k--;
                matchstart[z] = k;
                z--;
                i--;
                flags &= ~MASK_PAT_BLOCK;
            } else {
                // single character
                if (k == 0) return false;
                k--;
                i--;
                if ((flag & MATCH_SENSITIV) != 0) {
                    if (pattern.charAt(i) != matchstr.charAt(k))
                        return false;
                } else {
                    if (Character.toLowerCase(pattern.charAt(i)) !=
                            Character.toLowerCase(matchstr.charAt(k)))
                        return false;
                }
                flags &= ~MASK_PAT_BLOCK;
            }
        }
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
        if ((flag & MATCH_SENSITIV) == 0) {
            int wilds = 0;
            int wilde = z < wildstart.length ? wildstart[z] : pattern.length();
            int matchs = 0;
            int matche = z < matchstart.length ? matchstart[z] : matchstr.length();
            int tar = 0;
            for (int i = 0; ; i++) {
                if ((flag & MATCH_WORD) != 0 && patdelemiter.wordBreak1(i, target)) {
                    while (!patdelemiter.wordBreak2(i, target))
                        i++;
                    if (i < target.length() || !strip) {
                        buf.append(matchstr.substring(matchstart[z], matchend[z]));
                        tar = i;
                        wilds = wildend[z];
                        matchs = matchend[z];
                        z++;
                        wilde = z < wildstart.length ? wildstart[z] : pattern.length();
                        matche = z < matchstart.length ? matchstart[z] : matchstr.length();
                    }
                } else if ((flag & MATCH_PART) != 0 && i == 0) {
                    buf.append(matchstr.substring(matchstart[z], matchend[z]));
                    tar = i;
                    wilds = wildend[z];
                    matchs = matchend[z];
                    z++;
                    wilde = z < wildstart.length ? wildstart[z] : pattern.length();
                    matche = z < matchstart.length ? matchstart[z] : matchstr.length();
                }
                if (i < target.length()) {
                    char ch = target.charAt(i);
                    if (ch == CH_ANNONYM_MULT || ch == CH_ANNONYM_SINGLE) {
                        buf.append(matchstr.substring(matchstart[z], matchend[z]));
                        tar = i + 1;
                        wilds = wildend[z];
                        matchs = matchend[z];
                        z++;
                        wilde = z < wildstart.length ? wildstart[z] : pattern.length();
                        matche = z < matchstart.length ? matchstart[z] : matchstr.length();
                    } else {
                        if (i - tar + wilds < wilde && i - tar + matchs < matche &&
                                pattern.charAt(i - tar + wilds) != matchstr.charAt(i - tar + matchs)) {
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
                    }
                } else {
                    break;
                }
            }
            if ((flag & MATCH_WORD) != 0) {
                if (!strip)
                    buf.append(matchstr.substring(matchend[z - 1]));
            } else if ((flag & MATCH_PART) != 0) {
                if (!strip)
                    buf.append(matchstr.substring(matchstart[z], matchend[z]));
            }
        } else {
            for (int i = 0; ; i++) {
                if ((flag & MATCH_WORD) != 0 && patdelemiter.wordBreak1(i, target)) {
                    while (!patdelemiter.wordBreak2(i, target))
                        i++;
                    if (i < target.length() || !strip) {
                        buf.append(matchstr.substring(matchstart[z], matchend[z]));
                        z++;
                    }
                } else if ((flag & MATCH_PART) != 0 && i == 0) {
                    buf.append(matchstr.substring(matchstart[z], matchend[z]));
                    z++;
                }
                if (i < target.length()) {
                    char ch = target.charAt(i);
                    if (ch == CH_ANNONYM_MULT || ch == CH_ANNONYM_SINGLE) {
                        buf.append(matchstr.substring(matchstart[z], matchend[z]));
                        z++;
                    } else {
                        buf.append(ch);
                    }
                } else {
                    break;
                }
            }
            if ((flag & MATCH_WORD) != 0) {
                if (!strip)
                    buf.append(matchstr.substring(matchend[z - 1]));
            } else if ((flag & MATCH_PART) != 0) {
                if (!strip)
                    buf.append(matchstr.substring(matchstart[z], matchend[z]));
            }
        }
        return buf.toString();
    }

}
