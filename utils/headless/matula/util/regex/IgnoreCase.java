package matula.util.regex;

import java.util.Comparator;

/**
 * <p>Case insensitive comparator faithful to simple specimen.</p>
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
public final class IgnoreCase implements Comparator<String> {
    public static final Comparator<String> DEFAULT = new IgnoreCase();

    /**
     * <p>Create a ignore case comparator.</p>
     */
    private IgnoreCase() {
    }

    /**
     * <p>Compare two strings.</p>
     *
     * @param o1 The first string.
     * @param o2 The second string.
     * @return < 0 if less than, 0 if equal, > 0 if greater than.
     */
    public int compare(String o1, String o2) {
        int k1 = 0;
        int k2 = 0;
        while (k1 < o1.length() && k2 < o2.length()) {
            int ch1 = o1.codePointAt(k1);
            int ch2 = o2.codePointAt(k2);
            k1 += Character.charCount(ch1);
            k2 += Character.charCount(ch2);
            ch1 = Character.toLowerCase(ch1);
            ch2 = Character.toLowerCase(ch2);
            if (ch1 != ch2)
                return ch1 - ch2;
        }
        if (!(k1 < o1.length())) {
            if (!(k2 < o2.length())) {
                return 0;
            } else {
                return -1;
            }
        } else {
            return 1;
        }
    }

}
