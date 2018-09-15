package jekpro.reference.structure;

import java.util.Enumeration;

/**
 * <p>The backtracking data for the atom cursor.</p>
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
final class AtomCursor implements Enumeration<Integer> {
    private String str;
    private int cfrom;
    private int from;
    private int to;
    private boolean forward;

    /**
     * <p>Create a new atom cursor.</p>
     *
     * @param s  The atom.
     * @param cf The from codepoint index.
     * @param f  The from word index.
     * @param t  The to word index.
     */
    AtomCursor(String s, int cf, int f, int t) {
        str = s;
        cfrom = cf;
        from = f;
        to = t;
        forward = (from <= to);
    }

    /**
     * <p>Retrieve the from codepoint index.</p>
     *
     * @return The from codepoint index.
     */
    Integer getCFrom() {
        return Integer.valueOf(cfrom);
    }

    /**
     * <p>Check whether there are more elements.</p>
     *
     * @return True if there are more elements, otherwise false.
     */
    public boolean hasMoreElements() {
        return (forward ? from <= to : from >= to);
    }

    /**
     * <p>Retrieve the next element and advance the cursor.</p>
     *
     * @return The next element.
     */
    public Integer nextElement() {
        Integer val = Integer.valueOf(from);
        if (forward) {
            if (from < to) {
                cfrom++;
                int ch = str.codePointAt(from);
                from = from + Character.charCount(ch);
            } else {
                from++;
            }
        } else {
            if (from > to) {
                cfrom--;
                int ch = str.codePointBefore(from);
                from = from - Character.charCount(ch);
            } else {
                from--;
            }
        }
        return val;
    }

}