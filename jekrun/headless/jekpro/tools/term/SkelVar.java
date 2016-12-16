package jekpro.tools.term;

import jekpro.model.molec.Display;

/**
 * <p>This class provides variable skeletons.</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SkelVar extends AbstractSkel implements Comparable<SkelVar> {
    private static final int CACHE_SIZE = 8;
    private static final SkelVar[] cache = new SkelVar[CACHE_SIZE];

    static {
        for (int i = 0; i < CACHE_SIZE; i++)
            cache[i] = new SkelVar(i);
    }

    public int id;

    /**
     * <p>Create a skel var.</p>
     *
     * @param i The index into the display.
     */
    public SkelVar(int i) {
        id = i;
    }

    /**
     * <p>Create a skel var, possibly cached.</p>
     *
     * @param i The index into the display.
     * @return The skel var.
     */
    public static SkelVar valueOf(int i) {
        if (i < CACHE_SIZE)
            return cache[i];
        return new SkelVar(i);
    }

    /**
     * <p>Return the hash code of the id.</p>
     *
     * @return The hash code.
     */
    public int hashCode() {
        return id;
    }

    /**
     * <p>Compare this skel var to another skel var.</p>
     *
     * @param o The other skel var.
     * @return The comparison result.
     */
    public int compareTo(SkelVar o) {
        return id - o.id;
    }

    /**
     * <p>Return a string of a skeleton.</p>
     *
     * @return The string.
     */
    public String toString() {
        return SkelVar.sernoToString(id, true);
    }

    /**
     * <p>The hash code of a vaiable inside a display.</p>
     *
     * @param d The display.
     * @return The hash code.
     */
    public int hashCode(Display d) {
        return d.hashCode() * 31 + hashCode();
    }

    /**
     * <p>Convert a serno to a string.</p>
     *
     * @param k     The serno.
     * @param under The underscore flag.
     * @return The string.
     */
    public static String sernoToString(int k, boolean under) {
        StringBuilder buf = new StringBuilder();
        if (under)
            buf.appendCodePoint('_');
        buf.appendCodePoint(k % 26 + 'A');
        if (k >= 26)
            buf.append(k / 26);
        return buf.toString();
    }

}
