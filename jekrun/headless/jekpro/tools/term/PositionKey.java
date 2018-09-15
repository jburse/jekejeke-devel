package jekpro.tools.term;

/**
 * <p>This class encapsulates an atom position.</p>
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
public final class PositionKey {
    private String origin;
    private int lineno;

    /**
     * <p>Create a position key.</p>
     *
     * @param o The origin.
     * @param l The lineno.
     */
    public PositionKey(String o, int l) {
        if (o == null)
            throw new NullPointerException("origin null");
        origin = o;
        lineno = l;
    }

    /**
     * <p>Retrieve the origin.</p>
     *
     * @return The origin.
     */
    public String getOrigin() {
        return origin;
    }

    /**
     * <p>Retrieve the lineno.</p>
     *
     * @return The lineno.
     */
    public int getLineNo() {
        return lineno;
    }

    /**
     * <p>Check whether this position equals an other position.</p>
     *
     * @param o The other position.
     * @return True if this position equals the other position, otherwise false.
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof PositionKey))
            return false;
        PositionKey other = (PositionKey) o;
        if (lineno != other.lineno)
            return false;
        if (!origin.equals(other.origin))
            return false;
        return true;
    }

    /**
     * <p>Compute the hash code.</p>
     *
     * @return The hash code.
     */
    public int hashCode() {
        int result = origin.hashCode();
        result = 31 * result + lineno;
        return result;
    }

}
