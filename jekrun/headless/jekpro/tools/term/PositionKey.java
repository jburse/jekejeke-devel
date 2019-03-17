package jekpro.tools.term;

import matula.util.system.OpenOpts;

import java.io.Reader;
import java.util.Comparator;

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
public final class PositionKey implements Comparable<PositionKey> {
    public final static Comparator<PositionKey> DEFAULT = new Comparator<PositionKey>() {
        public int compare(PositionKey o1, PositionKey o2) {
            return o1.compareTo(o2);
        }
    };

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
     * <p>Create a position key from a reader.</p>
     *
     * @param lr The reader.
     * @return The position key.
     */
    public static PositionKey createPos(Reader lr) {
        String orig = OpenOpts.getPath(lr);
        if (orig != null) {
            return new PositionKey(orig, OpenOpts.getLineNumber(lr));
        } else {
            return null;
        }
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
        if (!origin.equals(other.origin))
            return false;
        if (lineno != other.lineno)
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

    /**
     * <p>Compare this position key, to another position key.</p>
     *
     * @param o The other position key.
     * @return <0 for this < o, =0 for this = o, >= for this > o.
     */
    public int compareTo(PositionKey o) {
        int k = origin.compareTo(o.origin);
        if (k != 0)
            return k;
        return lineno - o.lineno;
    }

}
