package jekpro.tools.term;

import jekpro.model.molec.AbstractCache;
import jekpro.model.pretty.AbstractSource;

import java.util.Comparator;

/**
 * <p>This class provides an atom.</p>
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
public class SkelAtom extends AbstractSkel implements Comparable<SkelAtom> {
    public final static int MASK_ATOM_ANNO = 0x00000001;
    public final static int MASK_ATOM_POSI = 0x00000002;
    public final static int MASK_ATOM_QALI = 0x00000004;

    private static final int CACHE_SIZE = 128;
    private static final String[] chars = new String[CACHE_SIZE];

    /* initialize the caches */
    static {
        for (int i = 0; i < CACHE_SIZE; i++)
            chars[i] = String.valueOf((char) i);
    }

    public final String fun;
    public final AbstractSource scope;
    public AbstractCache cache;

    /**
     * <p>Create an atom from a string.</p>
     *
     * @param f The string, not null.
     */
    public SkelAtom(String f) {
        if (f == null)
            throw new NullPointerException("string missing");
        fun = f;
        scope = null;
    }

    /**
     * <p>Create an atom from a string and a source.</p>
     *
     * @param f The string, not null.
     * @param s The source, not null.
     */
    public SkelAtom(String f, AbstractSource s) {
        if (f == null)
            throw new NullPointerException("string missing");
        fun = f;
        scope = s;
    }

    /**
     * <p>Return the hash code of the codes.</p>
     *
     * @return The hash code.
     */
    public int hashCode() {
        return fun.hashCode();
    }

    /**
     * <p>Check that the argument is an atom and that
     * this atom and the argument atom have the same codes.</p>
     *
     * @param o The other object.
     * @return True if equal, otherwise false.
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof SkelAtom))
            return false;
        return fun.equals(((SkelAtom) o).fun);
    }

    /**
     * <p>Compare this atom to another atom.</p>
     *
     * @param o The other atom.
     * @return <0 less, 0 equal, >0 greater
     */
    public int compareTo(SkelAtom o) {
        return fun.compareTo(o.fun);
    }

    /**
     * <p>Compare this atom to another atom.</p>
     *
     * @param o The other atom.
     * @param c The collator.
     * @return <0 less, 0 equal, >0 greater
     */
    public int compareTo(SkelAtom o, Comparator c) {
        return c.compare(fun, o.fun);
    }

    /********************************************************/
    /* Caching of Low Range Characters                      */
    /********************************************************/

    /**
     * <p>Create a string, possibly cached.</p>
     *
     * @param ch The code point.
     * @return The string.
     */
    public static String valueOf(int ch) {
        if (ch < CACHE_SIZE)
            return chars[ch];
        return new String(Character.toChars(ch));
    }

    /********************************************************/
    /* Position Handling                                    */
    /********************************************************/

    /**
     * <p>Retrieve the position.</p>
     *
     * @return The position, can be null.
     */
    public PositionKey getPosition() {
        return null;
    }

    /**
     * <p>Set the position.</p>
     *
     * @param o The position, can be null.
     */
    public void setPosition(PositionKey o) {
        /* do nothing */
    }

}
