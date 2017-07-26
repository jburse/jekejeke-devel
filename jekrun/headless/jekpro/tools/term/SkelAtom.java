package jekpro.tools.term;

import jekpro.model.pretty.AbstractSource;

import java.text.Collator;
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public class SkelAtom extends AbstractSkel implements Comparable<SkelAtom> {
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
    /* Filler Handling                                      */
    /********************************************************/

    /**
     * <p>Retrieve the hint.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @return The hint.
     */
    public int getHint() {
        return 0;
    }

    /**
     * <p>Set the hint.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param h The hint.
     */
    public void setHint(int h) {
        /* do nothing */
    }

    /**
     * <p>Retrieve the fillers of the atom.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @return The fillers.
     */
    public String[][] getFillers() {
        return null;
    }

    /**
     * <p>Set the fillers.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param f The fillers.
     */
    public void setFillers(String[][] f) {
        /* do nothing */
    }

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
