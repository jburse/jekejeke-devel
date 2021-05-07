package jekdev.model.pretty;

import jekpro.model.pretty.AbstractSource;
import jekpro.tools.term.PositionKey;
import jekpro.tools.term.SkelAtom;

/**
 * <p>Refinement of the atom class. The class has the following
 * additional fields:</p>
 * <ul>
 * <li><b>origin:</b> The source file of the clause.</li>
 * <li><b>lineno:</b> The line number of the clause.</li>
 * </ul>
 *
 * @author Copyright 2015, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.0.5 (a fast and small prolog interpreter)
 */
public final class SkelAtomTrace extends SkelAtom {
    private PositionKey pos;

    /**
     * <p>Create an atom from a string.</p>
     *
     * @param f The string, non null.
     * @param s The source.
     */
    public SkelAtomTrace(String f, AbstractSource s) {
        super(f, s);
    }

    /**
     * <p>Retrieve the position.</p>
     *
     * @return The position, can be null.
     */
    public PositionKey getPosition() {
        return pos;
    }

    /**
     * <p>Set the position.</p>
     *
     * @param p The position, can be null.
     */
    public void setPosition(PositionKey p) {
        pos = p;
    }

}
