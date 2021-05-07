package jekdev.model.pretty;

import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.SkelAtomAnno;
import jekpro.tools.term.PositionKey;

/**
 * <p>Refinement of the atom class.</p>
 *
 * @author Copyright 2015, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.0.5 (a fast and small prolog interpreter)
 */
public final class SkelAtomAnnoTrace extends SkelAtomAnno {
    private PositionKey pos;

    /**
     * <p>Internal constructor for anno atoms.</p>
     *
     * @param f The name.
     * @param s The scope.
     */
    public SkelAtomAnnoTrace(String f, AbstractSource s) {
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
