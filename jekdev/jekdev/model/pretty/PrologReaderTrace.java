package jekdev.model.pretty;

import jekpro.model.pretty.PrologReader;
import jekpro.tools.term.PositionKey;
import jekpro.tools.term.SkelAtom;

import java.io.Reader;

/**
 * <p>This class extends the Prolog reader.</p>
 * <p>It adds position keys to atoms.</p>
 *
 * @author Copyright 2015-2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.0.5 (a fast and small prolog interpreter)
 */
public final class PrologReaderTrace extends PrologReader {

    /**
     * <p>Retrieve atom position.</p>
     *
     * @return The atom position.
     */
    protected final PositionKey getAtomPos() {
        Reader lr = st.getReader();
        return PositionKey.createPos(lr);
    }

    /**
     * <p>Create an atom.</p>
     *
     * @param f The name.
     * @param p The atom position.
     * @return The atom.
     */
    protected final SkelAtom makePos(String f, PositionKey p) {
        if (p == null)
            return super.makePos(f, p);
        SkelAtomTrace sa = new SkelAtomTrace(f, source);
        sa.setPosition(p);
        return sa;
    }

}
