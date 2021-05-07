package jekdev.model.pretty;

import jekpro.model.pretty.PrologReaderAnno;
import jekpro.tools.term.PositionKey;
import jekpro.tools.term.SkelAtom;

import java.io.Reader;

/**
 * <p>This class extends the Prolog reader anno.</p>
 * <p>It adds position keys to atoms.</p>
 * <p>It adds hints and fillers to atoms.</p>
 *
 * @author Copyright 2015-2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.0.5 (a fast and small prolog interpreter)
 */
public final class PrologReaderAnnoTrace extends PrologReaderAnno {

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

    /**
     * <p>Factory for annotated atoms.</p>
     * <p>Can be overridden by a sub class.</p>
     *
     * @param sa The atom skeleton.
     * @param h  The hint.
     * @return The result.
     */
    protected final SkelAtom makeHint(SkelAtom sa, int h) {
        if (sa.getPosition() == null)
            return super.makeHint(sa, h);
        if (h == 0)
            return sa;
        SkelAtomAnnoTrace sa2;
        if (!(sa instanceof SkelAtomAnnoTrace)) {
            sa2 = new SkelAtomAnnoTrace(sa.fun, sa.scope);
            sa2.setPosition(sa.getPosition());
        } else {
            sa2 = (SkelAtomAnnoTrace) sa;
            h |= sa2.getHint();
        }
        sa2.setHint(h);
        return sa2;
    }

    /**
     * <p>Factory for annotated atoms.</p>
     * <p>Will preserve the hint of the atom.</p>
     * <p>Can be overridden by a sub class.</p>
     *
     * @param sa The atom skeleton.
     * @param f  The fillers.
     * @return The result.
     */
    protected final SkelAtom makeFillers(SkelAtom sa, String[][] f) {
        if (sa.getPosition() == null)
            return super.makeFillers(sa, f);
        if (f == null)
            return sa;
        SkelAtomAnnoTrace sa2;
        if (!(sa instanceof SkelAtomAnnoTrace)) {
            sa2 = new SkelAtomAnnoTrace(sa.fun, sa.scope);
            sa2.setPosition(sa.getPosition());
        } else {
            sa2 = (SkelAtomAnnoTrace) sa;
            String[][] f2 = sa2.getFillers();
            concatFillers(f, f2);
        }
        sa2.setFillers(f);
        return sa2;
    }

}
