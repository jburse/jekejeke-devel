package jekdev.model.pretty;

import jekpro.model.rope.Resource;
import jekpro.tools.term.PositionKey;

/**
 * <p>Refinement of the operator class. The class has the following
 * additional fields:</p>
 * <ul>
 * <li><b>origin:</b> The source file of the clause.</li>
 * <li><b>lineno:</b> The line number of the clause.</li>
 * </ul>
 *
 * @author Copyright 2015, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.0.5 (a fast and small prolog interpreter)
 */
final class ResourceTrace extends Resource {
    private PositionKey pos;

    /**
     * <p>Create a reesource entry.</p>
     *
     * @param key The source key.
     */
    public ResourceTrace(String key) {
        super(key);
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
