package jekdev.model.pretty;

import jekpro.model.rope.Operator;
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
final class OperatorTrace extends Operator {
    private PositionKey pos;

    /**
     * <p>Create an operator by key.</p>
     *
     * @param t The type.
     * @param k The key.
     */
    public OperatorTrace(int t, String k) {
        super(t, k);
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
