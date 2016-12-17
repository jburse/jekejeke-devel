package jekpro.reference.arithmetic;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Special;
import jekpro.model.molec.*;
import jekpro.model.rope.Goal;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides built-in predicates for the eval theory.</p>
 *
 * @author Copyright 1985-2015, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.8.6 (a fast and small prolog interpreter)
 */
public final class SpecialEval extends Special {
    private final static int SPECIAL_IS = 0;

    /**
     * <p>Create an arithmetic special.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialEval(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param r  The continuation skel.
     * @param u  The continuation display.
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public final boolean findFirst(Goal r, DisplayClause u,
                                   Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case SPECIAL_IS:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.computeExpr(temp[1], ref, r, u);
                if (!en.unifyTerm(temp[0], ref, en.skel, en.display, r, u))
                    return false;
                return r.getNext(u, en);
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

}
