package jekpro.reference.arithmetic;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Special;
import jekpro.model.molec.*;
import jekpro.model.rope.Goal;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides the compare evaluables.</p>
 *
 * @author Copyright 1985-2015, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.0 (a fast and small prolog interpreter)
 */
public final class EvaluableCompare extends Special {
    private final static int EVALUABLE_MIN = 0;
    private final static int EVALUABLE_MAX = 1;

    /**
     * <p>Create a compare evaluable.</p>
     *
     * @param i The built-in ID.
     */
    public EvaluableCompare(int i) {
        super(i);
        subflags |= MASK_DELE_ARIT;
    }

    /**
     * <p>Arithmetically evaluate an evaluable.</p>
     * <p>The evaluable is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The result is passed via the skel and display of the engine.</p>
     *
     * @param r  The continuation skel.
     * @param u  The continuation display.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public final void evalEvaluable(Goal r, DisplayClause u,
                                    Engine en)
            throws EngineMessage, EngineException {
        Object[] temp = ((SkelCompound) en.skel).args;
        Display ref = en.display;
        en.computeExpr(temp[0], ref, r, u);
        Number alfa = EngineMessage.castNumber(en.skel, en.display);
        en.computeExpr(temp[1], ref, r, u);
        Number beta = EngineMessage.castNumber(en.skel, en.display);
        switch (id) {
            case EVALUABLE_MIN:
                en.skel = min(alfa, beta);
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_MAX:
                en.skel = max(alfa, beta);
                en.display = Display.DISPLAY_CONST;
                return;
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /********************************************************************/
    /* Compare:                                                         */
    /*      (min)/2: min()                                              */
    /*      (max)/2: max()                                              */
    /********************************************************************/

    /**
     * <p>Min the two number.</p>
     *
     * @param a The first number.
     * @param b The second number.
     * @return The minimum of the two numbers.
     * @throws EngineMessage Shit happens.
     */
    private static Number min(Number a, Number b) throws EngineMessage {
        if (SpecialCompare.computeCmp(a, b) < 0) {
            return a;
        } else {
            return b;
        }
    }

    /**
     * <p>Max the two number.</p>
     *
     * @param a The first number.
     * @param b The second number.
     * @return The minimum of the two numbers.
     * @throws EngineMessage Shit happens.
     */
    private static Number max(Number a, Number b) throws EngineMessage {
        if (SpecialCompare.computeCmp(a, b) > 0) {
            return a;
        } else {
            return b;
        }
    }

}
