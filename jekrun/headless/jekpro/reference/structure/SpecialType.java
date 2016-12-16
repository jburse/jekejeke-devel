package jekpro.reference.structure;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Special;
import jekpro.model.molec.*;
import jekpro.model.rope.Goal;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * <p>Provides built-in predicates for type checking.</p>
 *
 * @author Copyright 1985-2014, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.2 (a fast and small prolog interpreter)
 */
public final class SpecialType extends Special {
    private final static int SPECIAL_INTEGER = 0;
    private final static int SPECIAL_FLOAT = 1;
    private final static int SPECIAL_ATOM = 2;
    private final static int SPECIAL_COMPOUND = 3;
    private final static int SPECIAL_REFERENCE = 4;
    private final static int SPECIAL_DECIMAL = 5;
    private final static int SPECIAL_NUMBER = 6;
    private final static int SPECIAL_CALLABLE = 7;
    private final static int SPECIAL_ATOMIC = 8;
    private final static int SPECIAL_FLOAT32 = 9;
    private final static int SPECIAL_FLOAT64 = 10;

    /**
     * <p>Create a tyoe special.</p>
     *
     * @param i The id.
     */
    public SpecialType(int i) {
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
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean findFirst(Goal r, DisplayClause u,
                                   Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case SPECIAL_INTEGER:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof Integer) && !(en.skel instanceof BigInteger))
                    return false;
                return r.getNextRaw(u, en);
            case SPECIAL_FLOAT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof Float) && !(en.skel instanceof Double))
                    return false;
                return r.getNextRaw(u, en);
            case SPECIAL_ATOM:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof SkelAtom))
                    return false;
                return r.getNextRaw(u, en);
            case SPECIAL_COMPOUND:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof SkelCompound))
                    return false;
                return r.getNextRaw(u, en);
            case SPECIAL_REFERENCE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (en.skel instanceof AbstractSkel || en.skel instanceof Number)
                    return false;
                return r.getNextRaw(u, en);
            case SPECIAL_DECIMAL:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof Long) && !(en.skel instanceof BigDecimal))
                    return false;
                return r.getNextRaw(u, en);
            case SPECIAL_NUMBER:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof Number))
                    return false;
                return r.getNextRaw(u, en);
            case SPECIAL_CALLABLE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof SkelAtom) && !(en.skel instanceof SkelCompound))
                    return false;
                return r.getNextRaw(u, en);
            case SPECIAL_ATOMIC:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (en.skel instanceof SkelVar || en.skel instanceof SkelCompound)
                    return false;
                return r.getNextRaw(u, en);
            case SPECIAL_FLOAT32:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof Float))
                    return false;
                return r.getNextRaw(u, en);
            case SPECIAL_FLOAT64:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof Double))
                    return false;
                return r.getNextRaw(u, en);
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }
}
