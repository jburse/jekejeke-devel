package jekmin.reference.experiment;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Supervisor;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.wire.AbstractLivestock;

/**
 * <p>Provides built-in predicates for term domains.</p>
 *
 * @author Copyright 2014-2016, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.0.3 (a fast and small prolog interpreter)
 */
public final class SpecialUndo extends AbstractSpecial {
    private final static int SPECIAL_SYS_UNBIND = 0;
    private final static int SPECIAL_SYS_FREEZE_VAR = 1;
    private final static int SPECIAL_SYS_MELT_VAR = 2;
    private final static int SPECIAL_SYS_BOUND_VAR = 3;
    private final static int SPECIAL_SYS_FREEZER = 4;

    private final static String OP_DOMAIN_FREEZER = "freezer";


    /**
     * <p>Create a term special.</p>
     *
     * @param i The id.
     */
    public SpecialUndo(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case SPECIAL_SYS_UNBIND:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                boolean mask = (en.visor.flags & AbstractLivestock.MASK_LIVESTOCK_NOSG) == 0;
                boolean verify = (en.visor.flags & Supervisor.MASK_VISOR_NOCNT) == 0;
                BindUniv bu = new UndoGoal(mask, verify);
                bu.bindUniv(en.skel, en.display, en);
                return true;
            case SPECIAL_SYS_FREEZE_VAR:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof SkelVar))
                    throw new EngineMessage(EngineMessage.typeError(
                            SpecialAttr.OP_TYPE_VAR, en.skel), en.display);
                RefVar rv = new RefVar((SkelVar) en.skel, en.display);
                bu = new BindUniv();
                bu.bindUniv(en.skel, en.display, en);
                if (!en.unify(rv, Display.DISPLAY_CONST, temp[1], ref))
                    return false;
                return true;
            case SPECIAL_SYS_MELT_VAR:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                rv = SpecialUndo.derefAndCastVar(temp[0], ref);
                if (!en.unify(rv.skel, rv.display, temp[1], ref))
                    return false;
                return true;
            case SPECIAL_SYS_BOUND_VAR:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                rv = SpecialUndo.derefAndCastVar(temp[0], ref);
                if (rv.display.bind[rv.skel.id].display == null)
                    return false;
                return true;
            case SPECIAL_SYS_FREEZER:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof RefVar))
                    return false;
                return true;
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /****************************************************************/
    /* Variable Freezer Wrapping                                    */
    /****************************************************************/

    /**
     * <p>Cast a freezer.</p>
     *
     * @param m The term skeleton.
     * @param d The term display.
     * @return The variable freezer.
     * @throws EngineMessage Shit happens.
     */
    private static RefVar derefAndCastVar(Object m, Display d)
            throws EngineMessage {
        m = SpecialUniv.derefAndCastRef(m, d);
        if (m instanceof RefVar) {
            return (RefVar) m;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    OP_DOMAIN_FREEZER, m));
        }
    }

}
