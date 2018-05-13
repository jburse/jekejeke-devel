package jekdev.reference.inspection;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;


/**
 * <p>This module provides built-ins for direct predicate access.</p>
 * <p/>
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 * <p/>
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 * <p/>
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SpecialProvable extends AbstractSpecial {
    private final static int SPECIAL_SYS_CURRENT_PROVABLE_CHK = 1;
    private final static int SPECIAL_SYS_PROVABLE_PROPERTY = 2;

    private final static int SPECIAL_SET_PROVABLE_PROPERTY = 4;
    private final static int SPECIAL_RESET_PROVABLE_PROPERTY = 5;


    /**
     * <p>Create a provable direct access builtin.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialProvable(int i) {
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
            case SPECIAL_SYS_CURRENT_PROVABLE_CHK:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Predicate pick = SpecialLoad.indicatorToProvable(temp[0], ref, en);
                if (pick == null)
                    return false;
                return en.getNextRaw();
            case SPECIAL_SYS_PROVABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialLoad.indicatorToProvable(temp[0], ref, en);
                if (pick == null)
                    return false;
                SpecialPred.predicateToProperties(pick, en);
                if (!en.unifyTerm(temp[1], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_SET_PROVABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialLoad.indicatorToProvable(temp[0], ref, en);
                Predicate.checkExistentProvable(pick, temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialPred.addPredProp(en.skel, en.display, pick, en);
                return en.getNextRaw();
            case SPECIAL_RESET_PROVABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialLoad.indicatorToProvable(temp[0], ref, en);
                Predicate.checkExistentProvable(pick, temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialPred.removePredProp(en.skel, en.display, pick, en);
                return en.getNextRaw();
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

}
