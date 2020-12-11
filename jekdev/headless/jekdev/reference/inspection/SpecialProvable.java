package jekdev.reference.inspection;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Store;
import jekpro.reference.reflect.SpecialPred;
import jekpro.tools.term.SkelCompound;
import matula.util.data.MapEntry;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SpecialProvable extends AbstractSpecial {
    private final static int SPECIAL_SYS_CURRENT_PROVABLE = 0;
    private final static int SPECIAL_SYS_CURRENT_PROVABLE_CHK = 1;
    private final static int SPECIAL_SYS_PROVABLE_PROPERTY = 2;
    private final static int SPECIAL_SET_PROVABLE_PROPERTY = 3;
    private final static int SPECIAL_RESET_PROVABLE_PROPERTY = 4;

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
            case SPECIAL_SYS_CURRENT_PROVABLE:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                if (!en.unifyTerm(SpecialProvable.currentProvables(en), Display.DISPLAY_CONST, temp[0], ref))
                    return false;
                return true;
            case SPECIAL_SYS_CURRENT_PROVABLE_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                Predicate pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_UCHK);
                if (pick == null)
                    return false;
                return true;
            case SPECIAL_SYS_PROVABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_UCHK);
                if (pick == null)
                    return false;
                SpecialPred.predicateToProperties(pick, en);
                Display d = en.display;
                boolean multi = d.getAndReset();
                if (!en.unifyTerm(en.skel, d, temp[1], ref))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
            case SPECIAL_SET_PROVABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_UCHK);
                SpecialProvable.checkExistentProvable(pick, temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialPred.setPredProp(pick, en.skel, en.display, en);
                return true;
            case SPECIAL_RESET_PROVABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_UCHK);
                SpecialProvable.checkExistentProvable(pick, temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialPred.resetPredProp(pick, en.skel, en.display, en);
                return true;
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /**************************************************************/
    /* Provable Enumeration                                       */
    /**************************************************************/

    /**
     * <p>Create a prolog list with the directly accessible predicates.</p>
     *
     * @param en The engine.
     * @return The prolog list of the directly accessible predicates.
     * @throws EngineMessage Shit happens.
     */
    private static Object currentProvables(Engine en)
            throws EngineMessage {
        Store store = en.store;
        Object res = en.store.foyer.ATOM_NIL;
        while (store != null) {
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int j = sources.length - 1; j >= 0; j--) {
                AbstractSource base = sources[j].value;
                Predicate[] preds = base.snapshotRoutine();
                res = SpecialPred.consProvables(preds, res, en);
            }
            store = store.parent;
        }
        return res;
    }

    /**
     * <p>Assure that the predicate is existent.</p>
     *
     * @param pick The predicate.
     * @param t    The skel.
     * @param d    The display.
     * @throws EngineMessage Shit happens.
     */
    public static void checkExistentProvable(Predicate pick, Object t, Display d)
            throws EngineMessage {
        if (pick == null)
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_PROVABLE, t), d);
    }

}
