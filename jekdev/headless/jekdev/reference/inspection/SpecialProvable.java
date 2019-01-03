package jekdev.reference.inspection;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.builtin.SpecialBody;
import jekpro.model.inter.*;
import jekpro.model.molec.BindCount;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.reflect.SpecialPred;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.comp.sharik.AbstractBundle;
import matula.comp.sharik.AbstractTracking;
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
    /* private final static int SPECIAL_SYS_PROVABLE_PROPERTY_CHK = 3; */
    /* private final static int SPECIAL_SYS_PROVABLE_PROPERTY_IDX = 4; */
    private final static int SPECIAL_SET_PROVABLE_PROPERTY = 5;
    private final static int SPECIAL_RESET_PROVABLE_PROPERTY = 6;
    private final static int SPECIAL_SYS_CALLABLE_PROPERTY = 7;
    private final static int SPECIAL_SYS_CALLABLE_PROPERTY_CHK = 8;
    private final static int SPECIAL_SET_CALLABLE_PROPERTY = 9;
    private final static int SPECIAL_RESET_CALLABLE_PROPERTY = 10;

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
                BindCount[] ref = en.display;
                if (!en.unifyTerm(temp[0], ref,
                        SpecialProvable.currentProvables(en), BindCount.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_CURRENT_PROVABLE_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                Predicate pick = SpecialPred.indicatorToProvable(temp[0], ref, en);
                if (pick == null)
                    return false;
                return en.getNextRaw();
            case SPECIAL_SYS_PROVABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToProvable(temp[0], ref, en);
                if (pick == null)
                    return false;
                boolean multi = SpecialPred.predicateToProperties(pick, en);
                BindCount[] d = en.display;
                if (!en.unifyTerm(temp[1], ref, en.skel, d))
                    return false;
                if (multi)
                    BindCount.remTab(d, en);
                return en.getNext();
            case SPECIAL_SET_PROVABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToProvable(temp[0], ref, en);
                Predicate.checkExistentProvable(pick, temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialPred.addPredProp(en.skel, en.display, pick, en);
                return en.getNextRaw();
            case SPECIAL_RESET_PROVABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToProvable(temp[0], ref, en);
                Predicate.checkExistentProvable(pick, temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialPred.removePredProp(en.skel, en.display, pick, en);
                return en.getNextRaw();
            case SPECIAL_SYS_CALLABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                multi = callableToProperties(en.skel, en.display, en);
                d = en.display;
                if (!en.unifyTerm(temp[1], ref, en.skel, d))
                    return false;
                if (multi)
                    BindCount.remTab(d, en);
                return en.getNext();
            case SPECIAL_SYS_CALLABLE_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                StoreKey prop = StoreKey.propToStoreKey(temp[1], ref, en);
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                multi = callableToProperty(prop, en.skel, en.display, en);
                d = en.display;
                if (!en.unifyTerm(temp[2], ref, en.skel, d))
                    return false;
                if (multi)
                    BindCount.remTab(d, en);
                return en.getNext();
            case SPECIAL_SET_CALLABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[2];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                Object t = en.skel;
                d = en.display;
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                addAtomProp(en.skel, en.display, t, d, en);
                if (!en.unifyTerm(temp[0], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_RESET_CALLABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[2];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                t = en.skel;
                d = en.display;
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                removeAtomProp(en.skel, en.display, t, d, en);
                if (!en.unifyTerm(temp[0], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /***********************************************************************/
    /* High-Level Atom Property Access                                     */
    /***********************************************************************/

    /**
     * <p>Create a prolog list for the properties of the given atom.</p>
     * <p>Result is returned in skeleton and display.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param t2 The callable skeleton.
     *           @param d2 The callable display.
     * @param en The engine.
     * @return The multi flag.
     * @throws EngineMessage Shit happens.
     */
    private static boolean callableToProperties(Object t2, BindCount[] d2, Engine en)
            throws EngineMessage {
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = BindCount.DISPLAY_CONST;
        boolean multi = false;
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = snapshot.length - 1; i >= 0; i--) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractBranch branch = (AbstractBranch) entry.key;
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            StoreKey[] props = branch.listAtomProp();
            for (int j = props.length - 1; j >= 0; j--) {
                StoreKey prop = props[j];
                Object t = en.skel;
                BindCount[] d = en.display;
                Object[] vals = getPropCallable(prop, t2, d2, en);
                en.skel = t;
                en.display = d;
                multi = AbstractProperty.consArray(multi, vals, en);
            }
        }
        return multi;
    }

    /**
     * <p>Create a prolog list for the property of the given atom.</p>
     * <p>Result is returned in skeleton and display.</p>
     *
     * @param prop The property.
     * @param t2 The callable skeleton.
     *           @param d2 The callable display.
     * @param en   The engine.
     * @return The multi flag.
     * @throws EngineMessage Shit happens.
     */
    private static boolean callableToProperty(StoreKey prop, Object t2, BindCount[] d2,
                                          Engine en)
            throws EngineMessage {
        Object[] vals = getPropCallable(prop, t2, d2, en);
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = BindCount.DISPLAY_CONST;
        return AbstractProperty.consArray(false, vals, en);
    }

    /**
     * <p>Set a predicate property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param t  The value skeleton.
     * @param d  The value display.
     * @param t2 The callable skeleton.
     * @param d2 The callable display.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void addAtomProp(Object t, BindCount[] d,
                                      Object t2, BindCount[] d2, Engine en)
            throws EngineMessage {
        StoreKey prop = StackElement.callableToStoreKey(t);
        Object[] vals = getPropCallable(prop, t2, d2, en);
        vals = AbstractProperty.addValue(vals, AbstractTerm.createMolec(t, d));
        setPropCallable(prop, t2, d2, vals, en);
    }

    /**
     * <p>Reset a predicate property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param t  The value skeleton.
     * @param d  The value display.
     * @param t2 The callable skeleton.
     * @param d2 The callable display.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void removeAtomProp(Object t, BindCount[] d,
                                         Object t2, BindCount[] d2, Engine en)
            throws EngineMessage {
        StoreKey prop = StackElement.callableToStoreKey(t);
        Object[] vals = getPropCallable(prop, t2, d2, en);
        vals = AbstractProperty.removeValue(vals, AbstractTerm.createMolec(t, d));
        setPropCallable(prop, t2, d2, vals, en);
    }

    /***********************************************************************/
    /* Low-Level Atom Property Access                                      */
    /***********************************************************************/

    /**
     * <p>Retrieve an atom property.</p>
     * <p>Throws a domain error for undefined atom properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param prop The property.
     * @param t2 The callable skeleton.
     *           @param d2 The callable display.
     * @param en   The engine.
     * @return The value.
     * @throws EngineMessage Shit happens.
     */
    public static Object[] getPropCallable(StoreKey prop, Object t2, BindCount[] d2,
                                       Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractBranch branch = (AbstractBranch) entry.key;
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            Object[] vals = branch.getCallableProp(prop, t2, d2, en);
            if (vals != null)
                return vals;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                StoreKey.storeKeyToPropSkel(prop.getFun(), prop.getArity())));
    }

    /**
     * <p>Set an atom property.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param prop The property.
     * @param t2 The callable skeleton.
     *           @param d2 The callable display.
     * @param vals The values.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void setPropCallable(StoreKey prop, Object t2, BindCount[] d2,
                                        Object[] vals, Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractBranch branch = (AbstractBranch) entry.key;
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            if (branch.setCallableProp(prop, t2, d2, vals, en))
                return;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                StoreKey.storeKeyToPropSkel(prop.getFun(), prop.getArity())));
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

}
