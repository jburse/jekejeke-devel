package jekpro.reference.reflect;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.*;
import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.AbstractStore;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.Clause;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.tools.proxy.BranchAPI;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.comp.sharik.AbstractBundle;
import matula.comp.sharik.AbstractTracking;
import matula.util.data.MapEntry;

/**
 * <p>Provides built-in predicates for the module pred.</p>
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
public final class SpecialPred extends AbstractSpecial {
    private final static int SPECIAL_SYS_ENSURE_SHARED_STATIC = 0;
    private final static int SPECIAL_SYS_ATOM_PROPERTY = 1;
    private final static int SPECIAL_SYS_ATOM_PROPERTY_CHK = 2;
    private final static int SPECIAL_SET_ATOM_PROPERTY = 3;
    private final static int SPECIAL_RESET_ATOM_PROPERTY = 4;
    private final static int SPECIAL_SYS_CURRENT_PREDICATE = 5;
    private final static int SPECIAL_SYS_CURRENT_PREDICATE_CHK = 6;
    private final static int SPECIAL_SYS_PREDICATE_PROPERTY = 7;
    private final static int SPECIAL_SYS_PREDICATE_PROPERTY_CHK = 8;
    private final static int SPECIAL_SYS_CURRENT_MODULE = 9;
    private final static int SPECIAL_SYS_CURRENT_MODULE_CHK = 10;

    /**
     * <p>Create a pred special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialPred(int i) {
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
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineException, EngineMessage {
        switch (id) {
            case SPECIAL_SYS_ENSURE_SHARED_STATIC:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Predicate pick = Predicate.indicatorToPredicateDefined(temp[0], ref, en, true);
                SpecialPred.defineStatic(pick, en);
                return en.getNextRaw();
            case SPECIAL_SYS_ATOM_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                SkelAtom sa = Frame.callableToName(en.skel);
                atomToProperties(sa, en);
                if (!en.unifyTerm(temp[1], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_ATOM_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                sa = Frame.callableToName(en.skel);
                StoreKey prop = StoreKey.propToStoreKey(temp[1], ref, en);
                atomToProperty(prop, sa, en);
                if (!en.unifyTerm(temp[2], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_SET_ATOM_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                SpecialQuali.colonToCallable(temp[0], ref, en);
                EngineMessage.checkInstantiated(en.skel);
                sa = EngineMessage.castStringWrapped(en.skel, en.display);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkCallable(en.skel, en.display);
                sa = SpecialPred.addAtomProp(en.skel, en.display, sa, en);
                if (!en.unifyTerm(temp[2], ref, sa, Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_RESET_ATOM_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                SpecialQuali.colonToCallable(temp[0], ref, en);
                EngineMessage.checkInstantiated(en.skel);
                sa = EngineMessage.castStringWrapped(en.skel, en.display);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkCallable(en.skel, en.display);
                sa = SpecialPred.removeAtomProp(en.skel, en.display, sa, en);
                if (!en.unifyTerm(temp[2], ref, sa, Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_CURRENT_PREDICATE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                if (!en.unifyTerm(temp[0], ref,
                        currentPredicates(en), Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_CURRENT_PREDICATE_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = indicatorToPredicate(temp[0], ref, en);
                if (pick == null)
                    return false;
                return en.getNextRaw();
            case SPECIAL_SYS_PREDICATE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = indicatorToPredicate(temp[0], ref, en);
                if (pick == null)
                    return false;
                predicateToProperties(pick, en);
                if (!en.unifyTerm(temp[1], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_PREDICATE_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = indicatorToPredicate(temp[0], ref, en);
                if (pick == null)
                    return false;
                prop = StoreKey.propToStoreKey(temp[1], ref, en);
                predicateToProperty(pick, prop, en);
                if (!en.unifyTerm(temp[2], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_CURRENT_MODULE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                if (!en.unifyTerm(temp[0], ref,
                        currentModules(en), Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_CURRENT_MODULE_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                AbstractSource base = nameToModule(temp[0], ref, en);
                if (base == null)
                    return false;
                return en.getNextRaw();
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**************************************************************/
    /* Predicate Enumeration & Lookup                             */
    /**************************************************************/

    /**
     * <p>Create a prolog list with the visible predicates.</p>
     *
     * @param en The engine.
     * @return The prolog list of the visible predicates.
     * @throws EngineMessage Shit happens.
     */
    private static Object currentPredicates(Engine en)
            throws EngineMessage {
        AbstractStore store = en.store;
        Object res = en.store.foyer.ATOM_NIL;
        while (store != null) {
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int j = 0; j < sources.length; j++) {
                AbstractSource base = sources[j].value;
                Predicate[] preds = base.snapshotRoutine();
                for (int i = preds.length - 1; i >= 0; i--) {
                    Predicate pick = preds[i];
                    if (!pick.visiblePred(en.store.user))
                        continue;
                    SkelAtom sa = new SkelAtom(pick.getFun(), en.store.user);
                    Object val = SpecialQuali.indicatorToColonSkel(sa, pick.getArity(), en);
                    res = new SkelCompound(en.store.foyer.ATOM_CONS, val, res);
                }
            }
            store = store.parent;
        }
        return res;
    }

    /**
     * <p>Get a predicate by indicator.</p>
     *
     * @param t  The indicator skel.
     * @param d  The indicator display.
     * @param en The engine.
     * @return The predicate, or null.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static Predicate indicatorToPredicate(Object t, Display d,
                                                 Engine en)
            throws EngineMessage, EngineException {
        Integer arity = SpecialQuali.colonToIndicator(t, d, en);
        SkelAtom sa = (SkelAtom) en.skel;
        CachePredicate cp = CachePredicate.getPredicate(sa, arity.intValue(), en);
        en.skel = sa;
        if (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0)
            return null;
        return cp.pick;
    }

    /**************************************************************/
    /* Module Enumeration & Lookup                                */
    /**************************************************************/

    /**
     * <p>Create a prolog list with the modules.</p>
     *
     * @param en The engine.
     * @return The prolog list of the modules.
     * @throws EngineMessage Shit happens.
     */
    private static Object currentModules(Engine en)
            throws EngineMessage {
        AbstractStore store = en.store;
        Object res = en.store.foyer.ATOM_NIL;
        while (store != null) {
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int j = 0; j < sources.length; j++) {
                AbstractSource base = sources[j].value;
                String fullname = base.getFullName();
                if (fullname == null)
                    continue;
                Object val = Clause.moduleToSlashSkel(fullname, en.store.user, en);
                res = new SkelCompound(en.store.foyer.ATOM_CONS, val, res);
            }
            store = store.parent;
        }
        return res;
    }

    /**
     * <p>Get module by name.</p>
     *
     * @param t The name skel.
     * @param d The name display.
     * @param en The engine.
     * @return The module.
     * @throws EngineMessage Shit happens.
     */
    public static AbstractSource nameToModule(Object t, Display d,
                                              Engine en)
            throws EngineMessage {
        Object obj = SpecialQuali.slashToClass(t, d, false, true, en);
        String fun;
        /* reference */
        if (!(obj instanceof AbstractSkel) &&
                !(obj instanceof Number)) {
            fun = BranchAPI.classOrProxyName(obj);
            if (fun == null)
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_CLASS, t), d);
            /* atom */
        } else {
            fun = ((SkelAtom) obj).fun;
        }
        return AbstractSource.getModule(fun, en.store);
    }

    /**************************************************************/
    /* High-Level Predicate Property Access                       */
    /**************************************************************/

    /**
     * <p>Create a prolog list for the properties of the given predicate.</p>
     * <p>Result is returned in skeleton and display.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void predicateToProperties(Predicate pick, Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        for (int i = snapshot.length - 1; i >= 0; i--) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            AbstractProperty[] props = branch.listPredProp(pick);
            for (int j = props.length - 1; j >= 0; j--) {
                AbstractProperty prop = props[j];
                Object t = en.skel;
                Display d = en.display;
                Object[] vals = getPropPred(pick, prop, en);
                en.skel = t;
                en.display = d;
                AbstractProperty.consArray(vals, en);
            }
        }
    }

    /**
     * <p>Create a prolog list for the property of the given predicate.</p>
     * <p>Result is returned in skeleton and display.</p>
     *
     * @param pred The predicate.
     * @param prop The property.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void predicateToProperty(Predicate pred, StoreKey prop,
                                           Engine en)
            throws EngineMessage {
        Object[] vals = getPropPred(pred, prop, en);
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        AbstractProperty.consArray(vals, en);
    }

    /**
     * <p>Set a predicate property.</p>
     * <p>Throws a domain error for undefined predicate properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param prop The property.
     * @param pred The predicate.
     * @param vals The values, non null.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void setPropPred(StoreKey prop, Predicate pred,
                                    Object[] vals, Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            if (branch.setPredProp(prop, pred, vals, en))
                return;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                StoreKey.storeKeyToPropSkel(prop.getFun(), prop.getArity())));
    }

    /**
     * <p>Retrieve a predicate property.</p>
     * <p>Throws a domain error for undefined predicate properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param pred The predicate.
     * @param prop The property.
     * @param en   The engine.
     * @return The value.
     * @throws EngineMessage Shit happens.
     */
    public static Object[] getPropPred(Predicate pred, StoreKey prop,
                                       Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            Object[] vals = branch.getPredProp(pred, prop, en);
            if (vals != null)
                return vals;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                StoreKey.storeKeyToPropSkel(prop.getFun(), prop.getArity())));
    }

    /**
     * <p>Set a predicate property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param t    The value skeleton.
     * @param d    The value display.
     * @param pred The predicate.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void addPredProp(Object t, Display d, Predicate pred,
                                   Engine en)
            throws EngineMessage {
        StoreKey prop = Frame.callableToStoreKey(t);
        Object[] vals = getPropPred(pred, prop, en);
        vals = AbstractProperty.addValue(vals, AbstractTerm.createMolec(t, d));
        setPropPred(prop, pred, vals, en);
    }

    /**
     * <p>Reset a predicate property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param t    The value skeleton.
     * @param d    The value display.
     * @param pred The predicate.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void removePredProp(Object t, Display d, Predicate pred,
                                      Engine en)
            throws EngineMessage {
        StoreKey prop = Frame.callableToStoreKey(t);
        Object[] vals = getPropPred(pred, prop, en);
        vals = AbstractProperty.removeValue(vals, AbstractTerm.createMolec(t, d));
        setPropPred(prop, pred, vals, en);
    }

    /****************************************************************************/
    /* High-Level Atom Property Access                                          */
    /****************************************************************************/

    /**
     * <p>Create a prolog list for the properties of the given atom.</p>
     * <p>Result is returned in skeleton and display.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param sa The atom, or null.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void atomToProperties(SkelAtom sa, Engine en)
            throws EngineMessage {
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
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
                Display d = en.display;
                Object[] vals = getPropAtom(prop, sa, en);
                en.skel = t;
                en.display = d;
                AbstractProperty.consArray(vals, en);
            }
        }
    }

    /**
     * <p>Create a prolog list for the property of the given atom.</p>
     * <p>Result is returned in skeleton and display.</p>
     *
     * @param prop The property.
     * @param sa   The atom, or null.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void atomToProperty(StoreKey prop, SkelAtom sa,
                                       Engine en)
            throws EngineMessage {
        Object[] vals = getPropAtom(prop, sa, en);
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        AbstractProperty.consArray(vals, en);
    }

    /**
     * <p>Retrieve an atom property.</p>
     * <p>Throws a domain error for undefined atom properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param prop The property.
     * @param sa   The atom, or null.
     * @param en   The engine.
     * @return The value.
     * @throws EngineMessage Shit happens.
     */
    private static Object[] getPropAtom(StoreKey prop, SkelAtom sa,
                                        Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractBranch branch = (AbstractBranch) entry.key;
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            Object[] vals = branch.getAtomProp(prop, sa, en);
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
     * @param skel The atom.
     * @param vals The values.
     * @param en   The engine.
     * @return The new atom.
     * @throws EngineMessage Shit happens.
     */
    private static SkelAtom setPropAtom(StoreKey prop, SkelAtom skel,
                                        Object[] vals, Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractBranch branch = (AbstractBranch) entry.key;
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            if (branch.setAtomProp(prop, skel, vals, en))
                return (SkelAtom) en.skel;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                StoreKey.storeKeyToPropSkel(prop.getFun(), prop.getArity())));
    }

    /**
     * <p>Set a predicate property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param t  The value skeleton.
     * @param d  The value display.
     * @param sa The atom.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    private static SkelAtom addAtomProp(Object t, Display d,
                                        SkelAtom sa, Engine en)
            throws EngineMessage {
        StoreKey prop = Frame.callableToStoreKey(t);
        Object[] vals = getPropAtom(prop, sa, en);
        vals = AbstractProperty.addValue(vals, AbstractTerm.createMolec(t, d));
        return setPropAtom(prop, sa, vals, en);
    }

    /**
     * <p>Reset a predicate property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param t  The value skeleton.
     * @param d  The value display.
     * @param sa The atom.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    private static SkelAtom removeAtomProp(Object t, Display d,
                                           SkelAtom sa, Engine en)
            throws EngineMessage {
        StoreKey prop = Frame.callableToStoreKey(t);
        Object[] vals = getPropAtom(prop, sa, en);
        vals = AbstractProperty.removeValue(vals, AbstractTerm.createMolec(t, d));
        return setPropAtom(prop, sa, vals, en);
    }

    /*************************************************************/
    /* Predicate Promotion                                       */
    /*************************************************************/

    /**
     * <p>Define predicate as static.</p>
     *
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void defineStatic(Predicate pick, Engine en)
            throws EngineMessage {
        Predicate.checkUnsealed(pick, en);
        AbstractDefined.promoteStatic(pick, en.store);
        if ((pick.del.subflags & AbstractDefined.MASK_DEFI_STAT) != 0)
            return;
        SkelAtom sa = new SkelAtom(pick.getFun(), en.store.user);
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_COERCE,
                EngineMessage.OP_PERMISSION_PROCEDURE,
                SpecialQuali.indicatorToColonSkel(sa, pick.getArity(), en)));
    }

}
