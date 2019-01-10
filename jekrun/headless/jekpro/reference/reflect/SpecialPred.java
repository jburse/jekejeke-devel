package jekpro.reference.reflect;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractInformation;
import jekpro.model.inter.*;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.tools.array.AbstractDelegate;
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SpecialPred extends AbstractSpecial {
    private final static int SPECIAL_SYS_ENSURE_SHARED_STATIC = 0;
    private final static int SPECIAL_SYS_CURRENT_PREDICATE = 1;
    private final static int SPECIAL_SYS_CURRENT_PREDICATE_CHK = 2;
    private final static int SPECIAL_SYS_PREDICATE_PROPERTY = 3;
    private final static int SPECIAL_SYS_PREDICATE_PROPERTY_CHK = 4;
    private final static int SPECIAL_SYS_PREDICATE_PROPERTY_IDX = 5;
    /* private final static int SPECIAL_SYS_SET_PREDICATE_PROPERTY = 6; */
    /* private final static int SPECIAL_SYS_RESET_PREDICATE_PROPERTY = 7; */
    private final static int SPECIAL_SYS_PROVABLE_PROPERTY_CHK = 8;
    private final static int SPECIAL_SYS_PROVABLE_PROPERTY_IDX = 9;

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
                Predicate pick = Predicate.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_CRTE);
                SpecialPred.defineStatic(pick, en);
                return en.getNextRaw();
            case SPECIAL_SYS_CURRENT_PREDICATE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                if (!en.unifyTerm(temp[0], ref,
                        SpecialPred.currentPredicates(en),
                        Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_CURRENT_PREDICATE_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToPredicate(temp[0], ref, en);
                if (pick == null)
                    return false;
                return en.getNextRaw();
            case SPECIAL_SYS_PREDICATE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = indicatorToPredicate(temp[0], ref, en);
                if (pick == null)
                    return false;
                boolean multi = SpecialPred.predicateToProperties(pick, en);
                Display d = en.display;
                if (!en.unifyTerm(temp[1], ref, en.skel, d))
                    return false;
                if (multi)
                    BindCount.remTab(d.bind, en);
                return en.getNext();
            case SPECIAL_SYS_PREDICATE_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = indicatorToPredicate(temp[0], ref, en);
                if (pick == null)
                    return false;
                StoreKey prop = StoreKey.propToStoreKey(temp[1], ref, en);
                multi = SpecialPred.predicateToProperty(pick, prop, en);
                d = en.display;
                if (!en.unifyTerm(temp[2], ref, en.skel, d))
                    return false;
                if (multi)
                    BindCount.remTab(d.bind, en);
                return en.getNext();
            case SPECIAL_SYS_PREDICATE_PROPERTY_IDX:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                if (!en.unifyTerm(temp[1], ref,
                        SpecialPred.propertyToPredicates(en.skel, en.display, en),
                        Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_PROVABLE_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = indicatorToProvable(temp[0], ref, en);
                if (pick == null)
                    return false;
                prop = StoreKey.propToStoreKey(temp[1], ref, en);
                multi = SpecialPred.predicateToProperty(pick, prop, en);
                d = en.display;
                if (!en.unifyTerm(temp[2], ref, en.skel, d))
                    return false;
                if (multi)
                    BindCount.remTab(d.bind, en);
                return en.getNext();
            case SPECIAL_SYS_PROVABLE_PROPERTY_IDX:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                if (!en.unifyTerm(temp[1], ref,
                        propertyToProvables(en.skel, en.display, en),
                        Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**************************************************************/
    /* Predicate Enumeration                                      */
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
        Store store = en.store;
        Object res = en.store.foyer.ATOM_NIL;
        while (store != null) {
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int j = 0; j < sources.length; j++) {
                AbstractSource base = sources[j].value;
                Predicate[] preds = base.snapshotRoutine();
                res = consPredicates(preds, res, en);
            }
            store = store.parent;
        }
        return res;
    }

    /**
     * <p>Collect and filter predicate indicators.</p>
     *
     * @param preds The predicates.
     * @param res   The old predicate indicators.
     * @param en    The engine.
     * @return The new predicate indicators.
     * @throws EngineMessage Shit happens.
     */
    private static Object consPredicates(Predicate[] preds, Object res,
                                         Engine en)
            throws EngineMessage {
        for (int i = preds.length - 1; i >= 0; i--) {
            Predicate pick = preds[i];
            if (!CachePredicate.visiblePred(pick, en.store.user))
                continue;
            Object val = SpecialQuali.indicatorToColonSkel(
                    pick.getFun(), pick.getSource().getStore().user,
                    pick.getArity(), en);
            res = new SkelCompound(en.store.foyer.ATOM_CONS, val, res);
        }
        return res;
    }

    /**************************************************************/
    /* Predicate Enumeration                                      */
    /**************************************************************/

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
    /* High-Level Predicate Property Access                       */
    /**************************************************************/

    /**
     * <p>Create a prolog list for the properties of the given predicate.</p>
     * <p>Result is returned in skeleton and display.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param pick The predicate.
     * @param en   The engine.
     * @return The multi flag.
     * @throws EngineMessage Shit happens.
     */
    public static boolean predicateToProperties(Predicate pick, Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        boolean multi = false;
        for (int i = snapshot.length - 1; i >= 0; i--) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            AbstractInformation[] props = branch.listPredProp(pick);
            for (int j = props.length - 1; j >= 0; j--) {
                AbstractInformation prop = props[j];
                Object t = en.skel;
                Display d = en.display;
                Object[] vals = getPropPred(pick, prop, en);
                en.skel = t;
                en.display = d;
                multi = AbstractInformation.consArray(multi, vals, en);
            }
        }
        return multi;
    }

    /**
     * <p>Create a prolog list for the property of the given predicate.</p>
     * <p>Result is returned in skeleton and display.</p>
     *
     * @param pred The predicate.
     * @param prop The property.
     * @param en   The engine.
     * @return The multi flag.
     * @throws EngineMessage Shit happens.
     */
    public static boolean predicateToProperty(Predicate pred, StoreKey prop,
                                              Engine en)
            throws EngineMessage {
        Object[] vals = getPropPred(pred, prop, en);
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        return AbstractInformation.consArray(false, vals, en);
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
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot =
                en.store.foyer.snapshotTrackings();
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

    /**************************************************************/
    /* High-Level Predicate Property Access II                    */
    /**************************************************************/

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
        StoreKey prop = StackElement.callableToStoreKey(t);
        Object[] vals = getPropPred(pred, prop, en);
        vals = AbstractInformation.addValue(vals, AbstractTerm.createMolec(t, d));
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
        StoreKey prop = StackElement.callableToStoreKey(t);
        Object[] vals = getPropPred(pred, prop, en);
        vals = AbstractInformation.removeValue(vals, AbstractTerm.createMolec(t, d));
        setPropPred(prop, pred, vals, en);
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
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot =
                en.store.foyer.snapshotTrackings();
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

    /**************************************************************/
    /* High-Level Predicate Property Access III                   */
    /**************************************************************/

    /**
     * <p>Retrieve the predicates for a property.</p>
     *
     * @param t  The value skeleton.
     * @param d  The value display.
     * @param en The engine.
     */
    private static Object propertyToPredicates(Object t, Display d,
                                               Engine en)
            throws EngineMessage {
        StoreKey prop = StackElement.callableToStoreKey(t);
        Predicate[] vals = idxPropPred(t, d, prop, en);
        Object res = en.store.foyer.ATOM_NIL;
        res = consPredicates(vals, res, en);
        return res;
    }

    /**
     * <p>Retrieve predicates for a property.</p>
     * <p>Throws a domain error for unsupported predicate properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param t    The value skeleton.
     * @param d    The value display.
     * @param prop The property.
     * @param en   The engine.
     * @return The value.
     * @throws EngineMessage Shit happens.
     */
    public static Predicate[] idxPropPred(Object t, Display d, StoreKey prop,
                                          Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot =
                en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            Predicate[] vals = branch.idxPredProp(t, d, prop, en);
            if (vals != null)
                return vals;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                StoreKey.storeKeyToPropSkel(prop.getFun(), prop.getArity())));
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
        AbstractDelegate fun = AbstractDefined.promoteStatic(pick, en.store);
        if ((fun.subflags & AbstractDefined.MASK_DEFI_STAT) != 0)
            return;
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_COERCE,
                EngineMessage.OP_PERMISSION_PROCEDURE,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /**********************************************************/
    /* Moved From Debugger                                    */
    /**********************************************************/

    /**
     * <p>Get predicate by indicator.</p>
     *
     * @param t  The skel of the compound.
     * @param d  The display of the compound.
     * @param en The engine.
     * @return The predicate.
     * @throws EngineMessage Shit happens.
     */
    public static Predicate indicatorToProvable(Object t, Display d, Engine en)
            throws EngineMessage {
        Integer arity = SpecialQuali.colonToIndicator(t, d, en);
        SkelAtom sa = (SkelAtom) en.skel;
        AbstractSource base;
        if (!CacheFunctor.isQuali(sa.fun)) {
            return CachePredicate.getRoutineUser(arity.intValue(), sa.fun, en.store);
        } else {
            String s = CacheFunctor.sepModule(sa.fun);
            base = AbstractSource.getModule(s, en.store);
            if (base == null)
                return null;
            return base.getRoutine(arity.intValue(), sa.fun);
        }
    }

    /**
     * <p>Retrieve the predicates to a property.</p>
     *
     * @param t  The value skeleton.
     * @param d  The value display.
     * @param en The engine.
     */
    private static Object propertyToProvables(Object t, Display d,
                                              Engine en)
            throws EngineMessage {
        StoreKey prop = StackElement.callableToStoreKey(t);
        Predicate[] vals = idxPropPred(t, d, prop, en);
        Object res = en.store.foyer.ATOM_NIL;
        res = consProvables(vals, res, en);
        return res;
    }

    /**
     * <p>Collect predicate indicators.</p>
     *
     * @param preds The predicates.
     * @param res   The old predicate indicators.
     * @param en    The engine.
     * @return The new predicate indicators.
     * @throws EngineMessage Shit happens.
     */
    public static Object consProvables(Predicate[] preds, Object res,
                                       Engine en)
            throws EngineMessage {
        for (int i = preds.length - 1; i >= 0; i--) {
            Predicate pick = preds[i];
            Object val = SpecialQuali.indicatorToColonSkel(
                    pick.getFun(), pick.getSource().getStore().user,
                    pick.getArity(), en);
            res = new SkelCompound(en.store.foyer.ATOM_CONS, val, res);
        }
        return res;
    }

}
