package jekpro.reference.reflect;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.*;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.runtime.EvaluableLogic;
import jekpro.reference.runtime.SpecialDynamic;
import jekpro.reference.runtime.SpecialLogic;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.comp.sharik.AbstractTracking;
import matula.util.config.AbstractBundle;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;

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
    private final static int SPECIAL_SYS_PROVABLE_PROPERTY_IDX = 6;
    private final static int SPECIAL_SYS_PROVABLE_PROPERTY_CHK = 7;

    /**
     * <p>Create a pred special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialPred(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a term in a list of goals for the first time.</p>
     * <p>The term is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineException Validation Error.
     * @throws EngineMessage   Validation Error.
     */
    public final boolean moniFirst(Engine en)
            throws EngineException, EngineMessage {
        switch (id) {
            case SPECIAL_SYS_ENSURE_SHARED_STATIC:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Predicate pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_DEFI);
                SpecialPred.defineStatic(pick, en);
                return true;
            case SPECIAL_SYS_CURRENT_PREDICATE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                if (!en.unifyTerm(temp[0], ref,
                        SpecialPred.currentPredicates(en),
                        Display.DISPLAY_CONST))
                    return false;
                return true;
            case SPECIAL_SYS_CURRENT_PREDICATE_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToPredicate(temp[0], ref, en);
                if (pick == null)
                    return false;
                return true;
            case SPECIAL_SYS_PREDICATE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToPredicate(temp[0], ref, en);
                if (pick == null)
                    return false;
                SpecialPred.predicateToProperties(pick, en);
                Display d = en.display;
                boolean multi = d.getAndReset();
                if (!en.unifyTerm(temp[1], ref, en.skel, d))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
            case SPECIAL_SYS_PREDICATE_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToPredicate(temp[0], ref, en);
                if (pick == null)
                    return false;
                StoreKey prop = StoreKey.propToStoreKey(temp[1], ref, en);
                SpecialPred.predicateToProperty(pick, prop, en);
                d = en.display;
                multi = d.getAndReset();
                if (!en.unifyTerm(temp[2], ref, en.skel, d))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
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
                return true;
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
                return true;
            case SPECIAL_SYS_PROVABLE_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_UCHK);
                if (pick == null)
                    return false;
                prop = StoreKey.propToStoreKey(temp[1], ref, en);
                SpecialPred.predicateToProperty(pick, prop, en);
                d = en.display;
                multi = d.getAndReset();
                if (!en.unifyTerm(temp[2], ref, en.skel, d))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
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
     * @throws EngineMessage Validation Error.
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
     * @throws EngineMessage Validation Error.
     */
    private static Object consPredicates(Predicate[] preds, Object res,
                                         Engine en)
            throws EngineMessage {
        for (int i = preds.length - 1; i >= 0; i--) {
            Predicate pick = preds[i];
            if (!CachePredicate.visiblePred(pick, en.store.user))
                continue;
            Object val = indicatorToColonSkel(
                    pick.getFun(), pick.getSource().getStore().user,
                    pick.getArity(), en);
            res = new SkelCompound(en.store.foyer.ATOM_CONS, val, res);
        }
        return res;
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
     * @throws EngineMessage   Validation Error.
     * @throws EngineException Validation Error.
     */
    public static void predicateToProperties(Predicate pick, Engine en)
            throws EngineMessage, EngineException {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        for (int i = snapshot.length - 1; i >= 0; i--) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            ListArray<MapHashLink<StoreKey, AbstractProperty<Predicate>>> props = branch.getPredProps();
            for (int j = 0; j < props.size(); j++)
                predicatePropToProperties(pick, props.get(j), en);
        }
    }

    /**
     * <p>Create a prolog list of the properties of the given predicate and properties.</p>
     *
     * @param pick  The predicate.
     * @param props The properties.
     * @param en    The engine.
     * @throws EngineMessage   Validation Error.
     * @throws EngineException Validation Error.
     */
    private static void predicatePropToProperties(Predicate pick,
                                                  MapHashLink<StoreKey, AbstractProperty<Predicate>> props,
                                                  Engine en)
            throws EngineMessage, EngineException {
        for (MapEntry<StoreKey, AbstractProperty<Predicate>> entry2 =
             (props != null ? props.getLastEntry() : null);
             entry2 != null; entry2 = props.predecessor(entry2)) {
            AbstractProperty<Predicate> prop = entry2.value;
            Object t = en.skel;
            Display d = en.display;
            Object[] vals = prop.getObjProps(pick, en);
            en.skel = t;
            en.display = d;
            AbstractProperty.consArray(vals, en);
        }
    }

    /**
     * <p>Create a prolog list for the property of the given predicate.</p>
     * <p>Result is returned in skeleton and display.</p>
     *
     * @param pick The predicate.
     * @param sk   The property.
     * @param en   The engine.
     * @throws EngineMessage Validation Error.
     */
    public static void predicateToProperty(Predicate pick, StoreKey sk,
                                           Engine en)
            throws EngineMessage, EngineException {
        AbstractProperty<Predicate> prop = SpecialPred.findPredProperty(sk, en);
        Object[] vals = prop.getObjProps(pick, en);
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        AbstractProperty.consArray(vals, en);
    }

    /**************************************************************/
    /* High-Level Predicate Property Access II                    */
    /**************************************************************/

    /**
     * <p>Set a predicate property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param pick The predicate.
     * @param m    The value skeleton.
     * @param d    The value display.
     * @param en   The engine.
     * @throws EngineMessage Validation Error.
     */
    public static void setPredProp(Predicate pick, Object m, Display d,
                                   Engine en)
            throws EngineMessage {
        StoreKey sk = StackElement.callableToStoreKey(m);
        AbstractProperty<Predicate> prop = SpecialPred.findPredProperty(sk, en);
        if (!prop.setObjProp(pick, m, d, en))
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_PROPERTY,
                    StoreKey.storeKeyToSkel(sk)));
    }

    /**
     * <p>Reset a predicate property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param pick The predicate.
     * @param m    The value skeleton.
     * @param d    The value display.
     * @param en   The engine.
     * @throws EngineMessage Validation Error.
     */
    public static void resetPredProp(Predicate pick, Object m, Display d,
                                     Engine en)
            throws EngineMessage {
        StoreKey sk = StackElement.callableToStoreKey(m);
        AbstractProperty<Predicate> prop = SpecialPred.findPredProperty(sk, en);
        if (!prop.resetObjProp(pick, m, d, en))
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_PROPERTY,
                    StoreKey.storeKeyToSkel(sk)));
    }


    /**
     * <p>Retrieve the predicates for a property.</p>
     *
     * @param t  The value skeleton.
     * @param d  The value display.
     * @param en The engine.
     */
    private static Object propertyToPredicates(Object t, Display d,
                                               Engine en)
            throws EngineMessage, EngineException {
        StoreKey sk = StackElement.callableToStoreKey(t);
        AbstractProperty<Predicate> prop = SpecialPred.findPredProperty(sk, en);
        Predicate[] vals = prop.idxObjProp(t, d, en);
        Object res = en.store.foyer.ATOM_NIL;
        res = consPredicates(vals, res, en);
        return res;
    }

    /**
     * <p>Retrieve a predicate property.</p>
     * <p>Throws a domain error for undefined predicate properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param sk The property.
     * @param en The engine.
     * @return The predicate property.
     * @throws EngineMessage Validation Error.
     */
    public static AbstractProperty<Predicate> findPredProperty(StoreKey sk,
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
            ListArray<MapHashLink<StoreKey, AbstractProperty<Predicate>>> props = branch.getPredProps();
            for (int j = 0; j < props.size(); j++) {
                AbstractProperty<Predicate> prop = props.get(j).get(sk);
                if (prop != null)
                    return prop;
            }
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                StoreKey.storeKeyToSkel(sk)));
    }

    /*************************************************************/
    /* Predicate Promotion                                       */
    /*************************************************************/

    /**
     * <p>Define predicate as static.</p>
     *
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage Validation Error.
     */
    private static void defineStatic(Predicate pick, Engine en)
            throws EngineMessage {
        AbstractDelegate fun = AbstractDefined.promoteStatic(pick, en.store);
        if ((fun.subflags & AbstractDefined.MASK_DEFI_STAT) != 0)
            return;
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_COERCE,
                EngineMessage.OP_PERMISSION_PROCEDURE,
                indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /**********************************************************/
    /* Moved From Debugger                                    */
    /**********************************************************/

    /**
     * <p>Retrieve the predicates to a property.</p>
     *
     * @param m  The value skeleton.
     * @param d  The value display.
     * @param en The engine.
     */
    private static Object propertyToProvables(Object m, Display d,
                                              Engine en)
            throws EngineMessage, EngineException {
        StoreKey sk = StackElement.callableToStoreKey(m);
        AbstractProperty<Predicate> prop = SpecialPred.findPredProperty(sk, en);
        Predicate[] vals = prop.idxObjProp(m, d, en);
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
     * @throws EngineMessage Validation Error.
     */
    public static Object consProvables(Predicate[] preds, Object res,
                                       Engine en)
            throws EngineMessage {
        for (int i = preds.length - 1; i >= 0; i--) {
            Predicate pick = preds[i];
            Object val = indicatorToColonSkel(
                    pick.getFun(), pick.getSource().getStore().user,
                    pick.getArity(), en);
            res = new SkelCompound(en.store.foyer.ATOM_CONS, val, res);
        }
        return res;
    }

    /**************************************************************/
    /* Predicate Lookup                                           */
    /**************************************************************/

    /**
     * <p>Get a predicate by indicator.</p>
     *
     * @param t  The indicator skel.
     * @param d  The indicator display.
     * @param en The engine.
     * @return The predicate, or null.
     * @throws EngineMessage   Validation Error.
     * @throws EngineException Validation Error.
     */
    public static Predicate indicatorToPredicate(Object t, Display d,
                                                 Engine en)
            throws EngineMessage, EngineException {
        Integer arity = colonToIndicator(t, d, en);
        SkelAtom sa = (SkelAtom) en.skel;
        CachePredicate cp = CachePredicate.getPredicate(sa, arity.intValue(), en);
        en.skel = sa;
        if (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0)
            return null;
        return cp.pick;
    }

    /**
     * <p>Get predicate by indicator and possibly create it.</p>
     *
     * @param t    The indicator skel.
     * @param d    The indicator display.
     * @param en   The engine.
     * @param copt The create flag.
     * @return The predicate, or null.
     * @throws EngineMessage Validation Error.
     * @throws EngineMessage Validation Error.
     */
    public static Predicate indicatorToPredicateDefined(Object t, Display d,
                                                        Engine en, int copt)
            throws EngineMessage, EngineException {
        Integer arity = colonToIndicator(t, d, en);
        SkelAtom sa = (SkelAtom) en.skel;
        CachePredicate cp = CachePredicate.getPredicateDefined(sa,
                arity.intValue(), en, copt);
        en.skel = sa;
        if (cp == null || ((copt & CachePredicate.MASK_CACH_UCHK) == 0 &&
                (cp.flags & CachePredicate.MASK_PRED_VISI) == 0))
            return null;
        return cp.pick;
    }

    /***************************************************************/
    /* Indicator Colon                                             */
    /***************************************************************/

    /**
     * <p>Convert a qualified indicator to an indicator.</p>
     * <p>The name is returned in skel.</p>
     * <p>A qualified indicator has the following syntax.</p>
     * <pre>
     *     indicator --> module ":" indicator
     *                 | name "/" length.
     * </pre>
     *
     * @param en The engine.
     * @return The length.
     * @throws EngineMessage The indicator is not wellformed.
     */
    public static Integer colonToIndicator(Object t, Display d, Engine en)
            throws EngineMessage {
        try {
            en.skel = t;
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
            if (t instanceof SkelCompound &&
                    ((SkelCompound) t).args.length == 2 &&
                    ((SkelCompound) t).sym.fun.equals(EvaluableLogic.OP_COLON)) {
                SkelCompound temp = (SkelCompound) t;
                Object obj = EvaluableLogic.slashToClass(temp.args[0], d, false, true, en);
                SkelAtom mod = SpecialLogic.modToAtom(obj, temp.args[0], d, en);
                Integer arity = colonToIndicator(temp.args[1], d, en);
                SkelAtom sa = (SkelAtom) en.skel;
                en.skel = CacheFunctor.getFunctor(sa, mod, temp.sym, en);
                return arity;
            } else if (t instanceof SkelCompound &&
                    ((SkelCompound) t).args.length == 2 &&
                    ((SkelCompound) t).sym.fun.equals(Foyer.OP_SLASH)) {
                SkelCompound sc = (SkelCompound) t;
                Number num = SpecialEval.derefAndCastInteger(sc.args[1], d);
                SpecialEval.checkNotLessThanZero(num);
                SpecialEval.castIntValue(num);
                en.skel = SpecialUniv.derefAndCastStringWrapped(sc.args[0], d);
                return (Integer) num;
            } else {
                EngineMessage.checkInstantiated(t);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_PREDICATE_INDICATOR, t), d);
            }
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
        }
    }

    /**
     * <p>Convert an indicator to a qualified indicator.</p>
     *
     * @param sa    The name.
     * @param arity The length.
     * @param en    The engine.
     * @return The colon indictor
     * @throws EngineMessage Shit happens.
     */
    public static Object indicatorToColonSkel(SkelAtom sa, int arity,
                                              Engine en)
            throws EngineMessage {
        Object s;
        if (sa instanceof SkelAtomQuali) {
            SkelAtom mod = ((SkelAtomQuali) sa).getModule();
            AbstractSource src = (mod.scope != null ? mod.scope : en.store.user);
            s = SpecialDynamic.moduleToSlashSkel(mod.fun, src);
            int m = (sa.getPosition() != null ? SkelAtom.MASK_ATOM_POSI : 0);
            SkelAtom sa2 = en.store.foyer.createAtom(EvaluableLogic.OP_COLON, sa.scope, m);
            sa2.setPosition(sa.getPosition());

            Object t = new SkelCompound(en.store.foyer.ATOM_SLASH,
                    new SkelAtom(CacheFunctor.sepName(sa.fun)),
                    Integer.valueOf(arity));
            s = new SkelCompound(sa2, s, t);
        } else {
            s = new SkelCompound(en.store.foyer.ATOM_SLASH, sa,
                    Integer.valueOf(arity));
        }
        return s;
    }

    /**
     * <p>Convert an indicator to a qualified indicator.</p>
     *
     * @param fun   The name.
     * @param scope The scope, non null.
     * @param arity The length.
     * @param en    The engine.
     * @return The colon indictor
     * @throws EngineMessage Shit happens.
     */
    public static Object indicatorToColonSkel(String fun,
                                              AbstractSource scope,
                                              int arity,
                                              Engine en)
            throws EngineMessage {
        Object s;
        if (CacheFunctor.isQuali(fun)) {
            String mod = CacheFunctor.sepModule(fun);
            s = SpecialDynamic.moduleToSlashSkel(mod, scope);
            SkelAtom sa2 = new SkelAtom(EvaluableLogic.OP_COLON, scope);

            Object t = new SkelCompound(en.store.foyer.ATOM_SLASH,
                    new SkelAtom(CacheFunctor.sepName(fun), scope),
                    Integer.valueOf(arity));
            s = new SkelCompound(sa2, s, t);
        } else {
            s = new SkelCompound(en.store.foyer.ATOM_SLASH,
                    new SkelAtom(fun, scope),
                    Integer.valueOf(arity));
        }
        return s;
    }

}
