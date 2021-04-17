package jekpro.reference.reflect;

import matula.comp.sharik.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.builtin.Branch;
import jekpro.model.inter.*;
import jekpro.model.molec.*;
import jekpro.model.pretty.*;
import jekpro.model.rope.LoadOpts;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.runtime.EvaluableLogic;
import jekpro.reference.runtime.SpecialDynamic;
import jekpro.reference.runtime.SpecialLogic;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.array.Types;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.comp.sharik.AbstractTracking;
import matula.util.config.AbstractBundle;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;

import java.io.Writer;

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
    private final static String OP_SET_PREDICATE_PROPERTY = "set_predicate_property";

    private final static int SPECIAL_SYS_ENSURE_SHARED_STATIC = 0;
    private final static int SPECIAL_SYS_CURRENT_PREDICATE = 1;
    private final static int SPECIAL_SYS_CURRENT_PREDICATE_CHK = 2;
    private final static int SPECIAL_SYS_PREDICATE_PROPERTY = 3;
    private final static int SPECIAL_SYS_PREDICATE_PROPERTY_CHK = 4;
    private final static int SPECIAL_SYS_PREDICATE_PROPERTY_IDX = 5;
    private final static int SPECIAL_SYS_PROVABLE_PROPERTY_IDX = 6;
    private final static int SPECIAL_SYS_PROVABLE_PROPERTY_CHK = 7;
    private final static int SPECIAL_SYS_SHOW_PROPERTIES = 8;


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
                if (!en.unify(SpecialPred.currentPredicates(en), Display.DISPLAY_CONST, temp[0], ref))
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
                if (!en.unify(en.skel, d, temp[1], ref))
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
                StoreKey prop = AbstractProperty.propToStoreKey(temp[1], ref, en);
                SpecialPred.predicateToProperty(pick, prop, en);
                d = en.display;
                multi = d.getAndReset();
                if (!en.unify(en.skel, d, temp[2], ref))
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
                if (!en.unify(SpecialPred.propertyToPredicates(en.skel, en.display, en),
                        Display.DISPLAY_CONST, temp[1], ref))
                    return false;
                return true;
            case SPECIAL_SYS_PROVABLE_PROPERTY_IDX:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                if (!en.unify(propertyToProvables(en.skel, en.display, en),
                        Display.DISPLAY_CONST, temp[1], ref))
                    return false;
                return true;
            case SPECIAL_SYS_PROVABLE_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_UCHK);
                if (pick == null)
                    return false;
                prop = AbstractProperty.propToStoreKey(temp[1], ref, en);
                SpecialPred.predicateToProperty(pick, prop, en);
                d = en.display;
                multi = d.getAndReset();
                if (!en.unify(en.skel, d, temp[2], ref))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
            case SPECIAL_SYS_SHOW_PROPERTIES:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;

                pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_UCHK);
                if (pick == null)
                    return false;

                SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(temp[1], ref);
                AbstractSource source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                if (source == null)
                    return false;
                if (pick.getDef(source) == null)
                    return false;

                SpecialPred.showProperties(pick, source, en);
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>List a predicate.</p>
     * <p>System clauses are excluded.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param pick The predicate.
     * @param src  The source, non null.
     * @param en   The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void showProperties(Predicate pick,
                                       AbstractSource src, Engine en)
            throws EngineMessage, EngineException {
        Object obj = en.visor.curoutput;
        LoadOpts.checkTextWrite(obj);
        Writer wr = (Writer) obj;
        PrologWriter pw = en.store.foyer.createWriter(Foyer.IO_TERM);
        pw.setDefaults(en.visor.peekStack());
        pw.setEngine(en);
        pw.setFlags(pw.getFlags() | (PrologWriter.FLAG_QUOT | PrologWriter.FLAG_NEWL | PrologWriter.FLAG_MKDT));
        pw.setSpez(PrologWriter.SPEZ_META);
        pw.setOffset(-1);
        pw.setWriter(wr);

        if (pick.del == null)
            return;
        /* flesh out properties */
        ListArray<SkelAtom> modifiers = null;
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            ListArray<MapHashLink<StoreKey, AbstractProperty<Predicate>>> props = branch.getPredProps();
            for (int j = 0; j < props.size(); j++)
                modifiers = SpecialPred.showProperties(pw, pick, src,
                        props.get(j), en, modifiers);
        }
        if (modifiers != null) {
            Object decl = provableToColonSkel(pick, src);
            decl = SpecialPred.prependModifiers(modifiers, decl);
            decl = new SkelCompound(new SkelAtom(Foyer.OP_TURNSTILE), decl);
            decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
            pw.unparseStatement(decl, Display.DISPLAY_CONST);
            SpecialLoad.flushWriter(pw.getWriter());
        }
    }

    /**
     * <p>List the provable properties.</p>
     *
     * @param pw    The print writer.
     * @param pick  The predicate.
     * @param src   The source, non null.
     * @param props The properties.
     * @param en    The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static ListArray<SkelAtom> showProperties(PrologWriter pw, Predicate pick,
                                                      AbstractSource src,
                                                      MapHashLink<StoreKey, AbstractProperty<Predicate>> props,
                                                      Engine en, ListArray<SkelAtom> modifiers)
            throws EngineMessage, EngineException {
        for (MapEntry<StoreKey, AbstractProperty<Predicate>> entry2 =
             (props != null ? props.getFirstEntry() : null);
             entry2 != null; entry2 = props.successor(entry2)) {
            AbstractProperty<Predicate> prop = entry2.value;
            if ((prop.getFlags() & AbstractProperty.MASK_PROP_SHOW) == 0)
                continue;
            if ((prop.getFlags() & AbstractProperty.MASK_PROP_DEFL) != 0 &&
                    (pick.del instanceof AbstractDefined) &&
                    SpecialDynamic.hasClause((AbstractDefined) pick.del, src, en))
                continue;
            if ((prop.getFlags() & AbstractProperty.MASK_PROP_SUPR) != 0 &&
                    SpecialPred.sameVisiblePredicate(pick, src, en))
                continue;
            Object[] vals = prop.getObjProps(pick, en);
            if ((prop.getFlags() & AbstractProperty.MASK_PROP_SLCF) != 0) {
                vals = SpecialPred.selectFirst(vals, src.getPathAtom());
            } else if ((prop.getFlags() & AbstractProperty.MASK_PROP_DELE) != 0) {
                vals = SpecialPred.delegateSpec(vals, pick, src);
            }
            if ((prop.getFlags() & AbstractProperty.MASK_PROP_MODI) != 0) {
                for (int j = 0; j < vals.length; j++) {
                    Object val = vals[j];
                    if ((prop.getFlags() & AbstractProperty.MASK_PROP_PRJF) != 0)
                        val = SpecialLoad.firstArg(val);
                    if (modifiers == null)
                        modifiers = new ListArray<>();
                    modifiers.add((SkelAtom) AbstractTerm.getSkel(val));
                }
            } else {
                for (int j = 0; j < vals.length; j++) {
                    Object val = vals[j];
                    Object decl;
                    if ((prop.getFlags() & AbstractProperty.MASK_PROP_SETP) != 0) {
                        decl = SpecialPred.predDeclSkelSet(
                                AbstractTerm.getSkel(val), pick, src);
                    } else if ((prop.getFlags() & AbstractProperty.MASK_PROP_META) != 0) {
                        decl = SpecialPred.predDeclSkelMeta(
                                AbstractTerm.getSkel(val), pick, src);
                    } else {
                        if ((prop.getFlags() & AbstractProperty.MASK_PROP_PRJF) != 0)
                            val = SpecialLoad.firstArg(val);
                        decl = SpecialPred.predDeclSkelIndicator(
                                AbstractTerm.getSkel(val), pick, src);
                    }
                    if (modifiers != null) {
                        decl = SpecialPred.prependModifiers(modifiers, decl);
                        modifiers = null;
                    }
                    decl = new SkelCompound(new SkelAtom(Foyer.OP_TURNSTILE), decl);
                    decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
                    pw.unparseStatement(decl, Display.DISPLAY_CONST);
                    SpecialLoad.flushWriter(pw.getWriter());
                }
            }
        }
        return modifiers;
    }

    /**
     * <p>Check whether a source and a predicate have the same visibility.</p>
     *
     * @param pick The predicate.
     * @param src  The source.
     * @param en   The engine.
     * @return True if the source and the predicate have the same visibility, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static boolean sameVisiblePredicate(Predicate pick, AbstractSource src,
                                                Engine en)
            throws EngineMessage, EngineException {
        StoreKey sk = new StoreKey(PropertySource.OP_SYS_SOURCE_VISIBLE, 1);
        AbstractProperty<AbstractSource> prop = SpecialSource.findSrcProperty(sk, en);
        Object[] vals = prop.getObjProps(src, en);
        StoreKey sk2 = new StoreKey(PropertyPredicate.OP_VISIBLE, 1);
        AbstractProperty<Predicate> prop1 = SpecialPred.findPredProperty(sk2, en);
        Object[] vals2 = prop1.getObjProps(pick, en);
        return SpecialLoad.sameValues(vals, vals2);
    }

    /*********************************************************/
    /* Predicate Declaration Formatting                      */
    /*********************************************************/

    /**
     * <p>Generate a set predicate declaration.</p>
     *
     * @param skel   The value.
     * @param pick   The predicate.
     * @param source The source, non null.
     * @return The set predicate declaration.
     * @throws EngineMessage Shit happens.
     */
    private static Object predDeclSkelSet(Object skel, Predicate pick,
                                          AbstractSource source)
            throws EngineMessage {
        return new SkelCompound(new SkelAtom(OP_SET_PREDICATE_PROPERTY, source),
                provableToColonSkel(pick,
                        source), skel);
    }

    /**
     * <p>Generate a meta predicate declaration.</p>
     *
     * @param skel   The value.
     * @param pick   The predicate.
     * @param source The source.
     * @return The meta predicate declaration.
     * @throws EngineMessage Shit happens.
     */
    private static Object predDeclSkelMeta(Object skel, Predicate pick,
                                           AbstractSource source)
            throws EngineMessage {
        SkelCompound sc = (SkelCompound) skel;
        Object t = sc.args[sc.args.length - 1];
        int k = 0;
        while (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_CONS)) {
            t = ((SkelCompound) t).args[1];
            k++;
        }
        if (k != 0) {
            Object[] args = new Object[k];
            t = sc.args[sc.args.length - 1];
            k = 0;
            while (t instanceof SkelCompound &&
                    ((SkelCompound) t).args.length == 2 &&
                    ((SkelCompound) t).sym.fun.equals(Foyer.OP_CONS)) {
                args[k] = ((SkelCompound) t).args[0];
                t = ((SkelCompound) t).args[1];
                k++;
            }
            t = new SkelCompound(new SkelAtom(pick.getFun(), source), args);
        } else {
            t = new SkelAtom(pick.getFun(), source);
        }
        Object[] args = new Object[sc.args.length];
        args[sc.args.length - 1] = provableToColonSkel(pick, t, source);
        if (sc.args.length > 1)
            System.arraycopy(sc.args, 0, args, 0, sc.args.length - 1);
        SkelCompound sc2 = new SkelCompound(args, sc.sym);
        sc2.var = sc.var;
        return sc2;
    }

    /**
     * <p>Generate a indicator predicate declaration.</p>
     *
     * @param skel   The value.
     * @param pick   The predicate.
     * @param source The source.
     * @return The indicator predicate declaration.
     * @throws EngineMessage Shit happens.
     */
    private static Object predDeclSkelIndicator(Object skel, Predicate pick,
                                                AbstractSource source)
            throws EngineMessage {
        Object t = provableToColonSkel(pick, source);
        if (skel instanceof SkelAtom) {
            SkelAtom sa = (SkelAtom) skel;
            return new SkelCompound(sa, t);
        } else if (skel instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) skel;
            Object[] args = new Object[sc.args.length + 1];
            System.arraycopy(sc.args, 0, args, 1, sc.args.length);
            args[0] = t;
            return new SkelCompound(sc.sym, args);
        } else {
            throw new IllegalArgumentException("illegal property");
        }
    }

    /****************************************************************************/
    /* Predicate Property Filters                                               */
    /****************************************************************************/

    /**
     * <p>Find all values that have as a first argument some other value.</p>
     *
     * @param vals The values.
     * @param val  The other value.
     * @return The found values.
     */
    private static Object[] selectFirst(Object[] vals, Object val) {
        ListArray<Object> res = null;
        for (int i = 0; i < vals.length; i++) {
            Object val2 = vals[i];
            SkelCompound sc = (SkelCompound) AbstractTerm.getSkel(val2);
            val2 = AbstractTerm.createMolec(sc.args[0], AbstractTerm.getDisplay(val2));
            if (!val2.equals(val))
                continue;
            if (res == null)
                res = new ListArray<>();
            if (sc.args.length == 1) {
                res.add(sc.sym);
            } else {
                Object[] newargs = new Object[sc.args.length - 1];
                System.arraycopy(sc.args, 1, newargs, 0, newargs.length);
                res.add(AbstractTerm.createMolec(new SkelCompound(sc.sym, newargs),
                        AbstractTerm.getDisplay(val2)));
            }
        }
        if (res == null)
            return AbstractBranch.FALSE_PROPERTY;
        Object[] newvals = new Object[res.size()];
        res.toArray(newvals);
        return newvals;
    }

    /**
     * <p>Convert the value list to its spec.</p>
     *
     * @param vals   The value list only buit_in.
     * @param pick   The predicate.
     * @param source The source, non null.
     * @return The value list with spec.
     */
    private static Object[] delegateSpec(Object[] vals, Predicate pick,
                                         AbstractSource source)
            throws EngineMessage {
        if (vals.length == 0)
            return vals;
        Object[] newvals = new Object[vals.length];
        for (int i = 0; i < vals.length; i++) {
            Object val = pick.del.toSpec(source);
            newvals[i] = AbstractTerm.createMolec(val, Display.DISPLAY_CONST);
        }
        return newvals;
    }

    /**
     * <p>Prepend modifiers to a value.</p>
     *
     * @param modifiers The modifiers.
     * @param decl      The value.
     * @return The prependet value.
     */
    private static Object prependModifiers(ListArray<SkelAtom> modifiers, Object decl) {
        for (int i = modifiers.size() - 1; i >= 0; i--)
            decl = new SkelCompound(modifiers.get(i), decl);
        return decl;
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
            AbstractSource src = pick.getSource();
            Object val = indicatorToColonSkel(pick.getFun(), pick.getArity(),
                    src.getFullName(), src.getStore().user);
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
                    sk.storeKeyToSkel()));
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
                    sk.storeKeyToSkel()));
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
                sk.storeKeyToSkel()));
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
        AbstractSource src = pick.getSource();
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_COERCE,
                EngineMessage.OP_PERMISSION_PROCEDURE,
                indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src.getFullName(), src.getStore().user)));
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
            AbstractSource src = pick.getSource();
            Object val = indicatorToColonSkel(pick.getFun(), pick.getArity(),
                    src.getFullName(), src.getStore().user);
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
                Object obj = EvaluableLogic.slashToClass(temp.args[0], d, 0, en);
                SkelAtom mod = SpecialLogic.modToAtom(obj, temp.args[0], d, en);
                Integer arity = colonToIndicator(temp.args[1], d, en);
                SkelAtom sa = (SkelAtom) en.skel;
                en.skel = CacheFunctor.getModFunc(sa, mod, temp.sym, en);
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
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
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

            Object t = new SkelCompound(en.store.foyer.ATOM_SLASH, new SkelAtom(sa.fun, sa.scope),
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
     * @param arity The length.
     * @param mod   The module name.
     * @param scope The scope.
     * @return The colon indictor
     * @throws EngineMessage Shit happens.
     */
    public static Object indicatorToColonSkel(String fun,
                                              int arity,
                                              String mod,
                                              AbstractSource scope)
            throws EngineMessage {
        Object s;
        if (!Branch.OP_USER.equals(mod)) {
            s = SpecialDynamic.moduleToSlashSkel(mod, scope);
            SkelAtom sa2 = new SkelAtom(EvaluableLogic.OP_COLON, scope);
            Object t = new SkelCompound(scope.getStore().foyer.ATOM_SLASH,
                    new SkelAtom(fun, scope),
                    Integer.valueOf(arity));
            s = new SkelCompound(sa2, s, t);
        } else {
            s = new SkelCompound(scope.getStore().foyer.ATOM_SLASH,
                    new SkelAtom(fun, scope),
                    Integer.valueOf(arity));
        }
        return s;
    }

    /********************************************************************/
    /* Predicate Formatting Utilities                                   */
    /********************************************************************/

    /**
     * <p>Generate a provable indicator hash.</p>
     *
     * @param pick   The predicate.
     * @param source The source, non null.
     * @return The shortest predicate indicator.
     * @throws EngineMessage Shit happens.
     */
    public static Object provableToColonSkel(Predicate pick,
                                             AbstractSource source)
            throws EngineMessage {
        SkelCompound t = new SkelCompound(new SkelAtom(Foyer.OP_SLASH),
                new SkelAtom(pick.getFun(), source),
                Integer.valueOf(pick.getArity()));
        return provableToColonSkel(pick, t, source);
    }

    /**
     * <p>Convert a callable to a colon.</p>
     * <p>A colon callable has the following syntax.</p>
     * <pre>
     *     colon_callable --> slash : callable
     *                      | term.
     * </pre>
     * <p>The syntax is not recursive.</p>
     *
     * @param pick   The predicate.
     * @param t      The callable.
     * @param source The source, non null.
     * @return The colon callable.
     * @throws EngineMessage Shit happens.
     */
    public static Object provableToColonSkel(Predicate pick, Object t,
                                             AbstractSource source)
            throws EngineMessage {
        String orig = source.getFullName();
        String module = pick.getSource().getFullName();
        if (!orig.equals(module)) {
            Object s = SpecialDynamic.moduleToSlashSkel(module, source);
            return new SkelCompound(new SkelAtom(EvaluableLogic.OP_COLON, source), s, t);
        } else {
            return t;
        }
    }

}
