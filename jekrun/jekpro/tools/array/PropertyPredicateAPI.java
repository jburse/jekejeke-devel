package jekpro.tools.array;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.Clause;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.reflect.PropertyPredicate;
import jekpro.reference.runtime.EvaluableLogic;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;

import java.util.concurrent.locks.ReadWriteLock;

/**
 * <p>This class provides additional predicate properties.</p>
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
 * Only to be distributed with programs that add sgnificant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class PropertyPredicateAPI extends AbstractProperty<Predicate> {
    public static final MapHashLink<StoreKey, AbstractProperty<Predicate>> DEFAULT
            = new MapHashLink<>();

    public final static String OP_OVERRIDE = "override";
    private final static String OP_SYS_META_PREDICATE = "sys_meta_predicate";
    private final static String OP_AUTOMATIC = "automatic";
    private final static String OP_SYS_NOEXPAND = "sys_noexpand";
    private final static String OP_META_PREDICATE = "meta_predicate";
    private final static String OP_SYS_TABLED = "sys_tabled";
    private final static String OP_SYS_READWRITE_LOCK = "sys_readwrite_lock";
    private final static String OP_SYS_UNTABLED = "sys_untabled";

    private final static int PROP_OVERRIDE = 0;
    private final static int PROP_SYS_META_PREDICATE = 1;
    private final static int PROP_AUTOMATIC = 2;
    private final static int PROP_SYS_NOEXPAND = 3;
    private final static int PROP_META_PREDICATE = 4;
    private final static int PROP_SYS_TABLED = 5;
    private final static int PROP_SYS_READWRITE_LOCK = 6;
    private final static int PROP_SYS_UNTABLED = 7;

    static {
        DEFAULT.add(new StoreKey(OP_OVERRIDE, 1), new PropertyPredicateAPI(PROP_OVERRIDE,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_SLCF));
        DEFAULT.add(new StoreKey(OP_SYS_META_PREDICATE, 1), new PropertyPredicateAPI(PROP_SYS_META_PREDICATE));
        DEFAULT.add(new StoreKey(OP_AUTOMATIC, 0), new PropertyPredicateAPI(PROP_AUTOMATIC));
        DEFAULT.add(new StoreKey(OP_SYS_NOEXPAND, 0), new PropertyPredicateAPI(PROP_SYS_NOEXPAND,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_SETP));
        DEFAULT.add(new StoreKey(OP_META_PREDICATE, 1), new PropertyPredicateAPI(PROP_META_PREDICATE,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_META));
        DEFAULT.add(new StoreKey(OP_SYS_TABLED, 0), new PropertyPredicateAPI(PROP_SYS_TABLED));
        DEFAULT.add(new StoreKey(OP_SYS_READWRITE_LOCK, 1), new PropertyPredicateAPI(PROP_SYS_READWRITE_LOCK));
        DEFAULT.add(new StoreKey(OP_SYS_UNTABLED, 0), new PropertyPredicateAPI(PROP_SYS_UNTABLED));
    }


    /**
     * <p>Create an additional predicate property.</p>
     *
     * @param i The id of the additional predicate property.
     */
    private PropertyPredicateAPI(int i) {
        super(i);
    }

    /**
     * <p>Create an additional predicate property.</p>
     *
     * @param i The id of the additional predicate property.
     * @param f The flags.
     */
    private PropertyPredicateAPI(int i, int f) {
        super(i, f);
    }

    /**
     * <p>Retrieve all the predicate properties.</p>
     *
     * @param pick The predicate.
     * @param en   The engine.
     * @return The predicate properties.
     */
    public Object[] getObjProps(Predicate pick, Engine en) {
        switch (id) {
            case PROP_OVERRIDE:
                ListArray<Object> res = PropertyPredicate.filterDefs(pick,
                        Predicate.MASK_TRCK_OVRD, en);
                if (res == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return PropertyPredicate.snapshotToVals(
                        new SkelAtom(OP_OVERRIDE), res);
            case PROP_SYS_META_PREDICATE:
                res = PropertyPredicate.filterDefs(pick,
                        Predicate.MASK_TRCK_META, en);
                if (res == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return PropertyPredicate.snapshotToVals(
                        new SkelAtom(OP_SYS_META_PREDICATE), res);
            case PROP_AUTOMATIC:
                if ((pick.getBits() & Predicate.MASK_PRED_AUTO) != 0) {
                    return new Object[]{new SkelAtom(OP_AUTOMATIC)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_NOEXPAND:
                if ((pick.getBits() & Predicate.MASK_PRED_NOEX) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_NOEXPAND)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_META_PREDICATE:
                Object val = pick.meta_predicate;
                if (val != null) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(OP_META_PREDICATE), val), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_TABLED:
                if ((pick.getBits() & Predicate.MASK_PRED_TABL) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_TABLED)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_READWRITE_LOCK:
                val = getPredicateLock(pick, en);
                if (val != null) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(OP_SYS_READWRITE_LOCK), val), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_UNTABLED:
                if ((pick.getBits() & Predicate.MASK_PRED_UTBL) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_UNTABLED)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /**
     * <p>Set a predicate property.</p>
     *
     * @param pick The predicate.
     * @param m    The property skeleton.
     * @param d    The property display.
     * @param en   The engine.
     * @return True if property could be set, otherwise false.
     * @throws EngineMessage Validation Error.
     */
    public boolean setObjProp(Predicate pick, Object m, Display d, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_OVERRIDE:
                AbstractSource src = PropertyPredicate.derefAndCastDef(m, d, OP_OVERRIDE, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.addDef(src, Predicate.MASK_TRCK_OVRD);
                return true;
            case PROP_SYS_META_PREDICATE:
                src = PropertyPredicate.derefAndCastDef(m, d, OP_SYS_META_PREDICATE, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.addDef(src, Predicate.MASK_TRCK_META);
                return true;
            case PROP_AUTOMATIC:
                pick.setBit(Predicate.MASK_PRED_AUTO);
                return true;
            case PROP_SYS_NOEXPAND:
                pick.setBit(Predicate.MASK_PRED_NOEX);
                return true;
            case PROP_META_PREDICATE:
                pick.meta_predicate = derefAndCastMeta(pick, m, d, en);
                return true;
            case PROP_SYS_TABLED:
                pick.setBit(Predicate.MASK_PRED_TABL);
                return true;
            case PROP_SYS_READWRITE_LOCK:
                /* can't modify */
                return false;
            case PROP_SYS_UNTABLED:
                pick.setBit(Predicate.MASK_PRED_UTBL);
                return true;
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /**
     * <p>Reset a predicate property.</p>
     *
     * @param pick The predicate.
     * @param m    The property skeleton.
     * @param d    The property display.
     * @param en   The engine.
     * @return True if property could be set, otherwise false.
     * @throws EngineMessage Validation Error.
     */
    public boolean resetObjProp(Predicate pick, Object m, Display d, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_OVERRIDE:
                AbstractSource src = PropertyPredicate.derefAndCastDef(m, d, OP_OVERRIDE, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.removeDef(src, Predicate.MASK_TRCK_OVRD);
                return true;
            case PROP_SYS_META_PREDICATE:
                src = PropertyPredicate.derefAndCastDef(m, d, OP_SYS_META_PREDICATE, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.removeDef(src, Predicate.MASK_TRCK_META);
                return true;
            case PROP_AUTOMATIC:
                pick.resetBit(Predicate.MASK_PRED_AUTO);
                return true;
            case PROP_SYS_NOEXPAND:
                pick.resetBit(Predicate.MASK_PRED_NOEX);
                return true;
            case PROP_META_PREDICATE:
                pick.meta_predicate = null;
                return true;
            case PROP_SYS_TABLED:
                pick.resetBit(Predicate.MASK_PRED_TABL);
                return true;
            case PROP_SYS_READWRITE_LOCK:
                /* can't modify */
                return false;
            case PROP_SYS_UNTABLED:
                pick.resetBit(Predicate.MASK_PRED_UTBL);
                return true;
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /**
     * <p>Retrieve all the objects for a property.</p>
     *
     * @param en The engine.
     * @param m  The property skeleton.
     * @param d  The property display.
     * @return The properties.
     */
    public Predicate[] idxObjProp(Object m, Display d, Engine en) {
        if (id == PROP_SYS_TABLED) {
            ListArray<Predicate> res = null;
            Store store = en.store;
            while (store != null) {
                MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
                for (int i = 0; i < sources.length; i++) {
                    AbstractSource base = sources[i].value;
                    Predicate[] preds = base.snapshotRoutine();
                    for (int j = 0; j < preds.length; j++) {
                        Predicate pick = preds[j];
                        if ((pick.getBits() & Predicate.MASK_PRED_TABL) == 0)
                            continue;
                        if (res == null)
                            res = new ListArray<>();
                        res.add(pick);
                    }
                }
                store = store.parent;
            }
            if (res == null)
                return AbstractBranch.FALSE_PREDS;
            Predicate[] vals = new Predicate[res.size()];
            res.toArray(vals);
            return vals;
        } else {
            if (id < PROP_SYS_META_PREDICATE || id > PROP_SYS_UNTABLED)
                throw new IllegalArgumentException("illegal prop");
            return null;
        }
    }

    /****************************************************************/
    /* Deref Utility                                                */
    /****************************************************************/

    /**
     * <p>Deref and cast to predicate meta.</p>
     *
     * @param pick The predicate.
     * @param m    The term skeleton.
     * @param d    The term display.
     * @param en   The engine.
     * @return The meta term.
     * @throws EngineMessage Shit happens.
     */
    private static Object derefAndCastMeta(Predicate pick, Object m, Display d,
                                           Engine en)
            throws EngineMessage {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 1 &&
                ((SkelCompound) m).sym.fun.equals(OP_META_PREDICATE)) {
            return checkMeta(pick, ((SkelCompound) m).args[0], d, en);
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Retrieve the predicate lock.</p>
     *
     * @param pick The predicate.
     * @param en   The engine.
     * @return The read write lock.
     */
    private static ReadWriteLock getPredicateLock(Predicate pick, Engine en) {
        AbstractDelegate del = pick.del;
        if (del instanceof AbstractDefined) {
            return ((AbstractDefined) del).getLock(en);
        } else {
            return null;
        }
    }

    /**********************************************************/
    /* Meta Check                                             */
    /**********************************************************/

    /**
     * <p>Check a meta predicate declaration.</p>
     * <p>The following syntax is used:</p>
     * <pre>
     *     meta_signature    --> predicate_name
     *         [ "(" meta_specifier { "," meta_specifier } ")" ].
     * </pre>
     *
     * @param pred The predicate.
     * @param t    The declaration skeleton.
     * @param d    The declaration display.
     * @param en   The engine.
     * @return The meta predicate declaration.
     * @throws EngineMessage Shit happens.
     */
    private static Object checkMeta(Predicate pred,
                                    Object t, Display d, Engine en)
            throws EngineMessage {
        ListArray<Object> res = new ListArray<>();
        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;
        while (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_CONS)) {
            Object[] sc = ((SkelCompound) t).args;
            res.add(checkMetaSpez(sc[0], d, en));
            en.skel = sc[1];
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
        }
        if (t instanceof SkelAtom &&
                ((SkelAtom) t).fun.equals(Foyer.OP_NIL)) {
            /* */
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_LIST, t), d);
        }
        if (res.size() != pred.getArity())
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_PROPERTY_VALUE, t), d);

        Object val = en.store.foyer.ATOM_NIL;
        for (int i = res.size() - 1; i >= 0; i--)
            val = new SkelCompound(en.store.foyer.ATOM_CONS, res.get(i), val);
        return val;
    }

    /**
     * <p>Check a meta argument spezifier.</p>
     * <p>Returns a meta argument spezifier skeleton.</p>
     * <p>The following syntax is used:</p>
     * <pre>
     *     meta_specifier    --> integer
     *                         | "?"
     *                         | "::(" meta_specifier2 ")"
     * </pre>
     *
     * @param t  The spezifier skeleton.
     * @param d  The spezifier display.
     * @param en The engine.
     * @return The meta argument spezifier.
     * @throws EngineMessage Shit happens.
     */
    public static Object checkMetaSpez(Object t, Display d,
                                       Engine en)
            throws EngineMessage {
        try {
            en.skel = t;
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
            if (t instanceof Number) {
                Number num = (Number) t;
                SpecialEval.castIntValue(num);
                return num;
            } else if (t instanceof SkelAtom &&
                    ((SkelAtom) t).fun.equals(Predicate.OP_QUESTION)) {
                return t;
            } else if (t instanceof SkelCompound &&
                    ((SkelCompound) t).args.length == 1 &&
                    ((SkelCompound) t).sym.fun.equals(EvaluableLogic.OP_COLONCOLON)) {
                return new SkelCompound(new SkelAtom(EvaluableLogic.OP_COLONCOLON),
                        checkMetaSpezArg2(((SkelCompound) t).args[0],
                                d, en));
            } else {
                EngineMessage.checkInstantiated(t);
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_META_ARG,
                        t), d);
            }
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
        }
    }

    /**
     * <p>Check a meta argument spezifier.</p>
     * <p>Returns a meta argument spezifier skeleton.</p>
     * <p>The following syntax is used:</p>
     * <pre>
     *     meta_specifier2   --> integer
     *                         | "::(" meta_specifier2 ")".
     * </pre>
     *
     * @param t  The spezifier skeleton.
     * @param d  The spezifier display.
     * @param en The engine.
     * @return The meta argument spezifier.
     * @throws EngineMessage Shit happens.
     */
    private static Object checkMetaSpezArg2(Object t, Display d,
                                            Engine en)
            throws EngineMessage {
        try {
            en.skel = t;
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
            if (t instanceof Number) {
                Number num = (Number) t;
                SpecialEval.castIntValue(num);
                return num;
            } else if (t instanceof SkelCompound &&
                    ((SkelCompound) t).args.length == 1 &&
                    ((SkelCompound) t).sym.fun.equals(EvaluableLogic.OP_COLONCOLON)) {
                return new SkelCompound(new SkelAtom(EvaluableLogic.OP_COLONCOLON),
                        checkMetaSpezArg2(((SkelCompound) t).args[0], d, en));
            } else {
                EngineMessage.checkInstantiated(t);
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_META_ARG, t), d);
            }
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
        }
    }

}