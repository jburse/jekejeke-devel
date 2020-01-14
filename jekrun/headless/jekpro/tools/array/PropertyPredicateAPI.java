package jekpro.tools.array;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.Clause;
import jekpro.reference.reflect.PropertyPredicate;
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
            = new MapHashLink<StoreKey, AbstractProperty<Predicate>>();

    private final static String OP_SYS_META_PREDICATE = "sys_meta_predicate";
    private final static String OP_SYS_META_FUNCTION = "sys_meta_function";
    private final static String OP_SYS_NOEXPAND = "sys_noexpand";
    private final static String OP_SYS_NOMACRO = "sys_nomacro";
    private final static String OP_META_PREDICATE = "meta_predicate";
    private final static String OP_META_FUNCTION = "meta_function";
    private final static String OP_SYS_TABLED = "sys_tabled";
    private final static String OP_SYS_READWRITE_LOCK = "sys_readwrite_lock";

    public final static int PROP_SYS_META_PREDICATE = 0;
    public final static int PROP_SYS_META_FUNCTION = 1;
    public final static int PROP_SYS_NOEXPAND = 2;
    public final static int PROP_SYS_NOMACRO = 3;
    public final static int PROP_META_PREDICATE = 4;
    public final static int PROP_META_FUNCTION = 5;
    public final static int PROP_SYS_TABLED = 6;
    public final static int PROP_SYS_READWRITE_LOCK = 7;

    static {
        DEFAULT.add(new StoreKey(OP_SYS_META_PREDICATE, 1), new PropertyPredicateAPI(PROP_SYS_META_PREDICATE));
        DEFAULT.add(new StoreKey(OP_SYS_META_FUNCTION, 1), new PropertyPredicateAPI(PROP_SYS_META_FUNCTION));
        DEFAULT.add(new StoreKey(OP_SYS_NOEXPAND, 0), new PropertyPredicateAPI(PROP_SYS_NOEXPAND,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_SETP));
        DEFAULT.add(new StoreKey(OP_SYS_NOMACRO, 0), new PropertyPredicateAPI(PROP_SYS_NOMACRO,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_SETP));
        DEFAULT.add(new StoreKey(OP_META_PREDICATE, 1), new PropertyPredicateAPI(PROP_META_PREDICATE,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_META));
        DEFAULT.add(new StoreKey(OP_META_FUNCTION, 1), new PropertyPredicateAPI(PROP_META_FUNCTION,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_META));
        DEFAULT.add(new StoreKey(OP_SYS_TABLED, 0), new PropertyPredicateAPI(PROP_SYS_TABLED));
        DEFAULT.add(new StoreKey(OP_SYS_READWRITE_LOCK, 1), new PropertyPredicateAPI(PROP_SYS_READWRITE_LOCK));
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
            case PROP_SYS_META_PREDICATE:
                ListArray<Object> res = PropertyPredicate.filterDefs(pick,
                        Predicate.MASK_TRCK_PRED, en);
                if (res == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return PropertyPredicate.snapshotToVals(
                        new SkelAtom(OP_SYS_META_PREDICATE), res);
            case PROP_SYS_META_FUNCTION:
                res = PropertyPredicate.filterDefs(pick,
                        Predicate.MASK_TRCK_FUNC, en);
                if (res == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return PropertyPredicate.snapshotToVals(
                        new SkelAtom(OP_SYS_META_FUNCTION), res);
            case PROP_SYS_NOEXPAND:
                if ((pick.getBits() & Predicate.MASK_PRED_NOEX) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_NOEXPAND)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_NOMACRO:
                if ((pick.getBits() & Predicate.MASK_PRED_NOMC) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_NOMACRO)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_META_PREDICATE:
                Object t = pick.meta_predicate;
                if (t != null) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(OP_META_PREDICATE), t), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_META_FUNCTION:
                t = pick.meta_function;
                if (t != null) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(OP_META_FUNCTION), t), Display.DISPLAY_CONST)};
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
                t = getPredicateLock(pick, en);
                if (t != null) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(OP_SYS_READWRITE_LOCK), t), Display.DISPLAY_CONST)};
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
            case PROP_SYS_META_PREDICATE:
                AbstractSource src = PropertyPredicate.derefAndCastDef(m, d, OP_SYS_META_PREDICATE, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.addDef(src, Predicate.MASK_TRCK_PRED, en);
                return true;
            case PROP_SYS_META_FUNCTION:
                src = PropertyPredicate.derefAndCastDef(m, d, OP_SYS_META_FUNCTION, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.addDef(src, Predicate.MASK_TRCK_FUNC, en);
                return true;
            case PROP_SYS_NOEXPAND:
                pick.setBit(Predicate.MASK_PRED_NOEX);
                return true;
            case PROP_SYS_NOMACRO:
                pick.setBit(Predicate.MASK_PRED_NOMC);
                return true;
            case PROP_META_PREDICATE:
                pick.meta_predicate = PropertyPredicateAPI.derefAndCastMeta(pick,
                        m, d, OP_META_PREDICATE, en);
                return true;
            case PROP_META_FUNCTION:
                pick.meta_function = PropertyPredicateAPI.derefAndCastMeta(pick,
                        m, d, OP_META_FUNCTION, en);
                return true;
            case PROP_SYS_TABLED:
                pick.setBit(Predicate.MASK_PRED_TABL);
                return true;
            case PROP_SYS_READWRITE_LOCK:
                /* can't modify */
                return false;
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
            case PROP_SYS_META_PREDICATE:
                AbstractSource src = PropertyPredicate.derefAndCastDef(m, d, OP_SYS_META_PREDICATE, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.removeDef(src, Predicate.MASK_TRCK_PRED);
                return true;
            case PROP_SYS_META_FUNCTION:
                src = PropertyPredicate.derefAndCastDef(m, d, OP_SYS_META_FUNCTION, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.removeDef(src, Predicate.MASK_TRCK_FUNC);
                return true;
            case PROP_SYS_NOEXPAND:
                pick.resetBit(Predicate.MASK_PRED_NOEX);
                return true;
            case PROP_SYS_NOMACRO:
                pick.resetBit(Predicate.MASK_PRED_NOMC);
                return true;
            case PROP_META_PREDICATE:
                pick.meta_predicate = null;
                return true;
            case PROP_META_FUNCTION:
                pick.meta_function = null;
                return true;
            case PROP_SYS_TABLED:
                pick.resetBit(Predicate.MASK_PRED_TABL);
                return true;
            case PROP_SYS_READWRITE_LOCK:
                /* can't modify */
                return false;
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
                            res = new ListArray<Predicate>();
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
            if (id < PROP_SYS_META_PREDICATE || id > PROP_SYS_READWRITE_LOCK)
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
     * @param op   The meat name.
     * @param en   The engine.
     * @return The meta term.
     * @throws EngineMessage Shit happens.
     */
    private static Object derefAndCastMeta(Predicate pick, Object m, Display d,
                                           String op, Engine en)
            throws EngineMessage {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 1 &&
                ((SkelCompound) m).sym.fun.equals(op)) {
            m = ((SkelCompound) m).args[0];
            return Predicate.checkMetaSpez(pick, m, d, en);
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

}