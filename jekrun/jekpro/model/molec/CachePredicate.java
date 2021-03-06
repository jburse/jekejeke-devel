package jekpro.model.molec;

import jekpro.frequent.system.ForeignThread;
import jekpro.model.builtin.Branch;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Store;
import jekpro.reference.reflect.SpecialPred;
import jekpro.tools.term.SkelAtom;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;

import java.util.concurrent.TimeUnit;

/**
 * <p>The polymorphic cache for the qualified predicates.</p>
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
public final class CachePredicate extends AbstractCache {
    public static final int MASK_PRED_VISI = 0x00000001;
    public static final int MASK_PRED_STBL = 0x00000002;

    public static final int MASK_CACH_CRTE = 0x00000001;
    public static final int MASK_CACH_NSTS = 0x00000002;
    public static final int MASK_CACH_LOCA = 0x00000004;
    public static final int MASK_CACH_UCHK = 0x00000008;

    /* combined flags */
    public static final int MASK_CACH_DEFI = MASK_CACH_CRTE | MASK_CACH_LOCA;

    public Predicate pick;
    public int flags;
    AbstractSource base;
    Object basevers;

    /*********************************************************************/
    /* Lookup Predicates                                                 */
    /*********************************************************************/

    /**
     * <p>Find the base of a predicate name.</p>
     *
     * @param sa  The predicate name.
     * @param src The call-site.
     * @param en  The engine.
     * @return The base.
     */
    public static AbstractSource performBase(SkelAtom sa,
                                             AbstractSource src, Engine en)
            throws EngineException, EngineMessage {
        if (sa instanceof SkelAtomQuali) {
            sa = ((SkelAtomQuali) sa).getModule();
            return CacheSubclass.lookupKey(sa.fun, sa.scope, en);
        } else {
            return src;
        }
    }

    /**
     * <p>Resolve a name with import.</p>
     *
     * @param n     The predicate name.
     * @param arity The predicate length.
     * @param base  The lookup base.
     * @param f     The qualiied flag.
     * @return The resolved name.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Predicate performLookup(String n, int arity,
                                           AbstractSource base, boolean f)
            throws EngineMessage, InterruptedException {
        MapEntry<AbstractSource, Integer>[] deps2;
        /* wait for complete source */
        if (!base.getRead().tryLock(base.getStore().foyer.timeout, TimeUnit.MILLISECONDS))
            throw new EngineMessage(EngineMessage.limitError(
                    EngineMessage.OP_LIMIT_DEADLOCK_TIMEOUT));
        try {
            if (!Branch.OP_USER.equals(base.getFullName())) {
                Predicate pick = base.getRoutine(arity, n);
                if (pick != null)
                    return pick;
            } else if (f) {
                Predicate pick = getRoutineUser(arity, n, base.getStore());
                if (pick != null)
                    return pick;
            }
            deps2 = base.snapshotDeps();
        } finally {
            base.getRead().unlock();
        }
        Predicate pick = performDependent(n, arity, base, deps2, f);
        if (pick != null)
            return pick;
        if (!Branch.OP_USER.equals(base.getFullName()) || !f)
            return getRoutineUser(arity, n, base.getStore());
        return null;
    }

    /**
     * <p>Resolve a name without import and created.</p>
     *
     * @param sa    The predicate name.
     * @param arity The predicate length.
     * @param src   The call-site, non null.
     * @param base  The base.
     * @param en    The engine.
     * @param copt  The create flag.
     * @return The resolved name.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Predicate performLookupDefined(SkelAtom sa, int arity,
                                                  AbstractSource src,
                                                  AbstractSource base,
                                                  Engine en, int copt)
            throws InterruptedException, EngineMessage {
        /* wait for complete source */
        if (!base.getRead().tryLock(base.getStore().foyer.timeout, TimeUnit.MILLISECONDS))
            throw new EngineMessage(EngineMessage.limitError(
                    EngineMessage.OP_LIMIT_DEADLOCK_TIMEOUT));
        try {
            if (!Branch.OP_USER.equals(base.getFullName())) {
                /* create name%pred */
                return ((copt & CachePredicate.MASK_CACH_CRTE) != 0 ?
                        base.defineRoutine(arity, sa.fun, sa, copt, en) :
                        base.getRoutine(arity, sa.fun));
            } else {
                /* create pred */
                return ((copt & CachePredicate.MASK_CACH_CRTE) != 0 ?
                        defineRoutineUser(arity, sa.fun, sa, src.getStore(), en, copt) :
                        getRoutineUser(arity, sa.fun, src.getStore()));
            }
        } finally {
            base.getRead().unlock();
        }
    }

    /**
     * <p>Determine the predicate that this predicate overrides.</p>
     *
     * @param sa    The predicate name.
     * @param arity The predicate length.
     * @param base  The lookup base.
     * @return The predicate that is overridden, or null.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    public static Predicate performOverrides(SkelAtom sa, int arity,
                                             AbstractSource base)
            throws EngineMessage, InterruptedException {
        String n = sa.fun;
        boolean f = (sa instanceof SkelAtomQuali);
        MapEntry<AbstractSource, Integer>[] deps2;
        String s;
        /* wait for complete source */
        if (!base.getRead().tryLock(base.getStore().foyer.timeout, TimeUnit.MILLISECONDS))
            throw new EngineMessage(EngineMessage.limitError(
                    EngineMessage.OP_LIMIT_DEADLOCK_TIMEOUT));
        try {
            s = base.getFullName();
            deps2 = base.snapshotDeps();
        } finally {
            base.getRead().unlock();
        }
        Predicate pick = performDependent(n, arity, base, deps2, true);
        if (pick != null)
            return pick;
        if (!Branch.OP_USER.equals(s))
            return getRoutineUser(arity, n, base.getStore());
        return null;
    }

    /*****************************************************/
    /* Lookup Dependency                                 */
    /*****************************************************/

    /**
     * <p>Lookup the dependent predicates.</p>
     *
     * @param n     The predicate name.
     * @param arity The predicate length.
     * @param base  The lookup base, non-null
     * @param deps2 The deps.
     * @param f     The qualiied flag.
     * @return The resolved predicate or null.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Predicate performDependent(String n, int arity,
                                              AbstractSource base,
                                              MapEntry<AbstractSource, Integer>[] deps2,
                                              boolean f)
            throws EngineMessage, InterruptedException {
        ListArray visited = new ListArray<AbstractCache>();
        visited.add(base);
        if (f) {
            Predicate pick = performReexported(n, arity, base, deps2, visited);
            if (pick != null)
                return pick;
        } else {
            Predicate pick = performImported(n, arity, base, deps2, visited);
            if (pick != null)
                return pick;
            pick = performParent(n, arity, base, deps2, visited);
            if (pick != null)
                return pick;
        }
        return null;
    }

    /**
     * <p>Lookup the imported predicates.</p>
     *
     * @param n       The predicate name.
     * @param arity   The predicate length.
     * @param src     The call-site, non null.
     * @param deps    The deps.
     * @param visited The visited sources.
     * @return The resolved predicate or null.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Predicate performImported(String n, int arity,
                                             AbstractSource src,
                                             MapEntry<AbstractSource, Integer>[] deps,
                                             ListArray<AbstractSource> visited)
            throws InterruptedException, EngineMessage {
        for (int i = 0; i < deps.length; i++) {
            MapEntry<AbstractSource, Integer> dep = deps[i];
            if ((dep.value.intValue() & AbstractSource.MASK_IMPT_MODL) == 0)
                continue;
            AbstractSource base = dep.key;

            if (visited.contains(base))
                continue;
            MapEntry<AbstractSource, Integer>[] deps2;
            /* wait for complete source */
            if (!base.getRead().tryLock(base.getStore().foyer.timeout, TimeUnit.MILLISECONDS))
                throw new EngineMessage(EngineMessage.limitError(
                        EngineMessage.OP_LIMIT_DEADLOCK_TIMEOUT));
            try {
                String s = base.getFullName();
                if (!Branch.OP_USER.equals(s)) {
                    Predicate pick = base.getRoutine(arity, n);
                    if (pick != null && CachePredicate.visiblePred(pick, src))
                        return pick;
                }
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().unlock();
            }
            visited.add(base);
            Predicate pick = performReexported(n, arity, base, deps2, visited);
            if (pick != null)
                return pick;
        }
        return null;
    }

    /**
     * <p>Lookup the reexported predicates.</p>
     *
     * @param n       The predicate name.
     * @param arity   The predicate length.
     * @param src     The call-site, non null.
     * @param deps    The deps.
     * @param visited The visited sources.
     * @return The resolved name.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Predicate performReexported(String n, int arity,
                                               AbstractSource src,
                                               MapEntry<AbstractSource, Integer>[] deps,
                                               ListArray<AbstractSource> visited)
            throws InterruptedException, EngineMessage {
        for (int i = 0; i < deps.length; i++) {
            MapEntry<AbstractSource, Integer> dep = deps[i];
            if ((dep.value.intValue() & AbstractSource.MASK_IMPT_REEX) == 0)
                continue;
            AbstractSource base = dep.key;
            if (visited.contains(base))
                continue;
            MapEntry<AbstractSource, Integer>[] deps2;
            /* wait for complete source */
            if (!base.getRead().tryLock(base.getStore().foyer.timeout, TimeUnit.MILLISECONDS))
                throw new EngineMessage(EngineMessage.limitError(
                        EngineMessage.OP_LIMIT_DEADLOCK_TIMEOUT));
            try {
                String s = base.getFullName();
                if (!Branch.OP_USER.equals(s)) {
                    Predicate pick = base.getRoutine(arity, n);
                    if (pick != null && CachePredicate.visiblePred(pick, src))
                        return pick;
                }
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().unlock();
            }
            visited.add(base);
            Predicate pick = performReexported(n, arity, base, deps2, visited);
            if (pick != null)
                return pick;
        }
        return null;
    }

    /**
     * <p>Lookup the parented predicates.</p>
     *
     * @param n       The predicate name.
     * @param arity   The predicate length.
     * @param src     The call-site, non null.
     * @param deps    The deps.
     * @param visited The visited sources.
     * @return The resolved predicate or null.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Predicate performParent(String n, int arity,
                                           AbstractSource src,
                                           MapEntry<AbstractSource, Integer>[] deps,
                                           ListArray<AbstractSource> visited)
            throws InterruptedException, EngineMessage {
        for (int i = 0; i < deps.length; i++) {
            MapEntry<AbstractSource, Integer> dep = deps[i];
            if ((dep.value.intValue() & AbstractSource.MASK_IMPT_PAIM) == 0)
                continue;
            AbstractSource base = dep.key;
            if (visited.contains(base))
                continue;
            MapEntry<AbstractSource, Integer>[] deps2;
            /* wait for complete source */
            if (!base.getRead().tryLock(base.getStore().foyer.timeout, TimeUnit.MILLISECONDS))
                throw new EngineMessage(EngineMessage.limitError(
                        EngineMessage.OP_LIMIT_DEADLOCK_TIMEOUT));
            try {
                String s = base.getFullName();
                if (!Branch.OP_USER.equals(s)) {
                    Predicate pick = base.getRoutine(arity, n);
                    if (pick != null && CachePredicate.visiblePred(pick, src))
                        return pick;
                }
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().unlock();
            }
            visited.add(base);
            Predicate pick = performImported(n, arity, base, deps2, visited);
            if (pick != null)
                return pick;
            pick = performParent(n, arity, base, deps2, visited);
            if (pick != null)
                return pick;
        }
        return null;
    }

    /*******************************************************************/
    /* Main Entries                                                    */
    /*******************************************************************/

    /**
     * <p>Lookup and cache a predicate of the given length.</p>
     *
     * @param sa    The atom skeleton.
     * @param arity The length.
     * @param en    The engine.
     * @return The predicate.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static CachePredicate getPredicate(SkelAtom sa, int arity,
                                              Engine en)
            throws EngineMessage, EngineException {
        try {
            AbstractCache back = null;
            AbstractCache temp = sa.cache;
            for (; ; ) {
                if (temp == null) {
                    /* cache miss, so lookup */
                    AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
                    AbstractSource base = performBase(sa, src, en);
                    Object basevers = base.importvers;
                    Predicate pick = performLookup(sa.fun, arity, base, sa instanceof SkelAtomQuali);
                    /* cache if found */
                    CachePredicate cp;
                    if (pick != null) {
                        int flags = 0;
                        if (CachePredicate.visiblePred(pick, src))
                            flags |= MASK_PRED_VISI;
                        cp = new CachePredicate();
                        cp.pick = pick;
                        cp.flags = flags;
                        cp.base = base;
                        cp.basevers = basevers;
                        /* add to cache */
                        if (back == null) {
                            sa.cache = cp;
                        } else {
                            back.next = cp;
                        }
                    } else {
                        cp = null;
                    }
                    return cp;
                }
                if (temp instanceof CachePredicate) {
                    CachePredicate cp = (CachePredicate) temp;
                    Predicate pick = cp.pick;
                    if (pick.getArity() == arity) {
                        if (cp.basevers != cp.base.importvers) {
                            /* cache invalidated, so lookup */
                            AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
                            AbstractSource base = performBase(sa, src, en);
                            Object basevers = base.importvers;
                            pick = performLookup(sa.fun, arity, base, sa instanceof SkelAtomQuali);
                            /* update if found, otherwise remove */
                            if (pick != null) {
                                int flags = 0;
                                if (CachePredicate.visiblePred(pick, src))
                                    flags |= MASK_PRED_VISI;
                                cp.pick = pick;
                                cp.flags = flags;
                                cp.base = base;
                                cp.basevers = basevers;
                            } else {
                                /* remove from cache */
                                if (back == null) {
                                    sa.cache = cp.next;
                                } else {
                                    back.next = cp.next;
                                }
                                cp = null;
                            }
                        }
                        return cp;
                    }
                }
                back = temp;
                temp = back.next;
            }
        } catch (InterruptedException x) {
            throw (EngineMessage) ForeignThread.sysThreadClear();
        }
    }

    /**
     * <p>Lookup, create and cache a predicate of the given length.</p>
     *
     * @param sa    The atom skeleton.
     * @param arity The length.
     * @param en    The engine.
     * @param copt  The create flag.
     * @return The predicate.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static CachePredicate getPredicateDefined(SkelAtom sa, int arity,
                                                     Engine en, int copt)
            throws EngineMessage, EngineException {
        try {
            AbstractCache back = null;
            AbstractCache temp = sa.cache;
            for (; ; ) {
                if (temp == null) {
                    /* cache miss, so lookup */
                    AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
                    AbstractSource base = performBase(sa, src, en);
                    Object basevers = base.importvers;
                    Predicate pick = performLookupDefined(sa, arity,
                            src, base, en, copt);
                    /* cache if found */
                    CachePredicate cp;
                    if (pick != null) {
                        int flags = 0;
                        if (CachePredicate.visiblePred(pick, src))
                            flags |= MASK_PRED_VISI;
                        flags |= MASK_PRED_STBL;
                        cp = new CachePredicate();
                        cp.pick = pick;
                        cp.flags = flags;
                        cp.base = base;
                        cp.basevers = basevers;
                        /* add to cache */
                        if (back == null) {
                            sa.cache = cp;
                        } else {
                            back.next = cp;
                        }
                    } else {
                        cp = null;
                    }
                    if ((copt & CachePredicate.MASK_CACH_CRTE) != 0 &&
                            (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0))
                        throw new EngineMessage(EngineMessage.permissionError(
                                EngineMessage.OP_PERMISSION_MODIFY,
                                EngineMessage.OP_PERMISSION_PROCEDURE,
                                SpecialPred.indicatorToColonSkel(sa, arity, en)));
                    return cp;
                }
                if (temp instanceof CachePredicate) {
                    CachePredicate cp = (CachePredicate) temp;
                    Predicate pick = cp.pick;
                    if (pick.getArity() == arity) {
                        if (cp.basevers != cp.base.importvers || (cp.flags & MASK_PRED_STBL) == 0) {
                            /* cache invalidated or instable, so lookup */
                            AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
                            AbstractSource base = performBase(sa, src, en);
                            Object basevers = base.importvers;
                            pick = performLookupDefined(sa, arity,
                                    src, base, en, copt);
                            /* update if found, otherwise remove */
                            if (pick != null) {
                                int flags = 0;
                                if (CachePredicate.visiblePred(pick, src))
                                    flags |= MASK_PRED_VISI;
                                flags |= MASK_PRED_STBL;
                                cp.pick = pick;
                                cp.flags = flags;
                                cp.base = base;
                                cp.basevers = basevers;
                            } else {
                                /* remove from cache */
                                if (back == null) {
                                    sa.cache = cp.next;
                                } else {
                                    back.next = cp.next;
                                }
                                cp = null;
                            }
                        }
                        if ((copt & CachePredicate.MASK_CACH_CRTE) != 0 &&
                                (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0))
                            throw new EngineMessage(EngineMessage.permissionError(
                                    EngineMessage.OP_PERMISSION_MODIFY,
                                    EngineMessage.OP_PERMISSION_PROCEDURE,
                                    SpecialPred.indicatorToColonSkel(sa, arity, en)));
                        return cp;
                    }
                }
                back = temp;
                temp = back.next;
            }
        } catch (InterruptedException x) {
            throw (EngineMessage) ForeignThread.sysThreadClear();
        }
    }

    /*******************************************************************/
    /* Operator User Lookup                                            */
    /*******************************************************************/

    /**
     * <p>Retrieve predicate for the given store key.</p>
     *
     * @param arity The arity.
     * @param fun   The name.
     * @param store The store, can be null.
     * @return The predicate or null.
     */
    public static Predicate getRoutineUser(int arity, String fun,
                                           Store store) {
        while (store != null) {
            Predicate pick = store.user.getRoutine(arity, fun);
            if (pick != null)
                return pick;
            store = store.parent;
        }
        return null;
    }

    /**
     * <p>Define a user predicate.</p>
     *
     * @param arity The arity.
     * @param fun   The name.
     * @param sa    The call-site, non nulll.
     * @param store The store.
     * @param en    The engine.
     * @param copt  The create flag.
     * @return The predicate.
     * @throws EngineMessage Shit happens.
     */
    private static Predicate defineRoutineUser(int arity, String fun,
                                               SkelAtom sa, Store store,
                                               Engine en, int copt)
            throws EngineMessage {
        Predicate pick = getRoutineUser(arity, fun, store.parent);
        if (pick == null)
            pick = store.user.defineRoutine(arity, fun, sa, en);
        pick.usagePredicate(sa, en, copt);
        return pick;
    }

    /*****************************************************************/
    /* Predicate Visibility                                          */
    /*****************************************************************/

    /**
     * <p>Check whether a predicate is visible.</p>
     *
     * @param pick The predicate.
     * @param src  The call-site, non null.
     * @return True if the predicate is visible, otherwise false.
     */
    public static boolean visiblePred(Predicate pick, AbstractSource src) {
        if ((pick.getBits() & Predicate.MASK_PRED_VSPR) != 0) {
            return hasHome(pick, src);
        } else if ((pick.getBits() & Predicate.MASK_PRED_VSPU) == 0) {
            return hasPackage(pick, src);
        } else {
            return true;
        }
    }

    /**
     * <p>Check whether the predicate has a usage with the same home.</p>
     *
     * @param key The source key.
     * @return True if the predicate has such a usage, otherwise false.
     */
    private static boolean hasHome(Predicate pick, AbstractSource key) {
        MapEntry<AbstractSource, Integer>[] snapshot = pick.snapshotDefs();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractSource, Integer> entry = snapshot[i];
            if (OperatorSearch.sameHome(entry.key, key))
                return true;
        }
        return false;
    }

    /**
     * <p>Check whether the predicate has a usage with the same package.</p>
     *
     * @param key The source key.
     * @return True if the predicate has such a usage, otherwise false.
     */
    private static boolean hasPackage(Predicate pick, AbstractSource key) {
        MapEntry<AbstractSource, Integer>[] snapshot = pick.snapshotDefs();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractSource, Integer> entry = snapshot[i];
            if (OperatorSearch.samePackage(entry.key, key))
                return true;
        }
        return false;
    }

    /**************************************************************/
    /* Notify Importvers                                          */
    /**************************************************************/

    /**
     * <p>Notify the dependencies.</p>
     *
     * @param src The source that changed.
     * @param f   The importvers changes.
     */
    public static void notifyImportvers(AbstractSource src, int f) {
        if ((f & AbstractSource.MASK_IMPT_PAIM) != 0)
            CacheModule.notifyFixvers(src, ~AbstractSource.MASK_PCKG_LIBR);
        if ((f & AbstractSource.MASK_IMPT_VISI) != 0) {
            Object o = new Object();
            ListArray<AbstractSource> visited = new ListArray<>();
            if ((f & AbstractSource.MASK_IMPT_REEX) != 0)
                notifyInterface(src, o, visited);
            if ((f & AbstractSource.MASK_IMPT_INVM) != 0) {
                notifyImportversLocale(src, o);
                src.importvers = o;
            }
        }
    }

    /**
     * <p>Notify that interface has changed.</p>
     *
     * @param src     The source that changed.
     * @param o       The new importvers object.
     * @param visited The already visited sources.
     */
    private static void notifyInterface(AbstractSource src, Object o,
                                        ListArray<AbstractSource> visited) {
        visited.add(src);
        MapEntry<AbstractSource, Integer>[] depsinv = src.snapshotDepsInv();
        for (int i = 0; i < depsinv.length; i++) {
            MapEntry<AbstractSource, Integer> depinv = depsinv[i];
            if (visited.contains(depinv.key))
                continue;
            if ((depinv.value.intValue() & AbstractSource.MASK_IMPT_REEX) != 0)
                notifyInterface(depinv.key, o, visited);
            if ((depinv.value.intValue() & AbstractSource.MASK_IMPT_MODL) != 0) {
                AbstractSource src2 = depinv.key;
                notifyImportversLocale(src2, o);
                src2.importvers = o;
            }
        }
    }

    /**
     * <p>Notify that interface has changed.</p>
     *
     * @param src The source that changed.
     * @param o   The new importvers object.
     */
    private static void notifyImportversLocale(AbstractSource src, Object o) {
        MapEntry<AbstractSource, Integer>[] depsinv = src.snapshotDepsInv();
        for (int i = 0; i < depsinv.length; i++) {
            MapEntry<AbstractSource, Integer> depinv = depsinv[i];
            if ((depinv.value.intValue() & AbstractSource.MASK_IMPT_PAIM) != 0) {
                AbstractSource src2 = depinv.key;
                notifyImportversLocale(src2, o);
                src2.importvers = o;
            }
        }
    }

}
