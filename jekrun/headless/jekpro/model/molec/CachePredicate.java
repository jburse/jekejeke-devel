package jekpro.model.molec;

import jekpro.model.builtin.Branch;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.AbstractStore;
import jekpro.model.pretty.SourceLocal;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.LoadForce;
import jekpro.model.rope.LoadOpts;
import jekpro.reference.bootload.ForeignPath;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.tools.term.SkelAtom;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.wire.AbstractLivestock;

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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class CachePredicate extends AbstractCache {
    public static final int MASK_PRED_VISI = 0x00000001;
    public static final int MASK_PRED_STBL = 0x00000002;

    public Predicate pick;
    AbstractSource base;
    Object basevers;
    public int flags;

    /************************************************************************/
    /* Lookup Utilities                                                     */
    /************************************************************************/

    /**
     * <p>Retrieve the lookup base.</p>
     *
     * @param mod   The module.
     * @param scope The call-site, non-null.
     * @param en    The engine.
     * @return The lookup base, or null.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static AbstractSource lookupBase(String mod,
                                            AbstractSource scope,
                                            Engine en)
            throws EngineMessage, EngineException {
        if (Branch.OP_USER.equals(mod))
            return scope.getStore().user;

        LoadOpts opts = new LoadOpts();
        opts.setFlags(opts.getFlags() | LoadOpts.MASK_LOAD_COND);
        opts.setFlags(opts.getFlags() | LoadForce.MASK_LOAD_AUTO);
        en.enginecopy = null;
        en.enginewrap = null;
        mod = mod.replace(CachePackage.OP_CHAR_SEG, SourceLocal.OP_CHAR_OS);
        String key = Engine.findKey(mod, scope, ForeignPath.MASK_MODL_AUTO);

        if (key == null)
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_SOURCE_SINK,
                    new SkelAtom(mod)));

        return opts.makeLoad(scope, key, en);
    }

    /**
     * <p>Check whether the found predicate is a stable one.</p>
     *
     * @param fun  The predicate name.
     * @param base The base.
     * @param pick The predicate.
     * @return True if the predicate is stable, otherwise false.
     */
    private static boolean isStable(String fun,
                                    AbstractSource base,
                                    Predicate pick) {
        String n;
        if (!CacheFunctor.isQuali(fun)) {
            n = fun;
        } else {
            n = CacheFunctor.sepName(fun);
        }
        String s = base.getFullName();
        if (s == null) {
            /* check pred */
            return pick.getFun().equals(n);
        } else {
            s = CacheFunctor.composeQuali(s, n);
            /* check name%pred */
            return pick.getFun().equals(s);
        }
    }

    /*********************************************************************/
    /* Lookup Predicate                                                  */
    /*********************************************************************/

    /**
     * <p>Resolve a name with import.</p>
     *
     * @param fun   The predicate name.
     * @param arity The predicate length.
     * @param scope The call-site, non null.
     * @param base  The lookup base.
     * @return The resolved name.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Predicate performLookup(String fun, int arity,
                                           AbstractSource scope,
                                           AbstractSource base)
            throws EngineMessage, InterruptedException {
        String n;
        boolean f = CacheFunctor.isQuali(fun);
        if (!f) {
            n = fun;
        } else {
            n = CacheFunctor.sepName(fun);
        }
        String s = base.getFullName();
        StoreKey sk;
        MapEntry<AbstractSource, Integer>[] deps2;
        if (s == null) {
            sk = new StoreKey(n, arity);
            /* find pred */
            Predicate pick = (f ? getRoutineUser(sk, scope.getStore()) : null);
            if (pick != null)
                return pick;
            deps2 = base.snapshotDeps();
        } else {
            s = CacheFunctor.composeQuali(s, n);
            sk = new StoreKey(s, arity);
            /* wait for complete source */
            if (!base.getRead().attempt(base.getStore().foyer.timeout))
                throw new EngineMessage(EngineMessage.systemError(
                        EngineMessage.OP_SYSTEM_DEADLOCK_TIMEOUT));
            try {
                /* find name%pred */
                Predicate pick = base.getRoutine(sk);
                if (pick != null)
                    return pick;
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().release();
            }
        }
        Predicate pick = performDependent(n, arity, base, deps2, f);
        if (pick != null)
            return pick;
        if (s == null) {
            /* find pred */
            return (!f ? getRoutineUser(sk, scope.getStore()) : null);
        } else {
            /* find pred */
            sk = new StoreKey(n, arity);
            return getRoutineUser(sk, scope.getStore());
        }
    }

    /**
     * <p>Resolve a name without import and created.</p>
     *
     * @param fun    The predicate name.
     * @param arity  The predicate length.
     * @param scope  The call-site, non null.
     * @param base   The base.
     * @param create The create flag.
     * @return The resolved name.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Predicate performLookupDefined(String fun, int arity,
                                                  AbstractSource scope,
                                                  AbstractSource base,
                                                  boolean create)
            throws InterruptedException, EngineMessage {
        String n;
        if (!CacheFunctor.isQuali(fun)) {
            n = fun;
        } else {
            n = CacheFunctor.sepName(fun);
        }
        String s = base.getFullName();
        if (s == null) {
            StoreKey sk = new StoreKey(n, arity);
            return (create ?
                    defineRoutineUser(sk, scope, scope.getStore()) :
                    getRoutineUser(sk, scope.getStore()));
        } else {
            s = CacheFunctor.composeQuali(s, n);
            StoreKey sk = new StoreKey(s, arity);
            /* wait for complete source */
            if (!base.getRead().attempt(base.getStore().foyer.timeout))
                throw new EngineMessage(EngineMessage.systemError(
                        EngineMessage.OP_SYSTEM_DEADLOCK_TIMEOUT));
            try {
                /* create name%pred */
                return (create ?
                        base.defineRoutine(sk, scope) :
                        base.getRoutine(sk));
            } finally {
                base.getRead().release();
            }
        }
    }

    /**
     * <p>Determine the predicate that this predicate overrides.</p>
     *
     * @param fun   The predicate name.
     * @param arity The predicate length.
     * @param scope The call-site, not null.
     * @param base  The lookup base.
     * @return The predicate that is overridden, or null.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    public static Predicate performOverrides(String fun, int arity,
                                             AbstractSource scope,
                                             AbstractSource base)
            throws EngineMessage, InterruptedException {
        String n;
        boolean f = CacheFunctor.isQuali(fun);
        if (!f) {
            n = fun;
        } else {
            n = CacheFunctor.sepName(fun);
        }
        String s = base.getFullName();
        MapEntry<AbstractSource, Integer>[] deps2;
        if (s == null) {
            /* find dependent pred */
            deps2 = base.snapshotDeps();
        } else {
            /* wait for complete source */
            if (!base.getRead().attempt(base.getStore().foyer.timeout))
                throw new EngineMessage(EngineMessage.systemError(
                        EngineMessage.OP_SYSTEM_DEADLOCK_TIMEOUT));
            try {
                /* find dependent pred */
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().release();
            }
        }
        Predicate pick = performDependent(n, arity, base, deps2, f);
        if (pick != null)
            return pick;
        if (s == null) {
            return null;
        } else {
            /* find pred */
            StoreKey sk = new StoreKey(n, arity);
            return getRoutineUser(sk, scope.getStore());
        }
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
     * @param fun     The predicate name.
     * @param arity   The predicate length.
     * @param src     The call-site.
     * @param deps    The deps.
     * @param visited The visited sources.
     * @return The resolved predicate or null.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Predicate performImported(String fun, int arity,
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
            if (!base.getRead().attempt(base.getStore().foyer.timeout))
                throw new EngineMessage(EngineMessage.systemError(
                        EngineMessage.OP_SYSTEM_DEADLOCK_TIMEOUT));
            try {
                String s = base.getFullName();
                if (s == null)
                    continue;
                s = CacheFunctor.composeQuali(s, fun);
                StoreKey sk = new StoreKey(s, arity);
                Predicate pick = base.getRoutine(sk);
                if (pick != null && pick.visiblePred(src))
                    return pick;
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().release();
            }
            visited.add(base);
            Predicate pick = performReexported(fun, arity, base, deps2, visited);
            if (pick != null)
                return pick;
        }
        return null;
    }

    /**
     * <p>Lookup the reexported predicates.</p>
     *
     * @param fun     The predicate name.
     * @param arity   The predicate length.
     * @param src     The call-site.
     * @param deps    The deps.
     * @param visited The visited sources.
     * @return The resolved name.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Predicate performReexported(String fun, int arity,
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
            if (!base.getRead().attempt(base.getStore().foyer.timeout))
                throw new EngineMessage(EngineMessage.systemError(
                        EngineMessage.OP_SYSTEM_DEADLOCK_TIMEOUT));
            try {
                String s = base.getFullName();
                if (s == null)
                    continue;
                s = CacheFunctor.composeQuali(s, fun);
                StoreKey sk = new StoreKey(s, arity);
                Predicate pick = base.getRoutine(sk);
                if (pick != null && pick.visiblePred(src))
                    return pick;
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().release();
            }
            visited.add(base);
            Predicate pick = performReexported(fun, arity, base, deps2, visited);
            if (pick != null)
                return pick;
        }
        return null;
    }

    /**
     * <p>Lookup the parented predicates.</p>
     *
     * @param fun     The predicate name.
     * @param arity   The predicate length.
     * @param src     The call-site.
     * @param deps    The deps.
     * @param visited The visited sources.
     * @return The resolved predicate or null.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Predicate performParent(String fun, int arity,
                                           AbstractSource src,
                                           MapEntry<AbstractSource, Integer>[] deps,
                                           ListArray<AbstractSource> visited)
            throws InterruptedException, EngineMessage {
        for (int i = 0; i < deps.length; i++) {
            MapEntry<AbstractSource, Integer> dep = deps[i];
            if ((dep.value.intValue() & AbstractSource.MASK_IMPT_PARM) == 0)
                continue;
            AbstractSource base = dep.key;
            if (visited.contains(base))
                continue;
            MapEntry<AbstractSource, Integer>[] deps2;
            /* wait for complete source */
            if (!base.getRead().attempt(base.getStore().foyer.timeout))
                throw new EngineMessage(EngineMessage.systemError(
                        EngineMessage.OP_SYSTEM_DEADLOCK_TIMEOUT));
            try {
                String s = base.getFullName();
                if (s == null)
                    continue;
                s = CacheFunctor.composeQuali(s, fun);
                StoreKey sk = new StoreKey(s, arity);
                Predicate pick = base.getRoutine(sk);
                if (pick != null && pick.visiblePred(src))
                    return pick;
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().release();
            }
            visited.add(base);
            Predicate pick = performImported(fun, arity, base, deps2, visited);
            if (pick != null)
                return pick;
            pick = performParent(fun, arity, base, deps2, visited);
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
                    AbstractSource base = (CacheFunctor.isQuali(sa.fun) ? lookupBase(
                            CacheFunctor.sepModule(sa.fun), src, en) : src);
                    Object basevers = base.importvers;
                    Predicate pick = performLookup(sa.fun, arity, src, base);
                    /* cache if found */
                    CachePredicate cp;
                    if (pick != null && (pick.getBits() & Predicate.MASK_PRED_RMOV) == 0) {
                        cp = new CachePredicate();
                        cp.pick = pick;
                        cp.base = base;
                        int flags = 0;
                        if (pick.visiblePred(src))
                            flags |= MASK_PRED_VISI;
                        if (pick.getUsage(src) != null && isStable(sa.fun, base, pick))
                            flags |= MASK_PRED_STBL;
                        cp.flags = flags;
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
                            AbstractSource base = (CacheFunctor.isQuali(sa.fun) ? lookupBase(
                                    CacheFunctor.sepModule(sa.fun), src, en) : src);
                            Object basevers = base.importvers;
                            pick = performLookup(sa.fun, arity, src, base);
                            /* update if found, otherwise remove */
                            if (pick != null && (pick.getBits() & Predicate.MASK_PRED_RMOV) == 0) {
                                cp.pick = pick;
                                cp.base = base;
                                int flags = 0;
                                if (pick.visiblePred(src))
                                    flags |= MASK_PRED_VISI;
                                if (pick.getUsage(src) != null && isStable(sa.fun, base, pick))
                                    flags |= MASK_PRED_STBL;
                                cp.flags = flags;
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
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        }
    }

    /**
     * <p>Lookup, create and cache a predicate of the given length.</p>
     *
     * @param sa     The atom skeleton.
     * @param arity  The length.
     * @param en     The engine.
     * @param create The create flag.
     * @return The predicate.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static CachePredicate getPredicateDefined(SkelAtom sa, int arity,
                                                     Engine en, boolean create)
            throws EngineMessage, EngineException {
        try {
            AbstractCache back = null;
            AbstractCache temp = sa.cache;
            for (; ; ) {
                if (temp == null) {
                    /* cache miss, so lookup */
                    AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
                    AbstractSource base = (CacheFunctor.isQuali(sa.fun) ? lookupBase(
                            CacheFunctor.sepModule(sa.fun), src, en) : src);
                    Object basevers = base.importvers;
                    Predicate pick = performLookupDefined(sa.fun, arity, src, base, create);
                    CachePredicate cp;
                    if (pick != null && (pick.getBits() & Predicate.MASK_PRED_RMOV) == 0) {
                        cp = new CachePredicate();
                        cp.pick = pick;
                        cp.base = base;
                        int flags = MASK_PRED_STBL;
                        if (pick.visiblePred(src))
                            flags |= MASK_PRED_VISI;
                        cp.flags = flags;
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
                    if (create && (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0))
                        throw new EngineMessage(EngineMessage.permissionError(
                                EngineMessage.OP_PERMISSION_MODIFY,
                                EngineMessage.OP_PERMISSION_PROCEDURE,
                                SpecialQuali.indicatorToColonSkel(sa, arity, en)));
                    return cp;
                }
                if (temp instanceof CachePredicate) {
                    CachePredicate cp = (CachePredicate) temp;
                    Predicate pick = cp.pick;
                    if (pick.getArity() == arity) {
                        if (cp.basevers != cp.base.importvers || (cp.flags & MASK_PRED_STBL) == 0) {
                            /* cache invalidated or instable, so lookup */
                            AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
                            AbstractSource base = (CacheFunctor.isQuali(sa.fun) ? lookupBase(
                                    CacheFunctor.sepModule(sa.fun), src, en) : src);
                            Object basevers = base.importvers;
                            pick = performLookupDefined(sa.fun, arity, src, base, create);
                            /* update if found, otherwise remove */
                            if (pick != null && (pick.getBits() & Predicate.MASK_PRED_RMOV) == 0) {
                                cp.pick = pick;
                                cp.base = base;
                                int flags = MASK_PRED_STBL;
                                if (pick.visiblePred(src))
                                    flags |= MASK_PRED_VISI;
                                cp.flags = flags;
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
                        if (create && (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0))
                            throw new EngineMessage(EngineMessage.permissionError(
                                    EngineMessage.OP_PERMISSION_MODIFY,
                                    EngineMessage.OP_PERMISSION_PROCEDURE,
                                    SpecialQuali.indicatorToColonSkel(sa, arity, en)));
                        return cp;
                    }
                }
                back = temp;
                temp = back.next;
            }
        } catch (InterruptedException x) {
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        }
    }

    /*******************************************************************/
    /* Operator User Lookup                                            */
    /*******************************************************************/

    /**
     * <p>Retrieve predicate for the given store key.</p>
     *
     * @param sk    The store key.
     * @param store The store, can be null.
     * @return The predicate or null.
     */
    public static Predicate getRoutineUser(StoreKey sk,
                                           AbstractStore store) {
        while (store != null) {
            Predicate pick = store.user.getRoutine(sk);
            if (pick != null)
                return pick;
            store = store.parent;
        }
        return null;
    }

    /**
     * <p>Define a predicate.</p>
     *
     * @param sk    The store key.
     * @param scope The call-site, not nulll.
     * @param store The store.
     * @return The predicate.
     */
    public static Predicate defineRoutineUser(StoreKey sk,
                                              AbstractSource scope,
                                              AbstractStore store) {
        Predicate pick = getRoutineUser(sk, store.parent);
        if (pick == null)
            pick = store.user.checkRoutine(sk, scope);
        pick.defineUsage(scope);
        return pick;
    }

}
