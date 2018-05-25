package jekpro.model.molec;

import jekpro.model.inter.Engine;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.AbstractStore;
import jekpro.model.rope.Operator;
import jekpro.reference.reflect.SpecialOper;
import jekpro.tools.term.PositionKey;
import jekpro.tools.term.SkelAtom;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.wire.AbstractLivestock;

/**
 * <p>Search of qualified operators.</p>
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
public final class OperatorSearch {

    /**
     * <p>Retrieve an operator with module lookup.</p>
     *
     * @param key  The operator name.
     * @param type The operator type.
     * @param base The lookup base, or-null.
     * @return The operator or null.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Operator performLookup(String key, int type,
                                          AbstractSource scope,
                                          AbstractSource base)
            throws InterruptedException, EngineMessage {
        String n;
        boolean f = CacheFunctor.isQuali(key);
        if (!f) {
            n = key;
        } else {
            n = CacheFunctor.sepName(key);
        }
        String s = base.getFullName();
        MapEntry<AbstractSource, Integer>[] deps2;
        if (s == null) {
            /* find oper */
            Operator oper = (f ? getOperUser(type, s, scope.getStore()) : null);
            if (oper != null)
                return oper;
            deps2 = base.snapshotDeps();
        } else {
            s = CacheFunctor.composeQuali(s, n);
            /* wait for complete source */
            if (!base.getRead().attempt(base.getStore().foyer.timeout))
                throw new EngineMessage(EngineMessage.systemError(
                        EngineMessage.OP_SYSTEM_DEADLOCK_TIMEOUT));
            try {
                /* find name%oper */
                Operator oper = base.getOper(type, s);
                if (oper != null)
                    return oper;
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().release();
            }
        }
        Operator oper = performDependent(n, type, base, deps2, f);
        if (oper != null)
            return oper;
        if (s == null) {
            /* find oper */
            return (!f ? getOperUser(type, n, scope.getStore()) : null);
        } else {
            /* find oper */
            return getOperUser(type, n, scope.getStore());
        }
    }

    /**
     * <p>Define an operator with module lookup.</p>
     *
     * @param key    The operator name.
     * @param type   The operator type.
     * @param scope  The call-site, non-null.
     * @param pos    The position, can be null.
     * @param base   The lookup base, non-null.
     * @param create The no create flag.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Operator performLookupDefined(String key, int type,
                                                 AbstractSource scope,
                                                 PositionKey pos,
                                                 AbstractSource base,
                                                 boolean create)
            throws InterruptedException, EngineMessage {
        String n;
        if (!CacheFunctor.isQuali(key)) {
            n = key;
        } else {
            n = CacheFunctor.sepName(key);
        }
        String s = base.getFullName();
        if (s == null) {
            /* create oper */
            return (create ? defineOperUser(type, n, scope, pos, scope.getStore()) :
                    getOperUser(type, n, scope.getStore()));
        } else {
            s = CacheFunctor.composeQuali(s, n);
            /* wait for complete source */
            if (!base.getRead().attempt(base.getStore().foyer.timeout))
                throw new EngineMessage(EngineMessage.systemError(
                        EngineMessage.OP_SYSTEM_DEADLOCK_TIMEOUT));
            try {
                /* create name%oper */
                return (create ? base.defineOper(type, s, scope, pos) :
                        base.getOper(type, s));
            } finally {
                base.getRead().release();
            }
        }
    }

    /**
     * <p>Determine the operator that this operator overrides.</p>
     *
     * @param type  The operator type.
     * @param key   The operator name.
     * @param scope The call-site, non null.
     * @param base  The lookup base, non-null.
     * @return The operator that is overridden, or null.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    public static Operator performOverrides(int type, String key,
                                            AbstractSource scope,
                                            AbstractSource base)
            throws EngineMessage, InterruptedException {
        String n;
        boolean f = CacheFunctor.isQuali(key);
        if (!f) {
            n = key;
        } else {
            n = CacheFunctor.sepName(key);
        }
        String s = base.getFullName();
        MapEntry<AbstractSource, Integer>[] deps2;
        if (s == null) {
            /* find dependent oper */
            deps2 = base.snapshotDeps();
        } else {
            /* wait for complete source */
            if (!base.getRead().attempt(base.getStore().foyer.timeout))
                throw new EngineMessage(EngineMessage.systemError(
                        EngineMessage.OP_SYSTEM_DEADLOCK_TIMEOUT));
            try {
                /* find dependent oper */
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().release();
            }
        }
        Operator oper = performDependent(n, type, base, deps2, f);
        if (oper != null)
            return oper;
        if (s == null) {
            return null;
        } else {
            /* find oper */
            return getOperUser(type, n, scope.getStore());
        }
    }

    /*****************************************************/
    /* Lookup Dependency                                 */
    /*****************************************************/

    /**
     * <p>Lookup the dependent operator.</p>
     *
     * @param n     The operator name.
     * @param type  The operator type.
     * @param base  The base.
     * @param deps2 The deps.
     * @param f     The flag
     * @return The resolved operator or null.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Operator performDependent(String n, int type,
                                             AbstractSource base,
                                             MapEntry<AbstractSource, Integer>[] deps2,
                                             boolean f)
            throws EngineMessage, InterruptedException {
        ListArray<AbstractSource> visited = new ListArray<AbstractSource>();
        visited.add(base);
        if (f) {
            Operator oper = performReexported(n, type, base, deps2, visited);
            if (oper != null)
                return oper;
        } else {
            Operator oper = performImported(n, type, base, deps2, visited);
            if (oper != null)
                return oper;
            oper = performParent(n, type, base, deps2, visited);
            if (oper != null)
                return oper;
        }
        return null;
    }

    /**
     * <p>Lookup the imported operator.</p>
     *
     * @param key  The operator name.
     * @param type The operator type.
     * @param src  The call-site.
     * @param deps The deps.
     * @return The resolved operator or null.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Operator performImported(String key, int type,
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
                if (s != null) {
                    s = CacheFunctor.composeQuali(s, key);
                    Operator oper = base.getOper(type, s);
                    if (oper != null && oper.visibleOper(src))
                        return oper;
                }
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().release();
            }
            visited.add(base);
            Operator oper = performReexported(key, type, base, deps2, visited);
            if (oper != null)
                return oper;
        }
        return null;
    }

    /**
     * <p>Lookup the reexported operator.</p>
     *
     * @param fun     The name.
     * @param type    The type.
     * @param src     The call-site.
     * @param deps    The deps.
     * @param visited The visited sources.
     * @return The operator or null.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Operator performReexported(String fun, int type,
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
                if (s != null) {
                    s = CacheFunctor.composeQuali(s, fun);
                    Operator oper = base.getOper(type, s);
                    if (oper != null && oper.visibleOper(src))
                        return oper;
                }
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().release();
            }
            visited.add(base);
            Operator oper = performReexported(fun, type, base, deps2, visited);
            if (oper != null)
                return oper;
        }
        return null;
    }

    /**
     * <p>Lookup the parent operator.</p>
     *
     * @param key  The operator name.
     * @param type The operator type.
     * @param src  The call-site.
     * @param deps The deps.
     * @return The resolved operator or null.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Operator performParent(String key, int type,
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
            if (!base.getRead().attempt(base.getStore().foyer.timeout))
                throw new EngineMessage(EngineMessage.systemError(
                        EngineMessage.OP_SYSTEM_DEADLOCK_TIMEOUT));
            try {
                String s = base.getFullName();
                if (s != null) {
                    s = CacheFunctor.composeQuali(s, key);
                    Operator oper = base.getOper(type, s);
                    if (oper != null && oper.visibleOper(src))
                        return oper;
                }
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().release();
            }
            visited.add(base);
            Operator oper = performImported(key, type, base, deps2, visited);
            if (oper != null)
                return oper;
            oper = performParent(key, type, base, deps2, visited);
            if (oper != null)
                return oper;
        }
        return null;
    }

    /*******************************************************************/
    /* Main Entries                                                    */
    /*******************************************************************/

    /**
     * <p>Retrieve an operator with module lookup.</p>
     * <p>Respect source visibility.</p>
     *
     * @param sa   The atom skeleton.
     * @param type The type.
     * @param en   The engine.
     * @return The operator or null.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static Operator getOper(SkelAtom sa, int type,
                                   Engine en)
            throws EngineMessage, EngineException {
        try {
            AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
            AbstractSource base = (CacheFunctor.isQuali(sa.fun) ? CacheSubclass.lookupBase(
                    CacheFunctor.sepModule(sa.fun), src, en) : src);
            Operator op = performLookup(sa.fun, type, src, base);
            if (op != null && op.visibleOper(src))
                return op;
            return null;
        } catch (InterruptedException x) {
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        }
    }

    /**
     * <p>Define an operator with module lookup.</p>
     *
     * @param sa     The atom skeleton.
     * @param type   The type.
     * @param en     The engine.
     * @param create The no create flag.
     */
    public static Operator getOperDefined(SkelAtom sa, int type,
                                          Engine en,
                                          boolean create)
            throws EngineMessage, EngineException {
        try {
            AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
            AbstractSource base = (CacheFunctor.isQuali(sa.fun) ? CacheSubclass.lookupBase(
                    CacheFunctor.sepModule(sa.fun), src, en) : src);
            Operator op = performLookupDefined(sa.fun, type, src,
                    sa.getPosition(), base, create);
            if (op != null && op.visibleOper(src))
                return op;
            if (create)
                throw new EngineMessage(EngineMessage.permissionError(
                        EngineMessage.OP_PERMISSION_MODIFY,
                        EngineMessage.OP_PERMISSION_OPERATOR,
                        SpecialOper.operToColonSkel(type, sa, en)));
            return null;
        } catch (InterruptedException x) {
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        }
    }

    /*******************************************************************/
    /* Operator User Lookup                                            */
    /*******************************************************************/

    /**
     * <p>Retrieve a user oper.</p>
     *
     * @param type  The type.
     * @param fun   The name,
     * @param store The store, can be null.
     * @return The operator or null,
     */
    public static Operator getOperUser(int type, String fun,
                                       AbstractStore store) {
        while (store != null) {
            Operator oper = store.user.getOper(type, fun);
            if (oper != null)
                return oper;
            store = store.parent;
        }
        return null;
    }

    /**
     * <p>Define an operator.</p>
     *
     * @param type  The type.
     * @param fun   The name.
     * @param scope The call-site, not null.
     * @param pos   The position, can be null.
     * @param store The store.
     * @return The operator.
     */
    public static Operator defineOperUser(int type, String fun,
                                          AbstractSource scope,
                                          PositionKey pos,
                                          AbstractStore store) {
        Operator oper = getOperUser(type, fun, store.parent);
        if (oper == null)
            oper = store.user.defineOper(type, fun, scope, pos);
        return oper;
    }

}