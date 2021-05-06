package jekpro.model.molec;

import jekpro.frequent.system.ForeignThread;
import jekpro.model.builtin.Branch;
import jekpro.model.inter.Engine;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Store;
import jekpro.model.rope.Operator;
import jekpro.reference.reflect.SpecialOper;
import jekpro.tools.term.SkelAtom;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;

import java.util.concurrent.TimeUnit;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class OperatorSearch {

    /*********************************************************************/
    /* Lookup Predicates                                                 */
    /*********************************************************************/

    /**
     * <p>Retrieve an operator with module lookup.</p>
     *
     * @param n    operator name.
     * @param type The operator type.
     * @param base The lookup base, or-null.
     * @param f    The qualified flag.
     * @return The operator or null.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Operator performLookup(String n, int type,
                                          AbstractSource base, boolean f)
            throws InterruptedException, EngineMessage {
        MapEntry<AbstractSource, Integer>[] deps2;
        /* wait for complete source */
        if (!base.getRead().tryLock(base.getStore().foyer.timeout, TimeUnit.MILLISECONDS))
            throw new EngineMessage(EngineMessage.limitError(
                    EngineMessage.OP_LIMIT_DEADLOCK_TIMEOUT));
        try {
            if (!Branch.OP_USER.equals(base.getFullName())) {
                Operator oper = base.getOper(type, n);
                if (oper != null)
                    return oper;
            } else if (f) {
                Operator oper = getOperUser(type, n, base.getStore());
                if (oper != null)
                    return oper;
            }
            deps2 = base.snapshotDeps();
        } finally {
            base.getRead().unlock();
        }
        Operator oper = performDependent(n, type, base, deps2, f);
        if (oper != null)
            return oper;
        if (!Branch.OP_USER.equals(base.getFullName()) || !f)
            return getOperUser(type, n, base.getStore());
        return null;
    }

    /**
     * <p>Define an operator with module lookup.</p>
     *
     * @param sa   The operator name.
     * @param type The operator type.
     * @param src  The call-site, non-null.
     * @param base The lookup base, non-null.
     * @param en   The engine.
     * @param copt The create flag.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Operator performLookupDefined(SkelAtom sa, int type,
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
                /* create name%oper */
                return ((copt & CachePredicate.MASK_CACH_CRTE) != 0 ?
                        base.defineOper(type, sa.fun, sa, en) :
                        base.getOper(type, sa.fun));
            } else {
                /* create oper */
                return ((copt & CachePredicate.MASK_CACH_CRTE) != 0 ?
                        defineOperUser(type, sa.fun, sa, src.getStore(), en) :
                        getOperUser(type, sa.fun, src.getStore()));
            }
        } finally {
            base.getRead().unlock();
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
        ListArray<AbstractSource> visited = new ListArray<>();
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
     * @param n    The operator name.
     * @param type The operator type.
     * @param src  The call-site, non null.
     * @param deps The deps.
     * @return The resolved operator or null.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Operator performImported(String n, int type,
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
                    Operator oper = base.getOper(type, n);
                    if (oper != null && OperatorSearch.visibleOper(oper, src))
                        return oper;
                }
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().unlock();
            }
            visited.add(base);
            Operator oper = performReexported(n, type, base, deps2, visited);
            if (oper != null)
                return oper;
        }
        return null;
    }

    /**
     * <p>Lookup the reexported operator.</p>
     *
     * @param n       The name.
     * @param type    The type.
     * @param src     The call-site, non null.
     * @param deps    The deps.
     * @param visited The visited sources.
     * @return The operator or null.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Operator performReexported(String n, int type,
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
                    Operator oper = base.getOper(type, n);
                    if (oper != null && OperatorSearch.visibleOper(oper, src))
                        return oper;
                }
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().unlock();
            }
            visited.add(base);
            Operator oper = performReexported(n, type, base, deps2, visited);
            if (oper != null)
                return oper;
        }
        return null;
    }

    /**
     * <p>Lookup the parent operator.</p>
     *
     * @param n    The operator name.
     * @param type The operator type.
     * @param src  The call-site, non null.
     * @param deps The deps.
     * @return The resolved operator or null.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static Operator performParent(String n, int type,
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
                    Operator oper = base.getOper(type, n);
                    if (oper != null && OperatorSearch.visibleOper(oper, src))
                        return oper;
                }
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().unlock();
            }
            visited.add(base);
            Operator oper = performImported(n, type, base, deps2, visited);
            if (oper != null)
                return oper;
            oper = performParent(n, type, base, deps2, visited);
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
     * @param scope The call-site.
     * @param fun   The name.
     * @param type  The type.
     * @param en    The engine.
     * @return The operator or null.
     * @throws EngineMessage Shit happens.
     */
    public static Operator getOperQuick(AbstractSource scope, String fun,
                                        int type,
                                        Engine en)
            throws EngineMessage {
        try {
            AbstractSource src = (scope != null ? scope : en.store.user);
            Operator oper = performLookup(fun, type, src, false);
            if (oper != null && OperatorSearch.visibleOper(oper, src))
                return oper;
            return null;
        } catch (InterruptedException x) {
            throw (EngineMessage) ForeignThread.sysThreadClear();
        }
    }

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
    public static Operator getOper(SkelAtom sa,
                                   int type,
                                   Engine en)
            throws EngineMessage, EngineException {
        try {
            AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
            AbstractSource base = CachePredicate.performBase(sa, src, en);
            Operator oper = performLookup(sa.fun, type, base, sa instanceof SkelAtomQuali);
            if (oper != null && OperatorSearch.visibleOper(oper, src))
                return oper;
            return null;
        } catch (InterruptedException x) {
            throw (EngineMessage) ForeignThread.sysThreadClear();
        }
    }

    /**
     * <p>Define an operator with module lookup.</p>
     *
     * @param sa   The atom skeleton.
     * @param type The type.
     * @param en   The engine.
     * @param copt The create flag.
     */
    public static Operator getOperDefined(SkelAtom sa, int type,
                                          Engine en,
                                          int copt)
            throws EngineMessage, EngineException {
        try {
            AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
            AbstractSource base = CachePredicate.performBase(sa, src, en);
            Operator oper = performLookupDefined(sa, type, src,
                    base, en, copt);
            if ((copt & CachePredicate.MASK_CACH_CRTE) != 0 &&
                    (oper == null || !OperatorSearch.visibleOper(oper, src)))
                throw new EngineMessage(EngineMessage.permissionError(
                        EngineMessage.OP_PERMISSION_MODIFY,
                        EngineMessage.OP_PERMISSION_OPERATOR,
                        SpecialOper.operToColonSkel(type, sa, en)));
            return oper;
        } catch (InterruptedException x) {
            throw (EngineMessage) ForeignThread.sysThreadClear();
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
                                       Store store) {
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
     * @param sa    The call-site, non null.
     * @param store The store.
     * @param en    The engine.
     * @return The operator.
     * @throws EngineMessage Shit happens.
     */
    private static Operator defineOperUser(int type, String fun,
                                           SkelAtom sa,
                                           Store store, Engine en)
            throws EngineMessage {
        Operator oper = getOperUser(type, fun, store.parent);
        if (oper == null)
            oper = store.user.checkOper(type, fun, sa, en);
        oper.addDef(sa, en);
        return oper;
    }

    /*******************************************************************/
    /* Operator Visibility                                             */
    /*******************************************************************/

    /**
     * <p>Check whether the operator is visible.</p>
     *
     * @param oper The operator.
     * @param src  The call-site, non-null.
     * @return True if the operator is visible, otherwise false.
     */
    public static boolean visibleOper(Operator oper, AbstractSource src) {
        if ((oper.getBits() & Operator.MASK_OPER_VSPR) != 0) {
            return sameHome(oper.getScope(), src);
        } else if ((oper.getBits() & Operator.MASK_OPER_VSPU) == 0) {
            return samePackage(oper.getScope(), src);
        } else {
            return true;
        }
    }

    /**
     * <p>Check whether this source has the same home as another source.</p>
     *
     * @param fst The first source, non null.
     * @param snd The second source, non null.
     * @return True if the two sources share the same home, otherwise false.
     */
    public static boolean sameHome(AbstractSource fst, AbstractSource snd) {
        String path1 = fst.getPath();
        int k1 = path1.lastIndexOf(CacheModule.OP_CHAR_OS) + 1;
        int j = path1.indexOf(CacheSubclass.OP_CHAR_SYN, k1);
        k1 = (j == -1 ? path1.length() : j);

        String path2 = snd.getPath();
        int k2 = path2.lastIndexOf(CacheModule.OP_CHAR_OS) + 1;
        j = path2.indexOf(CacheSubclass.OP_CHAR_SYN, k2);
        k2 = (j == -1 ? path2.length() : j);

        return (k1 == k2 &&
                (k1 == 0 || path1.regionMatches(0, path2, 0, k1)));
    }


    /**
     * <p>Check whether this source has the same package as another source.</p>
     *
     * @param fst The first source, non null.
     * @param snd The second source, non null.
     * @return True if the two sources share the same package, otherwise false.
     */
    public static boolean samePackage(AbstractSource fst, AbstractSource snd) {
        String path1 = fst.getPath();
        int k1 = path1.lastIndexOf(CacheModule.OP_CHAR_OS) + 1;

        String path2 = snd.getPath();
        int k2 = path2.lastIndexOf(CacheModule.OP_CHAR_OS) + 1;

        return (k1 == k2 &&
                (k1 == 0 || path1.regionMatches(0, path2, 0, k1)));
    }

}