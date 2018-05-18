package jekpro.model.molec;

import jekpro.model.inter.Engine;
import jekpro.model.pretty.AbstractSource;
import jekpro.tools.term.SkelAtom;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.wire.AbstractLivestock;

/**
 * <p>The polymorphic cache for the subclass relation.</p>
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
public final class CacheSubclass extends AbstractCache {
    String fun;
    AbstractSource base;
    Object basevers;
    boolean res;

    /**
     * <p>Find a source in the reexport chain of another source.</p>
     *
     * @param fun  The full name to search.
     * @param base The start of the reexport chain.
     * @return True if the source was found, otherwise false.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static boolean lookupChain(String fun, AbstractSource base)
            throws InterruptedException, EngineMessage {
        String s = base.getFullName();
        MapEntry<AbstractSource, Integer>[] deps2;
        if (s == null) {
            deps2 = base.snapshotDeps();
        } else {
            if (fun.equals(s))
                return true;
            /* wait for complete source */
            if (!base.getRead().attempt(base.getStore().foyer.timeout))
                throw new EngineMessage(EngineMessage.systemError(
                        EngineMessage.OP_SYSTEM_DEADLOCK_TIMEOUT));
            try {
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().release();
            }
        }
        ListArray visited = new ListArray<AbstractCache>();
        visited.add(base);
        if (lookupChain(fun, deps2, visited))
            return true;
        return false;
    }

    /**
     * <p>Find a source in the reexport chain of another source.</p>
     *
     * @param fun     The full name to search.
     * @param deps    The deps.
     * @param visited The visited sources.
     * @return True if the source was found, otherwise false.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static boolean lookupChain(String fun,
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
            String s = base.getFullName();
            MapEntry<AbstractSource, Integer>[] deps2;
            if (s == null) {
                deps2 = base.snapshotDeps();
            } else {
                if (fun.equals(s))
                    return true;
                /* wait for complete source */
                if (!base.getRead().attempt(base.getStore().foyer.timeout))
                    throw new EngineMessage(EngineMessage.systemError(
                            EngineMessage.OP_SYSTEM_DEADLOCK_TIMEOUT));
                try {
                    deps2 = base.snapshotDeps();
                } finally {
                    base.getRead().release();
                }
            }
            visited.add(base);
            if (lookupChain(fun, deps2, visited))
                return true;
        }
        return false;
    }

    /*******************************************************************/
    /* Subclass Test                                                   */
    /*******************************************************************/

    /**
     * <p>Perform a sub class test.</p>
     *
     * @param sa   The atom skeleton.
     * @param fun The other atom.
     * @param en   The engine.
     * @return The module name.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static boolean getSubclass(SkelAtom sa, String fun,
                                      Engine en)
            throws EngineMessage, EngineException {
        try {
            AbstractCache back = null;
            AbstractCache temp = sa.cache;
            for (; ; ) {
                if (temp == null) {
                    AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
                    AbstractSource base = CachePredicate.lookupBase(sa.fun, src, en);
                    Object basevers = base.importvers;
                    boolean flag = lookupChain(fun, base);

                    CacheSubclass ca = new CacheSubclass();
                    ca.fun = fun;
                    ca.res = flag;
                    ca.base = base;
                    ca.basevers = basevers;
                    if (back == null) {
                        sa.cache = ca;
                    } else {
                        back.next = ca;
                    }
                    return flag;
                }
                if (temp instanceof CacheSubclass) {
                    CacheSubclass ca = (CacheSubclass) temp;
                    if (ca.fun.equals(fun)) {
                        boolean flag;
                        if (ca.basevers != ca.base.importvers) {
                            AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
                            AbstractSource base = CachePredicate.lookupBase(sa.fun, src, en);
                            Object basevers = base.importvers;
                            flag = lookupChain(fun, base);

                            ca.fun = fun;
                            ca.res = flag;
                            ca.base = base;
                            ca.basevers = basevers;
                        } else {
                            flag = ca.res;
                        }
                        return flag;
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
    /* Base Value                                                      */
    /*******************************************************************/

    /**
     * <p>Retrieve a base value.</p>
     *
     * @param sa The atom skeleton.
     * @param en The engine.
     * @return The module name.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static AbstractSource getBase(SkelAtom sa, Engine en)
            throws EngineException, EngineMessage {
        AbstractCache back = null;
        AbstractCache temp = sa.cache;
        for (; ; ) {
            if (temp == null) {
                AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
                AbstractSource base = CachePredicate.lookupBase(sa.fun, src, en);
                Object basevers = base.importvers;

                CacheSubclass ca = new CacheSubclass();
                ca.fun = null;
                ca.res = false;
                ca.base = base;
                ca.basevers = basevers;
                if (back == null) {
                    sa.cache = ca;
                } else {
                    back.next = ca;
                }
                return base;
            }
            if (temp instanceof CacheSubclass) {
                CacheSubclass ca = (CacheSubclass) temp;
                if (ca.fun == null) {
                    AbstractSource base;
                    if (ca.basevers != ca.base.importvers) {
                        AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
                        base = CachePredicate.lookupBase(sa.fun, src, en);
                        Object basevers = base.importvers;

                        ca.fun = null;
                        ca.res = false;
                        ca.base = base;
                        ca.basevers = basevers;
                    } else {
                        base = ca.base;
                    }
                    return base;
                }
            }
            back = temp;
            temp = back.next;
        }
    }

}
