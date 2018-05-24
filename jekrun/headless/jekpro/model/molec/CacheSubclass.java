package jekpro.model.molec;

import jekpro.model.builtin.Branch;
import jekpro.model.inter.Engine;
import jekpro.model.pretty.*;
import jekpro.model.rope.LoadForce;
import jekpro.model.rope.LoadOpts;
import jekpro.reference.bootload.ForeignPath;
import jekpro.tools.foreign.LookupBinary;
import jekpro.tools.foreign.LookupResource;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.system.ForeignUri;
import matula.util.wire.AbstractLivestock;

import java.io.IOException;

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
        String key = findKey(mod, scope, ForeignPath.MASK_MODL_AUTO);

        if (key == null)
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_SOURCE_SINK,
                    new SkelAtom(mod)));

        return opts.makeLoad(scope, key, en);
    }

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
        MapEntry<AbstractSource, Integer>[] deps2;
        /* wait for complete source */
        if (!base.getRead().attempt(base.getStore().foyer.timeout))
            throw new EngineMessage(EngineMessage.systemError(
                    EngineMessage.OP_SYSTEM_DEADLOCK_TIMEOUT));
        try {
            String s = base.getFullName();
            if (s == null)
                return false;
            if (fun.equals(s))
                return true;
            deps2 = base.snapshotDeps();
        } finally {
            base.getRead().release();
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
     * @param fun     The full name to search, not null.
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
            MapEntry<AbstractSource, Integer>[] deps2;
            /* wait for complete source */
            if (!base.getRead().attempt(base.getStore().foyer.timeout))
                throw new EngineMessage(EngineMessage.systemError(
                        EngineMessage.OP_SYSTEM_DEADLOCK_TIMEOUT));
            try {
                String s = base.getFullName();
                if (s == null)
                    continue;
                if (fun.equals(s))
                    return true;
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().release();
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
     * @param sa  The atom skeleton.
     * @param fun The other atom.
     * @param en  The engine.
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
                    AbstractSource base = lookupBase(sa.fun, src, en);
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
                            AbstractSource base = lookupBase(sa.fun, src, en);
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
                AbstractSource base = lookupBase(sa.fun, src, en);
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
                        base = lookupBase(sa.fun, src, en);
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

    /*****************************************************************/
    /* Find Key                                                      */
    /*****************************************************************/

    /**
     * <p>Find a key according to the auto loader.</p>
     *
     * @param path  The path.
     * @param scope The source, not null.
     * @param mask  The mask.
     * @return The source key, or null.
     * @throws EngineMessage Shit happens.
     */
    public static String findKey(String path, AbstractSource scope, int mask)
            throws EngineMessage {
        try {
            if ((mask & ForeignPath.MASK_FAIL_CHLD) != 0) {
                if (ForeignUri.sysUriIsRelative(path)) {
                    String res = LookupChild.findChildKey(path, scope);
                    if (res != null)
                        return res;
                }
            }

            if (SourceLocal.isLocal(path)) {
                String res = SourceLocal.sepHome(path);
                res = findKeyParent(res, scope, mask);
                if (res == null)
                    return null;
                path = SourceLocal.sepRest(path);
                return SourceLocal.composeLocal(res, path);
            } else {
                return findKeyParent(path, scope, mask);
            }

        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Find a key according in the best way.</p>
     *
     * @param path  The path.
     * @param src   The source, not null.
     * @param mask  The mask.
     * @return The source key.
     * @throws IOException Shit happens.
     */
    private static String findKeyParent(String path,
                                        AbstractSource src,
                                        int mask)
            throws IOException {

        /* special case */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            if (Branch.OP_USER.equals(path))
                return path;
        }

        /* library .p */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            if (ForeignUri.sysUriIsRelative(path)) {
                String key = LookupResource.findResourceSuffix(path, src, mask);
                if (key != null)
                    return key;
            }
        }

        /* foreign .class */
        if ((mask & ForeignPath.MASK_PRFX_FRGN) != 0) {
            if (ForeignUri.sysUriIsRelative(path)) {
                String key = LookupBinary.findBinarySuffix(path, src, mask);
                if (key != null)
                    return key;
            }
        }

        /* failure read */
        if ((mask & ForeignPath.MASK_FAIL_READ) != 0) {
            String key = LookupRead.findReadSuffix(path, src, mask);
            if (key != null)
                return key;
            key = LookupRead.findRead(path, src);
            if (key != null)
                return key;
        }

        // failure
        return null;
    }

    /*****************************************************************/
    /* Unfind Key                                                    */
    /*****************************************************************/

    /**
     * <p>Unfind a key in the best way.</p>
     *
     * @param path  The absolute or relative path.
     * @param scope The call-site, not null.
     * @param mask  The mask.
     * @return The path without suffix.
     */
    public static Object unfindKey(String path, AbstractSource scope, int mask)
            throws EngineMessage {
        try {
            if ((mask & ForeignPath.MASK_FAIL_CHLD) != 0) {
                String res = LookupChild.unfindChildSuffix(path, scope);
                if (res != null)
                    return new SkelCompound(new SkelAtom(LoadOpts.OP_PREFIX_VERBATIM),
                            new SkelAtom(res));
            }

            if (SourceLocal.isLocal(path)) {
                String res = SourceLocal.sepHome(path);
                path = SourceLocal.sepRest(path);
                Object temp = unfindKeyParent(res, scope, mask);
                if (temp instanceof SkelAtom) {
                    SkelAtom sa = (SkelAtom) temp;
                    path = SourceLocal.composeLocal(sa.fun, path);
                    return new SkelAtom(path);
                } else {
                    SkelCompound sc = (SkelCompound) temp;
                    path = SourceLocal.composeLocal(((SkelAtom) sc.args[0]).fun, path);
                    return new SkelCompound(sc.sym, new SkelAtom(path));
                }
            } else {
                return unfindKeyParent(path, scope, mask);
            }
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Unfind a key in the best way.</p>
     *
     * @param path  The absolute or relative path.
     * @param src   The call-site, not null.
     * @param mask  The mask.
     * @return The path without suffix.
     * @throws IOException Shit happens.
     */
    public static Object unfindKeyParent(String path,
                                         AbstractSource src,
                                         int mask)
            throws IOException, EngineMessage {

        /* special case */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            if (Branch.OP_USER.equals(path))
                return path;
        }

        /* foreign .class */
        if (ForeignUri.sysUriIsRelative(path)) {
            if ((mask & ForeignPath.MASK_PRFX_FRGN) != 0) {
                String res = LookupBinary.unfindBinarySuffix(path, src, mask);
                if (res != null)
                    return new SkelCompound(new SkelAtom(LoadOpts.OP_PREFIX_FOREIGN),
                            new SkelAtom(res));
            }
            return new SkelAtom(path);
        }

        String key = LookupRead.unfindReadSuffix(path, src, mask);
        if (key != null)
            path = key;

        if ((mask & ForeignPath.MASK_FAIL_READ) != 0) {
            String res = LookupRead.unfindRead(path, src);
            if (res != null)
                return new SkelAtom(res);
        }

        /* library .p */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            String res = LookupResource.unfindResourcePaths(path, src.getStore());
            if (res != null)
                return new SkelCompound(new SkelAtom(LoadOpts.OP_PREFIX_LIBRARY),
                        new SkelAtom(res));
        }

        return new SkelAtom(path);
    }

}
