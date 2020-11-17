package jekpro.model.molec;

import derek.util.protect.LicenseError;
import jekpro.frequent.system.ForeignThread;
import jekpro.model.builtin.Branch;
import jekpro.model.inter.Engine;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.LookupBase;
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

import java.io.IOException;
import java.util.concurrent.TimeUnit;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class CacheSubclass extends AbstractCache {
    public final static char OP_CHAR_SYN = '$';
    public final static String OP_STRING_SYN = "$";

    SkelAtom mod;
    AbstractSource base;
    Object basevers;
    boolean res;

    /**************************************************************/
    /* Lookup Key                                                 */
    /**************************************************************/

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
    public static AbstractSource lookupKey(String mod,
                                           AbstractSource scope,
                                           Engine en)
            throws EngineMessage, EngineException {

        if (Branch.OP_USER.equals(mod))
            return scope.getStore().user;

        LoadOpts opts = new LoadOpts();
        opts.setFlags(opts.getFlags() | LoadOpts.MASK_LOAD_COND);
        opts.setFlags(opts.getFlags() | LoadForce.MASK_LOAD_AUTO);
        mod = mod.replace(CachePackage.OP_CHAR_SEG, CacheModule.OP_CHAR_OS);
        String key;
        try {
            key = findKey(mod, scope, ForeignPath.MASK_MODL_BASE, null);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
        if (key == null)
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_VERBATIM, new SkelAtom(mod)));
        return opts.makeLoad(scope, key, en);
    }

    /**
     * <p>Find a source in the reexport chain of another source.</p>
     *
     * @param other The other source.
     * @param base  The start of the reexport chain.
     * @return True if the source was found, otherwise false.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static boolean lookupChain(AbstractSource other,
                                       AbstractSource base)
            throws InterruptedException, EngineMessage {
        MapEntry<AbstractSource, Integer>[] deps2;
        /* wait for complete source */
        if (!base.getRead().tryLock(base.getStore().foyer.timeout, TimeUnit.MILLISECONDS))
            throw new EngineMessage(EngineMessage.limitError(
                    EngineMessage.OP_LIMIT_DEADLOCK_TIMEOUT));
        try {
            String s = base.getFullName();
            if (Branch.OP_USER.equals(s))
                return false;
            if (other == base)
                return true;
            deps2 = base.snapshotDeps();
        } finally {
            base.getRead().unlock();
        }
        ListArray visited = new ListArray<AbstractCache>();
        visited.add(base);
        if (lookupChain(other, deps2, visited))
            return true;
        return false;
    }

    /**
     * <p>Find a source in the reexport chain of another source.</p>
     *
     * @param other   The other source.
     * @param deps    The deps.
     * @param visited The visited sources.
     * @return True if the source was found, otherwise false.
     * @throws EngineMessage        Shit happens.
     * @throws InterruptedException Shit happens.
     */
    private static boolean lookupChain(AbstractSource other,
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
                if (Branch.OP_USER.equals(s))
                    continue;
                if (other == base)
                    return true;
                deps2 = base.snapshotDeps();
            } finally {
                base.getRead().unlock();
            }
            visited.add(base);
            if (lookupChain(other, deps2, visited))
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
     * @param mod The other atom.
     * @param en  The engine.
     * @return The module name.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static boolean getSubclass(SkelAtom sa, SkelAtom mod,
                                      Engine en)
            throws EngineMessage, EngineException {
        try {
            AbstractCache back = null;
            AbstractCache temp = sa.cache;
            for (; ; ) {
                if (temp == null) {
                    AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
                    AbstractSource base = lookupKey(sa.fun, src, en);
                    Object basevers = base.importvers;
                    src = (mod.scope != null ? mod.scope : en.store.user);
                    AbstractSource other = lookupKey(mod.fun, src, en);
                    boolean flag = lookupChain(other, base);

                    CacheSubclass ca = new CacheSubclass();
                    ca.mod = mod;
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
                    if (ca.mod.fun.equals(mod.fun) &&
                            ca.mod.scope == mod.scope) {
                        boolean flag;
                        if (ca.basevers != ca.base.importvers) {
                            AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
                            AbstractSource base = lookupKey(sa.fun, src, en);
                            Object basevers = base.importvers;
                            src = (mod.scope != null ? mod.scope : en.store.user);
                            AbstractSource other = lookupKey(mod.fun, src, en);
                            flag = lookupChain(other, base);

                            ca.mod = mod;
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
            throw (EngineMessage) ForeignThread.sysThreadClear();
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
                AbstractSource base = lookupKey(sa.fun, src, en);
                Object basevers = base.importvers;

                CacheSubclass ca = new CacheSubclass();
                ca.mod = null;
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
                if (ca.mod == null) {
                    AbstractSource base;
                    if (ca.basevers != ca.base.importvers) {
                        AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
                        base = lookupKey(sa.fun, src, en);
                        Object basevers = base.importvers;

                        ca.mod = null;
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
     * @param scope The source, non null.
     * @param mask  The mask.
     * @param en    The engine, or null.
     * @return The source key, or null.
     * @throws IOException IO Error.
     */
    public static String findKey(String path,
                                 AbstractSource scope,
                                 int mask,
                                 Engine en)
            throws IOException {
        if (isLocal(path)) {
            String res = sepHome(path);
            res = findKeyParent(res, scope, mask, en);
            if (res != null)
                return composeLocal(res, sepRest(path));
        } else {
            String res = findKeyParent(path, scope, mask, en);
            if (res != null)
                return res;
        }
        return null;
    }

    /**
     * <p>Find a key according in the best way.</p>
     *
     * @param path The path.
     * @param src  The call-site, non null.
     * @param mask The mask.
     * @param en   The engine, or null.
     * @return The source key.
     * @throws IOException Shit happens.
     */
    public static String findKeyParent(String path,
                                       AbstractSource src,
                                       int mask,
                                       Engine en)
            throws IOException {

        // special case
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            if (Branch.OP_USER.equals(path))
                return path;
        }

        // library .p
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            if (ForeignUri.sysUriIsRelative(path)) {
                String key = LookupResource.findResourcePackSuffix(path, src, mask);
                if (key != null)
                    return key;
            }
        }

        // foreign .class
        if ((mask & ForeignPath.MASK_PRFX_FRGN) != 0) {
            if (ForeignUri.sysUriIsRelative(path)) {
                String key = LookupBinary.findBinarySuffix(path, src, mask);
                if (key != null)
                    return key;
            }
        }

        // failure read
        if ((mask & ForeignPath.MASK_FAIL_READ) != 0) {
            String key = LookupBase.findReadSuffix(path, src, mask, en);
            if (key != null)
                return key;
            key = LookupBase.findReadSuffix2(path, src, mask, en);
            if (key != null)
                return key;
            key = LookupBase.findRead(path, src, en);
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
     * @param scope The call-site, non null.
     * @param mask  The mask.
     * @param en    The engine.
     * @return The path without suffix.
     * @throws IOException  Shit happens.
     * @throws LicenseError Shit happens.
     */
    public static Object unfindKey(String path,
                                   AbstractSource scope,
                                   int mask,
                                   Engine en)
            throws IOException, LicenseError {
        if (isLocal(path)) {
            String res = sepHome(path);
            path = sepRest(path);
            Object temp = unfindKeyParent(res, scope, mask, en);
            if (temp instanceof SkelAtom) {
                SkelAtom sa = (SkelAtom) temp;
                path = composeLocal(sa.fun, path);
                return new SkelAtom(path);
            } else if (temp instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) temp;
                path = composeLocal(((SkelAtom) sc.args[0]).fun, path);
                return new SkelCompound(sc.sym, new SkelAtom(path));
            } else {
                throw new IllegalArgumentException("illegal spec");
            }
        } else {
            return unfindKeyParent(path, scope, mask, en);
        }
    }

    /**
     * <p>Unfind a key in the best way.</p>
     *
     * @param path The absolute or relative path.
     * @param src  The call-site, non null.
     * @param mask The mask.
     * @param en   The engine or null.
     * @return The path without suffix.
     * @throws IOException  Shit happens.
     * @throws LicenseError Shit happens.
     */
    private static Object unfindKeyParent(String path, AbstractSource src,
                                          int mask, Engine en)
            throws IOException, LicenseError {

        /* special case */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            if (Branch.OP_USER.equals(path))
                return new SkelCompound(new SkelAtom(LoadOpts.OP_PREFIX_LIBRARY),
                        new SkelAtom(path));
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

        int ext;
        String key = LookupBase.unfindReadSuffix(path, src, mask, en);
        if (key != null) {
            path = key;
            ext = ForeignPath.MASK_SUFX_TEXT;
        } else {
            key = LookupBase.unfindReadSuffix2(path, src, mask, en);
            if (key != null) {
                path = key;
                ext = ForeignPath.MASK_SUFX_RSCS;
            } else {
                ext = 0;
            }
        }

        /* library .p */
        if (ext != 0 && (mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            String res = LookupResource.unfindResourcePaths(path, src.getStore());
            if (res != null) {
                if (ext == ForeignPath.MASK_SUFX_TEXT) {
                    return new SkelCompound(new SkelAtom(LoadOpts.OP_PREFIX_LIBRARY),
                            new SkelAtom(res));
                } else {
                    return new SkelCompound(new SkelAtom(LoadOpts.OP_PREFIX_RESOURCE),
                            new SkelAtom(res));
                }
            }
        }

        /* relative */
        if ((mask & ForeignPath.MASK_FAIL_READ) != 0) {
            String res = LookupBase.unfindRead(path, src, en);
            if (res != null)
                return new SkelAtom(res);
        }

        return new SkelAtom(path);
    }

    /**************************************************************/
    /* Home Module                                                */
    /**************************************************************/

    /**
     * <p>Check whether the path is locale.</p>
     *
     * @param path The path.
     * @return True if path is locale, otherwise false.
     */
    public static boolean isLocal(String path) {
        int k = path.lastIndexOf(CacheModule.OP_CHAR_OS);
        k = path.indexOf(OP_CHAR_SYN, k + 1);
        return (k != -1);
    }

    /**
     * <p>Separate the home from the local path.</p>
     *
     * @param path The local path.
     * @return The home.
     */
    public static String sepHome(String path) {
        int k = path.lastIndexOf(CacheModule.OP_CHAR_OS);
        return path.substring(0, path.indexOf(OP_CHAR_SYN, k + 1));
    }

    /**
     * <p>Separate the rest from the local path.</p>
     *
     * @param path The local path.
     * @return The rest.
     */
    public static String sepRest(String path) {
        int k = path.lastIndexOf(CacheModule.OP_CHAR_OS);
        return path.substring(path.indexOf(OP_CHAR_SYN, k + 1) + 1);
    }

    /**
     * <p>Compose a local path from a parent and a child.</p>
     *
     * @param par   The parent.
     * @param child The child.
     * @return The local path.
     */
    public static String composeLocal(String par, String child) {
        StringBuilder buf = new StringBuilder();
        buf.append(par);
        buf.append(OP_CHAR_SYN);
        buf.append(child);
        return buf.toString();
    }

}
