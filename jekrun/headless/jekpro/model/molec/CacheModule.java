package jekpro.model.molec;

import jekpro.model.builtin.Branch;
import jekpro.model.inter.Engine;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.SourceLocal;
import jekpro.reference.bootload.ForeignPath;
import jekpro.tools.foreign.LookupBinary;
import jekpro.tools.foreign.LookupResource;
import jekpro.tools.term.SkelAtom;
import matula.util.data.MapEntry;

import java.io.IOException;

/**
 * <p>The polymorphic cache for structured module name.</p>
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
public final class CacheModule extends AbstractCache {
    public final static char OP_CHAR_OS = '/';
    public final static String OP_STRING_OS = "/";

    private static final int MASK_PRFX_FRGN = AbstractSource.MASK_PCKG_FRGN | AbstractSource.MASK_USES_FRGN;
    private static final int MASK_PRFX_LIBR = AbstractSource.MASK_PCKG_LIBR | AbstractSource.MASK_USES_LIBR;

    String fun;
    boolean sole;
    Object srcvers;
    SkelAtom res;

    /**
     * <p>Create a module name.</p>
     *
     * @param sa    The atom.
     * @param fun  The package, or null.
     * @param sole  The sole flag.
     * @param scope The call-site, not null.
     * @return The module name.
     * @throws EngineMessage Shit happens.
     */
    private static SkelAtom lookupModule(SkelAtom sa,
                                         String fun, boolean sole,
                                         AbstractSource scope)
            throws EngineMessage {
        if (!sole) {
            if (fun != null) {
                fun = CachePackage.composeStruct(sa.fun, fun);
            } else {
                fun = CachePackage.composeArray(sa.fun);
            }
        } else {
            fun = sa.fun;
        }

        /* lookup prefix from call-site */
        fun = fun.replace(CachePackage.OP_CHAR_SEG, OP_CHAR_OS);
        fun = findPrefix(fun, scope, ForeignPath.MASK_MODL_AUTO);
        fun = fun.replace(OP_CHAR_OS, CachePackage.OP_CHAR_SEG);

        /* create with call-site */
        return new SkelAtom(fun, scope);
    }

    /*******************************************************************/
    /* Main Entries                                                    */
    /*******************************************************************/

    /**
     * <p>Retrieve a module name.</p>
     *
     * @param sa    The atom skeleton.
     * @param fun  The package, or null.
     * @param sole  The sole flag.
     * @param scope The call-site, or null.
     * @param en    The engine.
     * @return The module name.
     * @throws EngineMessage Shit happens.
     */
    public static SkelAtom getModule(SkelAtom sa, String fun,
                                     boolean sole,
                                     AbstractSource scope,
                                     Engine en)
            throws EngineMessage {
        AbstractSource src = (scope != null ? scope : en.store.user);
        AbstractCache back = null;
        AbstractCache temp = sa.cache;
        for (; ; ) {
            if (temp == null) {
                /* cache miss, so lookup */
                Object fixvers = src.fixvers;
                SkelAtom sa2 = lookupModule(sa, fun, sole, src);
                CacheModule ca = new CacheModule();
                ca.fun = fun;
                ca.sole = sole;
                ca.res = sa2;
                ca.srcvers = fixvers;
                if (back == null) {
                    sa.cache = ca;
                } else {
                    back.next = ca;
                }
                return sa2;
            }
            if (temp instanceof CacheModule) {
                CacheModule ca = (CacheModule) temp;
                if ((ca.fun != null ? ca.fun.equals(fun) : null == fun) &&
                        ca.sole == sole && ca.res.scope == src) {
                    SkelAtom sa2;
                    if (ca.srcvers != src.fixvers) {
                        /* cache invalidated, so lookup */
                        Object fixvers = src.fixvers;
                        sa2 = lookupModule(sa, fun, sole, src);
                        ca.fun = fun;
                        ca.sole = sole;
                        ca.res = sa2;
                        ca.srcvers = fixvers;
                    } else {
                        sa2 = ca.res;
                    }
                    return sa2;
                }
            }
            back = temp;
            temp = back.next;
        }
    }

    /*****************************************************************/
    /* Find Prefix                                                   */
    /*****************************************************************/

    /**
     * <p>Find a prefix according to the auto loader.</p>
     * <p>Dollar separators are generated.</p>
     *
     * @param relpath The path.
     * @param scope  The call-site, not null.
     * @param mask The mask.
     * @return The prefixed path.
     * @throws EngineMessage Shit happens.
     */
    public static String findPrefix(String relpath, AbstractSource scope, int mask)
            throws EngineMessage {
        try {
            String res = findPrefixParent(relpath, scope, mask);
            if (res != null)
                return res;

            int k = relpath.lastIndexOf(OP_CHAR_OS);
            while (k != -1) {
                res = relpath.substring(0, k);
                res = findPrefixParent(res, scope, mask);
                if (res != null) {
                    relpath = relpath.substring(k + 1);
                    relpath = relpath.replace(OP_CHAR_OS, CacheSubclass.OP_CHAR_SYN);
                    return CacheSubclass.composeLocal(res, relpath);
                }
                k = relpath.lastIndexOf(OP_CHAR_OS, k - 1);
            }

            if ((mask & ForeignPath.MASK_FAIL_CHLD) != 0) {
                res = findChildPrefix(relpath, scope);
                if (res != null)
                    return res;
            }

            return relpath;
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Find a prefix in the best way.</p>
     *
     * @param relpath  The relative path.
     * @param src   The call-site, not null.
     * @param mask  The mask.
     * @return The prefixed path or null.
     * @throws IOException Shit happens.
     */
    public static String findPrefixParent(String relpath,
                                          AbstractSource src,
                                          int mask)
            throws IOException {

        /* special case */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            if (Branch.OP_USER.equals(relpath))
                return relpath;
        }

        AbstractSource src2 = SourceLocal.derefParentImport(src);

        /* library .p */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            String key = LookupResource.findResourceSuffix(relpath, src, mask);
            if (key != null)
                return relpath;
        }

        /* foreign .class */
        if ((mask & ForeignPath.MASK_PRFX_FRGN) != 0) {
            String key = LookupBinary.findBinarySuffix(relpath, src, mask);
            if (key != null)
                return relpath;
        }

        /* system library .p */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            MapEntry<String, Integer>[] fixes = src.getStore().foyer.SOURCE_SYSTEM.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & MASK_PRFX_LIBR) != 0) {
                    String path2 = fix.key + OP_STRING_OS + relpath;
                    String key = LookupResource.findResourceSuffix(path2, src, mask);
                    if (key != null)
                        return path2;
                }
            }
        }

        /* system imported .class */
        if ((mask & ForeignPath.MASK_PRFX_FRGN) != 0) {
            MapEntry<String, Integer>[] fixes = src.getStore().foyer.SOURCE_SYSTEM.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & MASK_PRFX_FRGN) != 0) {
                    String path2 = fix.key + OP_STRING_OS + relpath;
                    String key = LookupBinary.findBinarySuffix(path2, src, mask);
                    if (key != null)
                        return path2;
                }
            }
        }

        /* source library .p */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0 &&
                !src2.equals(src.getStore().foyer.SOURCE_SYSTEM)) {
            MapEntry<String, Integer>[] fixes = src2.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & MASK_PRFX_LIBR) != 0) {
                    String path2 = fix.key + OP_STRING_OS + relpath;
                    String key = LookupResource.findResourceSuffix(path2, src, mask);
                    if (key != null)
                        return path2;
                }
            }
        }

        /* source imported .class */
        if ((mask & ForeignPath.MASK_PRFX_FRGN) != 0 &&
                !src2.equals(src.getStore().foyer.SOURCE_SYSTEM)) {
            MapEntry<String, Integer>[] fixes = src2.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & MASK_PRFX_FRGN) != 0) {
                    String path2 = fix.key + OP_STRING_OS + relpath;
                    String key = LookupBinary.findBinarySuffix(path2, src, mask);
                    if (key != null)
                        return path2;
                }
            }
        }

        // failure
        return null;
    }

    /*****************************************************************/
    /* Unfind Prefix                                                 */
    /*****************************************************************/

    /**
     * <p>Remove the prefix in the best way.</p>
     * <p>Dollar separators are preserved.</p>
     *
     * @param relpath The path.
     * @param scope  The call-site, not null.
     * @param mask The mask.
     * @return The class.
     * @throws EngineMessage Shit happens.
     */
    public static String unfindPrefix(String relpath, AbstractSource scope, int mask)
            throws EngineMessage {
        try {
            if ((mask & ForeignPath.MASK_FAIL_CHLD) != 0) {
                String res = unfindChildPrefix(relpath, scope, mask);
                if (res != null)
                    return res;
            }

            if (CacheSubclass.isLocal(relpath)) {
                String res = CacheSubclass.sepHome(relpath);
                res = unfindPrefixParent(res, scope, mask);
                relpath = CacheSubclass.sepRest(relpath);
                relpath = relpath.replace(CacheSubclass.OP_CHAR_SYN, OP_CHAR_OS);
                return composeOs(res, relpath);
            } else {
                return unfindPrefixParent(relpath, scope, mask);
            }
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Remove the prefix in the best way.</p>
     *
     * @param relpath  The path.
     * @param src   The call-site, not null.
     * @param mask  The mask.
     * @return The class.
     * @throws IOException Shit happens.
     */
    public static String unfindPrefixParent(String relpath,
                                            AbstractSource src,
                                            int mask)
            throws IOException {

        /* special case */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            if (Branch.OP_USER.equals(relpath))
                return relpath;
        }

        AbstractSource src2 = SourceLocal.derefParentImport(src);

        /* source imported .class */
        if ((mask & ForeignPath.MASK_PRFX_FRGN) != 0 &&
                !src2.equals(src.getStore().foyer.SOURCE_SYSTEM)) {
            MapEntry<String, Integer>[] fixes = src2.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & MASK_PRFX_FRGN) != 0) {
                    if (relpath.startsWith(fix.key) &&
                            relpath.startsWith(OP_STRING_OS, fix.key.length())) {
                        String path2 = relpath.substring(fix.key.length() + 1);
                        if (relpath.equals(findPrefixParent(path2, src, mask)))
                            return path2;
                    }
                }
            }
        }

        /* source library .p */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0 &&
                !src2.equals(src.getStore().foyer.SOURCE_SYSTEM)) {
            MapEntry<String, Integer>[] fixes = src2.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & MASK_PRFX_LIBR) != 0) {
                    if (relpath.startsWith(fix.key) &&
                            relpath.startsWith(OP_STRING_OS, fix.key.length())) {
                        String path2 = relpath.substring(fix.key.length() + 1);
                        if (relpath.equals(findPrefixParent(path2, src, mask)))
                            return path2;
                    }
                }
            }
        }

        /* system imported .class */
        if ((mask & ForeignPath.MASK_PRFX_FRGN) != 0) {
            MapEntry<String, Integer>[] fixes = src.getStore().foyer.SOURCE_SYSTEM.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & MASK_PRFX_FRGN) != 0) {
                    if (relpath.startsWith(fix.key) &&
                            relpath.startsWith(OP_STRING_OS, fix.key.length())) {
                        String path2 = relpath.substring(fix.key.length() + 1);
                        if (relpath.equals(findPrefixParent(path2, src, mask)))
                            return path2;
                    }
                }
            }
        }

        /* system library .p */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            MapEntry<String, Integer>[] fixes = src.getStore().foyer.SOURCE_SYSTEM.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & MASK_PRFX_LIBR) != 0) {
                    if (relpath.startsWith(fix.key) &&
                            relpath.startsWith(OP_STRING_OS, fix.key.length())) {
                        String path2 = relpath.substring(fix.key.length() + 1);
                        if (relpath.equals(findPrefixParent(path2, src, mask)))
                            return path2;
                    }
                }
            }
        }

        // failure
        return relpath;
    }

    /***************************************************************/
    /* Find & Unfind Child                                         */
    /***************************************************************/

    /**
     * <p>Find a prefix according to the child rule.</p>
     *
     * @param relpath The path.
     * @param src     The call-site, not null.
     * @return The prefixed path or null.
     */
    public static String findChildPrefix(String relpath, AbstractSource src) {
        String res = src.getFullName();
        res = res.replace(CachePackage.OP_CHAR_SEG, CacheModule.OP_CHAR_OS);
        relpath = relpath.replace(CacheModule.OP_CHAR_OS, CacheSubclass.OP_CHAR_SYN);
        return CacheSubclass.composeLocal(res, relpath);
    }

    /**
     * <p>Remove the prefix according to the child rule.</p>
     *
     * @param relpath The path.
     * @param src     The call-site, not null.
     * @param mask    The mask.
     * @return The prefixed path or null.
     */
    public static String unfindChildPrefix(String relpath,
                                           AbstractSource src,
                                           int mask)
            throws IOException {
        String res = src.getFullName();
        res = res.replace(CachePackage.OP_CHAR_SEG, CacheModule.OP_CHAR_OS);
        if (relpath.startsWith(res) && relpath.startsWith(CacheSubclass.OP_STRING_SYN, res.length())) {
            relpath = relpath.substring(res.length() + CacheSubclass.OP_STRING_SYN.length());
            relpath = relpath.replace(CacheSubclass.OP_CHAR_SYN, CacheModule.OP_CHAR_OS);
            /* uniqueness check */
            if (CacheModule.findPrefixParent(relpath, src, mask) == null)
                return relpath;
        }

        // failure
        return null;
    }

    /***************************************************************/
    /* Operating System Paths                                      */
    /***************************************************************/

    /**
     * <p>Separate the directory from the OS path.</p>
     *
     * @param path The OS path.
     * @return The directory.
     */
    public static String sepDirectory(String path) {
        return path.substring(0, path.lastIndexOf(OP_CHAR_OS));
    }

    /**
     * <p>Separate the file from the OS path.</p>
     *
     * @param path The OS path.
     * @return The file.
     */
    public static String sepFile(String path) {
        return path.substring(path.lastIndexOf(OP_CHAR_OS) + 1);
    }

    /**
     * <p>Check whether a path is OS.</p>
     *
     * @param path The path.
     * @return True if the path is OS, otherwise false.
     */
    public static boolean isOs(String path) {
        return path.lastIndexOf(OP_CHAR_OS) != -1;
    }

    /**
     * <p>Compose a structured path from a directory and a file.</p>
     *
     * @param dir  The directory.
     * @param file The file.
     * @return The structured path.
     */
    public static String composeOs(String dir, String file) {
        StringBuilder buf = new StringBuilder();
        buf.append(dir);
        buf.append(OP_CHAR_OS);
        buf.append(file);
        return buf.toString();
    }

}
