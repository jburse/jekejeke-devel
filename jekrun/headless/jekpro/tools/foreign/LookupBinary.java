package jekpro.tools.foreign;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.molec.CacheModule;
import jekpro.model.molec.CachePackage;
import jekpro.model.molec.CacheSubclass;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.AbstractStore;
import jekpro.model.pretty.SourceLocal;
import jekpro.reference.bootload.ForeignPath;
import matula.util.data.MapEntry;
import matula.util.system.AbstractRuntime;

/**
 * <p>Concerned with the lookup of binaries, escpecially Java classes</p>
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
public final class LookupBinary {
    private static final String CLASS_EXTENSION = ".class";

    /**
     * <p>Convert a key to a java class.</p>
     *
     * @param relpath The path, in slash notation.
     * @param store   The store.
     * @return The class, or null.
     */
    public static Class keyToClass(String relpath, AbstractStore store) {
        AbstractBranch branch = LookupResource.RelativeURIstoRoots(relpath, store);
        if (branch != null) {
            Object obj = store.foyer.getCanonCache(relpath);
            if (obj != null)
                return ("".equals(obj) ? null : (Class) obj);
        }

        Object obj;
        String res = LookupBinary.removeClassExt(relpath);
        if (res != null) {
            res = res.replace(CacheModule.OP_CHAR_OS, CachePackage.OP_CHAR_SEG);
            Class clazz = AbstractRuntime.stringToClass(res, store.getLoader());
            if (clazz != null) {
                obj = clazz;
            } else {
                obj = "";
            }
        } else {
            obj = "";
        }

        if (branch != null)
            store.foyer.setCanonCache(relpath, obj);
        return ("".equals(obj) ? null : (Class) obj);
    }

    /**************************************************************/
    /* Find & Unfind Suffix                                       */
    /**************************************************************/

    /**
     * <p>Find a path suffix.</p>
     *
     * @param relpath The path, in slash notation.
     * @param src     The call-site, not null.
     * @param mask    The mask.
     * @return The source key, or null.
     */
    public static String findBinarySuffix(String relpath,
                                          AbstractSource src,
                                          int mask) {

        /* system binary suffix */
        if ((mask & ForeignPath.MASK_SUFX_BNRY) != 0) {
            AbstractStore store = src.getStore();
            while (store != null) {
                MapEntry<String, Integer>[] fixes = store.system.snapshotFixes();
                for (int i = 0; i < fixes.length; i++) {
                    MapEntry<String, Integer> fix = fixes[i];
                    if ((fix.value.intValue() & AbstractSource.MASK_USES_BNRY) != 0) {
                        String key = relpath + fix.key;
                        Class clazz = keyToClass(key, src.getStore());
                        if (clazz != null)
                            return key;
                    }
                }
                store = store.parent;
            }
        }

        // failure
        return null;
    }

    /**
     * <p>Unfind a path suffix.</p>
     *
     * @param relpath The path, in slash notation.
     * @param src     The call-site, not null.
     * @param mask    The mask.
     * @return The source key, or null.
     */
    public static String unfindBinarySuffix(String relpath,
                                            AbstractSource src,
                                            int mask) {

        /* system binary suffix */
        if ((mask & ForeignPath.MASK_SUFX_BNRY) != 0) {
            AbstractStore store = src.getStore();
            while (store != null) {
                MapEntry<String, Integer>[] fixes = store.system.snapshotFixes();
                for (int i = 0; i < fixes.length; i++) {
                    MapEntry<String, Integer> fix = fixes[i];
                    if ((fix.value.intValue() & AbstractSource.MASK_USES_BNRY) != 0) {
                        if (relpath.endsWith(fix.key)) {
                            String path2 = relpath.substring(0, relpath.length() - fix.key.length());
                            if (relpath.equals(findBinarySuffix(path2, src, mask)))
                                return path2;
                        }
                    }
                }
                store = store.parent;
            }
        }

        // failure
        return null;
    }

    /****************************************************************/
    /* Class Extension                                              */
    /****************************************************************/

    /**
     * <p>Remove the suffix in a best way.</p>
     *
     * @param path The path.
     * @return The path without suffix.
     */
    public static String removeClassExt(String path) {
        int k = path.lastIndexOf(CacheModule.OP_CHAR_OS);
        k = path.indexOf(CacheSubclass.OP_CHAR_SYN, k + 1);
        if (k != -1) {
            String res = path.substring(0, k);
            if (!res.endsWith(CLASS_EXTENSION))
                return null;
            res = res.substring(0, res.length() - CLASS_EXTENSION.length());
            return res + path.substring(k);
        } else {
            if (!path.endsWith(CLASS_EXTENSION))
                return null;
            return path.substring(0, path.length() - CLASS_EXTENSION.length());
        }
    }

    /**
     * <p>Add a suffix to a Java path.</p>
     *
     * @param path The Java path.
     * @return The extended Java path.
     */
    public static String addClassExt(String path) {
        int k = path.lastIndexOf(CacheModule.OP_CHAR_OS);
        k = path.indexOf(CacheSubclass.OP_CHAR_SYN, k + 1);
        if (k != -1) {
            String res = path.substring(0, k);
            res = res + CLASS_EXTENSION;
            return res + path.substring(k);
        } else {
            return path + CLASS_EXTENSION;
        }
    }

}
