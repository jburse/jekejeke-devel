package jekpro.tools.foreign;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.molec.CachePackage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.AbstractStore;
import jekpro.model.pretty.SourceLocal;
import jekpro.reference.bootload.ForeignPath;
import jekpro.tools.proxy.InterfaceHandler;
import jekpro.tools.proxy.InterfaceState;
import matula.util.data.MapEntry;
import matula.util.system.AbstractRuntime;

import java.lang.reflect.Proxy;

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
     * @param key   The path, in slash notation.
     * @param store The store.
     * @return The class, or null.
     */
    public static Class keyToClass(String key, AbstractStore store) {
        AbstractBranch branch = LookupResource.RelativeURIstoRoots(key, store);
        if (branch != null) {
            Object obj = store.foyer.getCanonCache(key);
            if (obj != null)
                return ("".equals(obj) ? null : (Class) obj);
        }

        Object obj;
        String res = LookupBinary.removeClassExt(key);
        if (res != null) {
            res = res.replace(SourceLocal.OP_CHAR_OS, CachePackage.OP_CHAR_SEG);
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
            store.foyer.setCanonCache(key, obj);
        return ("".equals(obj) ? null : (Class) obj);
    }

    /**
     * <p>Find a path suffix.</p>
     *
     * @param path  The path, in slash notation.
     * @param src   The call-site, not null.
     * @param mask  The mask.
     * @param store The store.
     * @return The source key, or null.
     */
    public static String findBinarySuffix(String path, AbstractSource src,
                                          int mask, AbstractStore store) {

        /* source binary suffix */
        if ((mask & ForeignPath.MASK_SUFX_BNRY) != 0 &&
                !src.equals(store.foyer.SOURCE_SYSTEM)) {
            MapEntry<String, Integer>[] fixes = src.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_USES_BNRY) != 0) {
                    String key = path + fix.key;
                    Class clazz = keyToClass(key, store);
                    if (clazz != null)
                        return key;
                }
            }
        }

        /* system binary suffix */
        if ((mask & ForeignPath.MASK_SUFX_BNRY) != 0) {
            MapEntry<String, Integer>[] fixes = store.foyer.SOURCE_SYSTEM.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_USES_BNRY) != 0) {
                    String key = path + fix.key;
                    Class clazz = keyToClass(key, store);
                    if (clazz != null)
                        return key;
                }
            }
        }

        /* failure */
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
        int k = path.lastIndexOf(SourceLocal.OP_CHAR_OS);
        k = path.indexOf(SourceLocal.OP_CHAR_SYN, k + 1);
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
        int k = path.lastIndexOf(SourceLocal.OP_CHAR_OS);
        k = path.indexOf(SourceLocal.OP_CHAR_SYN, k + 1);
        if (k != -1) {
            String res = path.substring(0, k);
            res = res + CLASS_EXTENSION;
            return res + path.substring(k);
        } else {
            return path + CLASS_EXTENSION;
        }
    }

}
