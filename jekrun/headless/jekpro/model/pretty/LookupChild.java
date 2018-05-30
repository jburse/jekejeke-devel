package jekpro.model.pretty;

import jekpro.model.builtin.Branch;
import jekpro.model.molec.CacheModule;
import jekpro.model.molec.CachePackage;
import jekpro.model.molec.CacheSubclass;
import matula.util.data.MapEntry;

import java.io.IOException;

/**
 * <p>Concerned with the lookup of inline local modules.</p>
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
public final class LookupChild {

    /***************************************************************/
    /* Find & Unfind Prefix                                        */
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
        if (Branch.OP_USER.equals(res) && !Branch.OP_USER.equals(src.getPath()))
            return null;
        res = res.replace(CachePackage.OP_CHAR_SEG, CacheSubclass.OP_CHAR_OS);
        relpath = relpath.replace(CacheSubclass.OP_CHAR_OS, CacheSubclass.OP_CHAR_SYN);
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
        if (Branch.OP_USER.equals(res) && !Branch.OP_USER.equals(src.getPath()))
            return null;
        res = res.replace(CachePackage.OP_CHAR_SEG, CacheSubclass.OP_CHAR_OS);
        if (relpath.startsWith(res) && relpath.startsWith(CacheSubclass.OP_STRING_SYN, res.length())) {
            relpath = relpath.substring(res.length() + CacheSubclass.OP_STRING_SYN.length());
            if (CacheModule.findPrefixParent(relpath, src, mask) == null) {
                relpath = relpath.replace(CacheSubclass.OP_CHAR_SYN, CacheSubclass.OP_CHAR_OS);
                return relpath;
            }
        }

        // failure
        return null;
    }

    /***************************************************************/
    /* Child Lookup                                                */
    /***************************************************************/

    /**
     * <p>Find a key according to the child rule.</p>
     *
     * @param relpath The path.
     * @param src     The call-site, not null.
     * @return The source key.
     */
    public static String findChildKey(String relpath, AbstractSource src) {
        String res = src.getFullName();
        if (Branch.OP_USER.equals(res) && !Branch.OP_USER.equals(src.getPath()))
            return null;
        res = (CacheSubclass.isLocal(res) ? CacheSubclass.sepHome(res) : res);
        res = res.replace(CachePackage.OP_CHAR_SEG, CacheSubclass.OP_CHAR_OS);
        if (relpath.startsWith(res) && relpath.startsWith(CacheSubclass.OP_STRING_SYN, res.length())) {
            relpath = relpath.substring(res.length() + CacheSubclass.OP_STRING_SYN.length());
            res = src.getPath();
            res = (CacheSubclass.isLocal(res) ? CacheSubclass.sepHome(res) : res);
            return CacheSubclass.composeLocal(res, relpath);
        }

        // failure
        return null;
    }

    /**
     * <p>Remove the suffix according to the child rule.</p>
     *
     * @param path The path.
     * @param src  The call-site, not null.
     * @return The path without suffix.
     */
    public static String unfindChildKey(String path, AbstractSource src) {
        String res = src.getPath();
        if (!Branch.OP_USER.equals(res) && Branch.OP_USER.equals(src.getFullName()))
            return null;
        res = (CacheSubclass.isLocal(res) ? CacheSubclass.sepHome(res) : res);
        if (path.startsWith(res) && path.startsWith(CacheSubclass.OP_STRING_SYN, res.length())) {
            path = path.substring(res.length() + CacheSubclass.OP_STRING_SYN.length());
            res = src.getFullName();
            res = (CacheSubclass.isLocal(res) ? CacheSubclass.sepHome(res) : res);
            res = res.replace(CachePackage.OP_CHAR_SEG, CacheSubclass.OP_CHAR_OS);
            return CacheSubclass.composeLocal(res, path);
        }

        // failure
        return null;
    }

    /*******************************************************************/
    /* Import Link                                                     */
    /*******************************************************************/

    /**
     * <p>Retrieve the primordial parent.</p>
     *
     * @param src The source.
     * @return The primordial parent.
     */
    public static AbstractSource derefParentImport(AbstractSource src) {
        AbstractSource src2 = LookupChild.getParentImport(src);
        while (src2 != null) {
            src = src2;
            src2 = LookupChild.getParentImport(src);
        }
        return src;
    }

    /**
     * <p>Retrieve the parent module.</p>
     *
     * @return The parent module.
     */
    public static AbstractSource getParentImport(AbstractSource src) {
        MapEntry<AbstractSource, Integer>[] deps = src.snapshotDeps();
        for (int i = 0; i < deps.length; i++) {
            MapEntry<AbstractSource, Integer> dep = deps[i];
            if ((dep.value.intValue() & AbstractSource.MASK_IMPT_PAIM) != 0)
                return dep.key;
        }
        return null;
    }

}