package jekpro.model.pretty;

import jekpro.model.builtin.Branch;
import jekpro.model.molec.CacheModule;
import jekpro.model.molec.CachePackage;
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
        String res = LookupChild.lookupFullName(src);
        res = res.replace(CachePackage.OP_CHAR_SEG, SourceLocal.OP_CHAR_OS);
        relpath = relpath.replace(SourceLocal.OP_CHAR_OS, SourceLocal.OP_CHAR_SYN);
        return SourceLocal.composeLocal(res, relpath);
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
        String res = LookupChild.lookupFullName(src);
        res = res.replace(CachePackage.OP_CHAR_SEG, SourceLocal.OP_CHAR_OS);
        if (relpath.startsWith(res) && relpath.startsWith(SourceLocal.OP_STRING_SYN, res.length())) {
            relpath = relpath.substring(res.length() + SourceLocal.OP_STRING_SYN.length());
            if (CacheModule.findPrefixParent(relpath, src, mask) == null) {
                relpath = relpath.replace(SourceLocal.OP_CHAR_SYN, SourceLocal.OP_CHAR_OS);
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
    public static String findChildKey(String relpath,
                                      AbstractSource src) {
        AbstractSource src2 = LookupChild.derefParent(src);
        String res = LookupChild.lookupFullName(src2);
        res = res.replace(CachePackage.OP_CHAR_SEG, SourceLocal.OP_CHAR_OS);
        if (relpath.startsWith(res) && relpath.startsWith(SourceLocal.OP_STRING_SYN, res.length())) {
            relpath = relpath.substring(res.length() + SourceLocal.OP_STRING_SYN.length());
            res = (src2.getName() != null ? src2.getPath() : Branch.OP_USER);
            return SourceLocal.composeLocal(res, relpath);
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
    public static String unfindChildSuffix(String path, AbstractSource src) {
        AbstractSource src2 = LookupChild.derefParent(src);
        String res = (src2.getName() != null ? src2.getPath() : Branch.OP_USER);
        if (path.startsWith(res) && path.startsWith(SourceLocal.OP_STRING_SYN, res.length())) {
            path = path.substring(res.length() + SourceLocal.OP_STRING_SYN.length());
            res = LookupChild.lookupFullName(src2);
            res = res.replace(CachePackage.OP_CHAR_SEG, SourceLocal.OP_CHAR_OS);
            return SourceLocal.composeLocal(res, path);
        }

        // failure
        return null;
    }

    /*******************************************************************/
    /* Module Names                                                    */
    /*******************************************************************/


    /**
     * <p>Lookup full name.</p>
     *
     * @return The full name.
     */
    public static String lookupFullName(AbstractSource src) {
        String temp = getChainName(src);
        src = LookupChild.derefParent(src);
        String res = getPackName(src);
        if (res != null) {
            return CachePackage.composeStruct(res, temp);
        } else {
            return temp;
        }
    }

    /**
     * <p>Retrieve the rest name.</p>
     *
     * @param src The source.
     * @return The rest name.
     */
    private static String getChainName(AbstractSource src) {
        String res = src.getName();
        if (res == null)
            res = Branch.OP_USER;
        src = LookupChild.getParent(src);
        while (src != null) {
            String res2 = src.getName();
            if (res2 == null)
                res2 = Branch.OP_USER;
            res = SourceLocal.composeLocal(res2, res);
            src = LookupChild.getParent(src);
        }
        return res;
    }


    /**
     * <p>Retrieve the package name.</p>
     *
     * @param src The source.
     * @return The package name.
     */
    private static String getPackName(AbstractSource src) {
        MapEntry<String, Integer>[] fixes = src.snapshotFixes();
        for (int i = 0; i < fixes.length; i++) {
            MapEntry<String, Integer> fix = fixes[i];
            if ((fix.value.intValue() & AbstractSource.MASK_PCKG_AUTO) != 0) {
                String temp = fix.key;
                temp = temp.replace(SourceLocal.OP_CHAR_OS, CachePackage.OP_CHAR_SEG);
                return temp;
            }
        }
        return null;
    }

    /*******************************************************************/
    /* Nested Modules                                                  */
    /*******************************************************************/

    /**
     * <p>Retrieve the primordial parent.</p>
     *
     * @param src The source.
     * @return The primordial parent.
     */
    public static AbstractSource derefParent(AbstractSource src) {
        AbstractSource src2 = LookupChild.getParent(src);
        while (src2 != null) {
            src = src2;
            src2 = LookupChild.getParent(src);
        }
        return src;
    }

    /**
     * <p>Retrieve the parent module.</p>
     *
     * @return The parent module.
     */
    public static AbstractSource getParent(AbstractSource src) {
        MapEntry<AbstractSource, Integer>[] deps = src.snapshotDeps();
        for (int i = 0; i < deps.length; i++) {
            MapEntry<AbstractSource, Integer> dep = deps[i];
            if ((dep.value.intValue() & AbstractSource.MASK_IMPT_PARM) != 0)
                return dep.key;
        }
        return null;
    }

}