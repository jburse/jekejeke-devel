package jekpro.model.pretty;

import jekpro.model.builtin.Branch;
import jekpro.model.molec.CachePackage;

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
    /* Child Lookup                                                */
    /***************************************************************/

    /**
     * <p>Find a prefix according to the child rule.</p>
     *
     * @param path  The path.
     * @param src   The call-site, not null.
     * @param mask  The mask.
     * @param store The store.
     * @return The prefixed path or null.
     * @throws IOException Shit happens.
     */
    public static String findChildPrefix(String path, AbstractSource src,
                                         int mask, AbstractStore store)
            throws IOException {
        String res = src.getHomeName();
        res = (res != null ? res.replace(CachePackage.OP_CHAR_SEG, SourceLocal.OP_CHAR_OS) : null);
        res = LookupRead.findPrefix((res != null ? res : Branch.OP_USER), src, mask, store);
        if (res == null)
            // failure
            return null;

        String res2 = src.getRestName();
        if (res2 != null)
            res = SourceLocal.composeLocal(res, res2);
        path = path.replace(SourceLocal.OP_CHAR_OS, SourceLocal.OP_CHAR_SYN);
        return SourceLocal.composeLocal(res, path);
    }

    /**
     * <p>Find a prefix according to the child rule.</p>
     *
     * @param path  The path.
     * @param src   The call-site, not null.
     * @param mask  The mask.
     * @param store The store.
     * @return The prefixed path or null.
     * @throws IOException Shit happens.
     */
    public static String unfindChildPrefix(String path, AbstractSource src,
                                           int mask, AbstractStore store)
            throws IOException {
        String res = src.getHomeName();
        res = (res != null ? res.replace(CachePackage.OP_CHAR_SEG, SourceLocal.OP_CHAR_OS) : null);
        res = LookupRead.findPrefix((res != null ? res : Branch.OP_USER), src, mask, store);
        if (res == null)
            // failure
            return null;

        String res2 = src.getRestName();
        if (res2 != null)
            res = SourceLocal.composeLocal(res, res2);
        res = res + SourceLocal.OP_STRING_SYN;
        if (path.startsWith(res)) {
            path = path.substring(res.length());
            path = path.replace(SourceLocal.OP_CHAR_SYN, SourceLocal.OP_CHAR_OS);
            return path;
        }

        // failure
        return null;
    }
}