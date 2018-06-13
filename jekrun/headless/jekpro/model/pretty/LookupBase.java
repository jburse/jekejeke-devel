package jekpro.model.pretty;

import jekpro.model.builtin.Branch;
import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.bootload.ForeignPath;
import matula.util.data.MapEntry;
import matula.util.system.ForeignFile;
import matula.util.system.ForeignUri;
import matula.util.system.OpenCheck;

import java.io.IOException;
import java.io.InterruptedIOException;
import java.net.SocketTimeoutException;

/**
 * <p>Concerned with the lookup of binaries/text, escpecially files and URLs</p>
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
public final class LookupBase {

    /***************************************************************/
    /* Find Write                                                  */
    /***************************************************************/

    /**
     * <p>Find a write path.</p>
     *
     * @param path The path, in slash notation.
     * @param en   The engine.
     * @return The source key, or null.
     * @throws IOException Shit happens.
     */
    public static String findWrite(String path,
                                   Engine en)
            throws IOException {
        if (ForeignUri.sysUriIsRelative(path)) {
            String base = en.store.getBase();
            if (base == null)
                throw new IOException(EngineMessage.OP_RESOURCE_BASEURL_MISSING);
            /* make it absolute */
            path = ForeignUri.sysUriAbsolute(base, path);
        }
        /* make it canonical */
        path = ForeignUri.sysCanonicalUri(path);

        return path;
    }

    /***************************************************************/
    /* Find & Unfind Read                                          */
    /***************************************************************/

    /**
     * <p>Find a read path.</p>
     *
     * @param path The path, in slash notation.
     * @param src  The call-site, not null.
     * @param en   The engine or null.
     * @return The source key, or null.
     * @throws IOException Shit happens.
     */
    public static String findRead(String path,
                                  AbstractSource src,
                                  Engine en)
            throws IOException {
        if (ForeignFile.STRING_EMPTY.equals(path))
            return null;

        if (ForeignUri.sysUriIsRelative(path)) {
            String base;
            if (ForeignUri.sysUriIsRelative(src.getPath())) {
                base = en.store.getBase();
                if (base == null)
                    throw new IOException(EngineMessage.OP_RESOURCE_BASEURL_MISSING);
            } else {
                base = src.getPath();
            }
            /* make it absolute */
            path = ForeignUri.sysUriAbsolute(base, path);
        }
        /* make it canonical */
        path = ForeignUri.sysCanonicalUri(path);

        boolean ok;
        try {
            ok = OpenCheck.DEFAULT_CHECK.checkHead(path);
        } catch (IOException x) {
            if (x instanceof InterruptedIOException &&
                    !(x instanceof SocketTimeoutException)) {
                throw x;
            } else {
                ok = false;
            }
        }
        if (ok)
            return path;
        return null;
    }

    /**
     * <p>Determine the relative variant of a path.</p>
     *
     * @param path The absolute path.
     * @param src  The call-site or null.
     * @param en   The engine or null.
     * @return The relative variant, or null.
     * @throws IOException Shit happens.
     */
    public static String unfindRead(String path,
                                    AbstractSource src,
                                    Engine en)
            throws IOException {
        String base;
        if (ForeignUri.sysUriIsRelative(src.getPath())) {
            base = en.store.getBase();
            if (base == null)
                throw new IOException(EngineMessage.OP_RESOURCE_BASEURL_MISSING);
        } else {
            base = src.getPath();
        }
        String path2 = ForeignUri.sysUriRelative(base, path);
        if (!path.equals(path2) && !ForeignFile.STRING_EMPTY.equals(path2))
            return path2;

        // failure
        return null;
    }

    /***************************************************************/
    /* Find & Unfind ReadSuffix                                    */
    /***************************************************************/

    /**
     * <p>Find a read path.</p>
     *
     * @param path The path.
     * @param src  The call-site, not null.
     * @param mask The mask.
     * @param en   The engine.
     * @return The source key, or null.
     * @throws IOException Shit happens.
     */
    public static String findReadSuffix(String path,
                                        AbstractSource src,
                                        int mask,
                                        Engine en)
            throws IOException {

        /* system text suffix */
        if ((mask & ForeignPath.MASK_SUFX_TEXT) != 0) {
            AbstractStore store = src.getStore();
            while (store != null) {
                MapEntry<String, Integer>[] fixes = store.system.snapshotFixes();
                for (int i = 0; i < fixes.length; i++) {
                    MapEntry<String, Integer> fix = fixes[i];
                    if ((fix.value.intValue() & AbstractSource.MASK_USES_TEXT) != 0) {
                        String key = findRead(path + fix.key, src, en);
                        if (key != null)
                            return key;
                    }
                }
                store = store.parent;
            }
        }

        /* system resource suffix */
        if ((mask & ForeignPath.MASK_SUFX_RSCS) != 0) {
            AbstractStore store = src.getStore();
            while (store != null) {
                MapEntry<String, Integer>[] fixes = store.system.snapshotFixes();
                for (int i = 0; i < fixes.length; i++) {
                    MapEntry<String, Integer> fix = fixes[i];
                    if ((fix.value.intValue() & AbstractSource.MASK_USES_RSCS) != 0) {
                        String key = findRead(path + fix.key, src, en);
                        if (key != null)
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
     * <p>Remove the suffix in the best way.</p>
     *
     * @param path The path.
     * @param src  The call-site, not null.
     * @param mask The mask.
     * @param en   The engine.
     * @return The path without suffix.
     * @throws IOException Shit happens.
     */
    public static String unfindReadSuffix(String path,
                                          AbstractSource src,
                                          int mask,
                                          Engine en)
            throws IOException {

        /* system text suffix */
        if ((mask & ForeignPath.MASK_SUFX_TEXT) != 0) {
            AbstractStore store = src.getStore();
            while (store != null) {
                MapEntry<String, Integer>[] fixes = store.system.snapshotFixes();
                for (int i = 0; i < fixes.length; i++) {
                    MapEntry<String, Integer> fix = fixes[i];
                    if ((fix.value.intValue() & AbstractSource.MASK_USES_TEXT) != 0) {
                        if (path.endsWith(fix.key)) {
                            String path2 = path.substring(0, path.length() - fix.key.length());
                            if (path.equals(findReadSuffix(path2, src, mask, en)))
                                return path2;
                        }
                    }
                }
                store = store.parent;
            }
        }

        /* system resource suffix */
        if ((mask & ForeignPath.MASK_SUFX_RSCS) != 0) {
            AbstractStore store = src.getStore();
            while (store != null) {
                MapEntry<String, Integer>[] fixes = store.system.snapshotFixes();
                for (int i = 0; i < fixes.length; i++) {
                    MapEntry<String, Integer> fix = fixes[i];
                    if ((fix.value.intValue() & AbstractSource.MASK_USES_RSCS) != 0) {
                        if (path.endsWith(fix.key)) {
                            String path2 = path.substring(0, path.length() - fix.key.length());
                            if (path.equals(findReadSuffix(path2, src, mask, en)))
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

}
