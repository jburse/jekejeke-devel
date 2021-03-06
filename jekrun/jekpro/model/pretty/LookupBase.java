package jekpro.model.pretty;

import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.bootload.ForeignPath;
import matula.util.data.MapEntry;
import matula.util.system.ForeignFile;
import matula.util.system.ForeignUri;
import matula.util.system.OpenCheck;
import matula.util.wire.FileExtension;

import java.io.IOException;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
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
     * @param path  The path, in slash notation.
     * @param store The store.
     * @return The source key, or null.
     * @throws IOException Shit happens.
     */
    public static String findWrite(String path, Store store)
            throws IOException {
        if (ForeignUri.sysUriIsRelative(path)) {
            String base = store.getBase();
            if (base == null)
                throw new IOException(EngineMessage.OP_RESOURCE_BASEURL_MISSING);
            /* make it absolute */
            path = ForeignUri.sysUriAbsolute(base, path);
        }
        /* make it canonical */
        return ForeignUri.sysCanonicalUri(path);
    }

    /**
     * <p>Determine the relative variant of a write path.</p>
     *
     * @param path The absolute path.
     * @param en   The engine or null.
     * @return The relative variant, or null.
     * @throws IOException Shit happens.
     */
    public static String unfindWrite(String path, Engine en)
            throws IOException {
        String base = en.store.getBase();
        if (base == null)
            throw new IOException(EngineMessage.OP_RESOURCE_BASEURL_MISSING);

        String path2 = ForeignUri.sysUriRelative(base, path);
        if (!path.equals(path2) && !ForeignFile.STRING_EMPTY.equals(path2))
            return path2;

        // failure
        return null;
    }

    /***************************************************************/
    /* Find & Unfind Read                                          */
    /***************************************************************/

    /**
     * <p>Find a read path.</p>
     *
     * @param path The path, in slash notation.
     * @param src  The call-site, non null.
     * @param en   The engine or null.
     * @return The source key, or null.
     * @throws IOException Shit happens.
     */
    public static String findRead(String path, AbstractSource src,
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
        return ForeignUri.sysCanonicalUri(path, OpenCheck.DEFAULT_CHECK);
    }

    /**
     * <p>Determine the relative variant of a read path.</p>
     *
     * @param path The absolute path.
     * @param src  The call-site or null.
     * @param en   The engine or null.
     * @return The relative variant, or null.
     * @throws IOException Shit happens.
     */
    public static String unfindRead(String path, AbstractSource src,
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
     * @param src  The call-site, non null.
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
            Store store = src.getStore();
            do {
                MapEntry<String, FileExtension>[] fixes = store.snapshotFileExtensions();
                for (int i = 0; i < fixes.length; i++) {
                    MapEntry<String, FileExtension> fix = fixes[i];
                    if ((fix.value.getType() & FileExtension.MASK_USES_TEXT) != 0) {
                        String key = findRead(addSuffix(path, fix.key), src, en);
                        if (key != null)
                            return key;
                    }
                }
                store = store.parent;
            } while (store != null);
        }

        // failure
        return null;
    }

    /**
     * <p>Find a read path.</p>
     *
     * @param path The path.
     * @param src  The call-site, non null.
     * @param mask The mask.
     * @param en   The engine.
     * @return The source key, or null.
     * @throws IOException Shit happens.
     */
    public static String findReadSuffix2(String path,
                                         AbstractSource src,
                                         int mask,
                                         Engine en)
            throws IOException {

        /* system resource suffix */
        if ((mask & ForeignPath.MASK_SUFX_RSCS) != 0) {
            Store store = src.getStore();
            do {
                MapEntry<String, FileExtension>[] fixes = store.snapshotFileExtensions();
                for (int i = 0; i < fixes.length; i++) {
                    MapEntry<String, FileExtension> fix = fixes[i];
                    if ((fix.value.getType() & FileExtension.MASK_USES_RSCS) != 0) {
                        String key = findRead(addSuffix(path, fix.key), src, en);
                        if (key != null)
                            return key;
                    }
                }
                store = store.parent;
            } while (store != null);
        }

        // failure
        return null;
    }

    /**
     * <p>Add a suffix.</p>
     *
     * @param adr    The URI.
     * @param suffix The suffix.
     * @return The result.
     */
    private static String addSuffix(String adr, String suffix) {
        String spec = ForeignUri.sysUriSpec(adr);
        String query = ForeignUri.sysUriQuery(adr);
        String hash = ForeignUri.sysUriHash(adr);

        return ForeignUri.sysUriMake(spec + suffix, query, hash);
    }

    /**
     * <p>Remove the suffix in the best way.</p>
     *
     * @param path The path.
     * @param src  The call-site, non null.
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
            Store store = src.getStore();
            do {
                MapEntry<String, FileExtension>[] fixes
                        = store.snapshotFileExtensions();
                for (int i = 0; i < fixes.length; i++) {
                    MapEntry<String, FileExtension> fix = fixes[i];
                    if ((fix.value.getType() & FileExtension.MASK_USES_TEXT) != 0) {
                        String path2;
                        if ((path2 = removeSuffix(path, fix.key)) != null) {
                            if (path.equals(findReadSuffix(path2, src, mask, en)))
                                return path2;
                        }
                    }
                }
                store = store.parent;
            } while (store != null);
        }

        // failure
        return null;
    }

    /**
     * <p>Remove the suffix in the best way.</p>
     *
     * @param path The path.
     * @param src  The call-site, non null.
     * @param mask The mask.
     * @param en   The engine.
     * @return The path without suffix.
     * @throws IOException Shit happens.
     */
    public static String unfindReadSuffix2(String path,
                                           AbstractSource src,
                                           int mask,
                                           Engine en)
            throws IOException {

        /* system resource suffix */
        if ((mask & ForeignPath.MASK_SUFX_RSCS) != 0) {
            Store store = src.getStore();
            do {
                MapEntry<String, FileExtension>[] fixes
                        = store.snapshotFileExtensions();
                for (int i = 0; i < fixes.length; i++) {
                    MapEntry<String, FileExtension> fix = fixes[i];
                    if ((fix.value.getType() & FileExtension.MASK_USES_RSCS) != 0) {
                        String path2;
                        if ((path2 = removeSuffix(path, fix.key)) != null) {
                            if (path.equals(findReadSuffix2(path2, src, mask, en)))
                                return path2;
                        }
                    }
                }
                store = store.parent;
            } while (store != null);
        }

        // failure
        return null;
    }

    /**
     * <p>Remove a suffix.</p>
     *
     * @param adr    The URI.
     * @param suffix The suffix.
     * @return The result or null.
     */
    private static String removeSuffix(String adr, String suffix) {
        String spec = ForeignUri.sysUriSpec(adr);
        if (!spec.endsWith(suffix))
            return null;
        String query = ForeignUri.sysUriQuery(adr);
        String hash = ForeignUri.sysUriHash(adr);

        return ForeignUri.sysUriMake(spec.substring(0, spec.length() -
                suffix.length()), query, hash);
    }

}
