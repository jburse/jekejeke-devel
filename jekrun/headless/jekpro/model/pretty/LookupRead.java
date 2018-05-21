package jekpro.model.pretty;

import jekpro.model.builtin.Branch;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.bootload.ForeignPath;
import jekpro.tools.foreign.LookupBinary;
import jekpro.tools.foreign.LookupResource;
import matula.util.data.MapEntry;
import matula.util.system.ForeignUri;
import matula.util.system.OpenCheck;

import java.io.IOException;
import java.io.InterruptedIOException;
import java.net.SocketTimeoutException;

/**
 * <p>Concerned with the lookup of binaries/text, escpecially files and URLs</p>
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
public final class LookupRead {

    /***************************************************************/
    /* Find Suffix                                                 */
    /***************************************************************/

    /**
     * <p>Find a path.</p>
     *
     * @param path  The path, in slash notation.
     * @param src   The call-site, not null.
     * @param store The store.
     * @return The source key, or null.
     * @throws IOException Shit happens.
     */
    public static String findRead(String path, AbstractSource src,
                                  AbstractStore store)
            throws IOException {
        /* make it absolute */
        if (ForeignUri.sysUriIsRelative(path)) {
            String base;
            if (Branch.OP_USER.equals(src.getPath())) {
                base = store.foyer.base;
                if (base == null)
                    throw new IOException(EngineMessage.OP_RESOURCE_BASEURL_MISSING);
            } else {
                base = src.getPath();
            }
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
     * <p>Find a read path.</p>
     *
     * @param path  The path.
     * @param src   The call-site, not null.
     * @param mask  The mask.
     * @param store The store.
     * @return The source key, or null.
     * @throws IOException Shit happens.
     */
    private static String findReadSuffix(String path, AbstractSource src,
                                        int mask, AbstractStore store)
            throws IOException {

        /* system text suffix */
        if ((mask & ForeignPath.MASK_SUFX_TEXT) != 0) {
            MapEntry<String, Integer>[] fixes = store.foyer.SOURCE_SYSTEM.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_USES_TEXT) != 0) {
                    String key = findRead(path + fix.key, src, store);
                    if (key != null)
                        return key;
                }
            }
        }

        /* system resource suffix */
        if ((mask & ForeignPath.MASK_SUFX_RSCS) != 0) {
            MapEntry<String, Integer>[] fixes = store.foyer.SOURCE_SYSTEM.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_USES_RSCS) != 0) {
                    String key = findRead(path + fix.key, src, store);
                    if (key != null)
                        return key;
                }
            }
        }

        /* source text suffix */
        if ((mask & ForeignPath.MASK_SUFX_TEXT) != 0 &&
                !src.equals(store.foyer.SOURCE_SYSTEM)) {
            MapEntry<String, Integer>[] fixes = src.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_USES_TEXT) != 0) {
                    String key = findRead(path + fix.key, src, store);
                    if (key != null)
                        return key;
                }
            }
        }

        /* source resource suffix */
        if ((mask & ForeignPath.MASK_SUFX_RSCS) != 0 &&
                !src.equals(store.foyer.SOURCE_SYSTEM)) {
            MapEntry<String, Integer>[] fixes = src.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_USES_RSCS) != 0) {
                    String key = findRead(path + fix.key, src, store);
                    if (key != null)
                        return key;
                }
            }
        }

        // failure
        return null;
    }

    /**
     * <p>Find a prefix according to the auto loader.</p>
     *
     * @param path  The path.
     * @param src   The call-site, not null.
     * @param mask  The mask.
     * @param store The store.
     * @return The prefixed path or null.
     * @throws IOException Shit happens.
     */
    public static String findPrefix(String path, AbstractSource src,
                                    int mask, AbstractStore store)
            throws IOException {

        /* special case */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            if (Branch.OP_USER.equals(path))
                return path;
        }

        src = AbstractSource.derefParent(src);

        /* library .p */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            String key = LookupResource.findResourceSuffix(path, src, mask, store);
            if (key != null)
                return path;
        }

        /* foreign .class */
        if ((mask & ForeignPath.MASK_PRFX_FRGN) != 0) {
            String key = LookupBinary.findBinarySuffix(path, src, mask, store);
            if (key != null)
                return path;
        }

        /* system library .p */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            MapEntry<String, Integer>[] fixes = store.foyer.SOURCE_SYSTEM.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_PRFX_LIBR) != 0) {
                    String path2 = fix.key + SourceLocal.OP_STRING_OS + path;
                    String key = LookupResource.findResourceSuffix(path2, src, mask, store);
                    if (key != null)
                        return path2;
                }
            }
        }

        /* system imported .class */
        if ((mask & ForeignPath.MASK_PRFX_FRGN) != 0) {
            MapEntry<String, Integer>[] fixes = store.foyer.SOURCE_SYSTEM.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_PRFX_FRGN) != 0) {
                    String path2 = fix.key + SourceLocal.OP_STRING_OS + path;
                    String key = LookupBinary.findBinarySuffix(path2, src, mask, store);
                    if (key != null)
                        return path2;
                }
            }
        }

        /* source library .p */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0 &&
                !src.equals(store.foyer.SOURCE_SYSTEM)) {
            MapEntry<String, Integer>[] fixes = src.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_PRFX_LIBR) != 0) {
                    String path2 = fix.key + SourceLocal.OP_STRING_OS + path;
                    String key = LookupResource.findResourceSuffix(path2, src, mask, store);
                    if (key != null)
                        return path2;
                }
            }
        }

        /* source imported .class */
        if ((mask & ForeignPath.MASK_PRFX_FRGN) != 0 &&
                !src.equals(store.foyer.SOURCE_SYSTEM)) {
            MapEntry<String, Integer>[] fixes = src.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_PRFX_FRGN) != 0) {
                    String path2 = fix.key + SourceLocal.OP_STRING_OS + path;
                    String key = LookupBinary.findBinarySuffix(path2, src, mask, store);
                    if (key != null)
                        return path2;
                }
            }
        }

        // failure
        return null;
    }

    /**
     * <p>Remove the prefix in the best way.</p>
     *
     * @param path  The path.
     * @param src   The call-site, not null.
     * @param mask  The mask.
     * @param store The store.
     * @return The class.
     * @throws IOException Shit happens.
     */
    public static String unfindPrefix(String path, AbstractSource src,
                                      int mask, AbstractStore store)
            throws IOException {

        src = AbstractSource.derefParent(src);

        /* source imported .class */
        if ((mask & ForeignPath.MASK_PRFX_FRGN) != 0 &&
                !src.equals(store.foyer.SOURCE_SYSTEM)) {
            MapEntry<String, Integer>[] fixes = src.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_PRFX_FRGN) != 0) {
                    if (path.startsWith(fix.key) && path.startsWith(SourceLocal.OP_STRING_OS, fix.key.length())) {
                        String path2 = path.substring(fix.key.length() + 1);
                        if (path.equals(LookupRead.findPrefix(path2, src, mask, store)))
                            return path2;
                    }
                }
            }
        }

         /* source library .p */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0 &&
                !src.equals(store.foyer.SOURCE_SYSTEM)) {
            MapEntry<String, Integer>[] fixes = src.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_PRFX_LIBR) != 0) {
                    if (path.startsWith(fix.key) && path.startsWith(SourceLocal.OP_STRING_OS, fix.key.length())) {
                        String path2 = path.substring(fix.key.length() + 1);
                        if (path.equals(LookupRead.findPrefix(path2, src, mask, store)))
                            return path2;
                    }
                }
            }
        }

        /* system imported .class */
        if ((mask & ForeignPath.MASK_PRFX_FRGN) != 0) {
            MapEntry<String, Integer>[] fixes = store.foyer.SOURCE_SYSTEM.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_PRFX_FRGN) != 0) {
                    if (path.startsWith(fix.key) && path.startsWith(SourceLocal.OP_STRING_OS, fix.key.length())) {
                        String path2 = path.substring(fix.key.length() + 1);
                        if (path.equals(LookupRead.findPrefix(path2, src, mask, store)))
                            return path2;
                    }
                }
            }
        }

        /* system library .p */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            MapEntry<String, Integer>[] fixes = store.foyer.SOURCE_SYSTEM.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_PRFX_LIBR) != 0) {
                    if (path.startsWith(fix.key) && path.startsWith(SourceLocal.OP_STRING_OS, fix.key.length())) {
                        String path2 = path.substring(fix.key.length() + 1);
                        if (path.equals(LookupRead.findPrefix(path2, src, mask, store)))
                            return path2;
                    }
                }
            }
        }

        // failure
        return path;
    }

    /**
     * <p>Find a key according to the auto loader.</p>
     *
     * @param path  The path.
     * @param src   The source, not null.
     * @param mask  The mask.
     * @param store The store.
     * @return The source key.
     * @throws IOException Shit happens.
     */
    public static String findKey(String path,
                                 AbstractSource src,
                                 int mask, AbstractStore store)
            throws IOException {

        /* special case */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            if (Branch.OP_USER.equals(path))
                return path;
        }

        src = AbstractSource.derefParent(src);

        /* library .p */
        if ((mask & ForeignPath.MASK_PRFX_LIBR) != 0) {
            String key = LookupResource.findResourceSuffix(path, src, mask, store);
            if (key != null)
                return key;
        }

        /* foreign .class */
        if ((mask & ForeignPath.MASK_PRFX_FRGN) != 0) {
            String key = LookupBinary.findBinarySuffix(path, src, mask, store);
            if (key != null)
                return key;
        }

        /* failure read */
        if ((mask & ForeignPath.MASK_FAIL_READ) != 0) {
            String key = findReadSuffix(path, src, mask, store);
            if (key != null)
                return key;
            key = findRead(path, src, store);
            if (key != null)
                return key;
        }

        // failure
        return null;
    }

    /**
     * <p>Remove the suffix in the best way.</p>
     *
     * @param path  The path.
     * @param src   The call-site, not null.
     * @param mask  The mask.
     * @param store The store.
     * @return The path without suffix.
     * @throws IOException Shit happens.
     */
    public static String unfindReadSuffix(String path, AbstractSource src,
                                          int mask, AbstractStore store)
            throws IOException {

        src = AbstractSource.derefParent(src);

        /* source text suffix */
        if ((mask & ForeignPath.MASK_SUFX_TEXT) != 0 &&
                !src.equals(store.foyer.SOURCE_SYSTEM)) {
            MapEntry<String, Integer>[] fixes = src.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_USES_TEXT) != 0) {
                    if (path.endsWith(fix.key)) {
                        String path2 = path.substring(0, path.length() - fix.key.length());
                        if (path.equals(findReadSuffix(path2, src, mask, store)))
                            return path2;
                    }
                }
            }
        }

        /* source resource suffix */
        if ((mask & ForeignPath.MASK_SUFX_RSCS) != 0 &&
                !src.equals(store.foyer.SOURCE_SYSTEM)) {
            MapEntry<String, Integer>[] fixes = src.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_USES_RSCS) != 0) {
                    if (path.endsWith(fix.key)) {
                        String path2 = path.substring(0, path.length() - fix.key.length());
                        if (path.equals(findReadSuffix(path2, src, mask, store)))
                            return path2;
                    }
                }
            }
        }

        /* system text suffix */
        if ((mask & ForeignPath.MASK_SUFX_TEXT) != 0) {
            MapEntry<String, Integer>[] fixes = store.foyer.SOURCE_SYSTEM.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_USES_TEXT) != 0) {
                    if (path.endsWith(fix.key)) {
                        String path2 = path.substring(0, path.length() - fix.key.length());
                        if (path.equals(findReadSuffix(path2, src, mask, store)))
                            return path2;
                    }
                }
            }
        }

        /* system resource suffix */
        if ((mask & ForeignPath.MASK_SUFX_RSCS) != 0) {
            MapEntry<String, Integer>[] fixes = store.foyer.SOURCE_SYSTEM.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_USES_RSCS) != 0) {
                    if (path.endsWith(fix.key)) {
                        String path2 = path.substring(0, path.length() - fix.key.length());
                        if (path.equals(findReadSuffix(path2, src, mask, store)))
                            return path2;
                    }
                }
            }
        }

        return path;
    }

    /**
     * <p>Determine the relative variant of a path.</p>
     *
     * @param path  The absolute path.
     * @param src   The source.
     * @param store The store.
     * @return The relative variant, or null.
     * @throws IOException Shit happens.
     */
    public static String unfindRead(String path,
                                    AbstractSource src,
                                    AbstractStore store)
            throws IOException {
        String base;
        if (src == null ||
                Branch.OP_USER.equals(src.getPath()) ||
                Branch.OP_SYSTEM.equals(src.getPath())) {
            base = store.foyer.base;
            if (base == null)
                throw new IOException(EngineMessage.OP_RESOURCE_BASEURL_MISSING);
        } else {
            base = src.getPath();
        }
        int k = base.lastIndexOf('/');
        if (k == -1)
            return null;
        base = base.substring(0, k + 1);
        if (path.startsWith(base))
            return path.substring(base.length());

        // failure
        return null;
    }

}
