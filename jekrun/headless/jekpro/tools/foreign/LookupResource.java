package jekpro.tools.foreign;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.Tracking;
import jekpro.model.molec.CacheSubclass;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.AbstractStore;
import jekpro.model.pretty.LookupChild;
import jekpro.reference.bootload.ForeignPath;
import matula.comp.sharik.AbstractBundle;
import matula.comp.sharik.AbstractTracking;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.system.ForeignUri;

import java.io.IOException;
import java.net.URL;
import java.util.Enumeration;

/**
 * <p>Concerned with the lookup of texts, escpecially Java resources</p>
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
public final class LookupResource {
    private final static String OP_ROOT = "root.propertiesx";
    private final static String OP_JAR_COLON = "jar:";
    private final static String OP_BANG_SLASH = "!/";

    /**
     * <p>Find a path.</p>
     *
     * @param relpath  The path, in slash notation.
     * @param store The store.
     * @return The source key, or null.
     * @throws IOException Shit happens.
     */
    public static String findResource(String relpath, AbstractStore store)
            throws IOException {
        AbstractBranch branch = RelativeURIstoRoots(relpath, store);
        if (branch != null) {
            String res = (String)store.foyer.getCanonCache(relpath);
            if (res != null)
                return ("".equals(res) ? null : res);
        }

        ClassLoader loader = store.getLoader();
        URL url = loader.getResource(relpath);
        String res;
        if (url != null) {
            res = ForeignUri.sysCanonicalUri(url.toString());
        } else {
            res = "";
        }

        if (branch != null)
            store.foyer.setCanonCache(relpath, res);
        return ("".equals(res) ? null : res);
    }

    /**
     * <p>Find a path suffix.</p>
     *
     * @param relpath  The path, in slash notation.
     * @param src   The source, not null.
     * @param mask  The mask.
     * @return The source key, or null.
     * @throws IOException Shit happens.
     */
    public static String findResourceSuffix(String relpath,
                                            AbstractSource src,
                                            int mask)
            throws IOException {

        AbstractSource src2 = LookupChild.derefParentImport(src);

        /* source text suffix */
        if ((mask & ForeignPath.MASK_SUFX_TEXT) != 0 &&
                !src2.equals(src.getStore().foyer.SOURCE_SYSTEM)) {
            MapEntry<String, Integer>[] fixes = src2.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_USES_TEXT) != 0) {
                    String key = LookupResource.findResource(relpath + fix.key, src.getStore());
                    if (key != null)
                        return key;
                }
            }
        }

        /* source resource suffix */
        if ((mask & ForeignPath.MASK_SUFX_RSCS) != 0 &&
                !src2.equals(src.getStore().foyer.SOURCE_SYSTEM)) {
            MapEntry<String, Integer>[] fixes = src2.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_USES_RSCS) != 0) {
                    String key = findResource(relpath + fix.key, src.getStore());
                    if (key != null)
                        return key;
                }
            }
        }

        /* system text suffix */
        if ((mask & ForeignPath.MASK_SUFX_TEXT) != 0) {
            MapEntry<String, Integer>[] fixes = src.getStore().foyer.SOURCE_SYSTEM.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_USES_TEXT) != 0) {
                    String key = findResource(relpath + fix.key, src.getStore());
                    if (key != null)
                        return key;
                }
            }
        }

        /* system resource suffix */
        if ((mask & ForeignPath.MASK_SUFX_RSCS) != 0) {
            MapEntry<String, Integer>[] fixes = src.getStore().foyer.SOURCE_SYSTEM.snapshotFixes();
            for (int i = 0; i < fixes.length; i++) {
                MapEntry<String, Integer> fix = fixes[i];
                if ((fix.value.intValue() & AbstractSource.MASK_USES_RSCS) != 0) {
                    String key = findResource(relpath + fix.key, src.getStore());
                    if (key != null)
                        return key;
                }
            }
        }

        // failure
        return null;
    }

    /***************************************************************/
    /* Archive URIs                                                */
    /***************************************************************/

    /**
     * <p>Precompute the uris of the roots.</p>
     *
     * @param roots The roots.
     * @param store The store.
     * @return The uris.
     * @throws IOException Shit happens.
     */
    public static String[][] rootsToAbsoluteURIs(String[] roots, AbstractStore store)
            throws IOException {
        ListArray<String[]> res = new ListArray<String[]>();
        ClassLoader loader = store.getLoader();
        for (int j = 0; j < roots.length; j++) {
            ListArray<String> res2 = new ListArray<String>();
            Enumeration<URL> urls = loader.getResources(roots[j] + LookupResource.OP_ROOT);
            while (urls.hasMoreElements()) {
                String uri = ForeignUri.sysCanonicalUri(urls.nextElement().toString());
                res2.add(uri.substring(0, uri.length() - LookupResource.OP_ROOT.length()));
            }
            String[] uris2 = new String[res2.size()];
            res2.toArray(uris2);
            res.add(uris2);
        }
        String[][] uris = new String[res.size()][];
        res.toArray(uris);
        return uris;
    }

    /**
     * <p>Determine the branch for a path.</p>
     * <p>Relative paths are assumed to start in one of the archives.</p>
     * <p>Absolute paths are assumed to start from nowhere.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param path  The relative or absolute path.
     * @param store The store.
     * @return The branch, or null.
     */
    public static AbstractBranch RelativeURIstoRoots(String path, AbstractStore store) {
        /* for the capabilities */
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            Tracking tracking = (Tracking) entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            String[] roots = branch.getArchiveRoots();
            for (int j = 0; j < roots.length; j++) {
                if (path.startsWith(roots[j]))
                    return branch;
            }
        }

        // failure
        return null;
    }

    /**
     * <p>Determine the branch for a path.</p>
     * <p>Relative paths are assumed to start in one of the archives.</p>
     * <p>Absolute paths are assumed to start from nowhere.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param path  The relative or absolute path.
     * @param store The store.
     * @return The branch, or null.
     */
    public static AbstractBranch AbsoluteURIstoRoots(String path, AbstractStore store) {
        /* for the capabilities */
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            Tracking tracking = (Tracking) entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            String[] roots = branch.getArchiveRoots();
            String[][] uris = tracking.getArchiveURIs();
            for (int j = 0; j < roots.length; j++) {
                String[] uris2 = uris[j];
                for (int k = 0; k < uris2.length; k++) {
                    if (path.startsWith(uris2[k]))
                        return branch;
                }
            }
        }

        // failure
        return null;
    }

    /***************************************************************/
    /* Class Path URIs                                             */
    /***************************************************************/

    /**
     * <p>Determine the relative variant of a path.</p>
     *
     * @param path  The absolute path.
     * @param store The store.
     * @return The relative variant or null.
     * @throws EngineMessage Shit happens.
     */
    public static String unfindResourcePaths(String path, AbstractStore store)
            throws EngineMessage {
        // parent success
        AbstractStore parent = store.parent;
        if (parent != null) {
            String res = unfindResourcePaths(path, parent);
            if (res != null)
                return res;
        }

        // check paths
        String[] cps = store.snapshotPaths();
        for (int i = 0; i < cps.length; i++) {
            String cp = cps[i];
            if (cp.endsWith(CacheSubclass.OP_STRING_OS)) {
                if (!path.startsWith(cp))
                    continue;
                return path.substring(cp.length());
            } else {
                if (!path.startsWith(OP_JAR_COLON))
                    continue;
                if (!path.startsWith(cp, OP_JAR_COLON.length()))
                    continue;
                if (!path.startsWith(OP_BANG_SLASH, OP_JAR_COLON.length() + cp.length()))
                    continue;
                return path.substring(OP_JAR_COLON.length() + cp.length() + OP_BANG_SLASH.length());
            }
        }

        // failure
        return null;
    }

    /**
     * <p>Determine whether path belongs to class paths.</p>
     *
     * @param path The path.
     * @param store The store.
     * @return True if path belongs to class paths, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public static boolean hasSourceFile(String path, AbstractStore store)
            throws EngineMessage {
        // check paths
        String[] cps = store.snapshotPaths();
        for (int i = 0; i < cps.length; i++) {
            String cp = cps[i];
            if (cp.endsWith(CacheSubclass.OP_STRING_OS)) {
                if (!path.startsWith(cp))
                    continue;
                return true;
            } else {
                if (!path.startsWith(OP_JAR_COLON))
                    continue;
                if (!path.startsWith(cp, OP_JAR_COLON.length()))
                    continue;
                if (!path.startsWith(OP_BANG_SLASH, OP_JAR_COLON.length() + cp.length()))
                    continue;
                return true;
            }
        }

        // failure
        return false;
    }

}
