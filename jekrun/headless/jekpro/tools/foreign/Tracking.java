package jekpro.tools.foreign;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import matula.comp.sharik.AbstractTracking;
import matula.util.config.AbstractBundle;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.system.CacheBounded;
import matula.util.system.ForeignUri;

import java.io.IOException;
import java.net.URL;
import java.util.Enumeration;

/**
 * <p>Information associate to a bundle at runtime.s</p>
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
public final class Tracking extends AbstractTracking {
    final static String NOT_FOUND = "";

    private String[] archiveuris;
    private final CacheBounded<String, Object> canoncache
            = new CacheBounded<String, Object>();

    /**
     * <p>Retrieve the archive URIs</p>
     *
     * @return The archive URIs.
     */
    public String[] getArchiveURIs() {
        return archiveuris;
    }

    /**
     * <p>Set the archive URIs.</p>
     *
     * @param a The archive URIs.
     */
    public void setArchiveURIs(String[] a) {
        archiveuris = a;
    }

    /**********************************************************/
    /* Canon Cache                                            */
    /**********************************************************/

    /**
     * <p>Retrieve the canon cache result.</p>
     *
     * @param path The path.
     * @return The result.
     */
    public Object getCanonCache(String path) {
        synchronized (this) {
            return canoncache.get(path);
        }
    }

    /**
     * <p>Set the canon cache result.</p>
     *
     * @param path The path.
     * @param res  The result.
     */
    public void setCanonCache(String path, Object res) {
        if (res == null)
            throw new NullPointerException("result missing");
        synchronized (this) {
            if (canoncache.get(path) == null)
                canoncache.add(path, res);
        }
    }

    /**
     * <p>Clear the canon cache.</p>
     */
    public void clearCanonCache() {
        synchronized (this) {
            canoncache.clear();
        }
    }

    /**
     * <p>Clear the canon cache.</p>
     *
     * @param foyer The foyer.
     */
    public static void clearCanonCaches(Foyer foyer) {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            if (!(tracking instanceof Tracking))
                continue;
            ((Tracking)tracking).clearCanonCache();
        }
    }

    /*********************************************************/
    /* Precompute Helper                                     */
    /*********************************************************/

    /**
     * <p>Precompute the uris of a root.</p>
     *
     * @param res    The target list.
     * @param root   The root.
     * @param loader The loader.
     * @param path   The well known path.
     * @throws IOException Shit happens.
     */
    public static void rootToAbsoluteCheck(ListArray<String> res, String root,
                                           ClassLoader loader, String path)
            throws IOException {
        Enumeration<URL> urls = loader.getResources(root + path);
        while (urls.hasMoreElements()) {
            String uri = urls.nextElement().toString();
            uri = ForeignUri.sysCanonicalUri(uri);
            uri = uri.substring(0, uri.length() - path.length());
            if (!res.contains(uri))
                res.add(uri);
        }
    }

    /**
     * <p>Determine the bundle and tracking for a relative path.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param path  The relative or absolute path.
     * @param foyer The foyer.
     * @return The branch, or null.
     */
    public static MapEntry<AbstractBundle, AbstractTracking> relativeURIstoRoots(String path, Foyer foyer) {
        /* for the capabilities */
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBundle bundle = entry.key;
            if (!(bundle instanceof AbstractBranch))
                continue;
            String[] roots = ((AbstractBranch)bundle).getArchiveRoots();
            if (roots == null)
                continue;
            for (int j = 0; j < roots.length; j++) {
                if (path.startsWith(roots[j]))
                    return entry;
            }
        }

        // failure
        return null;
    }

    /**
     * <p>Determine the bundle and tracking for an absolute path.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param path  The relative or absolute path.
     * @param foyer The foyer.
     * @return The branch, or null.
     */
    public static MapEntry<AbstractBundle, AbstractTracking> absoluteURIstoRoots(String path, Foyer foyer) {
        /* for the capabilities */
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            if (!(tracking instanceof Tracking))
                continue;
            String[] uris = ((Tracking)tracking).getArchiveURIs();
            if (uris == null)
                continue;
            for (int k = 0; k < uris.length; k++) {
                if (path.startsWith(uris[k]))
                    return entry;
            }
        }

        // failure
        return null;
    }

}
