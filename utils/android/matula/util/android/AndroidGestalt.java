package matula.util.android;

import android.app.Application;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.os.Environment;
import matula.util.config.GestaltEntry;
import matula.util.data.ListArray;
import matula.util.system.ForeignUri;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * <p>Hotspot current directory and path discovery.</p>
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
public final class AndroidGestalt {

    /**
     * <p>Retrieve the base url.</p>
     *
     * @return The base url.
     */
    public static String getBase() {
        File userdir = new File(Environment.getDataDirectory(), "app");
        return userdir.toURI().toString();
    }

    /**
     * <p>Load the discovery.</p>
     *
     * @param base The base.
     * @param data The application.
     * @return The discovery.
     * @throws MalformedURLException Shit happens.
     */
    public static ListArray<GestaltEntry> loadDiscoveries(String base, Object data)
            throws MalformedURLException {
        PackageManager pm = ((Application) data).getPackageManager();
        ListArray<GestaltEntry> paths = new ListArray<GestaltEntry>();
        String[] names = pm.getPackagesForUid(android.os.Process.myUid());
        for (int i = 0; i < names.length; i++) {
            String name = names[i];
            PackageInfo pi = null;
            try {
                pi = pm.getPackageInfo(name, PackageManager.GET_ACTIVITIES);
            } catch (PackageManager.NameNotFoundException x) {
                /* */
            }
            if (pi != null && pi.activities != null && pi.activities.length > 0)
                continue;
            Bundle bd = getBundle(pm, name);
            Object dstr = (bd != null ? bd.get("dontask") : null);
            String path = pi.applicationInfo.sourceDir;
            URL url = new URL(ForeignUri.SCHEME_FILE, null, path);
            path = ForeignUri.sysUriRelative(base, url.toString());
            boolean dontask = (dstr != null ? dstr.equals(Boolean.TRUE) : true);
            GestaltEntry pse = new GestaltEntry(path, dontask);
            paths.add(pse);
        }
        return paths;
    }

    /**
     * <p>Retrieve a bundle for an application.</p>
     *
     * @param pm   The package manager.
     * @param name The application name.
     * @return The bundle.
     */
    private static Bundle getBundle(PackageManager pm, String name) {
        ApplicationInfo ai = null;
        try {
            ai = pm.getApplicationInfo(name, PackageManager.GET_META_DATA);
        } catch (PackageManager.NameNotFoundException x) {
            /* */
        }
        return (ai != null ? ai.metaData : null);
    }

}
