package matula.util.android;

import android.app.Application;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.os.SystemClock;
import derek.util.protect.LicenseError;
import matula.comp.sharik.Enforced;
import matula.util.config.AbstractRuntime;
import matula.util.data.ListArray;
import matula.util.system.ForeignUri;

import java.net.MalformedURLException;
import java.net.URL;

/**
 * Android specialization of an abstract runtime.
 * <p/>
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 * <p/>
 * Rights & License
 * All industrial property rights regarding the information - copyright§
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
public final class RuntimeDalvik extends AbstractRuntime {
    public static RuntimeDalvik DEFAULT = new RuntimeDalvik();

    /**
     * <p>Create an activator android.</p>
     */
    private RuntimeDalvik() {
        setAspect(AbstractRuntime.ASPECT_ANDROID);
    }

    /*******************************************************************/
    /* New API                                                         */
    /*******************************************************************/

    /**
     * <p>Extend a class loader by a given path.</p>
     *
     * @param parent The parent.
     * @param adr    The URL.
     * @param stop   The stop loader.
     * @param data   The client data.
     * @return The new class loader.
     * @throws LicenseError License problem.
     */
    public ClassLoader addURL(ClassLoader parent, String adr,
                              ClassLoader stop, Object data)
            throws LicenseError {
        String spec = ForeignUri.sysUriSpec(adr);
        String scheme = ForeignUri.sysSpecScheme(spec);
        String path = ForeignUri.sysSpecPath(spec);
        if (!ForeignUri.SCHEME_FILE.equals(scheme))
            throw new LicenseError(LicenseError.ERROR_LICENSE_FILE_EXPECTED);
        if (!path.endsWith(".apk")) {
            if (parent != stop && parent instanceof ResidualClassLoader) {
                ((ResidualClassLoader) parent).addPath(path);
                return parent;
            } else {
                return new ResidualClassLoader(new String[]{path}, parent);
            }
        } else {
            return new InspectClassLoader(path, parent);
        }
    }

    /**
     * <p>Retrieve the paths.</p>
     *
     * @param loader The loader.
     * @param stop   The stop.
     * @param data   The client data.
     * @return The paths.
     * @throws LicenseError License problem.
     */
    public ListArray<String> getURLs(ClassLoader loader, ClassLoader stop, Object data)
            throws LicenseError {
        if (stop == loader)
            return new ListArray<String>();
        ListArray<String> res = getURLs(loader.getParent(), stop, data);
        URL[] urls = getURLs(loader, data);
        if (urls == null)
            return res;
        for (int i = 0; i < urls.length; i++)
            res.add(urls[i].toString());
        return res;
    }

    /**
     * <p>Retrieve the paths.</p>
     *
     * @param loader The loader.
     * @param data   The application.
     * @return The paths.
     * @throws LicenseError License problem.
     */
    private URL[] getURLs(ClassLoader loader, Object data)
            throws LicenseError {
        if (loader instanceof InterfaceURLs) {
            InterfaceURLs urlloader = (InterfaceURLs) loader;
            return urlloader.getURLs();
        } else if (loader == ((Application) data).getClassLoader()) {
            PackageManager pm = ((Application) data).getPackageManager();
            String path = ((Application) data).getPackageName();
            PackageInfo pi;
            try {
                pi = pm.getPackageInfo(path, 0);
            } catch (PackageManager.NameNotFoundException x) {
                throw new LicenseError(LicenseError.ERROR_LICENSE_FILE_EXPECTED);
            }
            path = pi.applicationInfo.sourceDir;
            URL url;
            try {
                url = new URL(ForeignUri.SCHEME_FILE, null, path);
            } catch (MalformedURLException x) {
                throw new RuntimeException(x);
            }
            return new URL[]{url};
        } else {
            return null;
        }
    }

}
