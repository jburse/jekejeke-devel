package matula.util.android;

import android.app.Application;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.os.Bundle;
import derek.util.protect.LicenseError;
import matula.util.data.ListArray;
import matula.util.system.AbstractRuntime;
import matula.util.system.ForeignUri;

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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class RuntimeDalvik extends AbstractRuntime {
    public static RuntimeDalvik DEFAULT = new RuntimeDalvik();

    public static final String SCHEME_APK = "apk";

    /**
     * <p>Create an activator android.</p>
     */
    private RuntimeDalvik() {
    }

    /**
     * <p>Extend a class loader by a given path.</p>
     * <p>Only understands the file: and apk: protocol.</p>
     *
     * @param loader The old class loader.
     * @param adr    The path.
     * @param data   The client data.
     * @return The new class loader.
     * @throws LicenseError License problem.
     */
    public Object addPath(Object loader, String adr, Object data)
            throws LicenseError {
        String spec = ForeignUri.sysUriSpec(adr);
        String scheme = ForeignUri.sysSpecScheme(spec);
        String path = ForeignUri.sysSpecPath(spec);
        if (ForeignUri.SCHEME_FILE.equals(scheme)) {
            /* */
        } else if (SCHEME_APK.equals(scheme)) {
            PackageManager pm = ((Application) data).getPackageManager();
            PackageInfo pi;
            try {
                pi = pm.getPackageInfo(path, 0);
            } catch (PackageManager.NameNotFoundException x) {
                throw new LicenseError(LicenseError.ERROR_LICENSE_FILE_EXPECTED);
            }
            path = pi.applicationInfo.sourceDir;
        } else {
            throw new LicenseError(LicenseError.ERROR_LICENSE_FILE_EXPECTED);
        }
        if (loader instanceof DeferredLoader) {
            ((DeferredLoader) loader).addPath(path);
            return loader;
        } else {
            return new DeferredLoader(path, (ClassLoader) loader);
        }
    }

    /**
     * <p>Commit the extension of a class loader.</p>
     *
     * @param loader The old class loader.
     * @return The new class loader.
     */
    public Object commitPaths(Object loader) {
        if (loader instanceof DeferredLoader) {
            DeferredLoader defer = (DeferredLoader) loader;
            ListArray<String> paths = defer.getPaths();

            ListArray<String> reslist = new ListArray<String>();
            StringBuilder buf = new StringBuilder();
            for (int i = 0; i < paths.size(); i++) {
                String path = paths.get(i);
                if (path.endsWith("/")) {
                    reslist.add(path);
                } else {
                    if (i != 0)
                        buf.append(":");
                    buf.append(path);
                }
            }

            String[] res = new String[reslist.size()];
            reslist.toArray(res);
            return new DalvikExtensible(buf.toString(), defer.getParent(), res);
        } else {
            return loader;
        }
    }

    /****************************************************************/
    /* Application Bundle                                           */
    /****************************************************************/

    /**
     * <p>Retrieve a bundle for an application.</p>
     *
     * @param pm   The package manager.
     * @param name The application name.
     * @return The bundle.
     */
    public static Bundle getBundle(PackageManager pm, String name) {
        ApplicationInfo ai = null;
        try {
            ai = pm.getApplicationInfo(name, PackageManager.GET_META_DATA);
        } catch (PackageManager.NameNotFoundException x) {
            /* */
        }
        return (ai != null ? ai.metaData : null);
    }

}
