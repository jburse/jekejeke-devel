package matula.util.swing;

import derek.util.protect.LicenseError;
import matula.util.system.AbstractRuntime;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.jar.Attributes;
import java.util.jar.JarInputStream;
import java.util.jar.Manifest;

/**
 * <p>Swing specialization of an abstract runtime.</p>
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
public final class RuntimeHotspot extends AbstractRuntime {
    public static RuntimeHotspot DEFAULT = new RuntimeHotspot();
    public static final String DIRECTORY_APK = "apk";

    /**
     * <p>Create an activator android.</p>
     */
    private RuntimeHotspot() {
    }

    /**
     * <p>Extend a class loader by a given path.</p>
     *
     * @param loader The old class loader.
     * @param path   The path.
     * @param data   The client data.
     * @return The new class loader.
     * @throws LicenseError License problem.
     */
    public Object addPath(Object loader, String path, Object data)
            throws LicenseError {
        URL url;
        try {
            url = new URL(path);
        } catch (MalformedURLException x) {
            throw new LicenseError(LicenseError.ERROR_LICENSE_MALFORMED_URL);
        }
        if (loader instanceof ExtensibleLoader) {
            ((ExtensibleLoader) loader).addURL(url);
            return loader;
        } else {
            return new ExtensibleLoader(url, (ClassLoader) loader);
        }
    }

    /**
     * <p>Commit the extension of a class loader.</p>
     *
     * @param loader The old class loader.
     * @return The new class loader.
     */
    public Object commitPaths(Object loader) {
        return loader;
    }

    /****************************************************************/
    /* Jar Manifest                                                 */
    /****************************************************************/

    /**
     * <p>Retrieve the manifest attributes for a file.</p>
     *
     * @param file The file.
     * @return The manifest attributes.
     */
    public static Attributes getAttributes(File file) throws IOException {
        FileInputStream in = new FileInputStream(file);
        JarInputStream jr;
        try {
            jr = new JarInputStream(in);
        } catch (IOException x) {
            in.close();
            throw x;
        }
        Manifest mf = jr.getManifest();
        jr.close();
        in.close();
        return (mf != null ? mf.getMainAttributes() : null);
    }

}
