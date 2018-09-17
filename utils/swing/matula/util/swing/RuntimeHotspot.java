package matula.util.swing;

import derek.util.protect.LicenseError;
import matula.util.data.ListArray;
import matula.util.system.AbstractRuntime;
import matula.util.system.ForeignDomain;
import matula.util.system.ForeignUri;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
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

    /*******************************************************************/
    /* New API                                                         */
    /*******************************************************************/

    /**
     * <p>Extend a class loader by a given path.</p>
     *
     * @param parent The parent.
     * @param adr    The URL.
     * @param data   The client data.
     * @return The new class loader.
     * @throws LicenseError License problem.
     */
    public ClassLoader addURL(ClassLoader parent, String adr, Object data)
            throws LicenseError {
        adr = ForeignDomain.sysUriPuny(adr);
        adr = ForeignUri.sysUriEncode(adr);
        URL url;
        try {
            url = new URL(adr);
        } catch (MalformedURLException x) {
            throw new LicenseError(LicenseError.ERROR_LICENSE_MALFORMED_URL, adr);
        }
        return new URLClassLoader(new URL[]{url}, parent);
    }

    /**
     * <p>Retrieve the paths.</p>
     *
     * @param loader The loader.
     * @param stop   The stop.
     * @param data   The client data.
     * @return The paths.
     */
    public ListArray<String> getURLs(ClassLoader loader, ClassLoader stop,
                                     Object data)
            throws LicenseError {
        if (stop == loader)
            return new ListArray<String>();
        ListArray<String> res = getURLs(loader.getParent(), stop, data);
        URL[] urls = getURLs(loader);
        if (urls == null)
            return res;
        for (int i = 0; i < urls.length; i++) {
            String adr = urls[i].toString();
            adr = ForeignUri.sysUriDecode(adr);
            try {
                adr = ForeignDomain.sysUriUnpuny(adr);
            } catch (MalformedURLException x) {
                throw new LicenseError(LicenseError.ERROR_LICENSE_MALFORMED_URL, adr);
            }
            res.add(adr);
        }
        return res;
    }

    /**
     * <p>Retrieve the paths.</p>
     *
     * @param loader The loader.
     * @return The paths.
     */
    private URL[] getURLs(ClassLoader loader) {
        if (loader instanceof URLClassLoader) {
            URLClassLoader urlloader = (URLClassLoader) loader;
            return urlloader.getURLs();
        } else {
            return null;
        }
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

    /**
     * <p>Some testing.</p>
     *
     * @param args Not used.
     */
    /*
    public static void main(String[] args) throws LicenseError {
        ClassLoader cl = RuntimeHotspot.class.getClassLoader();
        System.out.println("cl=" + cl);
        while (cl != null) {
            Class class_ = cl.getClass();
            int ident = 0;
            while (class_ != null) {
                for (int i = 0; i < ident; i++)
                    System.out.print("  ");
                System.out.println(class_.getName());
                class_ = class_.getSuperclass();
                ident++;
            }
            cl = cl.getParent();
        }

        cl = RuntimeHotspot.class.getClassLoader();
        ListArray<String> res = DEFAULT.getURLs(cl, null, null);
        for (int i = 0; i < res.size(); i++)
            System.out.println(res.get(i));
    }
    */

}
