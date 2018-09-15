package matula.util.android;

import matula.util.system.ForeignUri;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;

/**
 * <p>Class loader that can find resources and return URLs.</p>
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
final class ResidualClassLoader extends ClassLoader implements InterfaceURLs {
    private final String[] residuals;

    /**
     * <p>Create an extensible class loader.</p>
     *
     * @param r The residual paths.
     * @param p The parent class loader.
     */
    ResidualClassLoader(String[] r, ClassLoader p) {
        super(p);
        residuals = r;
    }

    /**
     * <p>Find resource.</p>
     * <p>The URL is not canonical.</p>
     *
     * @param name The name.
     * @return The URL.
     */
    protected URL findResource(String name) {
        URL url = super.findResource(name);
        if (url != null)
            return url;
        for (int i = 0; i < residuals.length; i++) {
            File file = new File(residuals[i], name);
            if (file.exists() && file.isFile() && file.canRead()) {
                try {
                    return new URL(ForeignUri.SCHEME_FILE, null, file.toString());
                } catch (MalformedURLException x) {
                    throw new RuntimeException(x);
                }
            }
        }
        return null;
    }

    /**
     * <p>Find all resources.</p>
     * <p>The URLs are not canonical.</p>
     *
     * @param name The name.
     * @return The URLs.
     * @throws IOException Shit happens.
     */
    protected Enumeration<URL> findResources(String name) throws IOException {
        Enumeration<URL> en = super.findResources(name);
        ArrayList<URL> res = null;
        for (int i = 0; i < residuals.length; i++) {
            File file = new File(residuals[i], name);
            if (file.exists() && file.isFile() && file.canRead()) {
                if (res == null) {
                    res = new ArrayList<URL>();
                    while (en.hasMoreElements())
                        res.add(en.nextElement());
                }
                try {
                    res.add(new URL(ForeignUri.SCHEME_FILE, null, file.toString()));
                } catch (MalformedURLException x) {
                    throw new RuntimeException(x);
                }
            }
        }
        if (res != null)
            en = Collections.enumeration(res);
        return en;
    }

    /**
     * <p>Retrieve the URLs.</p>
     *
     * @return The URLs.
     */
    public URL[] getURLs() {
        URL[] res = new URL[residuals.length];
        for (int i = 0; i < residuals.length; i++) {
            try {
                res[i] = new URL(ForeignUri.SCHEME_FILE, null, residuals[i]);
            } catch (MalformedURLException x) {
                throw new RuntimeException(x);
            }
        }
        return res;
    }

}

