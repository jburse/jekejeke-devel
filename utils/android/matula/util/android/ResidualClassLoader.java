package matula.util.android;

import matula.util.config.ForeignArchive;
import matula.util.data.ListArray;
import matula.util.system.ForeignUri;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
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
    private final ListArray<String> residuals = new ListArray<String>();
    private String[] cacheres;

    /**
     * <p>Create an extensible class loader.</p>
     *
     * @param r The residual paths.
     * @param p The parent class loader.
     */
    ResidualClassLoader(String[] r, ClassLoader p) {
        super(p);
        for (int i = 0; i < r.length; i++)
            residuals.add(r[i]);
    }

    /**
     * <p>Extend the extensible class loader.</p>
     *
     * @param r The residual path.
     */
    public void addPath(String r) {
        synchronized (this) {
            residuals.add(r);
            cacheres = null;
        }
    }

    /**
     * <p>Retrieve a snapshot of the residual paths.</p>
     *
     * @return The snapshot.
     */
    private String[] snapshotPaths() {
        String[] res = cacheres;
        if (res != null)
            return res;
        synchronized (this) {
            res = cacheres;
            if (res != null)
                return res;
            res = new String[residuals.size()];
            residuals.toArray(res);
            cacheres = res;
        }
        return res;
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
        String[] paths = snapshotPaths();
        for (int i = 0; i < paths.length; i++) {
            String path = paths[i];
            if (path.endsWith("/")) {
                File file = new File(path, name);
                if (file.exists() && file.isFile() && file.canRead()) {
                    try {
                        return new URL(ForeignUri.SCHEME_FILE, null, file.toString());
                    } catch (MalformedURLException x) {
                        throw new RuntimeException(x);
                    }
                }
            } else {
                File f = new File(path);
                boolean found;
                try {
                    InputStream in = new FileInputStream(f);
                    found = ForeignArchive.existsEntry(in, name);
                } catch (IOException x) {
                    throw new RuntimeException(x);
                }
                if (found) {
                    try {
                        url = new URL(ForeignUri.SCHEME_FILE, null, f.toString());
                        return new URL(ForeignUri.SCHEME_JAR, null, url.toString() + "!/" + name);
                    } catch (MalformedURLException x) {
                        throw new RuntimeException(x);
                    }
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
        String[] paths = snapshotPaths();
        for (int i = 0; i < paths.length; i++) {
            String path = paths[i];
            if (path.endsWith("/")) {
                File file = new File(path, name);
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
            } else {
                File f = new File(path);
                boolean found;
                try {
                    InputStream in = new FileInputStream(f);
                    found = ForeignArchive.existsEntry(in, name);
                } catch (IOException x) {
                    throw new RuntimeException(x);
                }
                if (found) {
                    if (res == null) {
                        res = new ArrayList<URL>();
                        while (en.hasMoreElements())
                            res.add(en.nextElement());
                    }
                    try {
                        URL url = new URL(ForeignUri.SCHEME_FILE, null, f.toString());
                        res.add(new URL(ForeignUri.SCHEME_JAR, null, url.toString() + "!/" + name));
                    } catch (MalformedURLException x) {
                        throw new RuntimeException(x);
                    }
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
        String[] paths = snapshotPaths();
        URL[] res = new URL[paths.length];
        for (int i = 0; i < paths.length; i++) {
            try {
                res[i] = new URL(ForeignUri.SCHEME_FILE, null, paths[i]);
            } catch (MalformedURLException x) {
                throw new RuntimeException(x);
            }
        }
        return res;
    }

}

