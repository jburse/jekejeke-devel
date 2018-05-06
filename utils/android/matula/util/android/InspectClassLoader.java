package matula.util.android;

import dalvik.system.PathClassLoader;
import matula.util.system.ForeignUri;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.StringTokenizer;

/**
 * <p>Class loader that can find jar/apk and return URLs.</p>
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
final class InspectClassLoader extends PathClassLoader implements InterfaceURLs {
    private String dexpathback;

    /**
     * <p>Create an inspect class loader.</p>
     *
     * @param dexpath The jar/apk paths separated by ":".
     * @param parent  The parent.
     */
    public InspectClassLoader(String dexpath, ClassLoader parent) {
        super(dexpath, parent);
        dexpathback = dexpath;
    }

    /**
     * <p>Retrieve the URLs.</p>
     *
     * @return The URLs.
     */
    public URL[] getURLs() {
        StringTokenizer st = new StringTokenizer(dexpathback, ":");
        URL[] res = new URL[st.countTokens()];
        int k = 0;
        while (st.hasMoreTokens()) {
            try {
                res[k] = new URL(ForeignUri.SCHEME_FILE, null, st.nextToken());
            } catch (MalformedURLException x) {
                throw new RuntimeException(x);
            }
            k++;
        }
        return res;
    }

}