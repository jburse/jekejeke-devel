package matula.util.wire;

import matula.comp.text.DefaultRecognizer;
import matula.util.system.ForeignCache;

import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Locale;
import java.util.Properties;

/**
 * <p>This class provides internationalized properties lookup.</p>
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
public final class LangProperties {
    private static final HashMap<String, Properties> cache =
            new HashMap<String, Properties>();

    /*****************************************************************/
    /* Legacy API                                                    */
    /*****************************************************************/

    /**
     * <p>Cache and load encrypted language properties.</p>
     *
     * @param clazz  The class.
     * @param name   The name.
     * @param locale The locale.
     * @return The language properties, or null.
     */
    public static Properties getLang(Class<?> clazz, String name, Locale locale) {
        try {
            if (clazz == null)
                throw new NullPointerException("clazz missing");
            if (name == null)
                throw new NullPointerException("name missing");
            if (locale == null)
                throw new NullPointerException("locale missing");

            URL url = clazz.getResource(name + ".propertiesx");
            if (url == null)
                throw new NullPointerException("root missing");
            String adr = url.toString();
            String locstr = "_" + locale;
            int k = adr.lastIndexOf(".");
            String key = adr.substring(0, k) + locstr + adr.substring(k);
            Properties prop = ForeignCache.getCached(cache, key);
            return ForeignCache.getLang(prop, DefaultRecognizer.DEFAULT, adr, locstr);
        } catch (IOException x) {
            throw new RuntimeException("io exception", x);
        }
    }

}
