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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class LangProperties {
    public static final String PATTERN_DATE = "yyyy-MM-dd";
    public static final String PATTERN_DATETIME = "yyyy-MM-dd HH:mm:ss";

    private static final HashMap<String, Properties> cache =
            new HashMap<String, Properties>();

    private final static String EXT_PROPX = ".propertiesx";

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
    public static Properties getLang(Class<?> clazz, String name,
                                     Locale locale) {
        if (clazz == null)
            throw new NullPointerException("clazz missing");
        if (name == null)
            throw new NullPointerException("name missing");
        if (locale == null)
            throw new NullPointerException("locale missing");

        URL url = clazz.getResource(name + EXT_PROPX);
        if (url == null)
            throw new NullPointerException("root missing");

        String adr = url.toString();
        String locstr = "_" + locale;
        int k = adr.length() - EXT_PROPX.length();
        String key = adr.substring(0, k) + locstr + adr.substring(k);
        Properties prop = ForeignCache.getCached(cache, key);
        try {
            return ForeignCache.getLang(prop, DefaultRecognizer.DEFAULT, adr, locstr);
        } catch (IOException x) {
            throw new RuntimeException("io exception", x);
        }
    }

    /**
     * <p>Cache and load encrypted language properties.</p>
     *
     * @param loader The class loader.
     * @param name   The name.
     * @param locale The locale.
     * @return The language properties, or null.
     */
    public static Properties getLang(ClassLoader loader, String name, Locale locale) {
        if (loader == null)
            throw new NullPointerException("loader missing");
        if (name == null)
            throw new NullPointerException("name missing");
        if (locale == null)
            throw new NullPointerException("locale missing");

        URL url = loader.getResource(name + EXT_PROPX);
        if (url == null)
            throw new NullPointerException("root missing");

        String adr = url.toString();
        String locstr = "_" + locale;
        int k = adr.length() - EXT_PROPX.length();
        String key = adr.substring(0, k) + locstr + adr.substring(k);
        Properties prop = ForeignCache.getCached(cache, key);
        try {
            return ForeignCache.getLang(prop, DefaultRecognizer.DEFAULT, adr, locstr);
        } catch (IOException x) {
            throw new RuntimeException("io exception", x);
        }
    }

    /**
     * <p>Convert a string to a locale.</p>
     *
     * @param locstr The string.
     * @return The locale.
     */
    public static Locale stringToLocale(String locstr) {
        if (!"en_GB".equals(locstr)) {
            int k1 = locstr.indexOf('_');
            if (k1 == -1) {
                return new Locale(locstr);
            } else {
                int k2 = locstr.indexOf('_', k1 + 1);
                if (k2 == -1) {
                    return new Locale(locstr.substring(0, k1),
                            locstr.substring(k1 + 1));
                } else {
                    return new Locale(locstr.substring(0, k1),
                            locstr.substring(k1 + 1, k2), locstr.substring(k2 + 1));
                }
            }
        } else {
            return Locale.UK;
        }
    }

}
