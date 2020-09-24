package matula.util.config;

import matula.util.wire.LangProperties;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.Properties;

/**
 * <p>An abstract description such as a capability.</p>
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
public abstract class AbstractDescription {
    public final static String MODEL_DEFAULT = "model/builtin/description";

    public final static String PLATFORM_DIR = "platform/";
    public final static String PLATFORM_FILE = "/description";

    /* model specific */
    public final static String PROP_CAPA_FAMILY = "capa.family";
    public final static String PROP_CAPA_PRODUCT = "capa.product";
    public final static String PROP_CAPA_RELEASE = "capa.release";
    /* platform specific */
    public final static String PROP_CAPA_DATE = "capa.date";

    private String mainroot;

    /**
     * <p>Retrieve the main root.</p>
     *
     * @return The main root.
     */
    public String getMainRoot() {
        return mainroot;
    }

    /**
     * <p>Set the main root.</p>
     *
     * @param m The main root.
     */
    public void setMainRoot(String m) {
        mainroot = m;
    }

    /**
     * <p>Retrieve the bundle description.</p>
     *
     * @param locale The locale.
     * @param loader The class loader.
     * @return The properties or null.
     */
    public Properties getDescrModel(Locale locale,
                                    ClassLoader loader) {
        String name = getMainRoot() + MODEL_DEFAULT;
        return LangProperties.getLang(loader, name, locale);
    }

    /**
     * <p>Retrieve the bundle description.</p>
     *
     * @param locale    The locale.
     * @param loader    The class loader.
     * @param framework The framework.
     * @return The properties or null.
     */
    public Properties getDescrPlatform(Locale locale,
                                       ClassLoader loader,
                                       AbstractFramework framework) {
        String aspect = framework.getRuntime().getAspect();
        String name = getMainRoot() + PLATFORM_DIR + aspect + PLATFORM_FILE;
        return LangProperties.getLang(loader, name, locale);
    }

    /*********************************************************/
    /* Frequent Properties                                   */
    /*********************************************************/

    /**
     * <p>Retreve the family as an internationalized text.</p>
     *
     * @param locale The locale.
     * @param loader The class loader.
     * @return The family as an internationalized text or null.
     */
    public String getFamily(Locale locale, ClassLoader loader) {
        Properties descr = getDescrModel(locale, loader);
        if (descr != null) {
            return descr.getProperty(PROP_CAPA_FAMILY);
        } else {
            return null;
        }
    }

    /**
     * <p>Retrieve the product and release text.</p>
     *
     * @param locale    The locale.
     * @param loader    The class loader.
     * @param framework The framework.
     * @return The product and release.
     */
    public String getProductReleaseDate(Locale locale, ClassLoader loader,
                                        AbstractFramework framework) {
        Properties descr = getDescrModel(locale, loader);
        if (descr != null) {
            String product = descr.getProperty(PROP_CAPA_PRODUCT);
            String release = descr.getProperty(PROP_CAPA_RELEASE);
            return product + " " + release + sysDate(locale, loader, framework);
        } else {
            Properties resources = LangProperties.getLang(AbstractDescription.class, "intl", locale);
            return resources.getProperty("capa.missing");
        }
    }

    /**
     * <p>Retrieve the date formatted.</p>
     *
     * @param locale    The locale.
     * @param loader    The class loader.
     * @param framework The framework.
     * @return The date formatted or "".
     */
    private String sysDate(Locale locale, ClassLoader loader,
                           AbstractFramework framework) {
        Properties descr = getDescrPlatform(locale, loader, framework);
        String datestr = (descr != null ? descr.getProperty(PROP_CAPA_DATE) : null);
        if (datestr != null) {
            try {
                DateFormat df = new SimpleDateFormat(LangProperties.PATTERN_DATE, Locale.UK);
                Date date = df.parse(datestr);
                df = DateFormat.getDateInstance(DateFormat.LONG, locale);
                datestr = " (" + df.format(date) + ")";
            } catch (ParseException x) {
                datestr = "";
            }
        } else {
            datestr = "";
        }
        return datestr;
    }

}