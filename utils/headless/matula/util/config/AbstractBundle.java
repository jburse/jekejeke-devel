package matula.util.config;

import derek.util.protect.LicenseError;
import matula.comp.sharik.AbstractTracking;
import matula.comp.sharik.Check;
import matula.comp.sharik.Enforced;
import matula.util.wire.LangProperties;

import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;
import java.util.Properties;

/**
 * <p>An abstract bundle such as a capability.</p>
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
public abstract class AbstractBundle extends Check {
    public final static int MASK_BNDL_NACT = 0x00000001;

    public final static String PROP_CAPA_FAMILY = "capa.family";
    public final static String PROP_CAPA_PRODUCT = "capa.product";
    public final static String PROP_CAPA_RELEASE = "capa.release";
    public final static String PROP_CAPA_DATE = "capa.date";
    public final static String PROP_CAPA_ICON = "capa.icon";
    public final static String PROP_CAPA_BIGICON = "capa.bigicon";

    public final static String PROP_PRODUCT_SHOP = "product.shop";
    public final static String PROP_PRODUCT_COMPANY = "product.company";
    public final static String PROP_PRODUCT_LANG = "product.lang";
    public final static String PROP_PRODUCT_PACK = "product.pack";
    public final static String PROP_PRODUCT_INST = "product.inst";
    public final static String PROP_PRODUCT_USERDIR = "product.userdir";
    public final static String PROP_PRODUCT_SERVER = "product.server";

    public static final String[] VOID_LIST = new String[0];

    private int flags;
    private String mainroot;

    /**
     * <p>Retrieve the flags.</p>
     *
     * @return The flags.
     */
    public int getFlags() {
        return flags;
    }

    /**
     * <p>Set the flags.</p>
     *
     * @param f The flags.
     */
    public void setFlags(int f) {
        flags = f;
    }

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

    /***************************************************************/
    /* Variation Points                                            */
    /***************************************************************/

    /**
     * <p>Create the info.
     *
     * @return The info.
     */
    public abstract AbstractTracking createTracking();

    /**
     * <p>Prepare a stream.</p>
     *
     * @param in   The input stream.
     * @param know The recognizer.
     * @return The prepared input stream.
     * @throws IOException  IO error.
     * @throws LicenseError License problem.
     */
    public abstract InputStream prepareStream(InputStream in, AbstractRecognizer know)
            throws LicenseError, IOException;

    /**
     * <p>Retrieve the parameters of this branch.</p>
     *
     * @return The parameters of this brach.
     */
    public abstract String[] getParams();

    /**
     * <p>Retrieve the hash code of the bundle.</p>
     *
     * @return The hash code.
     */
    public int hashCode() {
        int res = getClass().hashCode();
        String[] params = getParams();
        for (int i = 0; i < params.length; i++)
            res = res * 31 + params[i].hashCode();
        return res;
    }

    /**
     * <p>Check the identity.</p>
     *
     * @param o The other object.
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof AbstractBundle))
            return false;
        if (!getClass().equals(o.getClass()))
            return false;
        String[] params = getParams();
        String[] oparams = ((AbstractBundle) o).getParams();
        if (params.length != oparams.length)
            return false;
        for (int i = 0; i < params.length; i++) {
            if (!params[i].equals(oparams[i]))
                return false;
        }
        return true;
    }

    /***************************************************************/
    /* Default Services                                            */
    /***************************************************************/

    /**
     * <p>Retrieve the bundle description.</p>
     *
     * @param locale The locale.
     * @return The properties or null.
     */
    public Properties getDescrModel(Locale locale) {
        String name = getMainRoot() + "model/builtin/description";
        ClassLoader loader = getClass().getClassLoader();
        return LangProperties.getLang(loader, name, locale);
    }

    /**
     * <p>Retrieve the bundle description.</p>
     *
     * @param locale The locale.
     * @param e      The enforced.
     * @return The properties or null.
     */
    public Properties getDescrPlatform(Locale locale, Enforced e) {
        String aspect = e.getFramework().getRuntime().getAspect();
        String name = getMainRoot() + "platform/" + aspect + "/description";
        ClassLoader loader = e.getRoot().getLoader();
        return LangProperties.getLang(loader, name, locale);
    }

}
