package matula.util.config;

import java.util.Locale;
import java.util.Properties;

/**
 * <p>An abstract framework such as a toolkit.</p>
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
public abstract class AbstractFramework {
    private AbstractActivator activator;
    private AbstractRuntime runtime;

    public final static String PROP_PRODUCT_SERVER = "product.server";
    public final static String PROP_PRODUCT_USERDIR = "product.userdir";

    /**
     * <p>Retrieve the activator.
     *
     * @return The activator.
     */
    public AbstractActivator getActivator() {
        return activator;
    }

    /**
     * <p>Set the activator.
     *
     * @param a The activator.
     */
    public void setActivator(AbstractActivator a) {
        activator = a;
    }

    /**
     * <p>Retrieve the runtime.</p>
     *
     * @return The runtime.
     */
    public AbstractRuntime getRuntime() {
        return runtime;
    }

    /**
     * <p>Set the runtime.</p>
     *
     * @param r The runtime.
     */
    public void setRuntime(AbstractRuntime r) {
        runtime = r;
    }

    /**
     * <p>Retrieve the product language properties.</p>
     *
     * @param locale The locale.
     * @return The properties.
     */
    public abstract Properties getProductLang(Locale locale);

    /**
     * <p>Retrieve the error language properties.</p>
     *
     * @param locale The locale.
     * @return The properties.
     */
    public abstract Properties getErrorLang(Locale locale);

}
