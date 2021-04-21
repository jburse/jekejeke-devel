package matula.util.config;

import matula.comp.sharik.Enforced;
import matula.util.misc.LicenseError;
import matula.util.wire.AbstractDomestic;
import matula.util.wire.LangProperties;

import java.util.Locale;
import java.util.Properties;

/**
 * *
 * <p>The class provides a default interactor.</p>
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
public final class DefaultInteractor extends AbstractInteractor {
    public static final String ERROR_BIND_INTERNAL_ERROR = "internal_error";
    public static final String ERROR_BIND_SERVICE_UNAVAILABLE = "service_unavailable";
    public static final String ERROR_BIND_CRYPT_EXCEPTION = "crypt_exception";
    public static final String ERROR_BIND_INVALID_FORMAT = "invalid_format";
    public static final String ERROR_BIND_FILE_EXPECTED = "file_expected";
    public static final String ERROR_BIND_PACKAGE_MISSING = "package_missing";

    public static final DefaultInteractor DEFAULT = new DefaultInteractor();

    /**
     * <p>Create a default interactor.</p>
     */
    private DefaultInteractor() {
        /* */
    }

    /**
     * <p>Put a tracking.</p>
     *
     * @param b The bundle.
     * @param t The tracking.
     * @param e The enforced.
     */
    public void putTracking(BaseBundle b, BaseTracking t,
                            Enforced e) {
        e.putTracking(b, t);
    }

    /**
     * <p>Remove a tracking.</p>
     *
     * @param b The bundle.
     * @param e The enforced.
     */
    public void removeTracking(BaseBundle b, Enforced e) {
        e.removeTracking(b);
    }

    /**
     * <p>Called when validation of the tracking failed and prompt mode.</p>
     * <p>Throw LicenseError or application specific exceptions.</p>
     *
     * @param b The abstract bundle.
     * @param t The tracking.
     * @param o The client data.
     * @throws Exception Shit happens.
     */
    public void trackingFailed(BaseBundle b, BaseTracking t, Object o)
            throws Exception {
        throw new LicenseError(t.getError());
    }

    /**
     * <p>Called when validation of the enforced failed.</p>
     *
     * @param e The enforced.
     */
    public void enforcedFailed(Enforced e, AbstractDomestic w) {
        Properties error = e.getFramework().getErrorLang(Locale.getDefault());
        System.err.println(error.getProperty(LicenseError.ERROR_LICENSE_ERROR + "." + e.getError()));
        System.exit(1);
    }

    /**
     * <p>Retrieve the text bundle.</p>
     *
     * @param locale The locale.
     * @return The text bundle.
     */
    public static Properties getLang(Locale locale) {
        return LangProperties.getLang(DefaultInteractor.class, "intl", locale);
    }

}
