package matula.util.config;

import derek.util.protect.LicenseError;
import matula.comp.sharik.AbstractTracking;
import matula.comp.sharik.Enforced;
import matula.util.wire.AbstractDomestic;

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
    public void putTracking(AbstractBundle b, AbstractTracking t,
                            Enforced e) {
        e.putTracking(b, t);
    }

    /**
     * <p>Remove a tracking.</p>
     *
     * @param b The bundle.
     * @param e The enforced.
     */
    public void removeTracking(AbstractBundle b, Enforced e) {
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
    public void trackingFailed(AbstractBundle b, AbstractTracking t, Object o)
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

}
