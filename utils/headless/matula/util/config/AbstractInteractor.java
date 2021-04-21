package matula.util.config;

import matula.util.wire.AbstractDomestic;

/**
 * <p>An abstract interactor such as a GUI dialog.</p>
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
public abstract class AbstractInteractor {

    /**
     * <p>Put the tracking identified by the given bundle.</p>
     * <p>Does nothing if there is already a tracking.</p>
     *
     * @param b The bundle.
     * @param t The new tracking.
     * @param e The enforced.
     * @return The old tracking.
     */
    public abstract BaseTracking putTracking(BaseBundle b,
                                             BaseTracking t, Enforced e);

    /**
     * <p>Remove the tracking identified by the given bundle.</p>
     * <p>Does nothing if there is no tracking.</p>
     *
     * @param b The bundle.
     * @param e The enforced.
     * @return The old tracking.
     */
    public abstract BaseTracking removeTracking(BaseBundle b, Enforced e);

    /**
     * <p>Called when validation of the tracking failed.</p>
     * <p>Throw LicenseError or application specific exceptions.</p>
     *
     * @param b The abstract bundle.
     * @param t The tracking.
     * @param o The client data.
     * @throws Exception Shit happens.
     */
    public abstract void trackingFailed(BaseBundle b,
                                        BaseTracking t, Object o)
            throws Exception;

    /**
     * <p>Called when validation of the enforced failed.</p>
     *
     * @param e The enforced.
     * @param w The watchdog.
     */
    public abstract void enforcedFailed(Enforced e, AbstractDomestic w);

}
