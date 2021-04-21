package matula.util.config;

import matula.util.config.BaseBundle;
import matula.util.config.AbstractFramework;
import matula.util.config.AbstractInteractor;
import matula.util.config.BaseTracking;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;
import matula.util.misc.LicenseError;
import matula.util.wire.AbstractDomestic;
import matula.util.wire.AbstractRecognizer;

/**
 * <p>This class represents an enforced list of trackings.</p>
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
public class Enforced {
    public final MapHashLink<BaseBundle, BaseTracking> trackings =
            new MapHashLink<>();
    private MapEntry<BaseBundle, BaseTracking>[] cachetrackings;
    public int numneedsact;
    public AbstractDomestic watchdog;
    private AbstractFramework framework;
    private String error = LicenseError.ERROR_LICENSE_OK;
    private AbstractInteractor interactor;
    private Object application;
    private AbstractRecognizer root;
    private int hint;

    public static final int HINT_MASK_MNGR = 0x00000010;
    public static final int HINT_MASK_LMTD = 0x00000100;

    /**
     * <p>Retrieve the framework.</p>
     *
     * @return The framework.
     */
    public AbstractFramework getFramework() {
        return framework;
    }

    /**
     * <p>Set the framework.</p>
     *
     * @param f The framework.
     */
    public void setFramework(AbstractFramework f) {
        framework = f;
    }

    /**
     * <p>Retrieve the error.</p>
     *
     * @return The error.
     */
    public String getError() {
        return error;
    }

    /**
     * <p>Set the error.</p>
     *
     * @param e The error.
     */
    public void setError(String e) {
        error = e;
    }

    /**
     * <p>Retrieve the interactor.
     *
     * @return The interactor.
     */
    public AbstractInteractor getInteractor() {
        return interactor;
    }

    /**
     * <p>Set the interactor.
     *
     * @param a The interactor.
     */
    public void setInteractor(AbstractInteractor a) {
        interactor = a;
    }

    /**
     * <p>Retrieve the application.</p>
     *
     * @return The application.
     */
    public Object getApplication() {
        return application;
    }

    /**
     * <p>Set the application.</p>
     *
     * @param c The application.
     */
    public void setApplication(Object c) {
        application = c;
    }

    /**
     * <p>Retrieve the root.</p>
     *
     * @return The root.
     */
    public AbstractRecognizer getRoot() {
        return root;
    }

    /**
     * <p>Set the root.</p>
     *
     * @param r The root.
     */
    public void setRoot(AbstractRecognizer r) {
        root = r;
    }

    /***********************************************************/
    /* Advanced Configuration                                  */
    /***********************************************************/

    /**
     * <p>Retrieve the hint.</p>
     *
     * @return The hint.
     */
    public int getHint() {
        return hint;
    }

    /**
     * <p>Set the hint.</p>
     *
     * @param h The hint.
     */
    public final void setHint(int h) {
        hint = h;
    }

    /************************************************************/
    /* Bundle Management                                        */
    /************************************************************/

    /**
     * <p>Put a bundle.</p>
     * <p>Throw LicenseError or application specific exceptions.</p>
     *
     * @param b The abstract bundle.
     * @param p The prompt flag.
     * @param o The client data.
     * @return The new tracking.
     * @throws Exception Shit happens.
     */
    public BaseTracking putBundle(BaseBundle b,
                                  boolean p, Object o)
            throws Exception {
        BaseTracking t = b.createTracking();
        if (p) {
            for (; ; ) {
                try {
                    framework.getActivator().checkTracking(this, b, t);
                    break;
                } catch (LicenseError x) {
                    getInteractor().trackingFailed(b, t, o);
                }
            }
        } else {
            framework.getActivator().checkTracking(this, b, t);
        }
        getInteractor().putTracking(b, t, this);
        return t;
    }

    /**
     * <p>Remove a bundle.</p>
     *
     * @param b The abstract bundle.
     */
    public void removeBundle(BaseBundle b) {
        getInteractor().removeTracking(b, this);
    }

    /************************************************************/
    /* Tracking Management                                      */
    /************************************************************/

    /**
     * <p>Put the tracking identified by the given abstract bundle.</p>
     * <p>Does nothing if there is already a tracking.</p>
     *
     * @param bundle   The abstract bundle.
     * @param newtrack The tracking.
     * @return The old tracking, or null.
     */
    public BaseTracking putTracking(BaseBundle bundle,
                                    BaseTracking newtrack) {
        synchronized (trackings) {
            BaseTracking oldtrack = trackings.get(bundle);
            if (oldtrack != null)
                return oldtrack;
            trackings.add(bundle, newtrack);
            cachetrackings = null;
            return null;
        }
    }

    /**
     * <p>Remove the tracking identified by the given bundle.</p>
     * <p>Does nothing if there is no tracking.</p>
     *
     * @param bundle The abstract bundle.
     * @return The old tracking, or null.
     */
    public BaseTracking removeTracking(BaseBundle bundle) {
        synchronized (trackings) {
            BaseTracking oldtrack = trackings.get(bundle);
            if (oldtrack == null)
                return null;
            trackings.remove(bundle);
            cachetrackings = null;
            return oldtrack;
        }
    }

    /**
     * <p>Retrieve the tracking for the given bundle.</p>
     *
     * @param bundle The bundle.
     * @return The tracking.
     */
    public BaseTracking getTracking(BaseBundle bundle) {
        synchronized (trackings) {
            return trackings.get(bundle);
        }
    }

    /**
     * <p>Retrieve a snapshot of the trackings.</p>
     *
     * @return The trackings.
     */
    public MapEntry<BaseBundle, BaseTracking>[] snapshotTrackings() {
        MapEntry<BaseBundle, BaseTracking>[] res = cachetrackings;
        if (res != null)
            return res;
        synchronized (trackings) {
            res = cachetrackings;
            if (res != null)
                return res;
            res = new MapEntry[trackings.size];
            trackings.toArray(res);
            cachetrackings = res;
        }
        return res;
    }

}
