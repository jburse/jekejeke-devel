package matula.util.config;

import matula.util.data.MapEntry;
import matula.util.misc.LicenseError;

import java.io.File;
import java.io.IOException;

/**
 * <p>The class represents an abstract activator.</p>
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
public abstract class AbstractActivator {

    /**
     * <p>Activate a bundle.</p>
     *
     * @param enforced The enforced.
     * @param bundle   The bundle.
     * @param hash     The license key.
     * @throws LicenseError License problem.
     * @throws IOException  IO problem.
     */
    public abstract void activateBundle(Enforced enforced,
                                        BaseBundle bundle,
                                        String hash)
            throws LicenseError, IOException;

    /**
     * <p>Calculate the install ID.</p>
     *
     * @param enforced The enforced.
     * @param bundle   The bundle.
     * @return The install ID.
     * @throws LicenseError License problem.
     * @throws IOException  IO problem.
     */
    public abstract String calcInstallID(Enforced enforced,
                                         BaseBundle bundle)
            throws LicenseError, IOException;

    /**
     * <p>Register the license text.</p>
     *
     * @param enforced The enforced.
     * @param bundle   The bundle.
     * @param text     The license text.
     * @throws LicenseError License problem.
     */
    public abstract void regLicenseText(Enforced enforced,
                                        BaseBundle bundle,
                                        String text)
            throws LicenseError;

    /**
     * <p>The registered license text.</p>
     *
     * @param enforced The enforced.
     * @param bundle   The bundle.
     * @return The license text.
     * @throws LicenseError License problem.
     */
    public abstract String regedLicenseText(Enforced enforced,
                                            BaseBundle bundle)
            throws LicenseError;

    /**
     * <p>Check a tracking.</p>
     * <p>Check result is found in exception and in error of tracking.</p>
     *
     * @param enforced The enforced.
     * @param bundle   The bundle.
     * @param tracking The tracking.
     * @throws LicenseError License problem.
     * @throws IOException  License problem.
     */
    public abstract void checkTracking(Enforced enforced,
                                       BaseBundle bundle,
                                       BaseTracking tracking)
            throws LicenseError, IOException;

    /**
     * <p>Check an enforced.</p>
     * <p>Will not re-check each tracking.</p>
     * <p>Check result is found in exception and in error of enforced.</p>
     *
     * @param enforced The enforced.
     * @param tracks   The tracks.
     * @throws LicenseError License problem.
     */
    public abstract void checkEnforced(Enforced enforced,
                                       MapEntry<BaseBundle,
                                               BaseTracking>[] tracks)
            throws LicenseError;

    /**
     * <p>Retrieve the user directory.</p>
     *
     * @param obj The application context.
     * @return The user directory.
     */
    public abstract File getUserDir(Object obj);

    /**
     * <p>Retrieve the bundle path.</p>
     *
     * @param loader      The class loader.
     * @param framework   The framework.
     * @param description The description.
     * @return The bundle path.
     */
    public abstract String getBundlePath(ClassLoader loader,
                                         AbstractFramework framework,
                                         AbstractDescription description);


    /**
     * <p>Retrieve the install architecture.</p>
     *
     * @param enforced    The enforced.
     * @param description The description.
     * @return The install architecture.
     */
    public abstract String getInstall(Enforced enforced,
                                      AbstractDescription description);

}
