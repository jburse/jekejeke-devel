package matula.util.config;

import derek.util.protect.ActivatorNet;
import derek.util.protect.LicenseError;
import matula.comp.sharik.AbstractTracking;
import matula.comp.sharik.Enforced;

import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;
import java.util.Properties;

/**
 * *
 * <p>The class provides a default bundle.</p>
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
public final class DefaultBundle extends AbstractBundle {
    public static final DefaultBundle DEFAULT = new DefaultBundle();

    /**
     * <p>Create the info.
     *
     * @return The info.
     */
    public AbstractTracking createTracking() {
        return null;
    }

    /**
     * <p>Prepare a stream.</p>
     *
     * @param in   The input stream.
     * @param know The recognizer.
     * @return The prepared input stream.
     * @throws IOException  IO error.
     * @throws LicenseError License problem.
     */
    public InputStream prepareStream(InputStream in, AbstractRecognizer know)
            throws LicenseError, IOException {
        return ActivatorNet.prepareStream(in);
    }

    /**
     * <p>Retrieve the bundle description.</p>
     *
     * @param locale The locale.
     * @return The properties or null.
     */
    public Properties getDescrModel(Locale locale, Enforced e) {
        return null;
    }

    /**
     * <p>Retrieve the bundle description.</p>
     *
     * @param locale The locale.
     * @param e      The enforced.
     * @return The properties or null.
     */
    public Properties getDescrPlatform(Locale locale, Enforced e) {
        return null;
    }

    /**
     * <p>Retrieve the parameters of this branch.</p>
     *
     * @return The parameters of this brach.
     */
    public String[] getParams() {
        return VOID_LIST;
    }

}
