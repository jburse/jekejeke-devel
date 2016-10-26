package matula.util.system;

import derek.util.protect.LicenseError;

import java.io.IOException;
import java.io.InputStream;

/**
 * <p>An abstract decoder such as a capability.</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public abstract class AbstractDecoder {

    /**
     * <p>Prepare a stream.</p>
     *
     * @param in   The input stream.
     * @param know The recognizer.
     * @return The prepared input stream.
     * @throws IOException IO error.
     * @throws LicenseError Shit happens.
     */
    public abstract InputStream prepareStream(InputStream in, AbstractRecognizer know)
            throws LicenseError, IOException;

}
