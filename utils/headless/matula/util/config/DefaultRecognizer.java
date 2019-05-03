package matula.util.config;

import matula.util.regex.ScannerError;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.Properties;

/**
 * *
 * <p>The class provides a default recognizer.</p>
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
public final class DefaultRecognizer extends AbstractRecognizer {
    public static final DefaultRecognizer DEFAULT = new DefaultRecognizer();

    /**
     * <p>Create a default recognizer.</p>
     */
    private DefaultRecognizer() {
        addFileExtension(".propertiesx", new FileExtension(FileExtension.MASK_USES_RSCS
                | FileExtension.MASK_DATA_ECRY, "application/properties"));
        addFileExtension(".properties", new FileExtension(FileExtension.MASK_USES_RSCS, "application/properties"));
    }

    /**
     * <p>Determine the decoder for a path.</p>
     *
     * @param path The path.
     * @return The decoder, or null.
     */
    public AbstractBundle pathToDecoder(String path) {
        return DefaultBundle.DEFAULT;
    }

    /**
     * <p>Load binary properties.</p>
     *
     * @param prop   The properties.
     * @param in The reader.
     * @throws IOException  Problem reading.
     */
    public void loadBinary(Properties prop, InputStream in)
            throws IOException {
        prop.load(in);
    }

    /**
     * <p>Load text properties.</p>
     *
     * @param prop   The properties.
     * @param reader The reader.
     */
    public void loadText(Properties prop, Reader reader)
            throws IOException, ScannerError {
        throw new IllegalArgumentException("not supported");
    }

    /*****************************************************************/
    /* Class Loader                                                  */
    /*****************************************************************/

    /**
     * <p>Retrieve the loader.</p>
     *
     * @return The loader.
     */
    public ClassLoader getLoader() {
        return null;
    }

    /**
     * <p>Retrieve the parent.</p>
     *
     * @return The parent.
     */
    public AbstractRecognizer getParent() {
        return null;
    }

}
