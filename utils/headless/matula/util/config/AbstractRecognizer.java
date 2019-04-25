package matula.util.config;

import derek.util.protect.LicenseError;
import matula.util.data.MapEntry;
import matula.util.data.MapTree;
import matula.util.regex.ScannerError;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.Properties;

/**
 * <p>An abstract recognizer such as a knowledgebase.</p>
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
public abstract class AbstractRecognizer {
    private MapTree<String, FileExtension> ext = new MapTree<String,
            FileExtension>(MapTree.DEFAULT);
    private MapEntry<String, FileExtension>[] cacheext;

    /**
     * <p>Determine the decoder for a path.</p>
     *
     * @param path The path.
     * @return The decoder, or null.
     */
    public abstract AbstractBundle pathToDecoder(String path);

    /**
     * <p>Retrieve the parent.</p>
     *
     * @return The parent.
     */
    public abstract AbstractRecognizer getParent();

    /**
     * <p>Load binary properties.</p>
     *
     * @param prop The properties.
     * @param in The input stream.
     * @throws IOException  Problem reading.
     * @throws ScannerError  Problem reading.
     */
    public abstract void loadBinary(Properties prop, InputStream in)
            throws IOException, ScannerError;

    /**
     * <p>Load text properties.</p>
     *
     * @param prop The properties.
     * @param reader The reader.
     * @throws IOException  Problem reading.
     * @throws ScannerError  Problem reading.
     */
    public abstract void loadText(Properties prop, Reader reader)
            throws IOException, ScannerError;

    /*****************************************************************/
    /* File Extensions                                               */
    /*****************************************************************/

    /**
     * <p>Add a file extension.</p>
     *
     * @param e The file extension.
     * @param fe The type and mime..
     */
    public void addFileExtension(String e, FileExtension fe) {
        synchronized (this) {
            if (ext.get(e) != null)
                return;
            ext.add(e, fe);
            cacheext = null;
        }
    }

    /**
     * <p>Remove a file extension.</p>
     *
     * @param e The file extension.
     */
    public void removeFileExtension(String e) {
        synchronized (this) {
            if (ext.get(e) == null)
                return;
            ext.remove(e);
            cacheext = null;
        }
    }

    /**
     * <p>Take a snapshot of the file extensions.</p>
     *
     * @return The file extensions.
     */
    public MapEntry<String, FileExtension>[] snapshotFileExtensions() {
        MapEntry<String, FileExtension>[] res = cacheext;
        if (res != null)
            return res;
        synchronized (this) {
            res = cacheext;
            if (res != null)
                return res;
            res = new MapEntry[ext.size()];
            ext.toArray(res);
            cacheext = res;
        }
        return res;
    }

    /*****************************************************************/
    /* Class Loader                                                  */
    /*****************************************************************/

    /**
     * <p>Retrieve the loader.</p>
     *
     * @return The loader.
     */
    public abstract ClassLoader getLoader();

    /**
     * <p>Take a snapshot of the class paths.</p>
     *
     * @return The class paths.
     * @throws LicenseError Shit happens.
     */
    public abstract String[] snapshotClassPaths() throws LicenseError;

}
