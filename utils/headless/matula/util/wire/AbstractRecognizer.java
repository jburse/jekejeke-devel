package matula.util.wire;

import derek.util.protect.ActivatorNet;
import matula.comp.sharik.SystemGestalt;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;
import matula.util.misc.SymmetricDecoder;
import matula.util.regex.ScannerError;
import matula.util.system.OpenDuplex;
import matula.util.system.OpenOpts;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.net.BindException;
import java.security.GeneralSecurityException;
import java.text.ParseException;
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
    public static final String ERROR_BIND_CRYPT_EXCEPTION = "crypt_exception";
    public static final String ERROR_BIND_INVALID_FORMAT = "invalid_format";

    private final MapHashLink<String, FileExtension> ext = new MapHashLink<>();
    private MapEntry<String, FileExtension>[] cacheext;

    /**
     * <p>Prepare a stream depending on path.</p>
     *
     * @param path The path.
     * @param in The stream.
     * @return The prepared stream.
     * @throws IOException I/O Error.
     */
    public final InputStream prepareStream(String path, InputStream in)
            throws IOException {
        SymmetricDecoder secrets = new SymmetricDecoder();
        secrets.setKey(SystemGestalt.KEY);
        try {
            in = secrets.decryptStream(in);
        } catch (GeneralSecurityException x) {
            in.close();
            throw new BindException(ERROR_BIND_CRYPT_EXCEPTION);
        } catch (ParseException x) {
            in.close();
            throw new BindException(ERROR_BIND_INVALID_FORMAT);
        }
        return in;
    }

    /**
     * <p>Load binary properties.</p>
     *
     * @param prop  The properties.
     * @param in    The input stream.
     * @param param The param or null.
     * @throws IOException  Problem reading.
     * @throws ScannerError Problem reading.
     */
    public abstract void loadBinary(Properties prop, InputStream in,
                                    Object param)
            throws IOException, ScannerError;

    /**
     * <p>Load text properties.</p>
     *
     * @param prop   The properties.
     * @param reader The reader.
     * @param param  The param or null.
     * @throws IOException  Problem reading.
     * @throws ScannerError Problem reading.
     */
    public abstract void loadText(Properties prop, Reader reader,
                                  Object param)
            throws IOException, ScannerError;

    /**
     * <p>Load binary properties.</p>
     *
     * @param adr   The URI.
     * @param prop  The properties.
     * @param param The param or null.
     * @throws IOException  Problem reading.
     * @throws ScannerError Problem reading.
     */
    public void loadBinary(String adr, Properties prop,
                           Object param)
            throws IOException, ScannerError {
        OpenOpts opts = new OpenOpts();
        opts.setFlags(opts.getFlags() | OpenDuplex.MASK_OPEN_BINR);
        opts.setRecognizer(this);
        InputStream in = (InputStream) opts.openRead(adr);
        try {
            loadBinary(prop, in, param);
        } catch (IOException x) {
            in.close();
            throw x;
        } catch (ScannerError x) {
            in.close();
            throw x;
        }
        in.close();
    }

    /**
     * <p>Load binary properties.</p>
     *
     * @param adr   The URI.
     * @param prop  The properties.
     * @param param The param or null.
     * @throws IOException  Problem reading.
     * @throws ScannerError Problem reading.
     */
    public void loadText(String adr, Properties prop,
                         Object param)
            throws IOException, ScannerError {
        OpenOpts opts = new OpenOpts();
        opts.setRecognizer(this);
        Reader reader = (Reader) opts.openRead(adr);
        try {
            loadText(prop, reader, param);
        } catch (IOException x) {
            reader.close();
            throw x;
        } catch (ScannerError x) {
            reader.close();
            throw x;
        }
        reader.close();
    }

    /*****************************************************************/
    /* File Extensions                                               */
    /*****************************************************************/

    /**
     * <p>Add a file extension.</p>
     *
     * @param e  The file extension.
     * @param fe The type and mime..
     */
    public void addFileExtension(String e, FileExtension fe) {
        synchronized (this) {
            if (ext.getEntry(e) != null)
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
            MapEntry<String, FileExtension> entry = ext.getEntry(e);
            if (entry == null)
                return;
            ext.removeEntry(entry);
            ext.resize();
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
     * <p>Retrieve the parent.</p>
     *
     * @return The parent.
     */
    public abstract AbstractRecognizer getParent();

}
