package matula.util.system;

import derek.util.protect.LicenseError;
import matula.util.config.AbstractRecognizer;
import matula.util.config.FileExtension;
import matula.util.data.ListArray;
import matula.util.regex.ScannerError;

import java.io.*;
import java.util.HashMap;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * <p>This class provides a properties cache.</p>
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
public final class ForeignCache {
    private static final String ATTR_STATE = "_state";
    private static final String STATE_LOADED = "loaded";
    private static final String STATE_FAILED = "failed";

    /************************************************************/
    /* Properties Caching                                       */
    /************************************************************/

    /**
     * <p>Retrieve a cached property.</p>
     *
     * @param cache The cache.
     * @param key   The key.
     * @return The cached property.
     */
    public static Properties getCached(HashMap<String, Properties> cache,
                                       String key) {
        Properties prop;
        synchronized (cache) {
            prop = cache.get(key);
            if (prop == null) {
                prop = new Properties();
                cache.put(key, prop);
            }
        }
        return prop;
    }

    /**
     * <p>Cache and load language properties.</p>
     *
     * @param prop   The language properties.
     * @param know   The recognizer.
     * @param adr    The URI.
     * @param locstr The locale prefixed by underscore.
     * @throws InterruptedIOException IO Interrupted.
     */
    public static void getLang(Properties prop,
                               AbstractRecognizer know,
                               String adr, String locstr)
            throws IOException {
        getLangCheck(prop, know, adr, locstr, FileExtension.MASK_USES_RSCS);
    }

    /**
     * <p>Cache and load language properties.</p>
     *
     * @param prop   The language properties.
     * @param know   The recognizer.
     * @param adr    The URI.
     * @param locstr The locale prefixed by underscore.
     * @param mask   The mask.
     * @throws InterruptedIOException IO Interrupted.
     */
    public static void getLangCheck(Properties prop,
                                    AbstractRecognizer know,
                                    String adr, String locstr,
                                    int mask)
            throws IOException {
        String state = prop.getProperty(ATTR_STATE);
        if (state != null)
            return;
        adr = ForeignCache.findLocale(adr, locstr);
        synchronized (prop) {
            state = prop.getProperty(ATTR_STATE);
            if (state != null)
                return;
            if (adr != null) {
                try {
                    if ((mask & FileExtension.MASK_USES_RSCS) != 0) {
                        ForeignCache.loadBinary(know, adr, prop);
                    } else {
                        ForeignCache.loadText(know, adr, prop);
                    }
                    state = STATE_LOADED;
                    prop.put(ATTR_STATE, state);
                } catch (LicenseError x) {
                    state = STATE_FAILED;
                    prop.put(ATTR_STATE, state);
                } catch (IOException x) {
                    if (OpenCheck.isInterrupt(x)) {
                        throw x;
                    } else {
                        state = STATE_FAILED;
                        prop.put(ATTR_STATE, state);
                    }
                } catch (ScannerError x) {
                    state = STATE_FAILED;
                    prop.put(ATTR_STATE, state);
                }
            } else {
                state = STATE_FAILED;
                prop.put(ATTR_STATE, state);
            }
        }
    }

    /**
     * <p>Check whether the properties are valid.</p>
     *
     * @param prop The properties.
     * @return True if the properties are valid, otherwise false.
     */
    public static boolean isValid(Properties prop) {
        String state = prop.getProperty(ATTR_STATE);
        return STATE_LOADED.equals(state);
    }

    /************************************************************/
    /* Cache Helpers                                            */
    /************************************************************/

    /**
     * <p>Find the best sub locale.</p>
     *
     * @param adr    The URI.
     * @param locstr The locale prefixed by underscore.
     */
    private static String findLocale(String adr, String locstr)
            throws IOException {
        int k = adr.lastIndexOf('.');
        for (; ; ) {
            String key = adr.substring(0, k) + locstr + adr.substring(k);
            boolean ok;
            try {
                ok = OpenCheck.DEFAULT_CHECK.checkHead(key);
            } catch (IOException x) {
                if (OpenCheck.isInterrupt(x)) {
                    throw x;
                } else {
                    ok = false;
                }
            }
            if (ok)
                return key;
            int j = locstr.lastIndexOf('_');
            if (j == -1)
                return null;
            locstr = locstr.substring(0, j);
        }
    }

    /**
     * <p>Load binary properties.</p>
     *
     * @param know The recogbizer.
     * @param adr  The URI.
     * @param prop The properties.
     * @throws LicenseError Problem reading.
     * @throws IOException  Problem reading.
     * @throws ScannerError Problem reading.
     */
    public static void loadBinary(AbstractRecognizer know,
                                  String adr, Properties prop)
            throws LicenseError, IOException, ScannerError {
        OpenOpts opts = new OpenOpts();
        opts.setFlags(opts.getFlags() | OpenOpts.MASK_OPEN_BINR);
        InputStream in = (InputStream) opts.openRead(know, adr);
        try {
            know.loadBinary(prop, in);
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
     * @param know The recogbizer.
     * @param adr  The URI.
     * @param prop The properties.
     * @throws LicenseError Problem reading.
     * @throws IOException  Problem reading.
     * @throws ScannerError Problem reading.
     */
    public static void loadText(AbstractRecognizer know,
                                String adr, Properties prop)
            throws LicenseError, IOException, ScannerError {
        OpenOpts opts = new OpenOpts();
        Reader reader = (Reader) opts.openRead(know, adr);
        try {
            know.loadText(prop, reader);
        } catch (IOException x) {
            reader.close();
            throw x;
        } catch (ScannerError x) {
            reader.close();
            throw x;
        }
        reader.close();
    }

}
