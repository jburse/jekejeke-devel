package matula.util.system;

import derek.util.protect.LicenseError;
import matula.util.regex.ScannerError;

import java.io.IOException;
import java.io.InputStream;
import java.io.InterruptedIOException;
import java.net.SocketTimeoutException;
import java.util.HashMap;
import java.util.Properties;

/**
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
     * @return The language properties, or null.
     * @throws InterruptedIOException IO Interrupted.
     */
    public static Properties getLang(Properties prop,
                                     AbstractRecognizer know,
                                     String adr, String locstr)
            throws IOException {
        String state = prop.getProperty(ATTR_STATE);
        if (state == null) {
            adr = ForeignCache.findLocale(adr, locstr);
            synchronized (prop) {
                state = prop.getProperty(ATTR_STATE);
                if (state == null) {
                    if (adr != null) {
                        try {
                            ForeignCache.loadProperties(know, adr, prop);
                            state = STATE_LOADED;
                            prop.put(ATTR_STATE, state);
                        } catch (LicenseError x) {
                            state = STATE_FAILED;
                            prop.put(ATTR_STATE, state);
                        } catch (ScannerError x) {
                            state = STATE_FAILED;
                            prop.put(ATTR_STATE, state);
                        } catch (IOException x) {
                            if (x instanceof InterruptedIOException &&
                                    !(x instanceof SocketTimeoutException)) {
                                throw x;
                            } else {
                                state = STATE_FAILED;
                                prop.put(ATTR_STATE, state);
                            }
                        }
                    } else {
                        state = STATE_FAILED;
                        prop.put(ATTR_STATE, state);
                    }
                }
            }
        }
        if (state.equals(STATE_LOADED))
            return prop;
        return null;
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
                if (x instanceof InterruptedIOException &&
                        !(x instanceof SocketTimeoutException)) {
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
     * <p>Load properties.</p>
     *
     * @param know The recogbizer.
     * @param adr  The URI.
     * @param prop The properties.
     * @throws IOException  Problem reading.
     * @throws LicenseError Problem reading.
     */
    public static void loadProperties(AbstractRecognizer know,
                                      String adr, Properties prop)
            throws LicenseError, IOException, ScannerError {
        OpenOpts opts = new OpenOpts();
        opts.setFlags(opts.getFlags() | OpenOpts.MASK_OPEN_BINR);
        InputStream in = (InputStream) opts.openRead(know, adr);
        try {
            prop.load(in);
        } catch (IOException x) {
            in.close();
            throw x;
        }
        in.close();
    }

}
