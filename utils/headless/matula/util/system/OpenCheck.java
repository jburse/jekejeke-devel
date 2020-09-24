package matula.util.system;

import java.io.*;
import java.net.*;
import java.nio.channels.AsynchronousCloseException;
import java.nio.channels.ClosedByInterruptException;
import java.nio.channels.FileLockInterruptionException;

/**
 * <p>This class represent the stream check options.</p>
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
public class OpenCheck {
    public static final int MASK_OPEN_CACH = 0x00000001;

    public static final OpenCheck DEFAULT_CHECK = new OpenCheck();

    private int flags;

    /**
     * <p>Retrieve the flags.</p>
     *
     * @return The flags.
     */
    public int getFlags() {
        return flags;
    }

    /**
     * <p>Set the flags.</p>
     *
     * @param f The flags.
     */
    public void setFlags(int f) {
        flags = f;
    }

    /**
     * <p>Check a read stream.</p>
     *
     * @param adr The uri.
     * @return True if a reader could be optained, otherwise false.
     * @throws IOException IO error.
     */
    public boolean checkHead(String adr)
            throws IOException {
        try {
            String spec = ForeignUri.sysUriSpec(adr);
            String scheme = ForeignUri.sysSpecScheme(spec);
            if (ForeignUri.SCHEME_FILE.equals(scheme)) {
                String path = ForeignUri.sysSpecPath(spec);
                File file = new File(path.replace('/', File.separatorChar));
                boolean res = file.exists();
                return res;
            } else {
                adr = ForeignDomain.sysUriPuny(adr);
                adr = ForeignUri.sysUriEncode(adr);
                URL url = new URL(adr);
                URLConnection con = url.openConnection();
                con.setUseCaches((getFlags() & MASK_OPEN_CACH) != 0);

                if (con instanceof HttpURLConnection) {
                    ((HttpURLConnection) con).setInstanceFollowRedirects(false);
                    /* Workaround for https://code.google.com/p/android/issues/detail?id=61013 */
                    con.addRequestProperty("Accept-Encoding", "identity");
                    ((HttpURLConnection) con).setRequestMethod("HEAD");
                    int res = ((HttpURLConnection) con).getResponseCode();
                    if (res == HttpURLConnection.HTTP_INTERNAL_ERROR)
                        return false;
                    if (res == HttpURLConnection.HTTP_UNAVAILABLE)
                        return false;
                    if (res == HttpURLConnection.HTTP_NOT_MODIFIED)
                        return false;
                    /* spare an IOException */
                    if (res != HttpURLConnection.HTTP_OK)
                        return false;
                }

                InputStream in = con.getInputStream();
                in.close();
                return true;
            }
        } catch (FileNotFoundException x) {
            return false;
        } catch (UnknownHostException x) {
            return false;
        } catch (SocketException x) {
            return false;
        }
    }

    /**
     * <p>Check a read stream.</p>
     *
     * @param adr The uri.
     * @return The new uri or null..
     * @throws IOException IO error.
     */
    public String checkRedirect(String adr)
            throws IOException {
        try {
            String spec = ForeignUri.sysUriSpec(adr);
            String scheme = ForeignUri.sysSpecScheme(spec);
            if (ForeignUri.SCHEME_FILE.equals(scheme)) {
                return adr;
            } else {
                adr = ForeignDomain.sysUriPuny(adr);
                adr = ForeignUri.sysUriEncode(adr);
                URL url = new URL(adr);
                URLConnection con = url.openConnection();
                con.setUseCaches((getFlags() & MASK_OPEN_CACH) != 0);

                /* server change check */
                if (con instanceof HttpURLConnection) {
                    ((HttpURLConnection) con).setInstanceFollowRedirects(false);
                    /* Workaround for https://code.google.com/p/android/issues/detail?id=61013 */
                    con.addRequestProperty("Accept-Encoding", "identity");
                    ((HttpURLConnection) con).setRequestMethod("HEAD");
                    int res = ((HttpURLConnection) con).getResponseCode();
                    if (res == HttpURLConnection.HTTP_INTERNAL_ERROR)
                        return null;
                    if (res == HttpURLConnection.HTTP_UNAVAILABLE)
                        return null;
                    if (res == HttpURLConnection.HTTP_NOT_MODIFIED)
                        return null;
                    /* spare an IOException */
                    if (res != HttpURLConnection.HTTP_MOVED_PERM &&
                            res != HttpURLConnection.HTTP_MOVED_TEMP)
                        return null;
                }

                String loc = con.getHeaderField("Location");
                if (loc == null)
                    return null;
                adr = ForeignUri.sysUriAbsolute(adr, loc);
                adr = ForeignUri.sysUriDecode(adr);
                adr = ForeignDomain.sysUriUnpuny(adr);
                return adr;
            }
        } catch (FileNotFoundException x) {
            return null;
        } catch (UnknownHostException x) {
            return null;
        } catch (SocketException x) {
            return null;
        }
    }

    /**
     * <p>Check whether I/O exception is an interrupt.</p>
     *
     * @param x The I/O exception.
     * @return True if the I/O exception is an interrupt, otherwise false.
     */
    public static boolean isInterrupt(IOException x) {
        if (x instanceof SocketTimeoutException) {
            return false;
        } else if (x instanceof InterruptedIOException) {
            return true;
        } else if (x instanceof FileLockInterruptionException) {
            return true;
        } else if (x instanceof ClosedByInterruptException) {
            return true;
        } else if (x instanceof AsynchronousCloseException) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Some tests.</p>
     *
     * @param args The arguments, unused.
     * @throws IOException Domain assembling problem.
     */
    /*
    public static void main(String[] args)
            throws IOException {
        String adr="http://5ch.net/";
        System.out.println("adr=" + adr);
        boolean flag = OpenCheck.DEFAULT_CHECK.checkHead(adr);
        System.out.println("head(adr)=" + flag);

        adr="http://qb5.5ch.net/saku2ch/";
        System.out.println("adr=" + adr);
        flag = OpenCheck.DEFAULT_CHECK.checkHead(adr);
        System.out.println("head(adr)=" + flag);

        String adr = "https://www.stadt-zuerich.ch/robots.txt";
        System.out.println("adr=" + adr);
        boolean flag = OpenCheck.DEFAULT_CHECK.checkHead(adr);
        System.out.println("head(adr)=" + flag);

        System.out.println();

        adr = "http://xn--zrich-kva.ch/robots.txt";
        System.out.println("adr=" + adr);
        adr = OpenCheck.DEFAULT_CHECK.checkRedirect(adr);
        System.out.println("redirect(adr)=" + adr);
    }
    */

}