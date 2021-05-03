package matula.util.system;

import java.io.*;
import java.net.JarURLConnection;
import java.net.URLConnection;
import java.nio.charset.Charset;
import java.nio.charset.UnsupportedCharsetException;
import java.util.List;
import java.util.StringTokenizer;

/**
 * <p>This class represent the stream wrapping options.</p>
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
public class OpenDuplex extends OpenCheck {
    public static final int MASK_OPEN_BINR = 0x00000010;
    public static final int MASK_OPEN_BOMW = 0x00000020;
    public static final int MASK_OPEN_NOBR = 0x00000040;

    public static final String UNIX_NEWLINE = "\n";
    public static final String MAC_NEWLINE = "\r";
    public static final String WINDOWS_NEWLINE = "\r\n";

    public static final char BOM = '\uFEFF';
    public static final String ERROR_TOKEN = "-ERROR- ";

    private String encoding;
    private int buffer = 8192;
    private String newline = OpenOpts.UNIX_NEWLINE;

    static final String ENCODING_UTF16BE = "UTF-16BE";
    static final String ENCODING_UTF16LE = "UTF-16LE";

    private URLConnection paracon;
    private File parafile;

    /**
     * <p>Set the character set encoding.</p>
     *
     * @param c The character set encoding.
     */
    public void setEncoding(String c) {
        encoding = c;
    }

    /**
     * <p>Retrieve the character set encoding.</p>
     *
     * @return The character set encoding, or null.
     */
    public String getEncoding() {
        return encoding;
    }

    /**
     * <p>Set the buffer size.</p>
     *
     * @param b The buffer size.
     */
    public void setBuffer(int b) {
        buffer = b;
    }

    /**
     * <p>Retrieve the buffer size.</p>
     *
     * @return The buffer size.
     */
    public int getBuffer() {
        return buffer;
    }

    /**
     * <p>Set the new line string.</p>
     *
     * @param n The new line string.
     */
    public void setNewLine(String n) {
        newline = n;
    }

    /**
     * <p>Retrieve the new line string.</p>
     *
     * @return The new line string.
     */
    public String getNewLine() {
        return newline;
    }

    /**
     * <p>Set the connection.</p>
     *
     * @param c The connection.
     */
    public void setCon(URLConnection c) {
        paracon = c;
    }

    /**
     * <p>Set the file.</p>
     *
     * @param f The file.
     */
    public void setFile(File f) {
        parafile = f;
    }

    /**
     * <p>Wrap a read stream.</p>
     *
     * @param in The input stream.
     * @return The read stream, or null if not modified.
     * @throws IOException              IO error.
     * @throws IllegalArgumentException Illegal paremeter combination.
     */
    public Object wrapRead(InputStream in)
            throws IOException {
        ConnectionInput cin;
        if (getBuffer() != 0) {
            cin = new ConnectionInput(new BufferedInputStream(in, getBuffer()));
            cin.setBuffer(getBuffer());
        } else {
            cin = new ConnectionInput(in);
        }
        Object res;
        if ((getFlags() & MASK_OPEN_BINR) != 0) {
            res = cin;
        } else {
            boolean hasbom = false;
            String theencoding = getEncoding();
            if (theencoding == null && paracon != null)
                theencoding = getCharSet(paracon);
            if ((getFlags() & MASK_OPEN_NOBR) == 0 &&
                    theencoding == null &&
                    getBuffer() != 0) {
                cin.mark(3);
                theencoding = detectBom(cin);
                cin.reset();
                hasbom = (theencoding != null);
            }
            if (theencoding == null)
                theencoding = ForeignUri.ENCODING_UTF8;
            InputStreamReader isr;
            try {
                isr = new InputStreamReader(cin, theencoding);
            } catch (UnsupportedEncodingException x) {
                cin.close();
                throw x;
            }
            ConnectionReader crd = new ConnectionReader(isr);
            crd.setUncoded(cin);
            crd.setBom(hasbom);
            res = crd;
        }
        if (parafile != null) {
            long lmod = parafile.lastModified();
            OpenDuplex.setLastModified(res, lmod);
            OpenDuplex.setETag(res, Long.toString(lmod));
            OpenDuplex.setDate(res, System.currentTimeMillis());
        } else if (paracon != null) {
            OpenDuplex.setLastModified(res, getLastModified(paracon));
            OpenDuplex.setETag(res, getETag(paracon));
            OpenDuplex.setDate(res, getDate(paracon));
            OpenDuplex.setExpiration(res, getExpiration(paracon));
            OpenDuplex.setMaxAge(res, getMaxAge(paracon));
        }
        return res;
    }

    /**
     * <p>Wrap a write stream.</p>
     *
     * @param out The the output stream.
     * @return The write stream.
     * @throws IOException              IO error.
     * @throws IllegalArgumentException Illegal paremeter combination.
     */
    public Object wrapWrite(OutputStream out)
            throws IOException {
        ConnectionOutput cout;
        if (getBuffer() != 0) {
            cout = new ConnectionOutput(new BufferedOutputStream(out, getBuffer()));
            cout.setBuffer(getBuffer());
        } else {
            cout = new ConnectionOutput(out);
        }
        if ((getFlags() & MASK_OPEN_BINR) != 0) {
            return cout;
        } else {
            String theencoding = getEncoding();
            if (theencoding == null)
                theencoding = ForeignUri.ENCODING_UTF8;
            if ((getFlags() & MASK_OPEN_BOMW) != 0) {
                try {
                    generateBom(cout, theencoding);
                } catch (UnsupportedEncodingException x) {
                    cout.close();
                    throw x;
                }
            }
            OutputStreamWriter osw;
            try {
                osw = new OutputStreamWriter(cout, theencoding);
            } catch (UnsupportedEncodingException x) {
                cout.close();
                throw x;
            }
            ConnectionWriter cwr = new ConnectionWriter(osw);
            cwr.setUncoded(cout);
            cwr.setNewLine(newline);
            return cwr;
        }
    }

    /*************************************************************/
    /* Connection  Properties                                    */
    /*************************************************************/

    /**
     * <p>Retrieve the charset from the connection.</p>
     *
     * @param con The connection.
     * @return The charset or null.
     */
    private static String getCharSet(URLConnection con) {
        if (con instanceof JarURLConnection)
            return null;
        String typ = con.getContentType();
        if (typ == null)
            return null;
        MimeHeader mh = MimeHeader.getInstance(typ);
        if (mh == null)
            return null;
        return mh.getValue(MimeHeader.MIME_CHARSET);
    }

    /**
     * <p>Retrieve the last modified date of the connection.</p>
     *
     * @param con The connection.
     * @return The last modified date.
     * @throws IOException IO error.
     */
    static long getLastModified(URLConnection con)
            throws IOException {
        if (con instanceof JarURLConnection) {
            long res = ((JarURLConnection) con).getJarEntry().getTime();
            return (res != -1 ? res : 0);
        } else {
            return con.getLastModified();
        }
    }

    /**
     * <p>Retrieve the ETag of the connection.</p>
     *
     * @param con The connection.
     * @return The ETag, or "".
     * @throws IOException IO error.
     */
    static String getETag(URLConnection con) throws IOException {
        if (con instanceof JarURLConnection) {
            long res = ((JarURLConnection) con).getJarEntry().getTime();
            return (res != -1 ? Long.toString(res) : "");
        } else {
            String res = con.getHeaderField("ETag");
            return (res != null ? res : "");
        }
    }

    /**
     * <p>Retrieve the date of the connection.</p>
     *
     * @param con The connection.
     * @return The date.
     */
    private static long getDate(URLConnection con) {
        if (con instanceof JarURLConnection) {
            return 0;
        } else {
            return con.getDate();
        }
    }

    /**
     * <p>Retrieve the expiration date of the connection.</p>
     *
     * @param con The connection.
     * @return The expiration date.
     */
    private static long getExpiration(URLConnection con) {
        if (con instanceof JarURLConnection) {
            return 0;
        } else {
            return con.getExpiration();
        }
    }

    /**
     * <p>Retrieve the cache control max age of the connection.</p>
     *
     * @param con The connection.
     * @return The cache control max age.
     */
    private static int getMaxAge(URLConnection con) {
        if (con instanceof JarURLConnection)
            return -1;
        List<String> controls = con.getHeaderFields().get("cache-control");
        if (controls == null)
            return -1;
        for (int i = 0; i < controls.size(); i++) {
            StringTokenizer st = new StringTokenizer(controls.get(i), ",");
            while (st.hasMoreTokens()) {
                String control = st.nextToken().trim();
                if (control.startsWith("max-age="))
                    return Integer.parseInt(control.substring("max-age=".length()));
            }
        }
        return -1;
    }

    /*************************************************************/
    /* Read Stream Properties                                    */
    /*************************************************************/

    /**
     * <p>Set the last modified.</p>
     *
     * @param obj  The connection reader or input.
     * @param lmod The last modified.
     */
    public static void setLastModified(Object obj, long lmod) {
        if (obj instanceof ConnectionReader)
            obj = ((ConnectionReader) obj).getUncoded();
        ((ConnectionInput) obj).setLastModified(lmod);
    }

    /**
     * <p>Set the etag.</p>
     *
     * @param obj  The connection reader or input.
     * @param etag The etag.
     */
    public static void setETag(Object obj, String etag) {
        if (obj instanceof ConnectionReader)
            obj = ((ConnectionReader) obj).getUncoded();
        ((ConnectionInput) obj).setETag(etag);
    }

    /**
     * <p>Set the mime type.</p>
     *
     * @param obj  The connection reader or input.
     * @param mime The mime type.
     */
    public static void setMimeType(Object obj, String mime) {
        if (obj instanceof ConnectionReader)
            obj = ((ConnectionReader) obj).getUncoded();
        ((ConnectionInput) obj).setMimeType(mime);
    }

    /**
     * <p>Set the date.</p>
     *
     * @param obj  The connection reader or input.
     * @param date The date.
     */
    public static void setDate(Object obj, long date) {
        if (obj instanceof ConnectionReader)
            obj = ((ConnectionReader) obj).getUncoded();
        ((ConnectionInput) obj).setDate(date);
    }

    /**
     * <p>Set the expiration.</p>
     *
     * @param obj The connection reader or input.
     * @param exp The expiration.
     */
    public static void setExpiration(Object obj, long exp) {
        if (obj instanceof ConnectionReader)
            obj = ((ConnectionReader) obj).getUncoded();
        ((ConnectionInput) obj).setExpiration(exp);
    }

    /**
     * <p>Set the max age.</p>
     *
     * @param obj  The connection reader or input.
     * @param mage The max age.
     */
    public static void setMaxAge(Object obj, int mage) {
        if (obj instanceof ConnectionReader)
            obj = ((ConnectionReader) obj).getUncoded();
        ((ConnectionInput) obj).setMaxAge(mage);
    }

    /*************************************************************/
    /* Read/Write Stream Properties                              */
    /*************************************************************/

    /**
     * <p>Set the random access file.</p>
     *
     * @param obj The connection writer/reader or output/input.
     * @param raf The random access file.
     */
    public static void setRaf(Object obj, RandomAccessFile raf) {
        if (obj instanceof ConnectionReader) {
            obj = ((ConnectionReader) obj).getUncoded();
        } else if (obj instanceof ConnectionWriter) {
            obj = ((ConnectionWriter) obj).getUncoded();
        }
        if (obj instanceof ConnectionInput) {
            ((ConnectionInput) obj).setRaf(raf);
        } else {
            ((ConnectionOutput) obj).setRaf(raf);
        }
    }

    /**
     * <p>Set the path.</p>
     *
     * @param obj  The connection writer/reader or output/input.
     * @param path The path.
     */
    public static void setPath(Object obj, String path) {
        if (obj instanceof ConnectionReader) {
            obj = ((ConnectionReader) obj).getUncoded();
        } else if (obj instanceof ConnectionWriter) {
            obj = ((ConnectionWriter) obj).getUncoded();
        }
        if (obj instanceof ConnectionInput) {
            ((ConnectionInput) obj).setPath(path);
        } else {
            ((ConnectionOutput) obj).setPath(path);
        }
    }

    /*************************************************************/
    /* Write Stream Properties                              */
    /*************************************************************/

    /**
     * <p>Set the append flag.</p>
     *
     * @param obj    The connection writer or output.
     * @param append The append flag.
     */
    public static void setAppend(Object obj, boolean append) {
        if (obj instanceof ConnectionWriter)
            obj = ((ConnectionWriter) obj).getUncoded();
        ((ConnectionOutput) obj).setAppend(append);
    }

    /*************************************************************/
    /* Byte Order Mask                                           */
    /*************************************************************/

    /**
     * <p>Detect a bom and return derived encoding.</p>
     *
     * @param in The input stream.
     * @return The encoding, or null.
     * @throws IOException IO error.
     */
    public static String detectBom(InputStream in)
            throws IOException {
        String enc = null;
        int ch = in.read();
        if (ch == 0xFE) {
            ch = in.read();
            if (ch == 0xFF)
                enc = ENCODING_UTF16BE;
        } else if (ch == 0xFF) {
            ch = in.read();
            if (ch == 0xFE)
                enc = ENCODING_UTF16LE;
        } else if (ch == 0xEF) {
            ch = in.read();
            if (ch == 0xBB) {
                ch = in.read();
                if (ch == 0xBF)
                    enc = ForeignUri.ENCODING_UTF8;
            }
        }
        return enc;
    }

    /**
     * <p>Write a bom if possible.</p>
     *
     * @param out The output stream.
     * @param enc The encoding.
     * @throws IOException                 IO error.
     * @throws UnsupportedCharsetException Validation error.
     */
    public static void generateBom(OutputStream out, String enc)
            throws IOException, UnsupportedCharsetException {
        enc = Charset.forName(enc).name();
        if (enc.equals(ENCODING_UTF16BE)) {
            out.write(0xFE);
            out.write(0xFF);
        } else if (enc.equals(ENCODING_UTF16LE)) {
            out.write(0xFF);
            out.write(0xFE);
        } else if (enc.equals(ForeignUri.ENCODING_UTF8)) {
            out.write(0xEF);
            out.write(0xBB);
            out.write(0xBF);
        } else {
            throw new UnsupportedCharsetException(enc);
        }
    }

}