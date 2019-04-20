package matula.util.system;

import derek.util.protect.LicenseError;
import matula.util.config.AbstractBundle;
import matula.util.config.AbstractRecognizer;
import matula.util.config.FileExtension;
import matula.util.data.MapEntry;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.JarURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.Charset;
import java.nio.charset.UnsupportedCharsetException;
import java.util.List;

/**
 * <p>This class represent the stream open options.</p>
 * <p>The following flags are supported:</p
 * <ul>
 * <li><b>MASK_OPEN_RPOS:</b> Random access file.</li>
 * <li><b>MASK_OPEN_NOBR:</b> Don't detect a BOM.</li>
 * <li><b>MASK_OPEN_BOMW:</b> Write a BOM.</li>
 * </ul>
 * <p>For text stream, if no encoding is specified, UTF-8 is used.</p>
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
public final class OpenOpts extends OpenDuplex {
    public static final int MASK_OPEN_RPOS = 0x00000100;
    public static final int MASK_OPEN_NOBR = 0x00000200;
    public static final int MASK_OPEN_BOMW = 0x00000400;

    public static final int MASK_CLSE_FRCE = 0x00000001;

    private static final String ENC_UTF_16BE = "UTF-16BE";
    private static final String ENC_UTF_16LE = "UTF-16LE";
    private static final String ENC_UTF_8 = "UTF-8";

    private long ifmodifiedsince;
    private String ifnonematch = "";
    private String newline = OpenOpts.UNIX_NEWLINE;

    /**
     * <p>Retrieve the if-modified-since date.</p>
     *
     * @return The if-modified-since date, or 0.
     */
    private long getIfModifiedSince() {
        return ifmodifiedsince;
    }

    /**
     * <p>Set the if-modified-since date.</p>
     *
     * @param i The if-modified-since date.
     */
    public void setIfModifiedSince(long i) {
        ifmodifiedsince = i;
    }

    /**
     * <p>Retrieve the if-none-match ETag.</p>
     *
     * @return The if-none-match ETag, or "".
     */
    public String getIfNoneMatch() {
        return ifnonematch;
    }

    /**
     * <p>Set the if-none-match ETag.</p>
     *
     * @param i The ETag.
     */
    public void setIfNoneMatch(String i) {
        ifnonematch = i;
    }

    /*************************************************************************/
    /* Opening Stream from Options                                           */
    /*************************************************************************/

    /**
     * <p>Open a read stream.</p>
     *
     * @param know The knowledgebase.
     * @param adr2 The uri.
     * @return The read stream, or null if not modified.
     * @throws IOException              IO error.
     * @throws LicenseError             Decryption error.
     * @throws IllegalArgumentException Illegal paremeter combination.
     */
    public Object openRead(AbstractRecognizer know, String adr2)
            throws LicenseError, IOException {
        if ((getFlags() & MASK_OPEN_RPOS) != 0) {
            String spec = ForeignUri.sysUriSpec(adr2);
            String scheme = ForeignUri.sysSpecScheme(spec);
            if (!ForeignUri.SCHEME_FILE.equals(scheme))
                throw new IllegalArgumentException("file needed");
            String path = ForeignUri.sysSpecPath(spec);
            File file = new File(path.replace('/', File.separatorChar));

            /* client change check */
            long ims = getIfModifiedSince();
            if (ims != 0) {
                long modified = file.lastModified();
                if (modified != 0 && ims >= modified)
                    return null;
            }
            RandomAccessFile raf = new RandomAccessFile(file, "r");
            InputStream in = new FileInputStream(raf.getFD());
            FileExtension fe = OpenOpts.getFileExtension(spec, know);
            if (fe != null && (fe.getType() & FileExtension.MASK_DATA_ECRY) != 0) {
                AbstractBundle cap = know.pathToDecoder(adr2);
                if (cap != null)
                    in = cap.prepareStream(in, know);
            }
            if (getBuffer() != 0)
                in = new BufferedInputStream(in, getBuffer());
            if ((getFlags() & MASK_OPEN_BINR) != 0) {
                ConnectionInput cin = new ConnectionInput(in);
                cin.setLastModified(file.lastModified());
                cin.setETag(Long.toString(cin.getLastModified()));
                String mt = (fe != null ? fe.getMimeType() : null);
                cin.setMimeType(mt != null ? mt : "");
                cin.setRaf(raf);
                cin.setPath(adr2);
                cin.setBuffer(getBuffer());
                cin.setDate(System.currentTimeMillis());
                return cin;
            } else {
                boolean hasbom = false;
                String theencoding = getEncoding();
                if ((getFlags() & MASK_OPEN_NOBR) == 0 &&
                        theencoding == null &&
                        getBuffer() != 0) {
                    in.mark(3);
                    theencoding = detectBom(in);
                    in.reset();
                    hasbom = (theencoding != null);
                }
                if (theencoding == null)
                    theencoding = ForeignUri.ENCODING_UTF8;
                InputStreamReader isr;
                try {
                    isr = new InputStreamReader(in, theencoding);
                } catch (UnsupportedEncodingException x) {
                    in.close();
                    raf.close();
                    throw x;
                }
                ConnectionReader crd = new ConnectionReader(isr);
                crd.setEncoding(isr.getEncoding());
                crd.setUnbuf(isr);
                crd.setBom(hasbom);
                crd.setLastModified(file.lastModified());
                crd.setETag(Long.toString(crd.getLastModified()));
                String mt = (fe != null ? fe.getMimeType() : null);
                crd.setMimeType(mt != null ? mt : "");
                crd.setLineNumber(1);
                crd.setRaf(raf);
                crd.setPath(adr2);
                crd.setBuffer(getBuffer());
                crd.setDate(System.currentTimeMillis());
                return crd;
            }
        } else {
            String spec = ForeignUri.sysUriSpec(adr2);
            String scheme = ForeignUri.sysSpecScheme(spec);
            if (ForeignUri.SCHEME_FILE.equals(scheme)) {
                String path = ForeignUri.sysSpecPath(spec);
                File file = new File(path.replace('/', File.separatorChar));

                /* client change check */
                long ims = getIfModifiedSince();
                if (ims != 0) {
                    long modified = file.lastModified();
                    if (modified != 0 && ims >= modified)
                        return null;
                }

                InputStream in = new FileInputStream(file);
                FileExtension fe = OpenOpts.getFileExtension(spec, know);
                if (fe != null && (fe.getType() & FileExtension.MASK_DATA_ECRY) != 0) {
                    AbstractBundle cap = know.pathToDecoder(adr2);
                    if (cap != null)
                        in = cap.prepareStream(in, know);
                }
                if (getBuffer() != 0)
                    in = new BufferedInputStream(in, getBuffer());
                if ((getFlags() & MASK_OPEN_BINR) != 0) {
                    ConnectionInput cin = new ConnectionInput(in);
                    cin.setLastModified(file.lastModified());
                    cin.setETag(Long.toString(cin.getLastModified()));
                    String mt = (fe != null ? fe.getMimeType() : null);
                    cin.setMimeType(mt != null ? mt : "");
                    cin.setPath(adr2);
                    cin.setBuffer(getBuffer());
                    cin.setDate(System.currentTimeMillis());
                    return cin;
                } else {
                    boolean hasbom = false;
                    String theencoding = getEncoding();
                    if ((getFlags() & MASK_OPEN_NOBR) == 0 && theencoding == null && getBuffer() != 0) {
                        in.mark(3);
                        theencoding = detectBom(in);
                        in.reset();
                        hasbom = (theencoding != null);
                    }
                    if (theencoding == null)
                        theencoding = ForeignUri.ENCODING_UTF8;
                    InputStreamReader isr;
                    try {
                        isr = new InputStreamReader(in, theencoding);
                    } catch (UnsupportedEncodingException x) {
                        in.close();
                        throw x;
                    }
                    ConnectionReader crd = new ConnectionReader(isr);
                    crd.setEncoding(isr.getEncoding());
                    crd.setUnbuf(isr);
                    crd.setBom(hasbom);
                    crd.setLastModified(file.lastModified());
                    crd.setETag(Long.toString(crd.getLastModified()));
                    String mt = (fe != null ? fe.getMimeType() : null);
                    crd.setMimeType(mt != null ? mt : "");
                    crd.setLineNumber(1);
                    crd.setPath(adr2);
                    crd.setBuffer(getBuffer());
                    crd.setDate(System.currentTimeMillis());
                    return crd;
                }
            } else {
                String adr = ForeignDomain.sysUriPuny(adr2);
                adr = ForeignUri.sysUriEncode(adr);
                URL url = new URL(adr);
                URLConnection con = url.openConnection();
                con.setUseCaches((getFlags() & MASK_OPEN_CACH) != 0);

                /* server change check */
                long ims = getIfModifiedSince();
                con.setIfModifiedSince(ims);
                String inm = getIfNoneMatch();
                if (!"".equals(inm))
                    con.setRequestProperty("If-None-Match", inm);
                if (con instanceof HttpURLConnection) {
                    int res = ((HttpURLConnection) con).getResponseCode();
                    if (res == HttpURLConnection.HTTP_INTERNAL_ERROR)
                        throw new LicenseError(LicenseError.ERROR_LICENSE_INTERNAL_ERROR);
                    if (res == HttpURLConnection.HTTP_UNAVAILABLE)
                        throw new LicenseError(LicenseError.ERROR_LICENSE_SERVICE_UNAVAILABLE);
                    if (res == HttpURLConnection.HTTP_NOT_MODIFIED)
                        return null;
                }

                /* client change check */
                if (!"".equals(inm)) {
                    String etag = OpenOpts.getETag(con);
                    if (!"".equals(etag) && inm.equals(etag))
                        return null;
                } else if (ims != 0) {
                    long modified = OpenOpts.getLastModified(con);
                    if (modified != 0 && ims >= modified)
                        return null;
                }

                InputStream in = con.getInputStream();
                FileExtension fe = OpenOpts.getFileExtension(spec, know);
                if (fe != null && (fe.getType() & FileExtension.MASK_DATA_ECRY) != 0) {
                    AbstractBundle cap = know.pathToDecoder(adr2);
                    if (cap != null)
                        in = cap.prepareStream(in, know);
                }
                if (getBuffer() != 0)
                    in = new BufferedInputStream(in, getBuffer());
                if ((getFlags() & MASK_OPEN_BINR) != 0) {
                    ConnectionInput cin = new ConnectionInput(in);
                    cin.setLastModified(OpenOpts.getLastModified(con));
                    cin.setETag(OpenOpts.getETag(con));
                    cin.setExpiration(OpenOpts.getExpiration(con));
                    String mt = (fe != null ? fe.getMimeType() : null);
                    cin.setMimeType(mt != null ? mt : "");
                    cin.setPath(adr2);
                    cin.setBuffer(getBuffer());
                    cin.setDate(con.getDate());
                    cin.setMaxAge(OpenOpts.getMaxAge(con));
                    return cin;
                } else {
                    boolean hasbom = false;
                    String theencoding = getEncoding();
                    if (theencoding == null)
                        theencoding = OpenOpts.getCharSet(con);
                    if ((getFlags() & MASK_OPEN_NOBR) == 0 && theencoding == null && getBuffer() != 0) {
                        in.mark(3);
                        theencoding = detectBom(in);
                        in.reset();
                        hasbom = (theencoding != null);
                    }
                    if (theencoding == null)
                        theencoding = ForeignUri.ENCODING_UTF8;
                    InputStreamReader isr;
                    try {
                        isr = new InputStreamReader(in, theencoding);
                    } catch (UnsupportedEncodingException x) {
                        in.close();
                        throw x;
                    }
                    ConnectionReader crd = new ConnectionReader(isr);
                    crd.setEncoding(isr.getEncoding());
                    crd.setUnbuf(isr);
                    crd.setBom(hasbom);
                    crd.setLastModified(OpenOpts.getLastModified(con));
                    crd.setETag(OpenOpts.getETag(con));
                    crd.setExpiration(OpenOpts.getExpiration(con));
                    String mt = (fe != null ? fe.getMimeType() : null);
                    crd.setMimeType(mt != null ? mt : "");
                    crd.setLineNumber(1);
                    crd.setPath(adr2);
                    crd.setBuffer(getBuffer());
                    crd.setDate(con.getDate());
                    crd.setMaxAge(OpenOpts.getMaxAge(con));
                    return crd;
                }
            }
        }
    }

    /**
     * <p>Open a write stream.</p>
     *
     * @param adr2 The uri.
     * @return The write stream.
     * @throws IOException              IO error.
     * @throws IllegalArgumentException Illegal paremeter combination.
     */
    public Object openWrite(String adr2)
            throws IOException {
        if ((getFlags() & MASK_OPEN_RPOS) != 0) {
            String spec = ForeignUri.sysUriSpec(adr2);
            String scheme = ForeignUri.sysSpecScheme(spec);
            if (!ForeignUri.SCHEME_FILE.equals(scheme))
                throw new IllegalArgumentException("file needed");
            String path = ForeignUri.sysSpecPath(spec);
            File file = new File(path.replace('/', File.separatorChar));
            RandomAccessFile raf = new RandomAccessFile(file, "rw");
            OutputStream out = new FileOutputStream(raf.getFD());
            if ((getFlags() & MASK_OPEN_BINR) != 0) {
                ConnectionOutput cout;
                if (getBuffer() != 0) {
                    cout = new ConnectionOutput(new BufferedOutputStream(out, getBuffer()));
                    cout.setBuffer(getBuffer());
                } else {
                    cout = new ConnectionOutput(out);
                }
                cout.setRaf(raf);
                cout.setPath(adr2);
                return cout;
            } else {
                String theencoding = getEncoding();
                if (theencoding == null)
                    theencoding = ForeignUri.ENCODING_UTF8;
                OutputStreamWriter osw;
                try {
                    osw = new OutputStreamWriter(out, theencoding);
                } catch (UnsupportedEncodingException x) {
                    out.close();
                    raf.close();
                    throw x;
                }
                ConnectionWriter cwr;
                if (getBuffer() != 0) {
                    cwr = new ConnectionWriter(new BufferedWriter(osw, getBuffer()));
                    cwr.setBuffer(getBuffer());
                } else {
                    cwr = new ConnectionWriter(osw);
                }
                cwr.setEncoding(osw.getEncoding());
                cwr.setUnbuf(osw);
                cwr.setRaf(raf);
                cwr.setPath(adr2);
                cwr.setNewLine(newline);
                return cwr;
            }
        } else {
            String spec = ForeignUri.sysUriSpec(adr2);
            String scheme = ForeignUri.sysSpecScheme(spec);
            OutputStream out;
            if (ForeignUri.SCHEME_FILE.equals(scheme)) {
                String path = ForeignUri.sysSpecPath(spec);
                File file = new File(path.replace('/', File.separatorChar));
                out = new FileOutputStream(file);
            } else {
                String adr = ForeignDomain.sysUriPuny(adr2);
                adr = ForeignUri.sysUriEncode(adr);
                URL url = new URL(adr);
                URLConnection con = url.openConnection();
                con.setDoInput(false);
                con.setDoOutput(true);
                out = con.getOutputStream();
            }
            if ((getFlags() & MASK_OPEN_BINR) != 0) {
                ConnectionOutput cout;
                if (getBuffer() != 0) {
                    cout = new ConnectionOutput(new BufferedOutputStream(out, getBuffer()));
                    cout.setBuffer(getBuffer());
                } else {
                    cout = new ConnectionOutput(out);
                }
                cout.setPath(adr2);
                return cout;
            } else {
                String theencoding = getEncoding();
                if (theencoding == null)
                    theencoding = ForeignUri.ENCODING_UTF8;
                if ((getFlags() & MASK_OPEN_BOMW) != 0) {
                    try {
                        generateBom(out, theencoding);
                    } catch (UnsupportedEncodingException x) {
                        out.close();
                        throw x;
                    }
                }
                OutputStreamWriter osw;
                try {
                    osw = new OutputStreamWriter(out, theencoding);
                } catch (UnsupportedEncodingException x) {
                    out.close();
                    throw x;
                }
                ConnectionWriter cwr;
                if (getBuffer() != 0) {
                    cwr = new ConnectionWriter(new BufferedWriter(osw, getBuffer()));
                    cwr.setBuffer(getBuffer());
                } else {
                    cwr = new ConnectionWriter(osw);
                }
                cwr.setEncoding(osw.getEncoding());
                cwr.setUnbuf(osw);
                cwr.setPath(adr2);
                cwr.setNewLine(newline);
                return cwr;
            }
        }
    }

    /**
     * <p>Open a write stream in append mode.</p>
     *
     * @param adr2 The uri.
     * @return The read stream.
     * @throws IOException              IO error.
     * @throws IllegalArgumentException Illegal paremeter combination.
     */
    public Object openAppend(String adr2)
            throws IOException {
        if ((getFlags() & MASK_OPEN_RPOS) != 0) {
            String spec = ForeignUri.sysUriSpec(adr2);
            String scheme = ForeignUri.sysSpecScheme(spec);
            if (!ForeignUri.SCHEME_FILE.equals(scheme))
                throw new IllegalArgumentException("file needed");
            String path = ForeignUri.sysSpecPath(spec);
            File file = new File(path.replace('/', File.separatorChar));
            RandomAccessFile raf = new RandomAccessFile(file, "rw");
            raf.seek(raf.length());
            OutputStream out = new FileOutputStream(raf.getFD());
            if ((getFlags() & MASK_OPEN_BINR) != 0) {
                ConnectionOutput cout;
                if (getBuffer() != 0) {
                    cout = new ConnectionOutput(new BufferedOutputStream(out, getBuffer()));
                    cout.setBuffer(getBuffer());
                } else {
                    cout = new ConnectionOutput(out);
                }
                cout.setRaf(raf);
                cout.setPath(adr2);
                cout.setAppend(true);
                return cout;
            } else {
                String theencoding = getEncoding();
                if (theencoding == null)
                    theencoding = ForeignUri.ENCODING_UTF8;
                OutputStreamWriter osw;
                try {
                    osw = new OutputStreamWriter(out, theencoding);
                } catch (UnsupportedEncodingException x) {
                    out.close();
                    raf.close();
                    throw x;
                }
                ConnectionWriter cwr;
                if (getBuffer() != 0) {
                    cwr = new ConnectionWriter(new BufferedWriter(osw, getBuffer()));
                    cwr.setBuffer(getBuffer());
                } else {
                    cwr = new ConnectionWriter(osw);
                }
                cwr.setEncoding(osw.getEncoding());
                cwr.setUnbuf(osw);
                cwr.setRaf(raf);
                cwr.setPath(adr2);
                cwr.setNewLine(newline);
                cwr.setAppend(true);
                return cwr;
            }
        } else {
            String spec = ForeignUri.sysUriSpec(adr2);
            String scheme = ForeignUri.sysSpecScheme(spec);
            if (!ForeignUri.SCHEME_FILE.equals(scheme))
                throw new IllegalArgumentException("file needed");
            String path = ForeignUri.sysSpecPath(spec);
            File file = new File(path.replace('/', File.separatorChar));
            OutputStream out = new FileOutputStream(file, true);
            if ((getFlags() & MASK_OPEN_BINR) != 0) {
                ConnectionOutput cout;
                if (getBuffer() != 0) {
                    cout = new ConnectionOutput(new BufferedOutputStream(out, getBuffer()));
                    cout.setBuffer(getBuffer());
                } else {
                    cout = new ConnectionOutput(out);
                }
                cout.setPath(adr2);
                cout.setAppend(true);
                return cout;
            } else {
                String theencoding = getEncoding();
                if (theencoding == null)
                    theencoding = ForeignUri.ENCODING_UTF8;
                OutputStreamWriter osw;
                try {
                    osw = new OutputStreamWriter(out, theencoding);
                } catch (UnsupportedEncodingException x) {
                    out.close();
                    throw x;
                }
                ConnectionWriter cwr;
                if (getBuffer() != 0) {
                    cwr = new ConnectionWriter(new BufferedWriter(osw, getBuffer()));
                    cwr.setBuffer(getBuffer());
                } else {
                    cwr = new ConnectionWriter(osw);
                }
                cwr.setEncoding(osw.getEncoding());
                cwr.setUnbuf(osw);
                cwr.setPath(adr2);
                cwr.setNewLine(newline);
                cwr.setAppend(true);
                return cwr;
            }
        }
    }

    /**
     * <p>Retrieve the last modified date of the connection.</p>
     *
     * @param con The connection.
     * @return The last modified date.
     * @throws IOException IO error.
     */
    private static long getLastModified(URLConnection con)
            throws IOException {
        if (con instanceof JarURLConnection) {
            long res = ((JarURLConnection) con).getJarEntry().getTime();
            return (res != -1 ? res : 0);
        } else {
            return con.getLastModified();
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
     * <p>Retrieve the ETag of the connection.</p>
     *
     * @param con The connection.
     * @return The ETag, or "".
     * @throws IOException IO error.
     */
    public static String getETag(URLConnection con) throws IOException {
        if (con instanceof JarURLConnection) {
            long res = ((JarURLConnection) con).getJarEntry().getTime();
            return (res != -1 ? Long.toString(res) : "");
        } else {
            String res = con.getHeaderField("ETag");
            return (res != null ? res : "");
        }
    }

    /**
     * <p>Retrieve the file extension of a URL spec.</p>
     *
     * @param spec The URL spec.
     * @param know The recognizer.
     * @return The mime type or null.
     */
    private static FileExtension getFileExtension(String spec, AbstractRecognizer know) {
        do {
            MapEntry<String, FileExtension>[] exts = know.snapshotFileExtensions();
            for (int i = 0; i < exts.length; i++) {
                MapEntry<String, FileExtension> ext = exts[i];
                if (spec.endsWith(ext.key))
                    return ext.value;
            }
            know = know.getParent();
        } while (know != null);
        return null;
    }

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
            String control = controls.get(i);
            if (control.startsWith("max-age="))
                return Integer.parseInt(control.substring("max-age=".length()));
        }
        return -1;
    }

    /*************************************************************/
    /* Stream Helper                                             */
    /*************************************************************/

    /**
     * <p>Retrieve the path of a stream.</p>
     *
     * @param o The stream.
     * @return The path.
     */
    public static String getPath(Object o) {
        if (o instanceof ConnectionReader)
            return ((ConnectionReader) o).getPath();
        return null;
    }

    /**
     * <p>Set the line number of a steram.</p>
     *
     * @param o The stream.
     * @param l The line number.
     */
    public static void setLineNumber(Object o, int l) {
        if (o instanceof ConnectionReader)
            ((ConnectionReader) o).setLineNumber(l);
    }

    /**
     * <p>Retrieve the line number of a stream.</p>
     *
     * @param o The stream.
     * @return The line number or 0.
     */
    public static int getLineNumber(Object o) {
        if (o instanceof ConnectionReader)
            return ((ConnectionReader) o).getLineNumber();
        return 0;
    }

    /**
     * <p>Retrieve the current offset of a stream.</p>
     *
     * @param o The stream.
     * @return The offset or 0.
     */
    public static int getOffset(Object o) {
        if (o instanceof ConnectionReader)
            return ((ConnectionReader) o).getOffset();
        return 0;
    }

    /**
     * <p>Retrieve the current line of a stream.</p>
     *
     * @param o The stream.
     * @return The line or null.
     */
    public static String getLine(Object o) {
        if (o instanceof ConnectionReader)
            return ((ConnectionReader) o).getLine();
        return null;
    }

    /*************************************************************/
    /* Formerly Connection Bom                                   */
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
                enc = ENC_UTF_16BE;
        } else if (ch == 0xFF) {
            ch = in.read();
            if (ch == 0xFE)
                enc = ENC_UTF_16LE;
        } else if (ch == 0xEF) {
            ch = in.read();
            if (ch == 0xBB) {
                ch = in.read();
                if (ch == 0xBF)
                    enc = ENC_UTF_8;
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
        if (enc.equals(ENC_UTF_16BE)) {
            out.write(0xFE);
            out.write(0xFF);
        } else if (enc.equals(ENC_UTF_16LE)) {
            out.write(0xFF);
            out.write(0xFE);
        } else if (enc.equals(ENC_UTF_8)) {
            out.write(0xEF);
            out.write(0xBB);
            out.write(0xBF);
        } else {
            throw new UnsupportedCharsetException(enc);
        }
    }

}
