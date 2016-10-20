package matula.util.system;

import derek.util.protect.LicenseError;
import matula.util.text.ScannerReader;

import java.io.*;
import java.net.*;
import java.nio.charset.Charset;
import java.nio.charset.UnsupportedCharsetException;

/**
 * <p>This class represent the stream open options.</p>
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
public final class OpenOpts {
    public static final int MASK_OPEN_NOBR = 0x00000001;
    public static final int MASK_OPEN_BINR = 0x00000002;
    public static final int MASK_OPEN_RPOS = 0x00000004;
    public static final int MASK_OPEN_CACH = 0x00000008;
    public static final int MASK_OPEN_BOMW = 0x00000010;

    private static final String ENC_UTF_16BE = "UTF-16BE";
    private static final String ENC_UTF_16LE = "UTF-16LE";
    private static final String ENC_UTF_8 = "UTF-8";

    public static final String UNIX_NEWLINE = "\n";
    public static final String MAC_NEWLINE = "\r";
    public static final String WINDOWS_NEWLINE = "\r\n";

    private int flags;
    private String encoding;
    private long ifmodifiedsince;
    private String ifnonematch = "";
    private int buffer = 8192;
    private String newline = OpenOpts.UNIX_NEWLINE;

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
    private String getEncoding() {
        return encoding;
    }

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

    /**
     * <p>Retrieve the buffer size.</p>
     *
     * @return The buffer size.
     */
    private int getBuffer() {
        return buffer;
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
     * <p>Retrieve the new line string.</p>
     *
     * @return The new line string.
     */
    public String getNewLine() {
        return newline;
    }

    /**
     * <p>Set the new line string.</p>
     *
     * @param n The new line string.
     */
    public void setNewLine(String n) {
        newline = n;
    }

    /*************************************************************************/
    /* Opening Stream from Options                                           */
    /*************************************************************************/

    /**
     * <p>Open a read stream.</p>
     *
     * @param know The knowledgebase.
     * @param adr  The uri.
     * @return The read stream, or null if not modified.
     * @throws IOException              IO error.
     * @throws LicenseError              Decryption error.
     * @throws IllegalArgumentException Illegal paremeter combination.
     */
    public Object openRead(AbstractRecognizer know, String adr)
            throws LicenseError, IOException {
        if ((getFlags() & MASK_OPEN_RPOS) != 0) {
            String spec = ForeignUri.sysUriSpec(adr);
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
            if ((getFlags() & MASK_OPEN_BINR) != 0) {
                ConnectionInput cin;
                if (getBuffer() != 0) {
                    cin = new ConnectionInput(new BufferedInputStream(in, getBuffer()));
                    cin.setBuffer(getBuffer());
                } else {
                    cin = new ConnectionInput(in);
                }
                cin.setLastModified(file.lastModified());
                cin.setRaf(raf);
                cin.setPath(adr);
                return cin;
            } else {
                String theenconding = getEncoding();
                if (theenconding == null)
                    theenconding = ForeignUri.ENCODING_UTF8;
                InputStreamReader isr;
                try {
                    isr = new InputStreamReader(in, theenconding);
                } catch (UnsupportedEncodingException x) {
                    in.close();
                    raf.close();
                    throw x;
                }
                ConnectionReader crd;
                if (getBuffer() != 0) {
                    crd = new ConnectionReader(new BufferedReader(isr, getBuffer()));
                    crd.setBuffer(getBuffer());
                } else {
                    crd = new ConnectionReader(isr);
                }
                crd.setEncoding(isr.getEncoding());
                crd.setUnbuf(isr);
                crd.setLastModified(file.lastModified());
                crd.setLineNumber(1);
                crd.setRaf(raf);
                crd.setPath(adr);
                return crd;
            }
        } else {
            String spec = ForeignUri.sysUriSpec(adr);
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
                AbstractDecoder cap = know.pathToDecoder(adr);
                if (cap != null)
                    in = cap.prepareStream(in, know);
                if ((getFlags() & MASK_OPEN_BINR) != 0) {
                    ConnectionInput cin;
                    if (getBuffer() != 0) {
                        cin = new ConnectionInput(new BufferedInputStream(in, getBuffer()));
                        cin.setBuffer(getBuffer());
                    } else {
                        cin = new ConnectionInput(in);
                    }
                    cin.setLastModified(file.lastModified());
                    cin.setPath(adr);
                    return cin;
                } else {
                    boolean hasbom = false;
                    String theencoding = getEncoding();
                    if ((getFlags() & MASK_OPEN_NOBR) == 0 && theencoding == null) {
                        String val = detectBom(in);
                        if (val != null) {
                            theencoding = val;
                            hasbom = true;
                        } else {
                            in.close();
                            in = new FileInputStream(file);
                            cap = know.pathToDecoder(adr);
                            if (cap != null)
                                in = cap.prepareStream(in, know);
                        }
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
                    ConnectionReader crd;
                    if (getBuffer() != 0) {
                        crd = new ConnectionReader(new BufferedReader(isr, getBuffer()));
                        crd.setBuffer(getBuffer());
                    } else {
                        crd = new ConnectionReader(isr);
                    }
                    crd.setEncoding(isr.getEncoding());
                    crd.setUnbuf(isr);
                    crd.setBom(hasbom);
                    crd.setLastModified(file.lastModified());
                    crd.setLineNumber(1);
                    crd.setPath(adr);
                    return crd;
                }
            } else {
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
                    int response = ((HttpURLConnection) con).getResponseCode();
                    if (response == HttpURLConnection.HTTP_UNAVAILABLE)
                        throw new LicenseError(LicenseError.ERROR_LICENSE_SERVICE_UNAVAILABLE);
                    if (response == HttpURLConnection.HTTP_NOT_MODIFIED)
                        return null;
                }

                /* client change check */
                if (ims != 0) {
                    long modified = OpenOpts.getLastModified(con);
                    if (modified != 0 && ims >= modified)
                        return null;
                }
                if (!"".equals(inm)) {
                    String etag = OpenOpts.getETag(con);
                    if (!"".equals(etag) && inm.equals(etag))
                        return null;
                }

                InputStream in = con.getInputStream();
                AbstractDecoder cap = know.pathToDecoder(adr);
                if (cap != null)
                    in = cap.prepareStream(in, know);
                if ((getFlags() & MASK_OPEN_BINR) != 0) {
                    ConnectionInput cin;
                    if (getBuffer() != 0) {
                        cin = new ConnectionInput(new BufferedInputStream(in, getBuffer()));
                        cin.setBuffer(getBuffer());
                    } else {
                        cin = new ConnectionInput(in);
                    }
                    cin.setLastModified(OpenOpts.getLastModified(con));
                    cin.setETag(OpenOpts.getETag(con));
                    cin.setExpiration(OpenOpts.getExpiration(con));
                    cin.setPath(adr);
                    return cin;
                } else {
                    String theencoding = getEncoding();
                    if (theencoding == null) {
                        String typ = con.getContentType();
                        if (typ != null) {
                            MimeHeader mh = new MimeHeader(typ);
                            String val = mh.getValue(MimeHeader.MIME_CHARSET);
                            if (val != null)
                                theencoding = val;
                        }
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
                    ConnectionReader crd;
                    if (getBuffer() != 0) {
                        crd = new ConnectionReader(new BufferedReader(isr, getBuffer()));
                        crd.setBuffer(getBuffer());
                    } else {
                        crd = new ConnectionReader(isr);
                    }
                    crd.setEncoding(isr.getEncoding());
                    crd.setUnbuf(isr);
                    crd.setLastModified(OpenOpts.getLastModified(con));
                    crd.setETag(OpenOpts.getETag(con));
                    crd.setExpiration(OpenOpts.getExpiration(con));
                    crd.setLineNumber(1);
                    crd.setPath(adr);
                    return crd;
                }
            }
        }
    }

    /**
     * <p>Open a write stream.</p>
     *
     * @param adr The uri.
     * @return The write stream.
     * @throws IOException IO error.
     */
    public Object openWrite(String adr)
            throws IOException {
        if ((getFlags() & MASK_OPEN_RPOS) != 0) {
            String spec = ForeignUri.sysUriSpec(adr);
            String scheme = ForeignUri.sysSpecScheme(spec);
            if (!ForeignUri.SCHEME_FILE.equals(scheme))
                throw new IllegalArgumentException();
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
                cout.setPath(adr);
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
                cwr.setPath(adr);
                cwr.setNewLine(newline);
                return cwr;
            }
        } else {
            String spec = ForeignUri.sysUriSpec(adr);
            String scheme = ForeignUri.sysSpecScheme(spec);
            OutputStream out;
            if (ForeignUri.SCHEME_FILE.equals(scheme)) {
                String path = ForeignUri.sysSpecPath(spec);
                File file = new File(path.replace('/', File.separatorChar));
                out = new FileOutputStream(file);
            } else {
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
                cout.setPath(adr);
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
                cwr.setPath(adr);
                cwr.setNewLine(newline);
                return cwr;
            }
        }
    }

    /**
     * <p>Open a write stream in append mode.</p>
     *
     * @param adr The uri.
     * @return The read stream.
     * @throws IOException IO error.
     */
    public Object openAppend(String adr)
            throws IOException {
        if ((getFlags() & MASK_OPEN_RPOS) != 0) {
            String spec = ForeignUri.sysUriSpec(adr);
            String scheme = ForeignUri.sysSpecScheme(spec);
            if (!ForeignUri.SCHEME_FILE.equals(scheme))
                throw new IllegalArgumentException();
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
                cout.setPath(adr);
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
                cwr.setPath(adr);
                cwr.setNewLine(newline);
                cwr.setAppend(true);
                return cwr;
            }
        } else {
            String spec = ForeignUri.sysUriSpec(adr);
            String scheme = ForeignUri.sysSpecScheme(spec);
            if (!ForeignUri.SCHEME_FILE.equals(scheme))
                throw new IllegalArgumentException();
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
                cout.setPath(adr);
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
                cwr.setPath(adr);
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
     * @throws IOException Shit happens.
     */
    private static long getLastModified(URLConnection con)
            throws IOException {
        if (con instanceof JarURLConnection) {
            return ((JarURLConnection) con).getJarEntry().getTime();
        } else {
            return con.getLastModified();
        }
    }

    /**
     * <p>Retrieve the expiration date of the connection.</p>
     *
     * @param con The connection.
     * @return The expiration date.
     * @throws IOException Shit happens.
     */
    private static long getExpiration(URLConnection con)
            throws IOException {
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
     * @throws IOException Shit happens.
     */
    public static String getETag(URLConnection con)
            throws IOException {
        String res = con.getHeaderField("ETag");
        return (res!=null?res:"");
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
        if (o instanceof ConnectionReader) {
            ((ConnectionReader) o).setLineNumber(l);
        } else if (o instanceof ScannerReader) {
            ((ScannerReader) o).setLineNumber(l);
        }
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
        if (o instanceof ScannerReader)
            return ((ScannerReader)o).getLineNumber();
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
        if (o instanceof ScannerReader)
            return ((ScannerReader)o).getOffset();
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
        if (o instanceof ScannerReader)
            return ((ScannerReader)o).getLine();
        return null;
    }

    /*************************************************************/
    /* Formerly Connection Head                                  */
    /*************************************************************/

    /**
     * <p>Check a read stream.</p>
     *
     * @param adr  The uri.
     * @return True if a reader could be optained, otherwise false.
     * @throws IOException              IO error.
     */
    public boolean getHead(String adr)
            throws IOException {
        String spec = ForeignUri.sysUriSpec(adr);
        String scheme = ForeignUri.sysSpecScheme(spec);
        if (ForeignUri.SCHEME_FILE.equals(scheme)) {
            String path = ForeignUri.sysSpecPath(spec);
            File file = new File(path.replace('/', File.separatorChar));

            /* spare an IOException */
            if (!file.exists() || !file.isFile())
                return false;

            /* client change check */
            long ims = getIfModifiedSince();
            if (ims != 0) {
                long modified = file.lastModified();
                if (modified != 0 && ims >= modified)
                    return false;
            }

            return true;
        } else {
            URL url = new URL(adr);
            try {
                URLConnection con = url.openConnection();
                con.setUseCaches((getFlags() & MASK_OPEN_CACH) != 0);

                /* server change check */
                long ims = getIfModifiedSince();
                con.setIfModifiedSince(ims);
                String inm = getIfNoneMatch();
                if (!"".equals(inm))
                    con.setRequestProperty("If-None-Match", inm);
                if (con instanceof HttpURLConnection) {
                    /* Workaround for https://code.google.com/p/android/issues/detail?id=61013 */
                    con.addRequestProperty("Accept-Encoding", "identity");
                    ((HttpURLConnection) con).setRequestMethod("HEAD");
                    int response = ((HttpURLConnection) con).getResponseCode();
                    if (response == HttpURLConnection.HTTP_UNAVAILABLE)
                        return false;
                    if (response == HttpURLConnection.HTTP_NOT_MODIFIED)
                        return false;
                    /* spare an IOException */
                    if (response != HttpURLConnection.HTTP_OK)
                        return false;
                }

                /* client change check */
                if (ims != 0) {
                    long modified = OpenOpts.getLastModified(con);
                    if (modified != 0 && ims >= modified)
                        return false;
                }
                if (!"".equals(inm)) {
                    String etag = OpenOpts.getETag(con);
                    if (!"".equals(etag) && inm.equals(etag))
                        return false;
                }

                InputStream in = con.getInputStream();
                in.close();
                return true;
            } catch (FileNotFoundException x) {
                return false;
            } catch (UnknownHostException x) {
                return false;
            } catch (SocketException x) {
                return false;
            }
        }
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
    public static String detectBom(InputStream in) throws IOException {
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
