package matula.util.system;

import matula.util.data.MapEntry;
import matula.util.wire.AbstractRecognizer;
import matula.util.wire.FileExtension;

import java.io.*;
import java.net.BindException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;

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
    public static final String ERROR_BIND_INTERNAL_ERROR = "internal_error";
    public static final String ERROR_BIND_SERVICE_UNAVAILABLE = "service_unavailable";

    public static final int MASK_OPEN_RPOS = 0x00000100;

    private long ifmodifiedsince;
    private String ifnonematch = "";
    private String newline = OpenOpts.UNIX_NEWLINE;

    private AbstractRecognizer paraknow;

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
     * <p>Set the recognizer.</p>
     *
     * @param k The recognizer.
     */
    public void setRecognizer(AbstractRecognizer k) {
        paraknow = k;
    }

    /*************************************************************************/
    /* Opening Stream from Options                                           */
    /*************************************************************************/

    /**
     * <p>Open a read stream.</p>
     *
     * @param adr2 The uri.
     * @return The read stream, or null if not modified.
     * @throws IOException              IO error.
     * @throws IllegalArgumentException Illegal paremeter combination.
     */
    public Object openRead(String adr2)
            throws IOException {
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

            FileExtension fe = OpenOpts.getFileExtension(spec, paraknow);
            if (fe != null && (fe.getType() & FileExtension.MASK_DATA_ECRY) != 0)
                in = paraknow.prepareStream(adr2, in);

            setFile(file);
            Object res = wrapRead(in);
            String mt = (fe != null ? fe.getMimeType() : null);
            OpenDuplex.setMimeType(res, mt != null ? mt : "");
            OpenDuplex.setRaf(res, raf);
            OpenDuplex.setPath(res, adr2);
            return res;
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
                FileExtension fe = OpenOpts.getFileExtension(spec, paraknow);
                if (fe != null && (fe.getType() & FileExtension.MASK_DATA_ECRY) != 0)
                    in = paraknow.prepareStream(adr2, in);

                setFile(file);
                Object res = wrapRead(in);
                String mt = (fe != null ? fe.getMimeType() : null);
                OpenDuplex.setMimeType(res, mt != null ? mt : "");
                OpenDuplex.setPath(res, adr2);
                return res;

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
                        throw new BindException(OpenOpts.ERROR_BIND_INTERNAL_ERROR);
                    if (res == HttpURLConnection.HTTP_UNAVAILABLE)
                        throw new BindException(OpenOpts.ERROR_BIND_SERVICE_UNAVAILABLE);
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

                FileExtension fe = OpenOpts.getFileExtension(spec, paraknow);
                if (fe != null && (fe.getType() & FileExtension.MASK_DATA_ECRY) != 0)
                    in = paraknow.prepareStream(adr2, in);

                setCon(con);
                Object res = wrapRead(in);
                String mt = (fe != null ? fe.getMimeType() : null);
                OpenDuplex.setMimeType(res, mt != null ? mt : "");
                OpenDuplex.setPath(res, adr2);
                return res;
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
            Object res = wrapWrite(out);
            OpenDuplex.setRaf(res, raf);
            OpenDuplex.setPath(res, adr2);
            return res;
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
            Object res = wrapWrite(out);
            OpenDuplex.setPath(res, adr2);
            OpenDuplex.setAppend(res, true);
            return res;
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
            Object res = wrapWrite(out);
            OpenDuplex.setRaf(res, raf);
            OpenDuplex.setPath(res, adr2);
            OpenDuplex.setAppend(res, true);
            return res;
        } else {
            String spec = ForeignUri.sysUriSpec(adr2);
            String scheme = ForeignUri.sysSpecScheme(spec);
            if (!ForeignUri.SCHEME_FILE.equals(scheme))
                throw new IllegalArgumentException("file needed");
            String path = ForeignUri.sysSpecPath(spec);
            File file = new File(path.replace('/', File.separatorChar));
            OutputStream out = new FileOutputStream(file, true);
            Object res = wrapWrite(out);
            OpenDuplex.setPath(res, adr2);
            OpenDuplex.setAppend(res, true);
            return res;
        }
    }

    /**
     * <p>Retrieve the file extension of a URL spec.</p>
     *
     * @param spec The URL spec.
     * @param know The recognizer.
     * @return The mime type or null.
     */
    private static FileExtension getFileExtension(String spec,
                                                  AbstractRecognizer know) {
        do {
            MapEntry<String, FileExtension>[] exts = know.snapshotFileExtensions();
            for (int i = 0; i < exts.length; i++) {
                MapEntry<String, FileExtension> ext = exts[i];
                if ((ext.value.getType() & FileExtension.MASK_USES_SUFX) == 0)
                    continue;
                if (spec.endsWith(ext.key))
                    return ext.value;
            }
            know = know.getParent();
        } while (know != null);
        return null;
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
        if (o instanceof ConnectionReader) {
            InputStream uncoded = ((ConnectionReader) o).getUncoded();
            if (uncoded instanceof ConnectionInput) {
                return ((ConnectionInput) uncoded).getPath();
            }
        }
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

    /**
     * <p>Some testing.</p>
     *
     * @param args Not used.
     */
    /*
    public static void main(String[] args) {
        StringTokenizer st = new StringTokenizer("foo, bar, baz", ",");
        int k=0;
        while (st.hasMoreTokens()) {
            String control = st.nextToken().trim();
            System.out.println("control["+k+"]="+control);
            k++;
        }
    }
    */


}
