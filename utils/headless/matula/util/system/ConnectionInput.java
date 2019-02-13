package matula.util.system;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;

/**
 * <p>Refinement of the filter input stream.</p>
 * <p>Allows the inspection of a couple of data.</p>
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
public final class ConnectionInput extends FilterInputStream {
    private long lastmodified;
    private String etag = "";
    private long expiration;
    private String mimetype = "";
    private RandomAccessFile raf;
    private String path;
    private int buffer;
    private long date;
    private int maxage = -1;

    /**
     * <p>Create a buffered input stream from an input stream.</p>
     *
     * @param i The input stream reader.
     */
    ConnectionInput(InputStream i) {
        super(i);
    }

    /**
     * <p>Retrieve the last modified.</p>
     *
     * @return The last modified, or 0.
     */
    public long getLastModified() {
        return lastmodified;
    }

    /**
     * <p>Set the last modified.</p>
     *
     * @param l The last modified.
     */
    void setLastModified(long l) {
        lastmodified = l;
    }

    /**
     * <p>Retrieve the ETag.</p>
     *
     * @return The ETag, or "".
     */
    public String getETag() {
        return etag;
    }

    /**
     * <p>Set the ETag.</p>
     *
     * @param e The ETag.
     */
    void setETag(String e) {
        etag = e;
    }

    /**
     * <p>Retrieve the expiration.</p>
     *
     * @return The expiration.
     */
    public long getExpiration() {
        return expiration;
    }

    /**
     * <p>Set the expiration.</p>
     *
     * @param e The expiration.
     */
    void setExpiration(long e) {
        expiration = e;
    }

    /**
     * <p>Retrieve the mime type.</p>
     *
     * @return The mime type.
     */
    public String getMimeType() {
        return mimetype;
    }

    /**
     * <p>Set the mime type.</p>
     *
     * @param m The mime type.
     */
    public void setMimeType(String m) {
        mimetype = m;
    }

    /**
     * <p>Retrieve the random access file.</p>
     *
     * @return The random access file.
     */
    public RandomAccessFile getRaf() {
        return raf;
    }

    /**
     * <p>Set the random access file.</p>
     *
     * @param r The randoma access file.
     */
    void setRaf(RandomAccessFile r) {
        raf = r;
    }

    /**
     * <p>Retrieve the path.</p>
     *
     * @return The path.
     */
    public String getPath() {
        return path;
    }

    /**
     * <p>Set the path.</p>
     *
     * @param p The path.
     */
    void setPath(String p) {
        path = p;
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
     * <p>Set the buffer size.</p>
     *
     * @param b The buffer size.
     */
    void setBuffer(int b) {
        buffer = b;
    }

    /**
     * <p>Retrieve the access time.</p>
     *
     * @return The access time.
     */
    public long getDate() {
        return date;
    }

    /**
     * <p>Set the access time.</p>
     *
     * @param a The access time.
     */
    void setDate(long a) {
        date = a;
    }

    /**
     * <p>Retrieve the max age.</p>
     *
     * @return The max age.
     */
    public int getMaxAge() {
        return maxage;
    }

    /**
     * <p>Set the max age.</p>
     *
     * @param m The max age.
     */
    void setMaxAge(int m) {
        maxage = m;
    }

    /**
     * <p>Close the stream and the file.</p>
     *
     * @throws IOException IO error.
     */
    public void close() throws IOException {
        in.close();
        if (raf != null)
            raf.close();
    }

}
