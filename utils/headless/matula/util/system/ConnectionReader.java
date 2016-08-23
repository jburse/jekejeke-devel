package matula.util.system;

import java.io.FilterReader;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.Reader;

/**
 * <p>Refinement of the filter reader.</p>
 * <p>Allows the inspection of a couple of data.</p>
 * <p>Also provides line termination sequence compression.</p>
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
public class ConnectionReader extends FilterReader {
    private boolean bom;
    private String encoding = "";
    private long lastmodified;
    private String etag = "";
    private long expiration;
    private RandomAccessFile raf;
    private String path;
    private int buffer;
    private int lineNumber = 0;
    private boolean skipLF;
    private boolean markedSkipLF;
    private int markedLineNumber;
    private Reader unbuf;

    private static final int maxSkipBufferSize = 8192;
    private char skipBuffer[] = null;

    /**
     * <p>Create a connection reader from a reader.</p>
     *
     * @param r The reader.
     */
    public ConnectionReader(Reader r) {
        super(r);
    }

    /**
     * <p>Retrieve the bom flag.</p>
     *
     * @return The bom flag.
     */
    public boolean getBom() {
        return bom;
    }

    /**
     * <p>Set the bom flag.</p>
     *
     * @param b The bom flag.
     */
    void setBom(boolean b) {
        bom = b;
    }

    /**
     * <p>Retrieve the encoding.</p>
     *
     * @return The encoding.
     */
    public String getEncoding() {
        return encoding;
    }

    /**
     * <p>Set the encoding.</p>
     *
     * @param e The encoding.
     */
    public void setEncoding(String e) {
        encoding = e;
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
     * <p>Retrieve the ETag,</p>
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
    public void setPath(String p) {
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
    public void setBuffer(int b) {
        buffer = b;
    }

    /**
     * <p>Set the current line number.</p>
     *
     * @param l An int specifying the line number
     */
    public void setLineNumber(int l) {
        lineNumber = l;
    }

    /**
     * <p>Get the current line number.</p>
     *
     * @return The current line number
     */
    public int getLineNumber() {
        return lineNumber;
    }

    /**
     * <p>Retrieve the unbuffered and unadored reader.</p>
     *
     * @return The unbuffered and unadored reader.
     */
    public Reader getUnbuf() {
        return unbuf;
    }

    /**
     * <p>Set the unbuffered and unadored reader.</p>
     *
     * @param u The unbuffered and unadored reader.
     */
    public void setUnbuf(Reader u) {
        unbuf = u;
    }

    /**
     * <p>Read a single character. Line termination sequences are
     * compressed into a single '\n' character and lines are
     * counted.</p>
     *
     * @return The character read, or -1 if end of stream has been reached.
     * @throws IOException IO error.
     */
    public int read() throws IOException {
        int c = in.read();
        if (skipLF) {
            skipLF = false;
            if (c == '\n')
                c = in.read();
        }
        if (c == '\r') {
            skipLF = true;
            lineNumber++;
            c = '\n';
        } else if (c == '\n') {
            lineNumber++;
        }
        return c;
    }

    /**
     * <p>Read characters into a portion of an array. Line termination
     * sequences are compressed into a single '\n' character and lines are
     * counted.</p>
     *
     * @param cbuf The destination buffer.
     * @param off  The offset at which to start storing characters.
     * @param len  The maximum number of characters to read.
     * @return The number of characters read, or -1 if at end of the stream.
     * @throws IOException IO error.
     */
    public int read(char[] cbuf, int off, int len) throws IOException {
        int n = in.read(cbuf, off, len);
        if (n == -1)
            return n;
        int p = off;
        for (int i = off; i < off + n; i++) {
            char c = cbuf[i];
            if (skipLF) {
                skipLF = false;
                if (c == '\n') {
                    i++;
                    if (i < off + n) {
                        c = cbuf[i];
                    } else {
                        break;
                    }
                }
            }
            if (c == '\r') {
                skipLF = true;
                lineNumber++;
                c = '\n';
            } else if (c == '\n') {
                lineNumber++;
            }
            cbuf[p++] = c;
        }
        return p - off;
    }

    /**
     * <p>Read characters into a portion of an array. Line termination
     * sequences are compressed into a single '\n' character and lines are
     * counted.</p>
     *
     * @param cbuf The destination buffer.
     * @return The number of characters read, or -1 if at end of the stream.
     * @throws IOException IO error.
     */
    public int read(char[] cbuf) throws IOException {
        return read(cbuf, 0, cbuf.length);
    }

    /**
     * Skip characters. Line termination sequences are compressed into a
     * single '\n' character and lines are counted.
     *
     * @param n The number of characters to skip
     * @return The number of characters actually skipped
     * @throws IOException              If an I/O error occurs
     * @throws IllegalArgumentException If <tt>n</tt> is negative
     */
    public long skip(long n) throws IOException {
        if (n < 0)
            throw new IllegalArgumentException("skip() value is negative");
        int nn = (int) Math.min(n, maxSkipBufferSize);
        if ((skipBuffer == null) || (skipBuffer.length < nn))
            skipBuffer = new char[nn];
        long r = n;
        while (r > 0) {
            int nc = read(skipBuffer, 0, (int) Math.min(r, nn));
            if (nc == -1)
                break;
            r -= nc;
        }
        return n - r;
    }

    /**
     * <p>Mark the reader.</p>
     *
     * @param r Limit on the number of characters
     * @throws IOException IO error.
     */
    public void mark(int r) throws IOException {
        in.mark(2 * r);
        markedLineNumber = lineNumber;
        markedSkipLF = skipLF;
    }

    /**
     * <p>Reset the reader.</p>
     *
     * @throws IOException IO error.
     */
    public void reset() throws IOException {
        in.reset();
        lineNumber = markedLineNumber;
        skipLF = markedSkipLF;
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
