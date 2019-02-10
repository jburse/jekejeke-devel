package matula.util.system;

import matula.util.regex.CodeType;

import java.io.FilterReader;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.Reader;

/**
 * <p>Refinement of the filter reader.</p>
 * <p>Allows the inspection of a couple of data.</p>
 * <p>Also provides line termination sequence compression.</p>
 * <p>Further provides current line and offset inspection.</p>
 * <p>Warning: The pre-allocated string buffer keeps using an
 * internal buffer of size >max of the encountered lines.</p>
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
public final class ConnectionReader extends FilterReader {
    private boolean bom;
    private String encoding = "";
    private long lastmodified;
    private String etag = "";
    private long expiration;
    private String mimetype = "";
    private RandomAccessFile raf;
    private String path;
    private int buffer;
    private Reader unbuf;

    private String line = "";
    private int offset;
    private int mark = -1;
    private int lineno;
    private boolean skiplf;
    private StringBuilder buf = new StringBuilder();

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
        lineno = l;
    }

    /**
     * <p>Get the current line number.</p>
     *
     * @return The current line number
     */
    public int getLineNumber() {
        return lineno;
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
     * <p>Retrieve the current line.</p>
     *
     * @return The line.
     */
    public String getLine() {
        return line;
    }

    /**
     * <p>Retrieve the current offset.</p>
     *
     * @return The offset.
     */
    public int getOffset() {
        return offset;
    }

    /**
     * <p>Read a single character. Line termination sequences are
     * compressed into a single '\n' character and lines are
     * counted.</p>
     *
     * @return The character read, or -1 if end of stream has been reached.
     * @throws IOException IO error.
     */
    public int read()
            throws IOException {
        if (offset == line.length())
            nextLine();
        if (offset == line.length())
            return -1;
        int ch = line.charAt(offset);
        offset++;
        return ch;
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
    public int read(char[] cbuf, int off, int len)
            throws IOException {
        if (offset == line.length())
            nextLine();
        if (offset == line.length())
            return -1;
        int k = Math.min(line.length() - offset, len);
        line.getChars(offset, offset + k, cbuf, off);
        offset += k;
        return k;
    }

    /**
     * Skip characters. Line termination sequences are compressed into a
     * single '\n' character and lines are counted.
     *
     * @param len The number of characters to skip
     * @return The number of characters actually skipped
     * @throws IOException If an I/O error occurs
     */
    public long skip(long len)
            throws IOException {
        if (len < 0) {
            long k = Math.max(-offset, len);
            offset += k;
            return k;
        } else {
            long done = 0;
            while (len > 0) {
                if (offset == line.length())
                    nextLine();
                if (offset == line.length())
                    return done;
                long k = Math.min(line.length() - offset, len);
                offset += k;
                len -= k;
                done += k;
            }
            return done;
        }
    }

    /**
     * <p>Check whether the stream is ready.</p>
     *
     * @return True if the stream is ready, otherwise false,
     */
    public boolean ready()
            throws IOException {
        if (offset == line.length())
            return in.ready();
        return true;
    }

    /**
     * <p>Check whether the stream supports mark.</p>
     *
     * @return True if the stream supports mark, otherwise false.
     */
    public boolean markSupported() {
        return true;
    }

    /**
     * <p>Mark the current positions.</p>
     *
     * @param len The requested lookahead.
     */
    public void mark(int len) {
        if (mark != -1)
            throw new IllegalArgumentException("already marked");
        mark = offset;
    }

    /**
     * <p>Unmark the current position.</p>
     */
    public void reset() {
        if (mark == -1)
            throw new IllegalArgumentException("already reset");
        offset = mark;
        mark = -1;
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

    /************************************************************/
    /* Read Line Helper                                         */
    /************************************************************/

    /**
     * <p>Retrieve a line including the newline character.</p>
     *
     * @throws IOException IO error.
     */
    private void nextLine()
            throws IOException {
        buf.setLength(0);
        int ch = in.read();
        if (skiplf && ch == CodeType.LINE_EOL)
            ch = in.read();
        while (ch != CodeType.LINE_EOL &&
                ch != CodeType.LINE_WIN &&
                ch != CodeType.LINE_EOF) {
            buf.append((char) ch);
            ch = in.read();
        }
        if (ch != CodeType.LINE_EOF)
            buf.append(CodeType.LINE_EOL);
        skiplf = (ch == CodeType.LINE_WIN);
        if (buf.length() != 0) {
            if (line.length() != 0 &&
                    line.charAt(line.length() - 1) == CodeType.LINE_EOL)
                lineno++;
            if (mark == line.length())
                mark = 0;
            line = buf.toString();
            offset = 0;
        }
    }

}
