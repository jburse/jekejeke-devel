package matula.util.system;

import matula.util.regex.CodeType;

import java.io.*;

/**
 * <p>Refinement of the filter reader.</p>
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
public final class ConnectionReader extends FilterReader {
    private final static int MAX_LINE = 1024;

    private boolean bom;
    private Reader unbuf;
    private InputStream uncoded;

    private String line = "";
    private int offset;
    private int mark = -1;
    private int lineno = 1;
    private boolean skiplf;
    private StringBuilder buf = new StringBuilder();

    /**
     * <p>Create a connection reader from a reader.</p>
     *
     * @param r The reader.
     */
    public ConnectionReader(Reader r) {
        super(r);
        unbuf = r;
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
     * <p>Retrieve the uncoded input stream.</p>
     *
     * @return The uncoded input stream.
     */
    public InputStream getUncoded() {
        return uncoded;
    }

    /**
     * <p>Set the uncoded input stream.</p>
     *
     * @param u The uncoded input stream.
     */
    public void setUncoded(InputStream u) {
        uncoded = u;
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
            nextLineMax();
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
            nextLineMax();
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
                    nextLineMax();
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

    /************************************************************/
    /* Read Line Helper                                         */
    /************************************************************/

    /**
     * <p>Retrieve a line including the newline character.</p>
     *
     * @throws IOException IO error.
     */
    private void nextLineMax() throws IOException {
        int len = MAX_LINE;
        buf.setLength(0);
        int ch = (0 < len ? in.read() : CodeType.LINE_EOF);
        if (skiplf && ch == CodeType.LINE_EOL)
            ch = in.read();
        while (ch != CodeType.LINE_EOL &&
                ch != CodeType.LINE_WIN &&
                ch != CodeType.LINE_EOF) {
            buf.append((char) ch);
            len--;
            ch = (0 < len ? in.read() : CodeType.LINE_EOF);
        }
        if (ch != CodeType.LINE_EOF)
            buf.append(CodeType.LINE_EOL);
        skiplf = (ch == CodeType.LINE_WIN);
        if (mark != -1) {
            if (mark != 0 &&
                    line.charAt(mark - 1) == CodeType.LINE_EOL)
                lineno++;
            offset = line.length() - mark;
            line = line.substring(mark) + buf.toString();
            mark = 0;
        } else {
            if (line.length() != 0 &&
                    line.charAt(line.length() - 1) == CodeType.LINE_EOL)
                lineno++;
            offset = 0;
            line = buf.toString();
        }
    }

}
