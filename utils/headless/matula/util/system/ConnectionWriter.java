package matula.util.system;

import matula.util.misc.CodeType;

import java.io.FilterWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.Writer;

/**
 * <p>Refinement of the filter writer.</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class ConnectionWriter extends FilterWriter {
    private boolean bom;
    private String encoding = "";
    private RandomAccessFile raf;
    private String path;
    private boolean append;
    private int buffer;
    private Writer unbuf;
    private String newline = OpenOpts.UNIX_NEWLINE;

    /**
     * <p>Create a connection writer from a write.</p>
     *
     * @param w The writer.
     */
    public ConnectionWriter(Writer w) {
        super(w);
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
     * <p>Retrieve the encoding encoding.</p>
     *
     * @return The encoding encoding
     */
    public String getEncoding() {
        return encoding;
    }

    /**
     * <p>Set the encoding encoding.</p>
     *
     * @param c The encoding encoding.
     */
    public void setEncoding(String c) {
        encoding = c;
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
     * <p>Retrieve the append flag.</p>
     *
     * @return The append flag.
     */
    public boolean getAppend() {
        return append;
    }

    /**
     * <p>Set the append flag.</p>
     *
     * @param a The append flag.
     */
    void setAppend(boolean a) {
        append = a;
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
     * <p>Retrieve the unbuffered and unadored writer.</p>
     *
     * @return The unbuffered and unadored write.
     */
    public Writer getUnbuf() {
        return unbuf;
    }

    /**
     * <p>Set the unbuffered and unadored writer.</p>
     *
     * @param u The unbuffered and unadored writer.
     */
    public void setUnbuf(Writer u) {
        unbuf = u;
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

    /***************************************************************/
    /* Newline Rewriting                                           */
    /***************************************************************/

    /**
     * Writes a single character.
     *
     * @throws IOException If an I/O error occurs
     */
    public void write(int c) throws IOException {
        if (c == CodeType.LINE_EOL) {
            out.write(newline);
            return;
        }
        out.write(c);
    }

    /**
     * Writes a portion of an array of characters.
     *
     * @param cbuf Buffer of characters to be written
     * @param off  Offset from which to start reading characters
     * @param len  Number of characters to be written
     * @throws IOException If an I/O error occurs
     */
    public void write(char cbuf[], int off, int len) throws IOException {
        int k = indexOf(cbuf, off, len, CodeType.LINE_EOL);
        while (k != -1) {
            if (k != off)
                out.write(cbuf, off, k - off);
            out.write(newline);
            k++;
            len = len - k + off;
            off = k;
            k = indexOf(cbuf, off, len, CodeType.LINE_EOL);
        }
        if (len != 0)
            out.write(cbuf, off, len);
    }

    /**
     * <p>Find the index of a character in a character buffer.</p>
     *
     * @param cbuf The character buffer.
     * @param off  The offset.
     * @param len  The length.
     * @param ch   The character.
     * @return The index, or -1.
     */
    private static int indexOf(char cbuf[], int off, int len, int ch) {
        for (int i = 0; i < len; i++) {
            if (cbuf[off + i] == ch)
                return off + i;
        }
        return -1;
    }

    /**
     * Writes a portion of a string.
     *
     * @param str String to be written
     * @param off Offset from which to start reading characters
     * @param len Number of characters to be written
     * @throws IOException If an I/O error occurs
     */
    public void write(String str, int off, int len) throws IOException {
        int k = indexOf(str, off, len, CodeType.LINE_EOL);
        while (k != -1) {
            if (k != off)
                out.write(str, off, k - off);
            out.write(newline);
            k++;
            len = len - k + off;
            off = k;
            k = indexOf(str, off, len, CodeType.LINE_EOL);
        }
        if (len != 0)
            out.write(str, off, len);
    }

    /**
     * <p>Find the index of a character in a string.</p>
     *
     * @param str The string.
     * @param off The offset.
     * @param len The length.
     * @param ch  The character.
     * @return The index, or -1.
     */
    private static int indexOf(String str, int off, int len, int ch) {
        for (int i = 0; i < len; i++) {
            if (str.charAt(off + i) == ch)
                return off + i;
        }
        return -1;
    }

}
