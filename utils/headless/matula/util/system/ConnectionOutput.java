package matula.util.system;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.RandomAccessFile;

/**
 * <p>Refinement of the filter output stream.</p>
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
public final class ConnectionOutput extends FilterOutputStream {
    private RandomAccessFile raf;
    private String path;
    private boolean append;
    private int buffer;

    /**
     * <p>Create a filter output stream from an output stream.</p>
     *
     * @param out The output stream.
     */
    ConnectionOutput(OutputStream out) {
        super(out);
    }

    /**
     * <p>Compensate for lack of overriding in filter output stream.</p>
     *
     * @param b   The bytes.
     * @param off the start offset in the data.
     * @param len the number of bytes to write.
     * @throws IOException IO error.
     */
    public void write(byte b[], int off, int len) throws IOException {
        out.write(b, off, len);
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
    void setBuffer(int b) {
        buffer = b;
    }

    /**
     * <p>Close the stream and the file.</p>
     *
     * @throws IOException IO error.
     */
    public void close() throws IOException {
        out.close();
        if (raf != null)
            raf.close();
    }

}
