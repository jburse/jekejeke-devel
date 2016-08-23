package matula.util.system;

import java.io.FilterWriter;
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
public class ConnectionWriter extends FilterWriter {
    private boolean bom;
    private String encoding = "";
    private RandomAccessFile raf;
    private String path;
    private boolean append;
    private int buffer;
    private Writer unbuf;

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

}
