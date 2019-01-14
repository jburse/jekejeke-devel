package matula.util.misc;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;

/**
 * <p>Provides a chunked output.</p>
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
final class ChunkedOutput extends FilterOutputStream {
    private static final int CHUNK_SIZE = 1024;

    private int count;
    private byte[] buf = new byte[CHUNK_SIZE];

    /**
     * <p>Create a chunked output.</p>
     *
     * @param out The output stream.
     */
    ChunkedOutput(OutputStream out) {
        super(out);
    }

    /**
     * <p>Write a byte to the chunked output.</p>
     *
     * @param b The byte.
     * @throws IOException I/O Error.
     */
    public void write(int b) throws IOException {
        buf[count++] = (byte) b;
        if (count >= buf.length)
            writeChunk();
    }

    /**
     * <p>Write bytes to the chunked output.</p>
     *
     * @param b   The bytes.
     * @param off The offset.
     * @param len The length.
     * @throws IOException I/O Error.
     */
    public void write(byte[] b, int off, int len) throws IOException {
        int fill = Math.min(buf.length - count, len);
        while (fill != 0) {
            System.arraycopy(b, off, buf, count, fill);
            count += fill;
            if (count >= buf.length)
                writeChunk();
            off += fill;
            len -= fill;
            fill = Math.min(buf.length - count, len);
        }
    }

    /**
     * <p>Flush the chunked output.</p>
     *
     * @throws IOException I/O Error.
     */
    public void flush() throws IOException {
        if (count > 0)
            writeChunk();
        out.flush();
    }

    /**
     * <p>Flush the chunked output.</p>
     *
     * @throws IOException I/O Error.
     */
    public void close() throws IOException {
        flush();
        writeChunk();
        out.close();
    }

    /***************************************************************/
    /* Chunk Utility                                               */
    /***************************************************************/

    /**
     * <p>Write a chunk to the underlying output.</p>
     *
     * @throws IOException I/O Error.
     */
    private void writeChunk() throws IOException {
        try {
            byte[] countbytes = Integer.toHexString(count).getBytes("US-ASCII");
            out.write(countbytes);
            out.write('\r');
            out.write('\n');
            out.write(buf, 0, count);
            out.write('\r');
            out.write('\n');
            count = 0;
        } catch (UnsupportedEncodingException x) {
            throw new RuntimeException("shouldn't happen");
        }
    }

}