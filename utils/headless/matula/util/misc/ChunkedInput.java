package matula.util.misc;

import java.io.*;

/**
 * <p>Provides a chunked input.</p>
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
final class ChunkedInput extends FilterInputStream {
    private static final int MAX_NIBBLES = 8;

    private int remaining;

    /**
     * <p>Create a chunked input.</p>
     *
     * @param in The input stream.
     */
    ChunkedInput(InputStream in) {
        super(in);
    }

    /**
     * <p>Read a byte from chunked input.</p>
     *
     * @return The byte or -1.
     * @throws IOException I/O Error.
     */
    public int read() throws IOException {
        if (remaining == 0)
            readChunkStart();
        if (remaining == -1)
            return -1;
        byte b = readByteOrThrow();
        remaining--;
        if (remaining == 0)
            readChunkEnd();
        return b;
    }

    /**
     * <p>Read bytes from chunked input.</p>
     *
     * @param b   The bytes.
     * @param off The offset.
     * @param len The length.
     * @return The length or -1.
     * @throws IOException I/O Error.
     */
    public int read(byte[] b, int off, int len) throws IOException {
        if (remaining == 0)
            readChunkStart();
        if (remaining == -1)
            return -1;
        int fill = Math.min(remaining, len);
        fill = in.read(b, off, fill);
        if (fill == -1)
            throw new EOFException("eof occured");
        remaining -= fill;
        if (remaining == 0)
            readChunkEnd();
        return fill;
    }

    /**
     * <p>Retrieve the available bytes.</p>
     *
     * @return The available bytes.
     * @throws IOException I/O Error.
     */
    public int available() throws IOException {
        if (remaining == 0)
            readChunkStart();
        if (remaining == -1)
            return 0;
        int n = in.available();
        return Math.min(n, remaining);
    }

    /***************************************************************/
    /* Chunk Utility                                               */
    /***************************************************************/

    /**
     * <p>Read a chunk data start or last chunk.</p>
     *
     * @throws IOException I/O Error.
     */
    private void readChunkStart() throws IOException {
        StringBuilder buf = new StringBuilder();
        byte b = readByteOrThrow();
        while (isHex(b) && buf.length() < MAX_NIBBLES) {
            buf.append((char) b);
            b = readByteOrThrow();
        }
        int val;
        try {
            val = Integer.parseInt(buf.toString(), 16);
        } catch (NumberFormatException x) {
            throw new StreamCorruptedException("large size");
        }
        while (b != '\r')
            b = readByteOrThrow();
        b = readByteOrThrow();
        if (b != '\n')
            throw new StreamCorruptedException("lf missing");
        remaining = (val != 0 ? val : -1);
    }

    /**
     * <p>Read a chunk data end.</p>
     *
     * @throws IOException I/O Error.
     */
    private void readChunkEnd() throws IOException {
        byte b = readByteOrThrow();
        if (b != '\r')
            throw new StreamCorruptedException("cr missing");
        b = readByteOrThrow();
        if (b != '\n')
            throw new StreamCorruptedException("lf missing");
    }

    /**
     * <p>Read a byte.</p>
     */
    private byte readByteOrThrow() throws IOException {
        int b = in.read();
        if (b == -1)
            throw new EOFException("eof occured");
        return (byte) b;
    }

    /**
     * <p>Check whether the byte is a hex.</p>
     *
     * @param b The byte.
     * @return True if the byte is a hex, otherwise false.
     */
    private boolean isHex(byte b) {
        if ('0' <= b && b <= '9')
            return true;
        if ('A' <= b && b <= 'F')
            return true;
        return false;
    }

    /***************************************************************/
    /* Mark Support                                                */
    /***************************************************************/

    /**
     * <p>Retrieve the mark supported property.</p>
     *
     * @return The mark supported property.
     */
    public boolean markSupported() {
        return false;
    }

    /**
     * <p>Perform a mark.</p>
     *
     * @param l The look ahead.
     */
    public void mark(int l) {
        /* do nothing */
    }

    /**
     * <p>Perform a rest.</p>
     */
    public void reset() {
        throw new IllegalArgumentException("not supported");
    }

}