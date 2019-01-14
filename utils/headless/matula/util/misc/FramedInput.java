package matula.util.misc;

import java.io.*;

/**
 * <p>Provides a web socket input.</p>
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
final class FramedInput extends FilterInputStream {
    private int remaining;
    public boolean fin;
    public byte opcode;
    public byte[] masking;
    public int pos;

    /**
     * <p>Create a web socket input.</p>
     *
     * @param in The input stream.
     */
    FramedInput(InputStream in) {
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
            readFrameStart();
        if (remaining == -1)
            return -1;
        byte b = readByteOrThrow();
        if (masking != null)
            b ^= masking[pos & 0x3];
        remaining--;
        pos++;
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
            readFrameStart();
        if (remaining == -1)
            return -1;
        int fill = Math.min(remaining, len);
        fill = in.read(b, off, fill);
        if (fill == -1)
            throw new EOFException("eof occured");
        if (masking != null) {
            for (int i = 0; i < fill; i++)
                b[off + i] ^= masking[(pos + i) & 0x3];
        }
        remaining -= fill;
        pos += fill;
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
            readFrameStart();
        if (remaining == -1)
            return 0;
        int n = in.available();
        return Math.min(n, remaining);
    }

    /***************************************************************/
    /* Frame Utility                                               */
    /***************************************************************/

    /**
     * <p>Read a chunk data start or last chunk.</p>
     *
     * @throws IOException I/O Error.
     */
    private void readFrameStart() throws IOException {
        readOpCode();
        int len = readLength();
        readMask();
        remaining = (opcode != Framed.OPCODE_CONNECTION_CLOSE ? len : -1);
        pos = 0;
    }

    /**
     * <p>Read the op code.</p>
     */
    private void readOpCode() throws IOException {
        byte b = readByteOrThrow();
        fin = (b & 0x80) != 0;
        if ((b & 0x40) != 0)
            throw new StreamCorruptedException("bad frame");
        if ((b & 0x20) != 0)
            throw new StreamCorruptedException("bad frame");
        if ((b & 0x10) != 0)
            throw new StreamCorruptedException("bad frame");
        opcode = (byte) (b & 0xf);
    }

    /**
     * <p>Read the length.</p>
     */
    private int readLength() throws IOException {
        byte b = readByteOrThrow();
        if ((b & 0x80) != 0) {
            masking = new byte[4];
        } else {
            masking = null;
        }
        int len = b & 0x7F;
        if (len <= 0x7D) {
            return len;
        } else if (len == 0x7E) {
            len = readByteOrThrow() & 0xFF;
            len <<= 8;
            len |= readByteOrThrow() & 0xFF;
            return len;
        } else if (len == 0x7F) {
            if (readByteOrThrow() != 0)
                throw new StreamCorruptedException("large size");
            if (readByteOrThrow() != 0)
                throw new StreamCorruptedException("large size");
            if (readByteOrThrow() != 0)
                throw new StreamCorruptedException("large size");
            if (readByteOrThrow() != 0)
                throw new StreamCorruptedException("large size");
            len = readByteOrThrow() & 0xFF;
            len <<= 8;
            len |= readByteOrThrow() & 0xFF;
            len <<= 8;
            len |= readByteOrThrow() & 0xFF;
            len <<= 8;
            len |= readByteOrThrow() & 0xFF;
            return len;
        } else {
            throw new StreamCorruptedException("bad frame");
        }
    }

    /**
     * <p>Read the mask.</p>
     */
    private void readMask() throws IOException {
        if (masking == null)
            return;
        masking[0] = readByteOrThrow();
        masking[1] = readByteOrThrow();
        masking[2] = readByteOrThrow();
        masking[3] = readByteOrThrow();
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