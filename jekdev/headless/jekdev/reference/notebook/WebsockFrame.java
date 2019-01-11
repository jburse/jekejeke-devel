package jekdev.reference.notebook;

import java.io.*;

/**
 * Provides a web socket frame.
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
final class WebsockFrame {
    public static final byte OPCODE_TEXT_FRAME = 0x1;
    public static final byte OPCODE_BINARY_FRAME = 0x2;
    public static final byte OPCODE_CONNECTION_CLOSE = 0x8;
    public static final byte OPCODE_CONNECTION_PING = 0x9;
    public static final byte OPCODE_CONNECTION_PONG = 0xA;

    public boolean fin;
    public byte opcode;
    public byte[] masking;
    public byte[] payload;
    public InputStream in;
    public OutputStream out;

    /*************************************************************/
    /* Write Frame                                               */
    /*************************************************************/

    /**
     * <p>Write a web socket frame.</p>
     */
    void write() throws IOException {
        writeOpCode();
        writeLength();
        writeMask();
        writePayload();
    }

    /**
     * <p>Write the op code.</p>
     */
    private void writeOpCode() throws IOException {
        byte b = 0;
        if (fin)
            b |= 0x80;
        b |= (opcode & 0xf);
        out.write(b);
    }

    /**
     * <p>Write the length.</p>
     */
    private void writeLength() throws IOException {
        int len = payload.length;
        if (len <= 0x7D) {
            byte b = (byte) len;
            if (masking != null)
                b |= 0x80;
            out.write(b);
        } else if (len <= 0xffff) {
            byte b = 0x7E;
            if (masking != null)
                b |= 0x80;
            out.write(b);
            out.write((byte) ((len >> 8) & 0xff));
            out.write((byte) ((len) & 0xff));
        } else {
            byte b = 0x7F;
            if (masking != null)
                b |= 0x80;
            out.write(b);
            out.write((byte) 0x0);
            out.write((byte) 0x0);
            out.write((byte) 0x0);
            out.write((byte) 0x0);
            out.write((byte) ((len >> 24) & 0xff));
            out.write((byte) ((len >> 16) & 0xff));
            out.write((byte) ((len >> 8) & 0xff));
            out.write((byte) ((len) & 0xff));
        }
    }

    /**
     * <p>Write the mask.</p>
     */
    private void writeMask() throws IOException {
        if (masking == null)
            return;
        out.write(masking[0]);
        out.write(masking[1]);
        out.write(masking[2]);
        out.write(masking[3]);
    }

    /**
     * <p>Write the payload.</p>
     */
    private void writePayload() throws IOException {
        int len = payload.length;
        for (int i = 0; i < len; i++) {
            byte b = payload[i];
            if (masking != null)
                b ^= masking[i & 0x3];
            out.write(b);
        }
    }

    /*************************************************************/
    /* Read Frame                                                */
    /*************************************************************/

    /**
     * <p>Read a web socket frame.</p>
     */
    void read() throws IOException {
        readOpCode();
        int len = readLength();
        readMask();
        readPayload(len);
    }

    /**
     * <p>Read the op code.</p>
     */
    private void readOpCode() throws IOException {
        byte b = readByteOrThrow();
        fin = (b & 0x80) != 0;
        if ((b & 0x40) != 0)
            throw new StreamCorruptedException();
        if ((b & 0x20) != 0)
            throw new StreamCorruptedException();
        if ((b & 0x10) != 0)
            throw new StreamCorruptedException();
        opcode = (byte)(b & 0xf);
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
                throw new StreamCorruptedException();
            if (readByteOrThrow() != 0)
                throw new StreamCorruptedException();
            if (readByteOrThrow() != 0)
                throw new StreamCorruptedException();
            if (readByteOrThrow() != 0)
                throw new StreamCorruptedException();
            len = readByteOrThrow() & 0xFF;
            len <<= 8;
            len |= readByteOrThrow() & 0xFF;
            len <<= 8;
            len |= readByteOrThrow() & 0xFF;
            len <<= 8;
            len |= readByteOrThrow() & 0xFF;
            return len;
        } else {
            throw new StreamCorruptedException();
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
     * <p>Read the payload.</p>
     */
    private void readPayload(int len) throws IOException {
        payload = new byte[len];
        for (int i = 0; i < len; i++) {
            byte b = readByteOrThrow();
            if (masking != null)
                b ^= masking[i & 0x3];
            payload[i] = b;
        }
    }

    /**
     * <p>Read a byte.</p>
     */
    private byte readByteOrThrow() throws IOException {
        int b = in.read();
        if (b == -1)
            throw new EOFException();
        return (byte) b;
    }

}