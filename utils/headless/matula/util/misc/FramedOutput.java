package matula.util.misc;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * <p>Provides a web socket output.</p>
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
final class FramedOutput extends FilterOutputStream {
    private int count;
    private byte[] buf;
    private boolean cont;
    private boolean fin;
    private byte opcode;
    private byte[] masking;
    private Object lock;

    /**
     * <p>Create a web socket output.</p>
     *
     * @param out The output stream.
     */
    FramedOutput(OutputStream out) {
        super(out);
    }

    /**
     * <p>Retrieve the output stream.</p>
     *
     * @return The output stream.
     */
    OutputStream getOut() {
        return out;
    }

    /**
     * <p>Set the buffer.</p>
     *
     * @param b The buffer.
     */
    void setBuf(byte[] b) {
        buf = b;
    }

    /**
     * <p>Set the lock.</p>
     *
     * @param l The lock.
     */
    void setLock(Object l) {
        lock = l;
    }

    /**
     * <p>Retrieve the lock.</p>
     *
     * @return The lock.
     */
    Object getLock() {
        return lock;
    }

    /**
     * <p>Set the masking.</p>
     *
     * @param m The masking.
     */
    void setMasking(byte[] m) {
        masking = m;
    }

    /**
     * <p>Write a byte to the web socket output.</p>
     *
     * @param b The byte.
     * @throws IOException I/O Error.
     */
    public void write(int b) throws IOException {
        buf[count++] = (byte) b;
        if (count >= buf.length) {
            fin = false;
            opcode = (!cont ? Framed.OPCODE_TEXT_FRAME : Framed.OPCODE_CONTINUATION_FRAME);
            synchronized (lock) {
                writeFrame();
            }
            cont = true;
        }
    }

    /**
     * <p>Write bytes to the web socket output.</p>
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
            if (count >= buf.length) {
                fin = false;
                opcode = (!cont ? Framed.OPCODE_TEXT_FRAME : Framed.OPCODE_CONTINUATION_FRAME);
                synchronized (lock) {
                    writeFrame();
                }
                cont = true;
            }
            off += fill;
            len -= fill;
            fill = Math.min(buf.length - count, len);
        }
    }

    /**
     * <p>Flush the web socket output.</p>
     *
     * @throws IOException I/O Error.
     */
    public void flush() throws IOException {
        if (count > 0) {
            fin = true;
            opcode = (!cont ? Framed.OPCODE_TEXT_FRAME : Framed.OPCODE_CONTINUATION_FRAME);
            synchronized (lock) {
                writeFrame();
                out.flush();
            }
            cont = false;
        } else {
            synchronized (lock) {
               out.flush();
            }
        }
    }

    /**
     * <p>Flush the web socket output.</p>
     *
     * @throws IOException I/O Error.
     */
    public void close() throws IOException {
        flush();
        fin = true;
        opcode = Framed.OPCODE_CONNECTION_CLOSE;
        synchronized (lock) {
            writeFrame();
            out.close();
        }
    }

    /**
     * <p>Send a pong.</p>
     *
     * @throws IOException I/O Error.
     */
    void pong() throws IOException {
        fin = true;
        opcode = Framed.OPCODE_CONNECTION_PONG;
        count = buf.length;
        synchronized (lock) {
            writeFrame();
            out.flush();
        }
    }

    /***************************************************************/
    /* Frame Utility                                               */
    /***************************************************************/

    /**
     * <p>Write a frame to the underlying output.</p>
     *
     * @throws IOException I/O Error.
     */
    private void writeFrame() throws IOException {
        writeOpCode();
        writeLength();
        writeMask();
        writePayload();
        count = 0;
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
        int len = count;
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
        if (masking != null) {
            for (int i = 0; i < count; i++)
                buf[i] ^= masking[i & 0x3];
        }
        if (count != 0)
            out.write(buf, 0, count);
    }

}