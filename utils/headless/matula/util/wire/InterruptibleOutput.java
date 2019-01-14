package matula.util.wire;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.io.OutputStream;

/**
 * <p>Provides an interruptible output.</p>
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
final class InterruptibleOutput extends FilterOutputStream {

    /**
     * <p>Create an interruptible output.</p>
     *
     * @param out The output stream.
     */
    InterruptibleOutput(OutputStream out) {
        super(out);
    }

    /**
     * <p>Write a byte to the interruptible output.</p>
     *
     * @param b The byte.
     * @throws IOException I/O Error.
     */
    public void write(int b) throws IOException {
        AbstractLivestock live = AbstractLivestock.currentLivestock(Thread.currentThread());
        if (live != null)
            live.closer = this;
        try {
            super.write(b);
        } finally {
            if (live != null)
                live.closer = null;
            if (Thread.interrupted())
                throw new InterruptedIOException();
        }
    }

    /**
     * <p>Write bytes to the interruptible output.</p>
     *
     * @param b   The bytes.
     * @param off The offset.
     * @param len The length.
     * @throws IOException I/O Error.
     */
    public void write(byte[] b, int off, int len) throws IOException {
        AbstractLivestock live = AbstractLivestock.currentLivestock(Thread.currentThread());
        if (live != null)
            live.closer = this;
        try {
            super.write(b,off,len);
        } finally {
            if (live != null)
                live.closer = null;
            if (Thread.interrupted())
                throw new InterruptedIOException();
        }
    }

}