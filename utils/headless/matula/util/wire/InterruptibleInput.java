package matula.util.wire;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InterruptedIOException;

/**
 * <p>Provides an interruptible input.</p>
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
final class InterruptibleInput extends FilterInputStream {

    /**
     * <p>Create an interruptible input.</p>
     *
     * @param in The input stream.
     */
    protected InterruptibleInput(InputStream in) {
        super(in);
    }

    /**
     * <p>Read a byte from interruptible input.</p>
     *
     * @return The byte or -1.
     * @throws IOException I/O Error.
     */
    public int read() throws IOException {
        AbstractLivestock live = AbstractLivestock.currentLivestock(Thread.currentThread());
        if (live != null)
            live.closer = this;
        try {
            return super.read();
        } finally {
            if (live != null)
                live.closer = null;
            if (Thread.interrupted())
                throw new InterruptedIOException();
        }
    }

    /**
     * <p>Read bytes from interruptible input.</p>
     *
     * @param b   The bytes.
     * @param off The offset.
     * @param len The length.
     * @return The length or -1.
     * @throws IOException I/O Error.
     */
    public int read(byte[] b, int off, int len) throws IOException {
        AbstractLivestock live = AbstractLivestock.currentLivestock(Thread.currentThread());
        if (live != null)
            live.closer = this;
        try {
            return super.read(b,off,len);
        } finally {
            if (live != null)
                live.closer = null;
            if (Thread.interrupted())
                throw new InterruptedIOException();
        }
    }

}