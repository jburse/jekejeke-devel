package jekdev.reference.system;

import java.io.FilterReader;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

/**
 * <p>Provides protocolling of a reader.</p>
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
public final class ProtocolReader extends FilterReader {
    private Writer protocol;
    private boolean peeking;

    /**
     * <p>Create a protocol reader over a reader.</p>
     *
     * @param r The reader.
     */
    public ProtocolReader(Reader r) {
        super(r);
    }

    /**
     * <p>Set the protocol writer.</p>
     *
     * @param w The protocol write.
     */
    public void setProtocol(Writer w) {
        protocol = w;
    }

    /**
     * <p>Retrieve the protocol writer.</p>
     *
     * @return The protocol writer.
     */
    public Writer getProtocol() {
        return protocol;
    }

    /**
     * <p>Read a character.</p>
     *
     * @return The character or -1.
     * @throws IOException IO Error.
     */
    public int read() throws IOException {
        int ch = in.read();
        if (ch == -1)
            return -1;
        if (!peeking) {
            if (protocol != null)
                protocol.write(ch);
        }
        return ch;
    }

    /**
     * <p>Read a portion of a character buffer.</p>
     *
     * @param cbuf The character buffer.
     * @param off  The offset.
     * @param len  The maximum.
     * @return The effective or -1.
     * @throws IOException Shit happens.
     */
    public int read(char[] cbuf, int off, int len) throws IOException {
        int n = in.read(cbuf, off, len);
        if (n == -1)
            return -1;
        if (!peeking) {
            if (protocol != null)
                protocol.write(cbuf, off, n);
        }
        return n;
    }

    /**
     * <p>Mark current position.</p>
     *
     * @param a The expiry length.
     * @throws IOException IO Error.
     */
    public void mark(int a) throws IOException {
        in.mark(a);
        peeking = true;
    }

    /**
     * <p>Reset to last marked position.</p>
     *
     * @throws IOException IO Error.
     */
    public void reset() throws IOException {
        in.reset();
        peeking = false;
    }

}
