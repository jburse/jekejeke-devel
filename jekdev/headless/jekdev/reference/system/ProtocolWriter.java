package jekdev.reference.system;

import java.io.FilterWriter;
import java.io.IOException;
import java.io.Writer;

/**
 * <p>Provides protocolling of a writer.</p>
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
public final class ProtocolWriter extends FilterWriter {
    private Writer protocol;

    /**
     * <p>Create a protocol writer over a writer.</p>
     *
     * @param w The writer.
     */
    public ProtocolWriter(Writer w) {
        super(w);
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
     * <p>Write a character.</p>
     *
     * @param c The character.
     * @throws IOException IO Error.
     */
    public void write(int c) throws IOException {
        out.write(c);
        if (protocol != null)
            protocol.write(c);
    }

    /**
     * <p>Write a character buffer portion.</p>
     *
     * @param cbuf The character buffer.
     * @param off  The offset.
     * @param len  The length.
     * @throws IOException IO Error.
     */
    public void write(char cbuf[], int off, int len) throws IOException {
        out.write(cbuf, off, len);
        if (protocol != null)
            protocol.write(cbuf, off, len);
    }

    /**
     * <p>Write a string portion.</p>
     *
     * @param str The string.
     * @param off The offset.
     * @param len The length.
     * @throws IOException IO Error.
     */
    public void write(String str, int off, int len) throws IOException {
        out.write(str, off, len);
        if (protocol != null)
            protocol.write(str, off, len);
    }

    /**
     * <p>Flush the stream.</p>
     *
     * @throws IOException IO error.
     */
    public void flush() throws IOException {
        out.flush();
        if (protocol != null)
            protocol.flush();
    }

    /**
     * <p>Close the stream.</p>
     *
     * @throws IOException IO Error.
     */
    public void close() throws IOException {
        out.close();
        if (protocol != null)
            protocol.close();
    }

}
