package matula.util.misc;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.net.SocketImpl;

/**
 * <p>Provides a web socket input/output pair.</p>
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
public final class Framed extends Socket {
    public static final byte OPCODE_CONTINUATION_FRAME = 0x0;
    public static final byte OPCODE_TEXT_FRAME = 0x1;
    public static final byte OPCODE_BINARY_FRAME = 0x2;
    public static final byte OPCODE_CONNECTION_CLOSE = 0x8;

    private FramedOutput out;
    private FramedInput in;

    /**
     * <p>Create a chunked socket.</p>
     *
     * @param s The socket.
     */
    public Framed(Socket s) throws IOException {
        super((SocketImpl) null);
        out = new FramedOutput(s.getOutputStream());
        in = new FramedInput(s.getInputStream());
    }

    /**
     * <p>Retrieve the output stream.</p>
     *
     * @return The output stream.
     */
    public OutputStream getOutputStream() {
        return out;
    }

    /**
     * <p>Retrieve the input stream.</p>
     *
     * @return The input stream.
     */
    public InputStream getInputStream() {
        return in;
    }

}