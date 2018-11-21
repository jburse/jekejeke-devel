package jekpro.frequent.misc;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.channels.ClosedByInterruptException;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;

/**
 * Provides the methods for the module misc/text.
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
public final class ForeignSocket {

    /**
     * <p>Open a server socket.</p>
     * <p>We use java.nio since it is interuptible.</p>
     *
     * @param port The port.
     * @return The socket.
     * @throws IOException I/O Error.
     */
    public static ServerSocket sysServerNew(int port)
            throws IOException {
        ServerSocketChannel chan = ServerSocketChannel.open();
        ServerSocket sock = chan.socket();
        sock.bind(new InetSocketAddress(port));
        return sock;
    }

    /**
     * <p>Derive a session socket from a server socket.</p>
     * <p>We use java.nio since it is interuptible.</p>
     *
     * @param sock The server socket.
     * @return The session socket.
     * @throws IOException                I/O Error.
     * @throws ClosedByInterruptException Interrupted.
     */
    public static Socket sysServerAccept(ServerSocket sock)
            throws IOException {
        ServerSocketChannel chan = sock.getChannel();
        SocketChannel chan2 = chan.accept();
        return chan2.socket();
    }

    /**
     * <p>Open a client socket.</p>
     * <p>We use java.nio since it is interuptible.</p>
     *
     * @param host The host name.
     * @param port The port.
     * @return The socket.
     * @throws IOException                I/O Error.
     * @throws ClosedByInterruptException Interrupted.
     */
    public static Socket sysClientNew(String host, int port)
            throws IOException {
        SocketChannel chan = SocketChannel.open();
        chan.connect(new InetSocketAddress(host, port));
        return chan.socket();
    }

}