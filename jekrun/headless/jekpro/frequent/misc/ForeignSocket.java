package jekpro.frequent.misc;

import matula.util.wire.AbstractLivestock;
import matula.util.wire.Interruptible;

import java.io.IOException;
import java.io.InterruptedIOException;
import java.net.*;
import java.nio.channels.ClosedByInterruptException;

/**
 * <p>Provides the methods for the module misc/socket.</p>
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
    private static final int MAX_DATA = 4096;

    /*************************************************************/
    /* TCP/IP Sockets                                            */
    /*************************************************************/

    /**
     * <p>Open a server socket.</p>
     *
     * @param port The port.
     * @return The socket.
     * @throws IOException I/O Error.
     */
    public static ServerSocket sysServerNew(int port)
            throws IOException {
        return new ServerSocket(port);
    }

    /**
     * <p>Retrieve the local server socket port.</p>
     *
     * @param socket The server socket.
     * @return The local port.
     */
    public static int sysServerPort(ServerSocket socket) {
        return socket.getLocalPort();
    }

    /**
     * <p>Derive a session socket from a server socket.</p>
     *
     * @param server The server socket.
     * @return The session socket.
     * @throws IOException                I/O Error.
     * @throws ClosedByInterruptException Interrupted.
     */
    public static Socket sysServerAccept(ServerSocket server)
            throws IOException {
        AbstractLivestock live = AbstractLivestock.currentLivestock(Thread.currentThread());
        if (live != null)
            live.closer = server;
        Socket sock;
        try {
            sock = server.accept();
        } finally {
            if (live != null)
                live.closer = null;
            if (Thread.interrupted())
                throw new InterruptedIOException();
        }
        return new Interruptible(sock);
    }

    /**
     * <p>Open a client socket.</p>
     *
     * @param host The host name.
     * @param port The port.
     * @return The socket.
     * @throws IOException                I/O Error.
     * @throws ClosedByInterruptException Interrupted.
     */
    public static Socket sysClientNew(String host, int port)
            throws IOException {
        Socket sock = new Socket(host, port);
        return new Interruptible(sock);
    }

    /*************************************************************/
    /* UDP/IP Sockets                                            */
    /*************************************************************/

    /**
     * <p>Open a datagram socket.</p>
     *
     * @param port The port.
     * @return The socket.
     * @throws SocketException Socket error.
     */
    public static DatagramSocket sysEndpointNew(int port) throws SocketException {
        return new DatagramSocket(port);
    }

    /**
     * <p>Retrieve the local datagram socket port.</p>
     *
     * @param socket The datagram socket.
     * @return The local port.
     */
    public static int sysEndpointPort(DatagramSocket socket) {
        return socket.getLocalPort();
    }

    /**
     * <p>Receive data from a datagram socket.</p>
     *
     * @param socket The datagram socket.
     * @return The data.
     * @throws IOException IO Error
     */
    public static byte[] sysEndpointReceive(DatagramSocket socket)
            throws IOException {
        byte[] buf = new byte[MAX_DATA];
        DatagramPacket packet = new DatagramPacket(buf, 0, buf.length);
        AbstractLivestock live = AbstractLivestock.currentLivestock(
                Thread.currentThread());
        if (live != null)
            live.closer = socket;
        try {
            socket.receive(packet);
        } finally {
            if (live != null)
                live.closer = null;
            if (Thread.interrupted())
                throw new InterruptedIOException();
        }
        int len = packet.getLength();
        if (len != buf.length) {
            byte[] res = new byte[len];
            System.arraycopy(buf, 0, res, 0, len);
            return res;
        } else {
            return buf;
        }
    }

    /**
     * <p>Send data on a datagram socket.</p>
     *
     * @param socket The datagram socket.
     * @param buf    The data.
     * @param host   The destination host.
     * @param port   The destination port.
     * @throws IOException IO Error
     */
    public static void sysEndpointSend(DatagramSocket socket, byte[] buf, String host, int port)
            throws IOException {
        if (buf.length > MAX_DATA)
            throw new IllegalArgumentException("data too long");
        InetAddress address = InetAddress.getByName(host);
        DatagramPacket packet = new DatagramPacket(buf, 0, buf.length, address, port);
        AbstractLivestock live = AbstractLivestock.currentLivestock(
                Thread.currentThread());
        if (live != null)
            live.closer = socket;
        try {
            socket.send(packet);
        } finally {
            if (live != null)
                live.closer = null;
            if (Thread.interrupted())
                throw new InterruptedIOException();
        }
    }

}