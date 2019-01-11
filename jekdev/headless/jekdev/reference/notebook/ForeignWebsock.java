package jekdev.reference.notebook;

import jekpro.tools.term.TermCompound;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * Provides the methods for the module notebook/websock.
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
public final class ForeignWebsock {
    public static String OP_MESSAGE = "message";

    /**
     * <p>Read a web socket message.</p>
     *
     * @param in The input stream.
     * @return The message or null.
     * @throws IOException Shit happens.
     */
    public static Object sysWebsockRead(InputStream in)
            throws IOException {
        WebsockFrame ws = new WebsockFrame();
        ws.in = in;
        ws.read();
        if (!ws.fin && ws.opcode != WebsockFrame.OPCODE_CONNECTION_CLOSE) {
            int opcode = ws.opcode;
            ByteArrayOutputStream buf = new ByteArrayOutputStream();
            buf.write(ws.payload, 0, ws.payload.length);
            ws.read();
            while (!ws.fin && ws.opcode != WebsockFrame.OPCODE_CONNECTION_CLOSE) {
                buf.write(ws.payload, 0, ws.payload.length);
                ws.read();
            }
            if (ws.opcode != WebsockFrame.OPCODE_CONNECTION_CLOSE) {
                return new TermCompound(OP_MESSAGE,
                        Integer.valueOf(opcode), buf.toByteArray());
            } else {
                return null;
            }
        } else {
            if (ws.opcode != WebsockFrame.OPCODE_CONNECTION_CLOSE) {
                return new TermCompound(OP_MESSAGE,
                        Integer.valueOf(ws.opcode), ws.payload);
            } else {
                return null;
            }
        }
    }

    /**
     * <p>Write a web socket message.</p>
     *
     * @param out     The output stream.
     * @param opcode  The op code.
     * @param payload The payload.
     * @throws IOException Shit happens.
     */
    public static void sysWebsockWrite(OutputStream out, byte opcode, byte[] payload)
            throws IOException {
        WebsockFrame ws = new WebsockFrame();
        ws.out = out;
        ws.fin = true;
        ws.opcode = opcode;
        ws.payload = payload;
        ws.write();
    }

}