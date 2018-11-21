package jekdev.reference.system;

import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import matula.util.system.ConnectionReader;
import matula.util.system.ConnectionWriter;

import java.io.Writer;

/**
 * The foreign predicates for the module protocol.
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
public final class ForeignProtocol {

    /**
     * <p>Retrieve the protocol writer of a stream.</p>
     *
     * @param para The protocolable stream.
     * @return The protocol writer or null.
     */
    public static Object sysGetStreamProtocol(Object para)
            throws InterpreterMessage {
        if (para instanceof ConnectionReader) {
            para = ((ConnectionReader) para).getUnbuf();
        } else if (para instanceof ConnectionWriter) {
            para = ((ConnectionWriter) para).getUnbuf();
        }
        Writer protocol;
        if (para instanceof ProtocolReader) {
            protocol = ((ProtocolReader) para).getProtocol();
        } else if (para instanceof ProtocolWriter) {
            protocol = ((ProtocolWriter) para).getProtocol();
        } else {
            throw new InterpreterMessage(
                    InterpreterMessage.permissionError("protocol", "stream", para));
        }
        return (protocol != null ? protocol : Knowledgebase.OP_NULL);
    }

    /**
     * <p>Set the protocol writer of a stream.</p>
     *
     * @param para    The protocolable stream.
     * @param proterm The protocol writer or null.
     * @throws InterpreterMessage Permission error
     */
    public static void sysSetStreamProtocol(Object para, Object proterm)
            throws InterpreterMessage {
        Writer protocol;
        if (proterm instanceof String && proterm.equals("null")) {
            protocol = null;
        } else if (proterm instanceof Writer) {
            protocol = (Writer) proterm;
        } else {
            throw new InterpreterMessage(
                    InterpreterMessage.typeError("protocol", para));
        }
        if (para instanceof ConnectionReader) {
            para = ((ConnectionReader) para).getUnbuf();
        } else if (para instanceof ConnectionWriter) {
            para = ((ConnectionWriter) para).getUnbuf();
        }
        if (para instanceof ProtocolReader) {
            ((ProtocolReader) para).setProtocol(protocol);
        } else if (para instanceof ProtocolWriter) {
            ((ProtocolWriter) para).setProtocol(protocol);
        } else {
            throw new InterpreterMessage(
                    InterpreterMessage.permissionError("protocol", "stream", para));
        }
    }

}
