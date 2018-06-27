package jekdev.reference.system;

import jekpro.frequent.stream.ForeignStream;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;

import java.io.ByteArrayOutputStream;
import java.io.StringWriter;

/**
 * The foreign predicates for the module memory.
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public class ForeignMemory {
    private final static String OP_ATOM = "atom";
    private final static String OP_BYTES = "bytes";

    /**
     * <p>Open a memory read stream.
     *
     * @param data The initial data.
     * @param opt  The open options.
     * @throws InterpreterMessage Validation error.
     */
    public static Object sysMemoryRead(Object data, Object opt)
            throws InterpreterMessage {
        MemoryOpts options = new MemoryOpts();
        options.decodeOpenOptions(opt);
        if (data instanceof TermCompound &&
                ((TermCompound) data).getArity() == 1 &&
                ((TermCompound) data).getFunctor().equals(OP_ATOM)) {
            Object help = ((TermCompound) data).getArg(0);
            options.setAtom(InterpreterMessage.castString(help));
        } else if (data instanceof TermCompound &&
                ((TermCompound) data).getArity() == 1 &&
                ((TermCompound) data).getFunctor().equals(OP_BYTES)) {
            Object help = ((TermCompound) data).getArg(0);
            options.setBytes(castBytes(help));
        } else {
            InterpreterMessage.checkInstantiated(data);
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    ForeignStream.OP_OPEN_OPTION, data));
        }
        return options.openRead();
    }

    /**
     * <p>Create a memory write stream.</p>
     *
     * @param opt The options.
     * @return The memory write stream.
     * @throws InterpreterMessage Validation error.
     */
    public static Object sysMemoryWrite(Object opt)
            throws InterpreterMessage {
        MemoryOpts options = new MemoryOpts();
        options.decodeOpenOptions(opt);
        return options.openWrite();
    }

    /**
     * <p>Retrieve the data of the stream.</p>
     *
     * @param str The stream.
     * @throws InterpreterMessage Validation error.
     * @throws InterpreterMessage Shit happens.
     */
    public static Object sysMemoryGet(Object str)
            throws InterpreterMessage {
        if (str instanceof StringWriter) {
            return new TermCompound(OP_ATOM, str.toString());
        } else if (str instanceof ByteArrayOutputStream) {
            byte[] buf = ((ByteArrayOutputStream) str).toByteArray();
            Object res = Knowledgebase.OP_NIL;
            for (int i = buf.length - 1; i >= 0; i--) {
                res = new TermCompound(Knowledgebase.OP_CONS,
                        Integer.valueOf(buf[i] & 0xFF), res);
            }
            return new TermCompound(OP_BYTES, res);
        } else {
            throw new InterpreterMessage(
                    InterpreterMessage.domainError("stream", str));
        }
    }

    /****************************************************************/
    /* Open Options                                                 */
    /****************************************************************/

    /**
     * <p>Cast a term to bytes.</p>
     *
     * @param data The term.
     * @return The bytes.
     * @throws InterpreterMessage Validation error.
     */
    private static byte[] castBytes(Object data)
            throws InterpreterMessage {
        int len = bytesLength(data);
        byte[] buf = new byte[len];
        bytesFill(data, buf);
        return buf;
    }

    /**
     * <p>Determine the length of the bytes.</p>
     *
     * @param help The bytes.
     * @return The length.
     * @throws InterpreterMessage Validation error.
     */
    private static int bytesLength(Object help)
            throws InterpreterMessage {
        int len = 0;
        while (help instanceof TermCompound &&
                ((TermCompound) help).getArity() == 2 &&
                ((TermCompound) help).getFunctor().equals(
                        Knowledgebase.OP_CONS)) {
            len++;
            help = ((TermCompound) help).getArg(1);
        }
        if (help.equals(Knowledgebase.OP_NIL)) {
            /* */
        } else {
            InterpreterMessage.checkInstantiated(help);
            throw new InterpreterMessage(InterpreterMessage.typeError(
                    InterpreterMessage.OP_TYPE_LIST, help));
        }
        return len;
    }

    /**
     * <p>Fill the data of the bytes.</p>
     *
     * @param help The bytes.
     * @param buf  The buffer.
     * @throws InterpreterMessage Validation error.
     */
    private static void bytesFill(Object help, byte[] buf)
            throws InterpreterMessage {
        int pos = 0;
        while (help instanceof TermCompound &&
                ((TermCompound) help).getArity() == 2 &&
                ((TermCompound) help).getFunctor().equals(
                        Knowledgebase.OP_CONS)) {
            Object temp = ((TermCompound) help).getArg(0);
            Number num = InterpreterMessage.castInteger(temp);
            int n = InterpreterMessage.castIntValue(num);
            InterpreterMessage.checkByte(n);
            buf[pos] = (byte) n;
            pos++;
            help = ((TermCompound) help).getArg(1);
        }
        if (help.equals(Knowledgebase.OP_NIL)) {
            /* */
        } else {
            InterpreterMessage.checkInstantiated(help);
            throw new InterpreterMessage(InterpreterMessage.typeError(
                    InterpreterMessage.OP_TYPE_LIST, help));
        }
    }

}
