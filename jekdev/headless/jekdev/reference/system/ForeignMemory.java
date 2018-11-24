package jekdev.reference.system;

import jekpro.frequent.stream.ForeignStream;
import jekpro.model.pretty.Foyer;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.StringReader;
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class ForeignMemory {
    public static final int MASK_OPEN_BINR = 0x00000001;

    /**
     * <p>Open a memory read stream.
     *
     * @param data The initial data.
     * @param opt  The open options.
     * @throws InterpreterMessage Validation error.
     */
    public static Object sysMemoryRead(Object data, Object opt)
            throws InterpreterMessage {
        int flags = decodeOpenOptions(opt);
        if ((flags & MASK_OPEN_BINR) != 0) {
            byte[] buf;
            if (data instanceof byte[]) {
                buf = (byte[]) data;
            } else {
                InterpreterMessage.checkInstantiated(data);
                throw new InterpreterMessage(InterpreterMessage.typeError("bytes", data));
            }
            return new ByteArrayInputStream(buf);
        } else {
            String str = InterpreterMessage.castString(data);
            return new StringReader(str);
        }
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
        int flags = decodeOpenOptions(opt);
        if ((flags & MASK_OPEN_BINR) != 0) {
            return new ByteArrayOutputStream();
        } else {
            return new StringWriter();
        }
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
            return str.toString();
        } else if (str instanceof ByteArrayOutputStream) {
            return ((ByteArrayOutputStream) str).toByteArray();
        } else {
            throw new InterpreterMessage(
                    InterpreterMessage.domainError("stream", str));
        }
    }

    /**
     * <p>Decode the memory options.</p>
     *
     * @param opt The memory options term.
     * @throws InterpreterMessage Validation error.
     */
    private static int decodeOpenOptions(Object opt)
            throws InterpreterMessage {
        int flags = 0;
        while (opt instanceof TermCompound &&
                ((TermCompound) opt).getArity() == 2 &&
                ((TermCompound) opt).getFunctor().equals(
                        Knowledgebase.OP_CONS)) {
            Object temp = ((TermCompound) opt).getArg(0);
            if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(ForeignStream.OP_TYPE)) {
                Object help = ((TermCompound) temp).getArg(0);
                if (ForeignStream.atomToType(help)) {
                    flags |= MASK_OPEN_BINR;
                } else {
                    flags &= ~MASK_OPEN_BINR;
                }
            } else {
                InterpreterMessage.checkInstantiated(temp);
                throw new InterpreterMessage(InterpreterMessage.domainError(
                        ForeignStream.OP_OPEN_OPTION, temp));
            }
            opt = ((TermCompound) opt).getArg(1);
        }
        if (opt.equals(Foyer.OP_NIL)) {
            /* */
        } else {
            InterpreterMessage.checkInstantiated(opt);
            throw new InterpreterMessage(InterpreterMessage.typeError(
                    InterpreterMessage.OP_TYPE_LIST, opt));
        }
        return flags;
    }

}
