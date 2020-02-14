package jekpro.reference.structure;

import jekpro.frequent.stream.ForeignStream;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.call.InterpreterMessage;
import matula.util.system.*;

import java.io.*;

/**
 * <p>The foreign predicates for the module bytes.</p>
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
public final class ForeignBytes {

    /****************************************************************/
    /* Atom Block Conversion                                        */
    /****************************************************************/

    /**
     * <p>Convert a block to an atom.</p>
     *
     * @param buf The block.
     * @return The atom.
     */
    public static String sysBlockToAtom(byte[] buf) {
        return new String(buf, 0);
    }

    /**
     * <p>Convert an atom to a block.</p>
     *
     * @param str The atom.
     * @return The block.
     * @throws InterpreterMessage Representation error.
     */
    public static byte[] sysAtomToBlock(String str)
            throws InterpreterMessage {
        int n = str.length();
        byte[] buf = new byte[n];
        for (int i = 0; i < n; i++) {
            int ch = str.charAt(i);
            if (0 <= ch && ch <= 255) {
                buf[i] = (byte) ch;
            } else {
                throw new InterpreterMessage(InterpreterMessage.representationError(
                        EngineMessage.OP_REPRESENTATION_OCTET));
            }
        }
        return buf;
    }

    /**
     * <p>Convert a block to an atom.</p>
     *
     * @param buf The block.
     * @param opt The block options term.
     * @return The atom.
     * @throws ClassCastException           Validation error.
     * @throws InterpreterMessage           Validation error.
     * @throws UnsupportedEncodingException Unsupported encoding.
     */
    public static String sysBlockToAtom(byte[] buf, Object opt)
            throws ClassCastException, InterpreterMessage, UnsupportedEncodingException {
        OpenDuplex od = ForeignStream.decodeOpenDuplex(ForeignStream.MODE_READ, opt);
        String enc = od.getEncoding();
        return new String(buf, (enc != null ? enc : ForeignUri.ENCODING_UTF8));
    }

    /**
     * <p>Convert an atom to a block.</p>
     *
     * @param str The atom.
     * @param opt The block options term.
     * @return The block.
     * @throws ClassCastException           Validation error.
     * @throws InterpreterMessage           Validation error.
     * @throws UnsupportedEncodingException Unsupported encoding.
     */
    public static byte[] sysAtomToBlock(String str, Object opt)
            throws ClassCastException, InterpreterMessage, UnsupportedEncodingException {
        OpenDuplex od = ForeignStream.decodeOpenDuplex(ForeignStream.MODE_WRITE, opt);
        String enc = od.getEncoding();
        return str.getBytes((enc != null ? enc : ForeignUri.ENCODING_UTF8));
    }

    /********************************************************************/
    /* Memory Socket                                                    */
    /********************************************************************/

    /**
     * <p>Retrieve the data of the output stream.</p>
     *
     * @param str The output stream.
     * @return The data.
     */
    public static byte[] sysMemoryGet(Object str) {
        if (str instanceof ConnectionWriter)
            str = ((ConnectionWriter) str).getUncoded();
        if (str instanceof ConnectionOutput)
            str = ((ConnectionOutput) str).getUnbuf();
        if (str instanceof ByteArrayOutputStream)
            return ((ByteArrayOutputStream) str).toByteArray();
        return null;
    }

    /**
     * <p>Retrieve the data of the output stream.</p>
     *
     * @param str The output stream.
     * @param opt The desired encoding.
     * @return The data.
     * @throws InterpreterMessage Validation error.
     * @throws ClassCastException Validation error.
     * @throws UnsupportedEncodingException Encoding problem.
     */
    public static String sysMemoryGet(Object str, Object opt)
            throws ClassCastException, UnsupportedEncodingException, InterpreterMessage {
        OpenDuplex od = ForeignStream.decodeOpenDuplex(ForeignStream.MODE_READ, opt);
        String enc = od.getEncoding();
        if (str instanceof ConnectionWriter)
            str = ((ConnectionWriter) str).getUncoded();
        if (str instanceof ConnectionOutput)
            str = ((ConnectionOutput) str).getUnbuf();
        if (str instanceof ByteArrayOutputStream)
            return ((ByteArrayOutputStream) str).toString(
                    (enc != null ? enc : ForeignUri.ENCODING_UTF8));
        return null;
    }

    /****************************************************************/
    /* Block Byte I/O                                               */
    /****************************************************************/

    /**
     * <p>Read a byte block.</p>
     *
     * @param in  The input stream.
     * @param arg The maximum length.
     * @return The byte block, or null.
     * @throws IOException I/O Error.
     */
    public static byte[] sysReadBlock(InputStream in, Integer arg)
            throws IOException {
        int len = arg.intValue();
        byte[] data = new byte[len];
        len = in.read(data, 0, len);
        if (len == -1)
            return null;
        if (len == data.length)
            return data;
        byte[] data2 = new byte[len];
        System.arraycopy(data, 0, data2, 0, len);
        return data2;
    }

    /**
     * <p>Write a byte block.</p>
     *
     * @param out  The output stream.
     * @param data The byte block.
     */
    public static void sysWriteBlock(OutputStream out, byte[] data)
            throws IOException {
        out.write(data);
    }

}