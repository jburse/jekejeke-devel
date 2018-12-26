package jekpro.frequent.stream;

import jekpro.reference.arithmetic.SpecialEval;
import jekpro.tools.call.InterpreterMessage;

import java.io.*;

/**
 * The foreign predicates for the module byte.
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
public final class ForeignByte {

    /****************************************************************/
    /* Byte I/O                                                     */
    /****************************************************************/

    /**
     * <p>Write a byte to a binary stream.</p>
     *
     * @param para The stream.
     * @param val  The byte.
     * @throws ClassCastException Validation error.
     * @throws IOException        IO error.
     */
    public static void sysPutByte(OutputStream para, Integer val)
            throws ClassCastException, IOException {
        int k = SpecialEval.castOctet(val);
        para.write(k);
    }

    /**
     * <p>Peek a byte from a binary stream.</p>
     *
     * @param para The stream.
     * @return The peeked byte or -1.
     * @throws IOException IO error.
     */
    public static int sysPeekByte(InputStream para)
            throws IOException {
        para.mark(1);
        int val;
        try {
            val = para.read();
        } catch (IOException x) {
            para.reset();
            throw x;
        }
        para.reset();
        return val;
    }

    /****************************************************************/
    /* General Stream Operations                                    */
    /****************************************************************/

    /**
     * <p>Check whether stream is at end.</p>
     *
     * @param para The stream.
     * @return True if stream is at end.
     * @throws InterpreterMessage Validation error.
     * @throws IOException        IO error.
     */
    public static boolean sysAtEndOfStream(Object para)
            throws InterpreterMessage, IOException {
        if (para instanceof Reader) {
            Reader reader = (Reader) para;
            reader.mark(1);
            boolean flag;
            try {
                flag = (reader.read() == -1);
            } catch (IOException x) {
                reader.reset();
                throw x;
            }
            reader.reset();
            return flag;
        } else if (para instanceof InputStream) {
            InputStream is = (InputStream) para;
            is.mark(1);
            boolean flag;
            try {
                flag = (is.read() == -1);
            } catch (IOException x) {
                is.reset();
                throw x;
            }
            is.reset();
            return flag;
        } else {
            throw new InterpreterMessage(
                    InterpreterMessage.permissionError("input",
                            "stream", para));
        }
    }

    /**
     * <p>Read a byte block.</p>
     *
     * @param in  The input stream.
     * @param len The maximum length.
     * @return The byte block, or null.
     * @throws IOException I/O Error.
     */
    public static byte[] sysReadBlock(InputStream in, int len)
            throws IOException {
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
