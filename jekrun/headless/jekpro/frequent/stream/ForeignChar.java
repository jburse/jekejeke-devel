package jekpro.frequent.stream;

import jekpro.tools.call.InterpreterMessage;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

/**
 * <p>The foreign predicates for the module char.</p>
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
public final class ForeignChar {

    /****************************************************************/
    /* Char I/O                                                     */
    /****************************************************************/

    /**
     * <p>Send an end of line sequence to a stream.</p>
     *
     * @param para The stream.
     * @throws IOException IO error.
     */
    public static void sysNl(Writer para)
            throws IOException {
        para.write('\n');
        para.flush();
    }

    /**
     * <p>Write a character to a text stream.</p>
     *
     * @param para The stream.
     * @param val  The character.
     * @throws InterpreterMessage Validation error.
     * @throws IOException        IO error.
     */
    public static void sysPutChar(Writer para, String val)
            throws InterpreterMessage, IOException {
        InterpreterMessage.castCharacter(val);
        para.write(val);
    }

    /**
     * <p>Write a code to a text stream.</p>
     *
     * @param para The stream.
     * @param val  The code.
     * @throws InterpreterMessage Validation error.
     * @throws IOException        IO error.
     */
    public static void sysPutCode(Writer para, int val)
            throws InterpreterMessage, IOException {
        InterpreterMessage.checkCharacterCode(val);
        para.write(Character.toChars(val));
    }

    /**
     * <p>Get a code from a text stream.</p>
     *
     * @param para The stream.
     * @return The read code or -1.
     * @throws IOException IO error.
     */
    public static int sysGetCode(Reader para)
            throws IOException {
        int ch = para.read();
        if (Character.isHighSurrogate((char) ch)) {
            para.mark(1);
            int ch2;
            try {
                ch2 = para.read();
            } catch (IOException x) {
                para.reset();
                throw x;
            }
            para.reset();
            if (Character.isLowSurrogate((char) ch2)) {
                ch = Character.toCodePoint((char) ch, (char) ch2);
                para.read();
            }
        }
        return ch;
    }

    /**
     * <p>Read a character from a text stream.</p>
     *
     * @param para The stream.
     * @return The read character or end_of_file.
     * @throws IOException IO error.
     */
    public static String sysGetChar(Reader para)
            throws IOException {
        int ch = sysGetCode(para);
        String val;
        if (ch == -1) {
            val = "end_of_file";
        } else {
            val = new String(Character.toChars(ch));
        }
        return val;
    }

    /**
     * <p>Peek a code from a text stream.</p>
     *
     * @param para The stream.
     * @return The peeked code or -1.
     * @throws IOException IO error.
     */
    public static int sysPeekCode(Reader para)
            throws IOException {
        para.mark(2);
        int ch;
        try {
            ch = para.read();
            if (Character.isHighSurrogate((char) ch)) {
                int ch2 = para.read();
                if (Character.isLowSurrogate((char) ch2))
                    ch = Character.toCodePoint((char) ch, (char) ch2);
            }
        } catch (IOException x) {
            para.reset();
            throw x;
        }
        para.reset();
        return ch;
    }


    /**
     * <p>Peek a character from a text stream.</p>
     *
     * @param para The stream.
     * @return The peeked character or end_of_file.
     * @throws IOException IO error.
     */
    public static String sysPeekChar(Reader para)
            throws IOException {
        int ch = sysPeekCode(para);
        String val;
        if (ch == -1) {
            val = "end_of_file";
        } else {
            val = new String(Character.toChars(ch));
        }
        return val;
    }

}
