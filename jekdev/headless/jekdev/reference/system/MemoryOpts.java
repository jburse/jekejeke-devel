package jekdev.reference.system;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.StringReader;
import java.io.StringWriter;

/**
 * <p>This class represent the memory stream open options.</p>
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
final class MemoryOpts {
    public static final int MASK_OPEN_BINR = 0x00000001;
    private static final byte[] VOID_BYTES = new byte[0];
    private static final String VOID_ATOM = "";

    private int flags;
    private String atom = VOID_ATOM;
    private byte[] bytes = VOID_BYTES;

    /**
     * <p>Retrieve the flags.</p>
     *
     * @return The flags.
     */
    public int getFlags() {
        return flags;
    }

    /**
     * <p>Set the flags.</p>
     *
     * @param f The flags.
     */
    public void setFlags(int f) {
        flags = f;
    }

    /**
     * <p>Retrieve the atom.</p>
     *
     * @return The atom.
     */
    public String getAtom() {
        return atom;
    }

    /**
     * <p>Set the atom.</p>
     *
     * @param a The atom.
     */
    public void setAtom(String a) {
        atom = a;
    }

    /**
     * <p>Retrieve the bytes.</p>
     *
     * @return The bytes.
     */
    public byte[] getBytes() {
        return bytes;
    }

    /**
     * <p>Set the bytes.</p>
     *
     * @param b The bytes.
     */
    public void setBytes(byte[] b) {
        bytes = b;
    }

    /**
     * <p>Open a read stream.</p>
     *
     * @return The read stream.
     */
    public Object openRead() {
        if ((getFlags() & MemoryOpts.MASK_OPEN_BINR) != 0) {
            return new ByteArrayInputStream(getBytes());
        } else {
            return new StringReader(getAtom());
        }
    }

    /**
     * <p>Open a write stream.</p>
     *
     * @return The write stream.
     */
    public Object openWrite() {
        if ((getFlags() & MemoryOpts.MASK_OPEN_BINR) != 0) {
            return new ByteArrayOutputStream();
        } else {
            return new StringWriter();
        }
    }

}
