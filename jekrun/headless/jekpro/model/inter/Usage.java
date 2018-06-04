package jekpro.model.inter;

import jekpro.tools.term.PositionKey;

/**
 * <p>This data structure records the usage of a predicate.</p>
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
public class Usage {
    public static final int MASK_TRCK_STYL = 0x00000001;
    public static final int MASK_TRCK_DISC = 0x00000002;

    public static final int MASK_TRCK_OVRD = 0x00000010;
    public static final int MASK_TRCK_MULT = 0x00000020;
    public static final int MASK_TRCK_VSPR = 0x00000040;
    public static final int MASK_TRCK_VSPU = 0x00000080;

    public static final int MASK_TRCK_PRED = 0x00000100;
    public static final int MASK_TRCK_FUNC = 0x00000200;
    public static final int MASK_TRCK_DYNA = 0x00000400;
    public static final int MASK_TRCK_TRLC = 0x00080800;

    private int flags;

    /**
     * <p>Retrieve the flags.</p>
     *
     * @return The flags.
     */
    public int getBits() {
        return flags;
    }

    /**
     * <p>Set a flag.</p>
     *
     * @param mask The flag mask.
     */
    public void setBit(int mask) {
        synchronized (this) {
            flags |= mask;
        }
    }

    /**
     * <p>Reset a flag.</p>
     *
     * @param mask The flag mask.
     */
    public void resetBit(int mask) {
        synchronized (this) {
            flags &= ~mask;
        }
    }

}
