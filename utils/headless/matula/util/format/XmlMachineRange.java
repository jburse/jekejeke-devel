package matula.util.format;

/**
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
final class XmlMachineRange {
    private int off;
    private int len;

    /**
     * <p>Create a machine range.</p>
     *
     * @param o The offset.
     * @param l The length.
     */
    XmlMachineRange(int o, int l) {
        off = o;
        len = l;
    }

    /**
     * <p>Retrieve the offset.</p>
     *
     * @return The offset.
     */
    int getOff() {
        return off;
    }

    /**
     * <p>Retrieve the length.</p>
     *
     * @return The length.
     */
    int getLen() {
        return len;
    }

}