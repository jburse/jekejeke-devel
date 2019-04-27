package jekpro.tools.bundle;

/**
 * <p>This class represents a discovered capability.</p>
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
public final class AirDropEntry {
    private final String capa;
    private final boolean dontask;

    /**
     * <p>Create a path settings entry.</p>
     *
     * @param c The capability name.
     * @param d The don't ask flag.
     */
    public AirDropEntry(String c, boolean d) {
        capa = c;
        dontask = d;
    }

    /**
     * <p>Retrieve the capability name.</p>
     *
     * @return The capability name.
     */
    public String getCapa() {
        return capa;
    }

    /**
     * <p>Retrieve the don't ask flag.</p>
     *
     * @return The don't ask flag.
     */
    public boolean getDontAsk() {
        return dontask;
    }

    /***************************************************************/
    /* Object Protocol                                             */
    /***************************************************************/

    /**
     * <p>Returns a string representation of this air drop entry.</p>
     *
     * @return A string representation of this air drop entry.
     */
    public String toString() {
        StringBuilder buf = new StringBuilder();
        buf.append("capa=");
        buf.append(capa);
        buf.append(";dontask=");
        buf.append(dontask);
        return buf.toString();
    }

}
