package jekpro.model.molec;

/**
 * <p>This class provides a serial number variable binder.</p>
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
public class BindLexical extends BindVar {
    public int serno = -1;

    /********************************************************/
    /* Lexical Allocation & Reallocation                    */
    /********************************************************/

    /**
     * <p>Create a new display.</p>
     * <p>Fill the binds with bind lexical.</p>
     *
     * @param s The size.
     * @return The new display.
     */
    public static BindUniv[] newLexical(int s) {
        if (s == 0)
            return BIND_CONST;
        BindUniv[] b = new BindUniv[s];
        for (int i = 0; i < s; i++)
            b[i] = new BindLexical();
        return b;
    }

    /**
     * <p>Set the bind size.</p>
     * <p>Refill the binds with bind lexical.</p>
     *
     * @param s The bind size.
     * @param b The display
     * @return The new display.
     */
    public static BindUniv[] resizeLexical(int s, BindUniv[] b) {
        int n = (b != null ? b.length : 0);
        if (n != s) {
            if (s == 0) {
                b = BIND_CONST;
            } else {
                BindUniv[] newbind = new BindUniv[s];
                n = Math.min(n, s);
                if (n != 0)
                    System.arraycopy(b, 0, newbind, 0, n);
                b = newbind;
            }
        }
        for (int i = 0; i < s; i++) {
            if (b[i] == null)
                b[i] = new BindLexical();
        }
        return b;
    }
}