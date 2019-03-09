package jekpro.model.molec;

import jekpro.model.inter.Engine;
import jekpro.model.inter.InterfaceStack;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Intermediate;

/**
 * <p>The class provides a clause display.</p>
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
public final class DisplayClause extends Display implements InterfaceStack {
    public final static int MASK_DPCL_MORE = 0x00000010;
    public final static int MASK_DPCL_SOFT = 0x00000020;

    public Intermediate contskel;
    public DisplayClause contdisplay;
    public int lastalloc;
    public int lastgc;
    public int number;
    public DisplayClause prune;
    public Clause def;

    /**
     * <p>Create a display clause.</p>
     */
    public DisplayClause() {
    }

    /**
     * <p>Retrieve the cont skel.</p>
     *
     * @return The cont skel.
     */
    public Intermediate getContSkel() {
        return contskel;
    }

    /**
     * <p>Retrieve the cont display.</p>
     *
     * @return The cont display.
     */
    public DisplayClause getContDisplay() {
        return contdisplay;
    }

    /********************************************************/
    /* Allocation & Reallocation                            */
    /********************************************************/

    /**
     * <p>Create a new display.</p>
     * <p>Don't fill the binds with bind lexical.</p>
     *
     * @param s The size.
     * @return The new display.
     */
    public static BindUniv[] newClause(int s) {
        return (s != 0 ? new BindUniv[s] : BindUniv.BIND_CONST);
    }

    /**
     * <p>Set the bind size.</p>
     * <p>Don't refill the binds with bind lexical.</p>
     *
     * @param s The bind size.
     * @param b The display
     * @return The new display.
     */
    public static BindUniv[] resizeClause(int s, BindUniv[] b) {
        int n = (b != null ? b.length : 0);
        if (n == s)
            return b;
        if (s == 0) {
            b = BindUniv.BIND_CONST;
        } else {
            BindUniv[] newbind = new BindUniv[s];
            n = Math.min(n, s);
            if (n != 0)
                System.arraycopy(b, 0, newbind, 0, n);
            b = newbind;
        }
        return b;
    }

    /********************************************************/
    /* Goal Preparation                                     */
    /********************************************************/

    /**
     * <p>Add a variable value to the prepared call.</p>
     *
     * @param m  The value skeleton.
     * @param d  The value display.
     * @param en The engine.
     */
    public final void addArgument(Object m, Display d, Engine en) {
        BindUniv b = new BindUniv();
        bind[lastalloc] = b;
        bind[lastalloc].bindVar(m, d, en);
        lastalloc++;
    }

    /**
     * <p>Prepare the call.</p>
     *
     * @param en The engine.
     */
    public final void setEngine(Engine en) {
        number = en.number;
        prune = this;
        contskel = en.contskel;
        contdisplay = en.contdisplay;
    }

}