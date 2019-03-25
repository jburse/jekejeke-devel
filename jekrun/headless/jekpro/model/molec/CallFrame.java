package jekpro.model.molec;

import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.rope.Clause;

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
public final class CallFrame extends StackElement {
    public final static int MASK_DPCL_MORE = 0x00000010;
    public final static int MASK_DPCL_SOFT = 0x00000020;
    public final static int MASK_DPCL_LTGC = 0x00000040;
    public final static int MASK_DPCL_NOBR = 0x00000040;

    public final Display disp;
    public int number;

    /**
     * <p>Create a new call frame.</p>
     *
     * @param d The display.
     */
    public CallFrame(Display d) {
        disp = d;
    }

    /**
     * <p>Create a new call frame.</p>
     *
     * @param size The requested size.
     */
    public CallFrame(int size) {
        this(new Display(size));
    }

    /**
     * <p>Set the clause data.</p>
     *
     * @param clause The clause.
     */
    public void setClause(Clause clause) {
        Display d = disp;
        d.vars = clause.vars;
        if ((clause.flags & Clause.MASK_CLAUSE_NOBR) != 0)
            d.flags |= MASK_DPCL_NOBR;
        if ((clause.flags & Clause.MASK_CLAUSE_NBDY) != 0)
            d.flags |= MASK_DPCL_LTGC;
    }

    /**
     * <p>Set an argument.</p>
     *
     * @param k  The argument index.
     * @param m  The value skeleton.
     * @param d  The value display.
     * @param en The engine.
     */
    public void setArg(int k, Object m, Display d, Engine en) {
        disp.bind[k].bindUniv(m, d, en);
    }

    /**
     * <p>Set the engine data.</p>
     *
     * @param en The engine.
     */
    public final void setEngine(Engine en) {
        contskel = en.contskel;
        contdisplay = en.contdisplay;
        number = en.number;
    }

}