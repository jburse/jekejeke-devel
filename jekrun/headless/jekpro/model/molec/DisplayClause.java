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
    public final static int MASK_DPCL_LTGC = 0x00000040;
    public final static int MASK_DPCL_NOBR = 0x00000040;

    public Intermediate contskel;
    public DisplayClause contdisplay;
    public int number;
    public Clause def;

    /**
     * <p>Create a new display.</p>
     *
     * @param size The requested size.
     */
    public DisplayClause(int size) {
        super(size);
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

    /**
     * <p>Prepare the call.</p>
     *
     * @param en The engine.
     */
    public final void setEngine(Engine en) {
        DisplayClause u = en.contdisplay;
        number = en.number;
        if ((def.flags & Clause.MASK_CLAUSE_NOBR) != 0)
            u.flags |= MASK_DPCL_NOBR;
        contskel = en.contskel;
        contdisplay = u;
    }

}