package jekpro.model.pretty;

import jekpro.tools.term.SkelAtom;

/**
 * <p>This class provides an annotated atom.</p>
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
public class SkelAtomAnno extends SkelAtom {
    private int hint;
    private String[][] fillers;

    /**
     * <p>Internal constructor for anno atoms.</p>
     *
     * @param f The name.
     * @param s The scope.
     */
    public SkelAtomAnno(String f, AbstractSource s) {
        super(f, s);
    }

    /**
     * <p>Retrieve the hint.</p>
     *
     * @return The hint.
     */
    public int getHint() {
        return hint;
    }

    /**
     * <p>Set the hint.</p>
     *
     * @param h The hint.
     */
    public void setHint(int h) {
        hint = h;
    }

    /**
     * <p>Retrieve the fillers of the atom.</p>
     *
     * @return The fillers.
     */
    public String[][] getFillers() {
        return fillers;
    }

    /**
     * <p>Set the fillers.</p>
     *
     * @param f The fillers.
     */
    public void setFillers(String[][] f) {
        fillers = f;
    }

}
