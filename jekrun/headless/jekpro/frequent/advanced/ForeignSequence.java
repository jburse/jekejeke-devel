package jekpro.frequent.advanced;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.tools.call.Interpreter;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;
import matula.util.data.SetEntry;

/**
 * <p>Provides built-in predicates for the module sequence.</p>
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
public final class ForeignSequence {

    /**
     * <p>Set the pivot.</p>
     *
     * @param inter The interpreter.
     * @param q     The pivot.
     * @param t     The term.
     */
    public static void sysPivotSet(Interpreter inter,
                                   SetEntry q, AbstractTerm t) {
        Object m = AbstractTerm.getSkel(t);
        Display d = AbstractTerm.getDisplay(t);
        Engine en = (Engine) inter.getEngine();
        q.value = AbstractSkel.copySkel(m, d, en);
    }

    /**
     * <p>Get and copy the pivot.</p>
     *
     * @param q The pivot.
     * @return The copy or null.
     */
    public static Object sysPivotGet(SetEntry q) {
        Object val = q.value;
        return (val != null ? AbstractSkel.newMolec(val) : null);
    }

}