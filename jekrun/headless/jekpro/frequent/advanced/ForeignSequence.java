package jekpro.frequent.advanced;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.tools.call.Interpreter;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;
import matula.util.data.AbstractSet;
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
     * <p>Set a pivot value.</p>
     *
     * @param inter The interpreter.
     * @param pivot The pivot.
     * @param val   The value.
     */
    public static void sysPivotSet(Interpreter inter, SetEntry pivot, Object val) {
        Engine en = (Engine) inter.getEngine();
        Display ref = AbstractTerm.getDisplay(val);
        val = AbstractTerm.getSkel(val);
        pivot.value = AbstractSkel.copySkel(val, ref, en);
    }

    /**
     * <p>Retreve a pivot value.</p>
     *
     * @param pivot The pivot.
     * @return The value.
     */
    public static Object sysPivotGet(SetEntry pivot) {
        Object val = pivot.value;
        if (val == null)
            return null;
        Display ref = AbstractSkel.createMarker(val);
        return AbstractTerm.createMolec(val, ref);
    }

    /**
     * <p>Place a copy into the drawer.</p>
     *
     * @param inter The interpreter.
     * @param set   The set.
     * @param val   The key.
     * @return True if the key is fresh, otherwise false.
     */
    public static boolean sysDrawerLookup(Interpreter inter,
                                          AbstractSet set, Object val) {
        Engine en = (Engine) inter.getEngine();
        Display d = AbstractTerm.getDisplay(val);
        val = AbstractTerm.getSkel(val);
        val = AbstractSkel.copySkel(val, d, en);
        SetEntry h = set.getEntry(val);
        if (h == null) {
            h = set.newEntry(val);
            set.putEntry(h);
            return true;
        } else {
            return false;
        }
    }

}