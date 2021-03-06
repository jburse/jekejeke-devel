package jekpro.frequent.advanced;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.reference.structure.AbstractLexical;
import jekpro.reference.structure.LexicalCollator;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;
import matula.util.data.*;

import java.util.Enumeration;

/**
 * <p>Provides built-in predicates for the module pivot.</p>
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
public final class ForeignPivot {

    /**
     * <p>Set a pivot value.</p>
     *
     * @param inter The interpreter.
     * @param pivot The pivot.
     * @param val   The value.
     */
    public static void sysPivotSet(Interpreter inter, SetEntry pivot, Object val) {
        Engine en = inter.getEngine();
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
     * <p>List the pivot.</p>
     *
     * @param co    The call out.
     * @param pivot The pivot.
     * @return The value.
     */
    public static Object sysPivotList(CallOut co,
                                      SetEntry<ListArray<Object>> pivot) {
        Enumeration<Object> at;
        if (co.getFirst()) {
            ListArray<Object> help = pivot.value;
            if (help == null)
                return null;
            at = help.elements();
            co.setData(at);
        } else {
            at = (Enumeration<Object>) co.getData();
        }
        Object val = at.nextElement();
        co.setRetry(at.hasMoreElements());
        Display ref = AbstractSkel.createMarker(val);
        return AbstractTerm.createMolec(val, ref);
    }

    /**
     * <p>Put a pivot value.</p>
     *
     * @param inter The interpreter.
     * @param pivot The pivot.
     * @param el    The variant comparator.
     * @param val   The value.
     */
    public static boolean sysPivotPut(Interpreter inter,
                                      SetEntry<AbstractSet<Object>> pivot,
                                      AbstractLexical el,
                                      Object val) {
        Engine en = inter.getEngine();
        Display ref = AbstractTerm.getDisplay(val);
        val = AbstractTerm.getSkel(val);
        AbstractSet<Object> help = pivot.value;
        if (help == null) {
            if (el instanceof LexicalCollator &&
                    ((LexicalCollator) el).getCmpStr() == null) {
                help = new SetHashLink<>();
            } else {
                help = new SetTree<>(el);
            }
            pivot.value = help;
        }
        val = AbstractSkel.copySkel(val, ref, en);
        if (help.getEntry(val) == null) {
            help.add(val);
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Enumerate the pivot.</p>
     *
     * @param co    The call out.
     * @param pivot The pivot.
     * @return The value.
     */
    public static Object sysPivotEnum(CallOut co,
                                      SetEntry<AbstractSet<Object>> pivot) {
        AbstractSet<Object> help = pivot.value;
        SetEntry<Object> at;
        if (co.getFirst()) {
            if (help == null)
                return null;
            at = help.getFirstEntry();
        } else {
            at = (SetEntry<Object>) co.getData();
        }
        SetEntry<Object> next = help.successor(at);
        co.setRetry(next != null);
        co.setData(next);
        Object val = at.value;
        Display ref = AbstractSkel.createMarker(val);
        return AbstractTerm.createMolec(val, ref);
    }

    /**
     * <p>Enumerate the pivot.</p>
     *
     * @param co    The call out.
     * @param pivot The pivot.
     * @param el    The variant comparator.
     * @return The value.
     */
    public static Object sysPivotEnum(CallOut co,
                                      SetEntry<AbstractSet<Object>> pivot,
                                      AbstractLexical el) {
        AbstractSet<Object> help = pivot.value;
        SetEntry<Object> at;
        if (co.getFirst()) {
            if (help == null)
                return null;
            if ((el.getFlags() & AbstractLexical.MASK_FLAG_RVRS) != 0) {
                at = help.getLastEntry();
            } else {
                at = help.getFirstEntry();
            }
        } else {
            at = (SetEntry<Object>) co.getData();
        }
        SetEntry<Object> next;
        if ((el.getFlags() & AbstractLexical.MASK_FLAG_RVRS) != 0) {
            next = help.successor(at);
        } else {
            next = help.predecessor(at);
        }
        co.setRetry(next != null);
        co.setData(next);
        Object val = at.value;
        Display ref = AbstractSkel.createMarker(val);
        return AbstractTerm.createMolec(val, ref);
    }

}