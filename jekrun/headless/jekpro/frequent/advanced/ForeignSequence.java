package jekpro.frequent.advanced;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.reference.structure.AbstractLexical;
import jekpro.reference.structure.LexicalCollator;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.*;

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
     * <p>Create a new variant comparator.</p>
     *
     * @param opt The sort options.
     * @return The variant comparator.
     * @throws InterpreterMessage Type Error.
     */
    public static LexicalCollator sysVariantComparator(Interpreter inter,
                                                       Object opt)
            throws InterpreterMessage {
        Engine engine = (Engine) inter.getEngine();
        LexicalCollator el;
        try {
            el = (LexicalCollator) AbstractLexical.decodeSortOpts(AbstractTerm.getSkel(opt),
                    AbstractTerm.getDisplay(opt), engine);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
        return el;
    }

    /**
     * <p>Create a new revolve.</p>
     *
     * @return The revolve.
     */
    public static AbstractMap sysRevolveNew() {
        return new MapTree(AbstractSkel.DEFAULT);
    }

    /**
     * <p>Create a new revolve.</p>
     *
     * @param el The variant comparator.
     * @return The revolve.
     */
    public static AbstractMap sysRevolveNew(AbstractLexical el) {
        if (el instanceof LexicalCollator && ((LexicalCollator) el).getCmpStr() == null) {
            return new MapHashLink();
        } else {
            return new MapTree(el);
        }
    }

    /**
     * <p>Place a copy into the revolve.</p>
     *
     * @param inter The interpreter.
     * @param map   The map.
     * @param val   The key.
     */
    public static SetEntry sysRevolveLookup(Interpreter inter,
                                            AbstractMap map, Object val) {
        Engine en = (Engine) inter.getEngine();
        Display d = AbstractTerm.getDisplay(val);
        val = AbstractTerm.getSkel(val);
        val = AbstractSkel.copySkel(val, d, en);
        MapEntry h = map.getEntry(val);
        if (h == null) {
            h = map.newEntry(val, null);
            map.putEntry(h);
        }
        return h;
    }

    /**
     * <p>Enumerate the revolve.</p>
     *
     * @param co  The call out.
     * @param map The revolve.
     * @return The pair.
     */
    public static Object sysRevolvePair(CallOut co, AbstractMap map) {
        MapEntry at;
        if (co.getFirst()) {
            at = map.getFirstEntry();
            if (at == null)
                return null;
        } else {
            at = (MapEntry) co.getData();
        }
        MapEntry next = map.successor(at);
        co.setRetry(next != null);
        co.setData(next);
        Object val = new SkelCompound(new SkelAtom(Foyer.OP_SUB), at.key, at);
        Display ref = AbstractSkel.createMarker(val);
        return AbstractTerm.createMolec(val, ref);
    }

}