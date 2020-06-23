package jekpro.frequent.advanced;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.pretty.Foyer;
import jekpro.reference.structure.AbstractLexical;
import jekpro.reference.structure.LexicalCollator;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.*;

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
public final class ForeignRevolve {

    /**
     * <p>Create a new revolve.</p>
     *
     * @return The revolve.
     */
    public static AbstractMap<Object, Object> sysRevolveNew() {
        return new MapTree<Object, Object>(AbstractSkel.DEFAULT);
    }

    /**
     * <p>Create a new revolve.</p>
     *
     * @param el The variant comparator.
     * @return The revolve.
     */
    public static AbstractMap<Object, Object> sysRevolveNew(AbstractLexical el) {
        if (el instanceof LexicalCollator &&
                ((LexicalCollator) el).getCmpStr() == null) {
            return new MapHashLink<Object, Object>();
        } else {
            return new MapTree<Object, Object>(el);
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
                                            AbstractMap<Object, Object> map,
                                            Object val) {
        Engine en = inter.getEngine();
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
    public static Object sysRevolvePair(CallOut co,
                                        AbstractMap<Object, Object> map) {
        MapEntry<Object, Object> at;
        if (co.getFirst()) {
            at = map.getFirstEntry();
            if (at == null)
                return null;
        } else {
            at = (MapEntry<Object, Object>) co.getData();
        }
        MapEntry<Object, Object> next = map.successor(at);
        co.setRetry(next != null);
        co.setData(next);
        Object val = new SkelCompound(new SkelAtom(Foyer.OP_SUB), at.key, at);
        Display ref = AbstractSkel.createMarker(val);
        return AbstractTerm.createMolec(val, ref);
    }

    /**
     * <p>Enumerate the revolve.</p>
     *
     * @param co  The call out.
     * @param map The map.
     * @param el  The variant comparator.
     * @return The pair.
     */
    public static Object sysRevolvePair(CallOut co,
                                        AbstractMap<Object, Object> map,
                                        AbstractLexical el) {
        MapEntry<Object, Object> at;
        if (co.getFirst()) {
            if ((el.getFlags() & AbstractLexical.MASK_FLAG_RVRS) != 0) {
                at = map.getLastEntry();
            } else {
                at = map.getFirstEntry();
            }
            if (at == null)
                return null;
        } else {
            at = (MapEntry<Object, Object>) co.getData();
        }
        MapEntry<Object, Object> next;
        if ((el.getFlags() & AbstractLexical.MASK_FLAG_RVRS) != 0) {
            next = map.predecessor(at);
        } else {
            next = map.successor(at);
        }
        co.setRetry(next != null);
        co.setData(next);
        Object val = new SkelCompound(new SkelAtom(Foyer.OP_SUB), at.key, at);
        Display ref = AbstractSkel.createMarker(val);
        return AbstractTerm.createMolec(val, ref);
    }

}