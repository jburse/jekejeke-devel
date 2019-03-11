package jekpro.frequent.advanced;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.pretty.Foyer;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.AbstractMap;
import matula.util.data.MapEntry;
import matula.util.data.SetEntry;

import java.util.Comparator;

/**
 * <p>Provides built-in predicates for the module aggregate.</p>
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
public final class ForeignAggregate {
    public final static Comparator<Object> DEFAULT = new Comparator<Object>() {
        public int compare(Object o1, Object o2) {
            return AbstractSkel.compareSkel(o1, o2);
        }
    };

    /**
     * <p>Place a copy into the revolve.</p>
     *
     * @param inter The interpreter.
     * @param r     The map.
     * @param k     The key.
     */
    public static SetEntry sysRevolveLookup(Interpreter inter,
                                            AbstractMap r, AbstractTerm k) {
        Object m = AbstractTerm.getSkel(k);
        Display d = AbstractTerm.getDisplay(k);
        Engine en = (Engine) inter.getEngine();
        Object val = AbstractSkel.copySkel(m, d, en);
        MapEntry h = r.getEntry(val);
        if (h == null) {
            h = r.newEntry(val, null);
            r.putEntry(h);
        }
        return h;
    }

    /**
     * <p>Enumerate the revolve.</p>
     *
     * @param co The call out.
     * @param r  The map.
     * @return The pair.
     */
    public static Object sysRevolvePair(CallOut co, AbstractMap r) {
        MapEntry at;
        if (co.getFirst()) {
            at = r.getFirstEntry();
        } else {
            at = (MapEntry) co.getData();
        }
        if (at == null)
            return null;
        MapEntry next = r.successor(at);
        co.setRetry(next != null);
        co.setData(next);
        Object val = new SkelCompound(new SkelAtom(Foyer.OP_SUB), at.key, at);
        Display ref = AbstractSkel.createMarker(val);
        return AbstractTerm.createMolec(val, ref);
    }

}