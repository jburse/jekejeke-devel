package jekpro.frequent.advanced;

import jekpro.frequent.standard.EngineCopy;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.pretty.Foyer;
import jekpro.tools.call.*;
import jekpro.tools.term.*;
import matula.util.data.MapEntry;
import matula.util.data.SetEntry;

/**
 * <p>Provides built-in predicates for the module micro.</p>
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
public final class ForeignMicro {

    /**
     * <p>Create a new micro engine.</p>
     *
     * @param inter The interpreter.
     * @param goal  The goal.
     * @return The micro engine.
     */
    public static CallIn sysMicroNew(Interpreter inter, AbstractTerm goal) {
        Object obj = AbstractTerm.copyMolec(inter, goal);
        return inter.iterable().iterator(obj);
    }

    /**
     * <p>Continue with micro engine.</p>
     *
     * @param callin The micro engine.
     * @return True if the continuation succeeded, otherwise false.
     * @throws InterpreterException Shit happens.
     * @throws InterpreterMessage   No such element.
     */
    public static boolean sysMicroCont(CallIn callin)
            throws InterpreterException, InterpreterMessage {
        if (callin.hasNext()) {
            callin.next();
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Place a copy into the pivot.</p>
     *
     * @param inter The interpreter.
     * @param q     The pivot.
     * @param t     The term.
     */
    public static void sysPivotSet(Interpreter inter,
                                   SetEntry q, AbstractTerm t) {
        Object obj = AbstractTerm.copyMolec(inter, t);
        q.value = obj;
    }

    /**
     * <p>Place a copy into the revolve.</p>
     *
     * @param inter The interpreter.
     * @param r     The revolve.
     * @param k     The key.
     */
    public static SetEntry sysRevolveLookup(Interpreter inter,
                                            Revolve r, AbstractTerm k) {
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
     * <p>Enumertae the revolve.</p>
     *
     * @param co The call out.
     * @param r  The revolve.
     * @return The pair.
     */
    public static Object sysRevolvePair(CallOut co, Revolve r) {
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
        int size = EngineCopy.displaySize(val);
        Display ref = (size != 0 ? new Display(Display.newBind(size)) : Display.DISPLAY_CONST);
        val = AbstractTerm.createMolec(val, ref);
        if (size != 0)
            AbstractTerm.setMarker(val, new ResetableBit());
        return val;
    }

}