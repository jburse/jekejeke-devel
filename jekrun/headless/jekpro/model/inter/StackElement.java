package jekpro.model.inter;

import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.CallFrame;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

/**
 * <p>This class provides a stack element.</p>
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
public class StackElement {
    public Intermediate contskel;
    public CallFrame contdisplay;

    /******************************************************************/
    /* Some Stack Navigation                                          */
    /******************************************************************/

    /**
     * <p>Find visible frame.</p>
     *
     * @param u  The stack element.
     * @param en The engine.
     * @return The new stack element.
     */
    public static StackElement skipNoTrace(StackElement u, Engine en)
            throws EngineException, EngineMessage {
        while (u != null && isNoTrace(u.contskel, u.contdisplay, en))
            u = u.contdisplay;
        return u;
    }

    /**
     * <p>Check whether the term is no trace.</p>
     *
     * @param r  The continuation skeleton.
     * @param u  The continuation display.
     * @param en The engine.
     * @return True if the term is no trace, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    private static boolean isNoTrace(Intermediate r, CallFrame u,
                                     Engine en)
            throws EngineException, EngineMessage {
        if (u == null)
            return true;

        callGoal(r, u, en);
        SkelAtom sa = callableToName(en.skel);
        if (sa != null && sa.scope != null &&
                (sa.scope.getBits() & AbstractSource.MASK_SRC_NOTR) != 0)
            return true;
        CachePredicate cp = callableToPredicate(en.skel, en);
        if (cp != null && (cp.flags & CachePredicate.MASK_PRED_VISI) != 0 &&
                (cp.pick.getBits() & Predicate.MASK_PRED_NOTR) != 0)
            return true;

        return false;
    }

    /**
     * <p>Determine the term of a frame.</p>
     * <p>The result is return in skel and display.</p>
     *
     * @param r  The frame skeleton.
     * @param u  The continuation display.
     * @param en The engine.
     */
    public static void callGoal(Intermediate r, CallFrame u, Engine en) {
        if (r instanceof Goal) {
            en.skel = ((Goal) r).term;
            en.display = u.disp;
            if ((r.flags & Goal.MASK_GOAL_NAKE) != 0)
                en.deref();
        } else if (r instanceof Clause) {
            callGoal(u.contskel, u.contdisplay, en);
            Clause clause = (Clause) r;
            SkelAtom sa = StackElement.callableToName(clause.term);
            en.skel = StackElement.callableFromName(en.skel, sa);
        } else {
            en.skel = null;
            en.display = null;
        }
    }

    /******************************************************************/
    /* Frame Access & Modification                                    */
    /******************************************************************/

    /**
     * <p>Retrieve the principal name.</p>
     *
     * @param t The term skeleton.
     * @return The name, or null.
     */
    public static SkelAtom callableToName(Object t) {
        if (t instanceof SkelCompound) {
            return ((SkelCompound) t).sym;
        } else if (t instanceof SkelAtom) {
            return (SkelAtom) t;
        } else {
            return null;
        }
    }

    /**
     * <p>Replace the name of a callable.</p>
     *
     * @param t2 The callable.
     * @param sa The new name.
     * @return The new callable.
     */
    public static Object callableFromName(Object t2, SkelAtom sa) {
        if (t2 instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) t2;
            return new SkelCompound(sa, sc.args, sc.var);
        } else if (t2 instanceof SkelAtom) {
            return sa;
        } else {
            throw new IllegalArgumentException("not a callable");
        }
    }

    /******************************************************************/
    /* Some Callable Utility                                          */
    /******************************************************************/

    /**
     * <p>Lookup the predicate of this term.</p>
     * <p>This conversion must be preceded by dereferencing.</p>
     * <p>Will cache the found predicate.</p>
     *
     * @param t  The term skeleton.
     * @param en The engine.
     * @return The predicate, or null.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static CachePredicate callableToPredicate(Object t,
                                                     Engine en)
            throws EngineMessage, EngineException {
        if (t instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) t;
            return CachePredicate.getPredicate(sc.sym, sc.args.length, en);
        } else if (t instanceof SkelAtom) {
            SkelAtom sa = (SkelAtom) t;
            return CachePredicate.getPredicate(sa, 0, en);
        } else {
            return null;
        }
    }

    /**
     * <p>Retrieve the principal length.</p>
     *
     * @param t The term skeleton.
     * @return The length, or -1.
     */
    public static int callableToArity(Object t) {
        if (t instanceof SkelCompound) {
            return ((SkelCompound) t).args.length;
        } else if (t instanceof SkelAtom) {
            return 0;
        } else {
            return -1;
        }
    }

    /**
     * <p>Retrieve a store key.</p>
     *
     * @param t The term skeleton.
     * @return The store key, or null.
     */
    public static StoreKey callableToStoreKey(Object t) {
        if (t instanceof SkelCompound) {
            SkelCompound mc = (SkelCompound) t;
            return new StoreKey(mc.sym.fun, mc.args.length);
        } else if (t instanceof SkelAtom) {
            return new StoreKey(((SkelAtom) t).fun, 0);
        } else {
            return null;
        }
    }

}