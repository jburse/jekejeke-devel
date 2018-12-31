package jekpro.model.inter;

import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
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
    public Display contdisplay;

    /**
     * <p>Create a new stack element.</p>
     */
    public StackElement() {
    }

    /**
     * <p>Create a new stack element.</p>
     *
     * @param r The continuation skeleton.
     * @param u The continuation display.
     */
    public StackElement(Intermediate r, Display u) {
        contskel = r;
        contdisplay = u;
    }

    /******************************************************************/
    /* Some Stack Navigation                                          */
    /******************************************************************/

    /**
     * <p>Find visible frame.</p>
     * <p>Result returned in skeleton and display of engine.</p>
     *
     * @param u  The continuation display.
     * @param en The engine.
     */
    public static Display skipNoTrace(Display u,
                                      Engine en)
            throws EngineException, EngineMessage {
        while (u != null && isNoTrace(u.contskel, u.contdisplay, en))
            u = u.contdisplay;
        return u;
    }

    /**
     * <p>Check whether the goal is no trace.</p>
     *
     * @param r  The continuation skeleton.
     * @param u  The continuation display.
     * @param en The engine.
     * @return True if the goal is no trace, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public static boolean isNoTrace(Intermediate r, Display u,
                                    Engine en)
            throws EngineException, EngineMessage {
        if (u == null)
            return true;

        callGoal(r, u, en);
        SkelAtom sa = callableToName(en.skel);
        if (sa != null && sa.scope != null &&
                (sa.scope.getBits() & AbstractSource.MASK_SRC_NOTR) != 0)
            return true;
        CachePredicate cp = Frame.callableToPredicate(en.skel, en);
        if (cp != null && (cp.flags & CachePredicate.MASK_PRED_VISI) != 0 &&
                (cp.pick.getBits() & Predicate.MASK_PRED_NOTR) != 0)
            return true;

        return false;
    }

    /*************************************************************/
    /* Frame Access & Modification                               */
    /*************************************************************/

    /**
     * <p>Determine the goal of a frame.</p>
     * <p>The result is return in skel and display.</p>
     *
     * @param r  The frame skeleton.
     * @param u  The continuation display.
     * @param en The engine.
     */
    public static void callGoal(Intermediate r, Display u, Engine en) {
        if (r instanceof Goal) {
            Goal cont = (Goal) r;
            en.skel = cont.goal;
            en.display = u.bind;
            if ((cont.flags & Goal.MASK_GOAL_NAKE) != 0)
                en.deref();
        } else if (r instanceof Clause) {
            callGoal(u.contskel, u.contdisplay, en);
            Clause clause = (Clause) r;
            SkelAtom sa = callableToName(clause.head);
            en.skel = callableFromName(en.skel, sa);
        } else {
            en.skel = null;
            en.display = null;
        }
    }

    /**
     * <p>Retrieve the principal name.</p>
     *
     * @param t The goal skeleton.
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

}