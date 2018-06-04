package jekpro.model.builtin;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.DisplayClause;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.model.rope.Operator;
import jekpro.tools.term.PositionKey;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides built-in predicates for body access.</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SpecialBody extends AbstractSpecial {
    private final static int SPECIAL_SYS_NEUTRAL_PREDICATE = 0;
    private final static int SPECIAL_SYS_NEUTRAL_OPER = 1;
    private final static int SPECIAL_SYS_SET_CONTEXT_PROPERTY = 2;
    private final static int SPECIAL_SYS_REPLACE_SITE = 4;
    private final static int SPECIAL_SYS_PARENT_GOAL = 5;
//    private final static int SPECIAL_WRAP_GOAL = 4;

    /**
     * <p>Create a predicate special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialBody(int i) {
        super(i);
    }



    /**
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineException, EngineMessage {
        switch (id) {
            case SPECIAL_SYS_NEUTRAL_PREDICATE:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Predicate.indicatorToPredicateDefined(temp[0], ref, en, true);
                return en.getNextRaw();
            case SPECIAL_SYS_NEUTRAL_OPER:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                Operator.operToOperatorDefined(temp[0], ref, en, true);
                return en.getNextRaw();
            case SPECIAL_SYS_SET_CONTEXT_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[2];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkCallable(en.skel, en.display);
                Object t = en.skel;
                Display d = en.display;

                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                String key = EngineMessage.castString(en.skel, en.display);
                AbstractSource scope;
                if (!"".equals(key)) {
                    scope = en.store.getSource(key);
                    AbstractSource.checkExistentSource(scope, key);
                } else {
                    scope = null;
                }

                t = SpecialBody.replaceContext(t, scope);
                if (!en.unifyTerm(temp[0], ref, t, d))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_REPLACE_SITE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[2];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkCallable(en.skel, en.display);
                t = en.skel;
                d = en.display;

                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkCallable(en.skel, en.display);
                SkelAtom sa = callableToName(en.skel);

                t = SpecialBody.replaceSite(t, sa, en);
                if (!en.unifyTerm(temp[0], ref, t, d))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_PARENT_GOAL:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                DisplayClause u = en.contdisplay;
                if (u.goaldisplay == null)
                    return false;
                callGoal(u.goalskel, u.goaldisplay, en);
                if (!en.unifyTerm(temp[0], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            /*
            case SPECIAL_WRAP_GOAL:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                en.wrapGoalSite();
                if (en.unifyTerm(en.skel,en.display,temp[1],ref,r,u)) {
                    en.skel = r.getNext(en);
                    en.display = u;
                    return true;
                }
                return false;
            */
            default:
                throw new IllegalArgumentException(
                        AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /*************************************************************/
    /* Replace Context & Site                                    */
    /*************************************************************/

    /**
     * <p>Set the context of a callable to another context.</p>
     *
     * @param t     The skeleton of the callable.
     * @param scope The context, or null.
     * @return The new callable.
     */
    private static Object replaceContext(Object t, AbstractSource scope) {
        SkelAtom sa = callableToName(t);

        SkelAtom sa3 = new SkelAtom(sa.fun, scope);

        return callableFromName(t, sa3);
    }

    /**
     * <p>Set the site of a callable to another site.</p>
     *
     * @param t   The skeleton of the callable.
     * @param sa2 The call-site, not null.
     * @param en  The engine.
     * @return The new callable.
     */
    private static Object replaceSite(Object t, SkelAtom sa2, Engine en) {
        AbstractSource scope = (sa2 != null ? sa2.scope : null);
        PositionKey pos = (sa2 != null ? sa2.getPosition() : null);

        SkelAtom sa = callableToName(t);

        int m = (pos != null ? SkelAtom.MASK_ATOM_POSI : 0);
        SkelAtom sa3 = en.store.foyer.createAtom(sa.fun, scope, m);
        sa3.setPosition(pos);

        return callableFromName(t, sa3);
    }

    /**
     * <p>Determine the goal of a frame.</p>
     * <p>The result is return in skel and display.</p>
     *
     * @param r  The frame skeleton.
     * @param u  The continuation display.
     * @param en The engine.
     */
    public static void callGoal(Intermediate r, DisplayClause u, Engine en) {
        if (r instanceof Goal) {
            Goal cont = (Goal) r;
            en.skel = cont.goal;
            en.display = u;
            if ((cont.flags & Goal.MASK_GOAL_NAKE) != 0)
                en.deref();
        } else if (r instanceof Clause) {
            callGoal(u.goalskel, u.goaldisplay, en);
            Clause clause = (Clause) r;
            SkelAtom sa = callableToName(clause.head);
            en.skel = replaceSite(en.skel, sa, en);
        } else {
            en.skel = null;
            en.display = null;
        }
    }

    /*************************************************************/
    /* Frame Access & Modification                               */
    /*************************************************************/

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
            return new SkelCompound(sa, sc.args, sc.vars);
        } else if (t2 instanceof SkelAtom) {
            return sa;
        } else {
            throw new IllegalArgumentException("not a callable");
        }
    }

}
