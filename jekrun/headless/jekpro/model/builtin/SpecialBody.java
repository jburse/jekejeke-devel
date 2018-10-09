package jekpro.model.builtin;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Frame;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.model.rope.Operator;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.reflect.SpecialOper;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.PositionKey;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.MapEntry;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
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
    private final static int SPECIAL_SYS_CURRENT_PROVABLE = 6;
    private final static int SPECIAL_SYS_PROVABLE_PROPERTY_CHK = 7;
    private final static int SPECIAL_SYS_PROVABLE_PROPERTY_IDX = 8;
    private final static int SPECIAL_SYS_CURRENT_SYNTAX = 9;
    private final static int SPECIAL_SYS_SYNTAX_PROPERTY_CHK = 10;
    private final static int SPECIAL_SYS_SYNTAX_PROPERTY_IDX = 11;

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
                EngineMessage.checkCallable(en.skel, en.display);
                Object t = en.skel;
                Display d = en.display;

                String key = SpecialUniv.derefAndCastString(temp[1], ref);
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
                EngineMessage.checkCallable(en.skel, en.display);
                t = en.skel;
                d = en.display;

                en.skel = temp[1];
                en.display = ref;
                en.deref();
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
            case SPECIAL_SYS_CURRENT_PROVABLE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                if (!en.unifyTerm(temp[0], ref,
                        SpecialBody.currentProvables(en), Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_PROVABLE_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                Predicate pick = indicatorToProvable(temp[0], ref, en);
                if (pick == null)
                    return false;
                StoreKey prop = StoreKey.propToStoreKey(temp[1], ref, en);
                boolean multi = SpecialPred.predicateToProperty(pick, prop, en);
                d = en.display;
                if (!en.unifyTerm(temp[2], ref, en.skel, d))
                    return false;
                if (multi)
                    d.remTab(en);
                return en.getNext();
            case SPECIAL_SYS_PROVABLE_PROPERTY_IDX:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                if (!en.unifyTerm(temp[1], ref,
                        propertyToProvables(en.skel, en.display, en),
                        Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_CURRENT_SYNTAX:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                if (!en.unifyTerm(temp[0], ref,
                        SpecialBody.currentSyntax(en), Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_SYNTAX_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                Operator oper = operToSyntax(temp[0], ref, en);
                if (oper == null)
                    return false;
                prop = StoreKey.propToStoreKey(temp[1], ref, en);
                multi = SpecialOper.operToProperty(prop, oper, en);
                d = en.display;
                if (!en.unifyTerm(temp[2], ref, en.skel, d))
                    return false;
                if (multi)
                    d.remTab(en);
                return en.getNext();
            case SPECIAL_SYS_SYNTAX_PROPERTY_IDX:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                if (!en.unifyTerm(temp[1], ref,
                        propertyToSyntax(en.skel, en.display, en),
                        Display.DISPLAY_CONST))
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
            return new SkelCompound(sa, sc.args, sc.var);
        } else if (t2 instanceof SkelAtom) {
            return sa;
        } else {
            throw new IllegalArgumentException("not a callable");
        }
    }

    /**************************************************************/
    /* Provable Lookup                                            */
    /**************************************************************/

    /**
     * <p>Get predicate by indicator.</p>
     *
     * @param t  The skel of the compound.
     * @param d  The display of the compound.
     * @param en The engine.
     * @return The predicate.
     * @throws EngineMessage Shit happens.
     */
    public static Predicate indicatorToProvable(Object t, Display d, Engine en)
            throws EngineMessage {
        Integer arity = SpecialQuali.colonToIndicator(t, d, en);
        SkelAtom sa = (SkelAtom) en.skel;
        AbstractSource base;
        if (!CacheFunctor.isQuali(sa.fun)) {
            return CachePredicate.getRoutineUser(arity.intValue(), sa.fun, en.store);
        } else {
            String s = CacheFunctor.sepModule(sa.fun);
            base = AbstractSource.getModule(s, en.store);
            if (base == null)
                return null;
            return base.getRoutine(arity.intValue(), sa.fun);
        }
    }

    /**************************************************************/
    /* Provable Enumeration I                                     */
    /**************************************************************/

    /**
     * <p>Create a prolog list with the directly accessible predicates.</p>
     *
     * @param en The engine.
     * @return The prolog list of the directly accessible predicates.
     * @throws EngineMessage Shit happens.
     */
    private static Object currentProvables(Engine en)
            throws EngineMessage {
        Store store = en.store;
        Object res = en.store.foyer.ATOM_NIL;
        while (store != null) {
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int j = sources.length - 1; j >= 0; j--) {
                AbstractSource base = sources[j].value;
                Predicate[] preds = base.snapshotRoutine();
                res = consProvables(preds, res, en);
            }
            store = store.parent;
        }
        return res;
    }

    /**
     * <p>Collect predicate indicators.</p>
     *
     * @param preds The predicates.
     * @param res   The old predicate indicators.
     * @param en    The engine.
     * @return The new predicate indicators.
     * @throws EngineMessage Shit happens.
     */
    public static Object consProvables(Predicate[] preds, Object res,
                                       Engine en)
            throws EngineMessage {
        for (int i = preds.length - 1; i >= 0; i--) {
            Predicate pick = preds[i];
            Object val = SpecialQuali.indicatorToColonSkel(
                    pick.getFun(), pick.getSource().getStore().user,
                    pick.getArity(), en);
            res = new SkelCompound(en.store.foyer.ATOM_CONS, val, res);
        }
        return res;
    }

    /**************************************************************/
    /* Provable Enumeration II                                    */
    /**************************************************************/

    /**
     * <p>Retrieve the predicates to a property.</p>
     *
     * @param t  The value skeleton.
     * @param d  The value display.
     * @param en The engine.
     */
    private static Object propertyToProvables(Object t, Display d,
                                              Engine en)
            throws EngineMessage {
        StoreKey prop = Frame.callableToStoreKey(t);
        Predicate[] vals = SpecialPred.idxPropPred(t, d, prop, en);
        Object res = en.store.foyer.ATOM_NIL;
        res = SpecialBody.consProvables(vals, res, en);
        return res;
    }

    /**************************************************************/
    /* Syntax Lookup                                              */
    /**************************************************************/

    /**
     * <p>Lookup an operator from a compound.</p>
     *
     * @param t  The compound skeleton.
     * @param d  The compound display.
     * @param en The engine copy.
     * @return The operator or null.
     * @throws EngineMessage Shit happends.
     */
    public static Operator operToSyntax(Object t, Display d, Engine en)
            throws EngineMessage {
        int type = SpecialOper.colonToOper(t, d, en);
        String fun = ((SkelAtom) en.skel).fun;
        if (!CacheFunctor.isQuali(fun)) {
            return OperatorSearch.getOperUser(type, fun, en.store);
        } else {
            String s = CacheFunctor.sepModule(fun);
            AbstractSource base = AbstractSource.getModule(s, en.store);
            if (base == null)
                return null;
            return base.getOper(type, fun);
        }
    }

    /**************************************************************/
    /* Syntax Enumeration I                                       */
    /**************************************************************/

    /**
     * <p>Create a prolog list with the directly accessible syntax operators.</p>
     *
     * @param en The engine.
     * @return The prolog list of the directly accessible syntax operators.
     * @throws EngineMessage Shit happens.
     */
    private static Object currentSyntax(Engine en)
            throws EngineMessage {
        Store store = en.store;
        Object res = en.store.foyer.ATOM_NIL;
        while (store != null) {
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int j = sources.length - 1; j >= 0; j--) {
                AbstractSource base = sources[j].value;
                Operator[] opers = base.snapshotOper();
                res = consSyntax(opers, res, en);
            }
            store = store.parent;
        }
        return res;
    }

    /**
     * <p>Collect and filter operator indicators.</p>
     *
     * @param opers The operators.
     * @param res   The old predicate indicators.
     * @param en    The engine.
     * @return The new predicate indicators.
     * @throws EngineMessage Shit happens.
     */
    public static Object consSyntax(Operator[] opers, Object res,
                                    Engine en)
            throws EngineMessage {
        for (int i = opers.length - 1; i >= 0; i--) {
            Operator oper = opers[i];
            Object val = SpecialOper.operToColonSkel(oper.getType(), oper.getKey(),
                    oper.getSource().getStore().user, en);
            res = new SkelCompound(en.store.foyer.ATOM_CONS, val, res);
        }
        return res;
    }

    /**************************************************************/
    /* Syntax Enumeration II                                      */
    /**************************************************************/

    /**
     * <p>Retrieve the operators for a property.</p>
     *
     * @param t  The value skeleton.
     * @param d  The value display.
     * @param en The engine.
     */
    private static Object propertyToSyntax(Object t, Display d,
                                           Engine en)
            throws EngineMessage {
        StoreKey prop = Frame.callableToStoreKey(t);
        Operator[] vals = SpecialOper.idxPropOper(t, d, prop, en);
        Object res = en.store.foyer.ATOM_NIL;
        res = consSyntax(vals, res, en);
        return res;
    }

}
