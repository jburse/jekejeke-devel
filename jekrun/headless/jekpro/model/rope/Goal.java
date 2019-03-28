package jekpro.model.rope;

import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.*;
import jekpro.model.pretty.Foyer;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.wire.AbstractLivestock;

/**
 * <p>The class provides the term intermediate code.</p>
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
public class Goal extends Intermediate {
    public static final String OP_DISJUNCTION = ";";
    public static final String OP_CONDITION = "->";
    public static final String OP_SOFT_CONDITION = "*->";
    public static final String OP_ALTERNATIVE = "sys_alter";

    public final static int MASK_GOAL_NAKE = 0x00000100;
    public final static int MASK_GOAL_CEND = 0x00000200;

    /**
     * <p>Create a goal.</p>
     */
    public Goal() {
    }

    /**
     * <p>Create a goal.</p>
     *
     * @param t The term.
     */
    public Goal(Object t) {
        term = t;
        int f3 = 0;
        if (t instanceof SkelVar)
            f3 |= Goal.MASK_GOAL_NAKE;
        flags = f3;
    }

    /**
     * <p>Resolve the current term.</p>
     *
     * @param en The engine.
     * @return True if success, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public final boolean resolveNext(Engine en)
            throws EngineException, EngineMessage {
        if (en.visor.signal != null &&
                (en.visor.flags & AbstractLivestock.MASK_LIVESTOCK_NOSG) == 0)
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        /* current term */
        CallFrame u = en.contdisplay;
        Object alfa = term;
        Display d1 = u.disp;
        if ((flags & Goal.MASK_GOAL_NAKE) != 0) {
            /* inlined deref */
            BindUniv b;
            while (alfa instanceof SkelVar &&
                    (b = d1.bind[((SkelVar) alfa).id]).display != null) {
                alfa = b.skel;
                d1 = b.display;
            }
        }
        CachePredicate cp;
        if (alfa instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) alfa;
            cp = CachePredicate.getPredicate(sc.sym, sc.args.length, en);
        } else if (alfa instanceof SkelAtom) {
            SkelAtom sa = (SkelAtom) alfa;
            cp = CachePredicate.getPredicate(sa, 0, en);
        } else {
            EngineMessage.checkInstantiated(alfa);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_CALLABLE, alfa));
        }
        if (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0) {
            SkelAtom sa = StackElement.callableToName(alfa);
            int arity = StackElement.callableToArity(alfa);
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_PROCEDURE,
                    SpecialQuali.indicatorToColonSkel(sa, arity, en)));
        }
        AbstractDelegate fun = cp.pick.del;
        if (fun == null) {
            SkelAtom sa = StackElement.callableToName(alfa);
            int arity = StackElement.callableToArity(alfa);
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_BODY,
                    SpecialQuali.indicatorToColonSkel(sa, arity, en)));
        }
        en.skel = alfa;
        en.display = d1;
        return fun.moniFirst(en);
    }

    /**************************************************************/
    /* Convert to Intermediate Form                               */
    /**************************************************************/

    /**
     * <p>Convert a body to intermediate form.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param dire The directive.
     * @param body The term list, or null.
     * @param en   The engine.
     */
    public static void bodyToInter(Directive dire, Object body, Engine en) {
        Goal back = null;
        while (body != null) {
            Object term = bodyToGoal(body);
            body = bodyToRest(body);
            if (term != null) {
                if (body == null && isDisjunction(term))
                    term = disjunctionToAlternative(dire, term, en);
                Goal goal = new Goal(term);
                if (back == null) {
                    dire.next = goal;
                } else {
                    back.next = goal;
                }
                back = goal;
            }
        }

        if (back == null) {
            dire.next = Success.DEFAULT;
        } else {
            back.next = Success.DEFAULT;
            if ((dire.flags & Directive.MASK_DIRE_STOP) == 0)
                back.flags |= Goal.MASK_GOAL_CEND;
        }
    }

    /**
     * <p>Convert a disjunction to an alternative.</p>
     *
     * @param dire The directive.
     * @param term The disjunction.
     * @param en   The engine.
     * @return The alternative.
     */
    public static Object disjunctionToAlternative(Directive dire,
                                                  Object term, Engine en) {
        SkelCompound sc = (SkelCompound) term;
        Directive left = makeDirective(dire, en);
        left.bodyToInter(sc.args[0], en);
        Directive right = makeDirective(dire, en);
        right.bodyToInter(sc.args[1], en);
        SkelAtom sa = SpecialQuali.makeAtom(OP_ALTERNATIVE, en, sc.sym);
        return new SkelCompound(sa, left, right);
    }

    /**************************************************************/
    /* Conversion Utilities                                       */
    /**************************************************************/

    /**
     * <p>Convert a body to a goal.</p>
     *
     * @param term The body.
     * @return The goal.
     */
    public static Object bodyToGoal(Object term) {
        if (term instanceof SkelCompound &&
                ((SkelCompound) term).args.length == 2 &&
                ((SkelCompound) term).sym.fun.equals(Foyer.OP_COMMA)) {
            SkelCompound sc = (SkelCompound) term;
            return sc.args[0];
        } else if (term instanceof SkelAtom &&
                ((SkelAtom) term).fun.equals(Foyer.OP_TRUE)) {
            return null;
        } else {
            return term;
        }
    }

    /**
     * <p>Convert a body to a rest.</p>
     *
     * @param term The body.
     * @return The rest.
     */
    public static Object bodyToRest(Object term) {
        if (term instanceof SkelCompound &&
                ((SkelCompound) term).args.length == 2 &&
                ((SkelCompound) term).sym.fun.equals(Foyer.OP_COMMA)) {
            SkelCompound sc = (SkelCompound) term;
            return sc.args[1];
        } else if (term instanceof SkelAtom &&
                ((SkelAtom) term).fun.equals(Foyer.OP_TRUE)) {
            return null;
        } else {
            return null;
        }
    }

    /**
     * <p>Check whether the given term is an alternative.</p>
     *
     * @param term The term.
     * @return True if the term is an alterantive.
     */
    public static boolean isDisjunction(Object term) {
        if (term instanceof SkelCompound &&
                ((SkelCompound) term).args.length == 2 &&
                ((SkelCompound) term).sym.fun.equals(OP_DISJUNCTION)) {
            SkelCompound sc = (SkelCompound) term;
            term = sc.args[0];
            if (term instanceof SkelCompound &&
                    ((SkelCompound) term).args.length == 2 &&
                    ((SkelCompound) term).sym.fun.equals(OP_CONDITION)) {
                return false;
            } else if (term instanceof SkelCompound &&
                    ((SkelCompound) term).args.length == 2 &&
                    ((SkelCompound) term).sym.fun.equals(OP_SOFT_CONDITION)) {
                return false;
            } else if (term instanceof SkelVar) {
                return false;
            } else {
                return true;
            }
        } else {
            return false;
        }
    }

    /**
     * <p>Create a new directive with same flags.</p>
     *
     * @param dire The directive.
     * @param en   The engine.
     * @return The new directive.
     */
    public static Directive makeDirective(Directive dire, Engine en) {
        int copt = 0;
        if ((dire.flags & Directive.MASK_DIRE_NLST) != 0)
            copt |= AbstractDefined.MASK_DEFI_NLST;
        if ((dire.flags & Directive.MASK_DIRE_STOP) != 0)
            copt |= AbstractDefined.MASK_DEFI_STOP;
        if ((dire.flags & Directive.MASK_DIRE_NBDY) != 0)
            copt |= AbstractDefined.MASK_DEFI_NBDY;
        if ((dire.flags & Directive.MASK_DIRE_NOBR) != 0)
            copt |= AbstractDelegate.MASK_DELE_NOBR;
        if ((dire.flags & Directive.MASK_DIRE_NIST) != 0)
            copt |= AbstractDefined.MASK_DEFI_NIST;
        return Directive.createDirective(copt, en);
    }

    /**************************************************************/
    /* Convert from Intermediate Form                             */
    /**************************************************************/

    /**
     * <p>Convert the intermediate form into a term.</p>
     *
     * @param en The store.
     * @return The vector.
     */
    public static Object interToBody(Intermediate temp,
                                     Engine en) {
        Foyer foyer = en.store.foyer;
        SkelCompound back = null;
        Object t;
        for (; ; ) {
            if (temp == Success.DEFAULT) {
                t = foyer.ATOM_TRUE;
                break;
            } else if (temp.next == Success.DEFAULT) {
                t = temp.term;
                if (isAlternative(t))
                    t = alternativeToDisjunction(t, en);
                break;
            } else {
                Object[] args = new Object[2];
                args[0] = temp.term;
                args[1] = back;
                back = new SkelCompound(foyer.ATOM_COMMA, args, null);
                temp = temp.next;
            }
        }
        while (back != null) {
            SkelCompound jack = (SkelCompound) back.args[back.args.length - 1];
            back.args[back.args.length - 1] = t;
            back.var = SkelCompound.makeExtra(back.args);
            t = back;
            back = jack;
        }
        return t;
    }

    /**
     * <p>Convert an alternative to a disjunction.</p>
     *
     * @param t  The alternative.
     * @param en The engine.
     * @return The disjunction.
     */
    public static Object alternativeToDisjunction(Object t, Engine en) {
        SkelCompound sc = (SkelCompound) t;
        Object left = interToBody(((Directive) sc.args[0]).next, en);
        Object right = interToBody(((Directive) sc.args[1]).next, en);
        SkelAtom sa = SpecialQuali.makeAtom(OP_DISJUNCTION, en, sc.sym);
        return new SkelCompound(sa, left, right);
    }

    /**************************************************************/
    /* Conversion Utilities                                       */
    /**************************************************************/

    /**
     * <p>Check whether the given term is an alternative.</p>
     *
     * @param term The term.
     * @return True if the term is an alterantive.
     */
    public static boolean isAlternative(Object term) {
        if (term instanceof SkelCompound &&
                ((SkelCompound) term).args.length == 2 &&
                ((SkelCompound) term).sym.fun.equals(OP_ALTERNATIVE)) {
            return true;
        } else {
            return false;
        }
    }

}
