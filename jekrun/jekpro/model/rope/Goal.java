package jekpro.model.rope;

import jekpro.frequent.system.ForeignThread;
import jekpro.model.builtin.SpecialBody;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.*;
import jekpro.reference.reflect.SpecialPred;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.term.AbstractSkel;
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
    public final static int MASK_GOAL_CEND = 0x00000020;

    public Object term;
    public Intermediate back;

    /**
     * <p>Create a goal.</p>
     *
     * @param t The term.
     */
    public Goal(Object t) {
        term = t;
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
            throw (EngineMessage) ForeignThread.sysThreadClear();

        /* current term */
        Object alfa = term;
        Display d1 = en.contdisplay.disp;
        /* inlined deref */
        BindUniv b;
        while (alfa instanceof SkelVar &&
                (b = d1.bind[((SkelVar) alfa).id]).display != null) {
            alfa = b.skel;
            d1 = b.display;
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
                    SpecialPred.indicatorToColonSkel(sa, arity, en)));
        }
        AbstractDelegate fun = cp.pick.del;
        if (fun == null) {
            SkelAtom sa = StackElement.callableToName(alfa);
            int arity = StackElement.callableToArity(alfa);
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_BODY,
                    SpecialPred.indicatorToColonSkel(sa, arity, en)));
        }
        en.skel = alfa;
        en.display = d1;
        return fun.moniFirst(en);
    }

    /**************************************************************/
    /* Skel Compilation                                           */
    /**************************************************************/

    /**
     * <p>Convert a body to intermediate form.</p>
     *
     * @param dire The directive.
     * @param b    The body skeleton.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    static void bodyToInterSkel(Directive dire, Object b, Engine en)
            throws EngineMessage {
        while (!SpecialBody.noBody(b)) {
            Object t = SpecialBody.bodyToGoalSkel(b);
            if (SpecialBody.alterType(t) != SpecialBody.TYPE_ALTR_NONE) {
                t = disjToAlterSkel(dire, t, en);
                Goal goal = new Goal(t);
                dire.addInter(goal, Directive.MASK_FIXUP_MOVE);
            } else if (SpecialBody.sequenType(t) != SpecialBody.TYPE_SEQN_NONE) {
                bodyToInterSkel(dire, t, en);
            } else if (SpecialBody.controlType(t) != SpecialBody.TYPE_CTRL_NONE) {
                Goal goal = new Goal(t);
                dire.addInter(goal, Directive.MASK_FIXUP_MOVE);
            } else {
                if ((dire.flags & AbstractDefined.MASK_DEFI_NBCV) == 0 && t instanceof SkelVar)
                    t = new SkelCompound(en.store.foyer.ATOM_CALL, t);
                if (!(t instanceof AbstractSkel))
                    throw new EngineMessage(EngineMessage.typeError(
                            EngineMessage.OP_TYPE_CALLABLE, t), Display.DISPLAY_CONST);
                Goal goal = new Goal(t);
                dire.addInter(goal, Directive.MASK_FIXUP_MOVE);
            }
            b = SpecialBody.bodyToRestSkel(b, en);
        }
    }

    /**
     * <p>Convert a disjunction to an alternative.</p>
     *
     * @param dire The directive.
     * @param t    The disjunction skeleton.
     * @param en   The engine.
     * @return The alternative.
     * @throws EngineMessage Shit happens.
     */
    public static Object disjToAlterSkel(Directive dire,
                                         Object t, Engine en)
            throws EngineMessage {
        SkelCompound back = null;
        L1:
        for (; ; ) {
            switch (SpecialBody.alterType(t)) {
                case SpecialBody.TYPE_ALTR_DISJ:
                    SkelCompound sc = (SkelCompound) t;
                    Object b = sc.args[0];
                    Directive left;
                    switch (SpecialBody.alterType(b)) {
                        case SpecialBody.TYPE_ALTR_COND:
                            left = condToInterSkel(dire, b, en);
                            break;
                        case SpecialBody.TYPE_ALTR_SOFT:
                            left = softCondToInterSkel(dire, b, en);
                            break;
                        default:
                            left = goalToInterSkel(dire, b, en);
                            break;
                    }
                    t = sc.args[1];
                    if (SpecialBody.failType(t) == SpecialBody.TYPE_FAIL_FAIL &&
                            SpecialBody.alterType(b) == SpecialBody.TYPE_ALTR_COND) {
                        t = new SkelCompound(en.store.foyer.ATOM_SYS_GUARD, left);
                        break L1;
                    } else {
                        Object[] args = new Object[2];
                        args[0] = left;
                        args[1] = back;
                        back = new SkelCompound(args, en.store.foyer.ATOM_SYS_ALTER);
                    }
                    break;
                case SpecialBody.TYPE_ALTR_COND:
                    t = condToInterSkel(dire, t, en);
                    t = new SkelCompound(en.store.foyer.ATOM_SYS_GUARD, t);
                    break L1;
                case SpecialBody.TYPE_ALTR_SOFT:
                    t = softCondToInterSkel(dire, t, en);
                    t = new SkelCompound(en.store.foyer.ATOM_SYS_GUARD, t);
                    break L1;
                default:
                    t = goalToInterSkel(dire, t, en);
                    break L1;
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
     * <p>Convert a condition branch to intermediate form.</p>
     *
     * @param dire The directive.
     * @param b    The condition skeleton.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    private static Directive condToInterSkel(Directive dire, Object b,
                                             Engine en)
            throws EngineMessage {
        Directive left = Directive.createDirective(dire.flags, en);
        SkelCompound sc = (SkelCompound) b;
        left.bodyToInterSkel(en.store.foyer.ATOM_SYS_BEGIN, en, false);
        left.bodyToInterSkel(sc.args[0], en, false);
        left.bodyToInterSkel(en.store.foyer.ATOM_SYS_COMMIT, en, false);
        left.bodyToInterSkel(sc.args[1], en, false);
        return left;
    }

    /**
     * <p>Convert a soft condition branch to intermediate form.</p>
     *
     * @param dire The directive.
     * @param b    The condition skeleton.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    private static Directive softCondToInterSkel(Directive dire, Object b,
                                                 Engine en)
            throws EngineMessage {
        Directive left = Directive.createDirective(dire.flags, en);
        SkelCompound sc = (SkelCompound) b;
        left.bodyToInterSkel(en.store.foyer.ATOM_SYS_SOFT_BEGIN, en, false);
        left.bodyToInterSkel(sc.args[0], en, false);
        left.bodyToInterSkel(en.store.foyer.ATOM_SYS_SOFT_COMMIT, en, false);
        left.bodyToInterSkel(sc.args[1], en, false);
        return left;
    }

    /**
     * <p>Convert a goal to intermediate form.</p>
     *
     * @param dire The directive.
     * @param b    The body skeleton.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    private static Directive goalToInterSkel(Directive dire, Object b,
                                             Engine en)
            throws EngineMessage {
        Directive left = Directive.createDirective(dire.flags, en);
        left.bodyToInterSkel(b, en, false);
        return left;
    }

}
