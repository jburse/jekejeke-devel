package jekpro.frequent.standard;

import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Goal;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>This class provides basic functions to compile terms.</p>
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
public class SupervisorCall {
    public final static int MASK_CALL_MLTI = 0x00000001;

    public int countvar;
    public Display last;
    public int flags;

    /**
     * <p>Prepare a term for execution.</p>
     *
     * @param copt The directive options.
     * @param en   The engine.
     * @return The new directive.
     * @throws EngineMessage Shit happens.
     */
    public static Directive callGoal(int copt, Engine en)
            throws EngineMessage {
        Object t = en.skel;
        Display d = en.display;
        EngineMessage.checkInstantiated(t);
        SupervisorCall ew = en.visor.getCall();
        ew.countvar = 0;
        ew.flags = 0;
        ew.last = Display.DISPLAY_CONST;
        ew.countBody(t, d, en);
        Directive dire;
        if ((ew.flags & SupervisorCall.MASK_CALL_MLTI) != 0) {
            ew.last = new Display(ew.countvar);
            dire = Directive.createDirective(copt, en);
        } else {
            dire = Directive.createDirective(copt | Directive.MASK_DIRE_PUSH, en);
        }
        ew.countvar = 0;
        dire.bodyToInter(t, d, en, true);
        en.display = ew.last;
        ew.last = Display.DISPLAY_CONST;
        return dire;
    }

    /**
     * <p>Prepare a term for execution.</p>
     *
     * @param copt The directive options.
     * @param en   The engine.
     * @return The new directive.
     * @throws EngineMessage Shit happens.
     */
    public static Directive callGoal2(int copt, Engine en)
            throws EngineMessage {
        Object t = en.skel;
        Display d = en.display;
        boolean ext = d.getAndReset();
        EngineMessage.checkInstantiated(t);
        SupervisorCall ew = en.visor.getCall();
        ew.countvar = 0;
        if (ext) {
            ew.flags = SupervisorCall.MASK_CALL_MLTI;
        } else {
            ew.flags = 0;
        }
        ew.last = Display.DISPLAY_CONST;
        ew.countBody(t, d, en);
        Directive dire;
        if ((ew.flags & SupervisorCall.MASK_CALL_MLTI) != 0) {
            ew.last = new Display(ew.countvar);
            dire = Directive.createDirective(copt, en);
        } else {
            dire = Directive.createDirective(copt | Directive.MASK_DIRE_PUSH, en);
        }
        ew.countvar = 0;
        dire.bodyToInter(t, d, en, true);
        if (ext)
            d.remTab(en);
        en.display = ew.last;
        ew.last = Display.DISPLAY_CONST;
        return dire;
    }

    /**************************************************************/
    /* Term Compilation Count                                     */
    /**************************************************************/

    /**
     * <p>Count the variables of a body.</p>
     *
     * @param b  The body skeleton.
     * @param c  The body display.
     * @param en The engine.
     */
    private void countBody(Object b, Display c, Engine en) {
        BindUniv bc;
        while (b instanceof SkelVar &&
                (bc = c.bind[((SkelVar) b).id]).display != null) {
            b = bc.skel;
            c = bc.display;
        }
        for (; ; ) {
            if (!Goal.noBody(b)) {
                Object t = Goal.bodyToGoalSkel(b);
                Display d = c;
                while (t instanceof SkelVar &&
                        (bc = d.bind[((SkelVar) t).id]).display != null) {
                    t = bc.skel;
                    d = bc.display;
                }
                if (Goal.alterType(t) != Goal.TYPE_ALTR_NONE) {
                    countDisj(t, d, en);
                } else if (Goal.sequenType(t) != Goal.TYPE_SEQN_NONE) {
                    countBody(t, d, en);
                } else {
                    if (SupervisorCopy.getVar(t) != null) {
                        countvar++;
                        if (last == Display.DISPLAY_CONST) {
                            last = d;
                        } else if (last != d) {
                            flags |= MASK_CALL_MLTI;
                        }
                    }
                }
            } else {
                break;
            }
            b = Goal.bodyToRestSkel(b, en);
            while (b instanceof SkelVar &&
                    (bc = c.bind[((SkelVar) b).id]).display != null) {
                b = bc.skel;
                c = bc.display;
            }
        }
    }

    /**
     * <p>Count the variables of a disjunction.</p>
     *
     * @param t  The disjunction skeleton.
     * @param d  The disjunction display.
     * @param en The engine.
     */
    private void countDisj(Object t, Display d, Engine en) {
        L1:
        for (; ; ) {
            switch (Goal.alterType(t)) {
                case Goal.TYPE_ALTR_DISJ:
                    SkelCompound sc = (SkelCompound) t;
                    Object b = sc.args[0];
                    Display c = d;
                    BindUniv bc;
                    while (b instanceof SkelVar &&
                            (bc = c.bind[((SkelVar) b).id]).display != null) {
                        b = bc.skel;
                        c = bc.display;
                    }
                    switch (Goal.alterType(b)) {
                        case Goal.TYPE_ALTR_COND:
                        case Goal.TYPE_ALTR_SOFT:
                            countCond(b, c, en);
                            break;
                        default:
                            countBody(b, c, en);
                            break;
                    }
                    t = sc.args[1];
                    while (t instanceof SkelVar &&
                            (bc = d.bind[((SkelVar) t).id]).display != null) {
                        t = bc.skel;
                        d = bc.display;
                    }
                    break;
                case Goal.TYPE_ALTR_COND:
                case Goal.TYPE_ALTR_SOFT:
                    countCond(t, d, en);
                    break L1;
                default:
                    countBody(t, d, en);
                    break L1;
            }
        }
    }

    /**
     * <p>Count the variables of a condition.</p>
     *
     * @param b  The condition skeleton.
     * @param c  The condition display.
     * @param en The engine.
     */
    private void countCond(Object b, Display c, Engine en) {
        SkelCompound sc = (SkelCompound) b;
        countBody(sc.args[0], c, en);
        countBody(sc.args[1], c, en);
    }

    /**************************************************************/
    /* Term Compilation Build                                     */
    /**************************************************************/

    /**
     * <p>Convert a body to intermediate form.</p>
     *
     * @param dire The directive.
     * @param b    The body skeleton.
     * @param c    The body display.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public final void bodyToInter(Directive dire, Object b,
                                  Display c, Engine en)
            throws EngineMessage {
        BindUniv bc;
        while (b instanceof SkelVar &&
                (bc = c.bind[((SkelVar) b).id]).display != null) {
            b = bc.skel;
            c = bc.display;
        }
        for (; ; ) {
            if (!Goal.noBody(b)) {
                Object t = Goal.bodyToGoalSkel(b);
                Display d = c;
                while (t instanceof SkelVar &&
                        (bc = d.bind[((SkelVar) t).id]).display != null) {
                    t = bc.skel;
                    d = bc.display;
                }
                if (Goal.alterType(t) != Goal.TYPE_ALTR_NONE) {
                    t = disjToAlter(dire, t, d, en);
                    Goal goal = new Goal(t);
                    dire.addInter(goal, Directive.MASK_FIXUP_MOVE);
                } else if (Goal.sequenType(t) != Goal.TYPE_SEQN_NONE) {
                    t = conjToSequen(dire, t, d, en);
                    Goal goal = new Goal(t);
                    dire.addInter(goal, Directive.MASK_FIXUP_MOVE);
                } else if (Directive.controlType(t) != Directive.TYPE_CTRL_NONE) {
                    Goal goal = new Goal(t);
                    dire.addInter(goal, Directive.MASK_FIXUP_MOVE);
                } else {
                    if ((dire.flags & AbstractDefined.MASK_DEFI_NBCV) == 0 && t instanceof SkelVar)
                        t = new SkelCompound(en.store.foyer.ATOM_CALL, t);
                    if (!(t instanceof AbstractSkel))
                        throw new EngineMessage(EngineMessage.typeError(
                                EngineMessage.OP_TYPE_CALLABLE, t), Display.DISPLAY_CONST);
                    if ((flags & MASK_CALL_MLTI) != 0 && SupervisorCopy.getVar(t) != null) {
                        SkelVar sv = SkelVar.valueOf(countvar);
                        countvar++;
                        last.bind[sv.id].bindUniv(t, d, en);
                        t = sv;
                    }
                    Goal goal = new Goal(t);
                    dire.addInter(goal, Directive.MASK_FIXUP_MOVE);
                }
            } else {
                break;
            }
            b = Goal.bodyToRestSkel(b, en);
            while (b instanceof SkelVar &&
                    (bc = c.bind[((SkelVar) b).id]).display != null) {
                b = bc.skel;
                c = bc.display;
            }
        }
    }

    /**
     * <p>Convert a disjunction to an alternative.</p>
     *
     * @param dire The directive.
     * @param t    The disjunction skeleton.
     * @param d    The disjunction display.
     * @param en   The engine.
     * @return The alternative.
     * @throws EngineMessage Shit happens.
     */
    public final Object disjToAlter(Directive dire,
                                    Object t, Display d, Engine en)
            throws EngineMessage {
        SkelCompound back = null;
        L1:
        for (; ; ) {
            switch (Goal.alterType(t)) {
                case Goal.TYPE_ALTR_DISJ:
                    SkelCompound sc = (SkelCompound) t;
                    Object b = sc.args[0];
                    Display c = d;
                    BindUniv bc;
                    while (b instanceof SkelVar &&
                            (bc = c.bind[((SkelVar) b).id]).display != null) {
                        b = bc.skel;
                        c = bc.display;
                    }
                    Directive left;
                    switch (Goal.alterType(b)) {
                        case Goal.TYPE_ALTR_COND:
                            left = condToInter(dire, b, c, en);
                            break;
                        case Goal.TYPE_ALTR_SOFT:
                            left = softCondToInter(dire, b, c, en);
                            break;
                        default:
                            left = goalToInter(dire, b, c, en);
                            break;
                    }
                    Object[] args = new Object[2];
                    args[0] = left;
                    args[1] = back;
                    back = new SkelCompound(args, en.store.foyer.ATOM_SYS_ALTER);
                    t = sc.args[1];
                    while (t instanceof SkelVar &&
                            (bc = d.bind[((SkelVar) t).id]).display != null) {
                        t = bc.skel;
                        d = bc.display;
                    }
                    break;
                case Goal.TYPE_ALTR_COND:
                    t = condToInter(dire, t, d, en);
                    t = new SkelCompound(en.store.foyer.ATOM_SYS_GUARD, t);
                    break L1;
                case Goal.TYPE_ALTR_SOFT:
                    t = softCondToInter(dire, t, d, en);
                    t = new SkelCompound(en.store.foyer.ATOM_SYS_GUARD, t);
                    break L1;
                default:
                    t = goalToInter(dire, t, d, en);
                    break L1;
            }
        }
        while (back != null) {
            SkelCompound jack = (SkelCompound) back.args[back.args.length - 1];
            back.args[back.args.length - 1] = t;
            back.makeExtra();
            t = back;
            back = jack;
        }
        return t;
    }

    /**
     * <p>Convert a conjunction to a sequent.</p>
     *
     * @param dire The directive.
     * @param t    The conjunction skeleton.
     * @param d    The conjunction display.
     * @param en   The engine.
     * @return The alternative.
     * @throws EngineMessage Shit happens.
     */
    public final Object conjToSequen(Directive dire,
                                     Object t, Display d, Engine en)
            throws EngineMessage {
        t = goalToInter(dire, t, d, en);
        t = new SkelCompound(en.store.foyer.ATOM_SYS_SEQUEN, t);
        return t;
    }

    /**
     * <p>Convert a condition branch to intermediate form.</p>
     *
     * @param dire The directive.
     * @param b    The condition skeleton.
     * @param c    The condition display.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    private Directive condToInter(Directive dire, Object b, Display c,
                                  Engine en)
            throws EngineMessage {
        Directive left = Directive.createDirective(dire.flags, en);
        SkelCompound sc = (SkelCompound) b;
        left.bodyToInter(en.store.foyer.ATOM_SYS_BEGIN, Display.DISPLAY_CONST, en, false);
        left.bodyToInter(sc.args[0], c, en, false);
        left.bodyToInter(en.store.foyer.ATOM_SYS_COMMIT, Display.DISPLAY_CONST, en, false);
        left.bodyToInter(sc.args[1], c, en, false);
        return left;
    }

    /**
     * <p>Convert a soft condition branch to intermediate form.</p>
     *
     * @param dire The directive.
     * @param b    The condition skeleton.
     * @param c    The condition display.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    private Directive softCondToInter(Directive dire, Object b, Display c,
                                      Engine en)
            throws EngineMessage {
        Directive left = Directive.createDirective(dire.flags, en);
        SkelCompound sc = (SkelCompound) b;
        left.bodyToInter(en.store.foyer.ATOM_SYS_SOFT_BEGIN, Display.DISPLAY_CONST, en, false);
        left.bodyToInter(sc.args[0], c, en, false);
        left.bodyToInter(en.store.foyer.ATOM_SYS_SOFT_COMMIT, Display.DISPLAY_CONST, en, false);
        left.bodyToInter(sc.args[1], c, en, false);
        return left;
    }

    /**
     * <p>Convert a goal to intermediate form.</p>
     *
     * @param dire The directive.
     * @param b    The body skeleton.
     * @param c    The body display.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    private Directive goalToInter(Directive dire, Object b, Display c,
                                  Engine en)
            throws EngineMessage {
        Directive left = Directive.createDirective(dire.flags, en);
        left.bodyToInter(b, c, en, false);
        return left;
    }

}