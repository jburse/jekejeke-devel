package jekdev.model.builtin;

import jekdev.model.pretty.FoyerTrace;
import jekdev.reference.system.SpecialMode;
import jekpro.model.builtin.SpecialBody;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>Refinement of intermediate for goal traces.</p>
 *
 * @author Copyright 2011-2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.2 (a fast and small prolog interpreter)
 */
public final class GoalTrace extends Goal {
    public Intermediate nexttrace;

    /**
     * <p>Create a traced goal.</p>
     *
     * @param t The term.
     */
    public GoalTrace(Object t) {
        super(t);
    }

    /******************************************************************/
    /* Variation Points                                               */
    /******************************************************************/

    /**
     * <p>Retrieve the next goal depending on debug mode.</p>
     *
     * @param en The engine.
     * @return The next goal.
     */
    public final Intermediate getNextRaw(Engine en) {
        if (SpecialMode.isDebug(en)) {
            return nexttrace;
        } else {
            return next;
        }
    }

    /**************************************************************/
    /* Skel Compilation                                           */
    /**************************************************************/

    /**
     * <p>Convert a body to intermediate form.</p>
     *
     * @param dire The intermediate.
     * @param b    The body skeleton.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    static void bodyToInterSkelTrace(Directive dire, Object b, Engine en)
            throws EngineMessage {
        if (dire instanceof Clause) {
            Goal at = new Goal(((FoyerTrace) en.store.foyer).ATOM_SYS_AT);
            at.back = dire;
            dire.addInterTrace(at, Directive.MASK_FIXUP_MOVE);
        }
        while (!SpecialBody.noBody(b)) {
            Object t = SpecialBody.bodyToGoalSkel(b);
            if (SpecialBody.alterType(t) != SpecialBody.TYPE_ALTR_NONE) {
                t = disjToAlterSkel(dire, t, en);
                Goal goal = new GoalTrace(t);
                dire.addInter(goal, Directive.MASK_FIXUP_MOVE);
                dire.addInterTrace(goal, Directive.MASK_FIXUP_MOVE);
            } else if (SpecialBody.sequenType(t) != SpecialBody.TYPE_SEQN_NONE) {
                t = conjToSequenSkel(dire, t, en);
                Goal goal = new GoalTrace(t);
                dire.addInter(goal, Directive.MASK_FIXUP_MOVE);
                dire.addInterTrace(goal, Directive.MASK_FIXUP_MOVE);
            } else if (SpecialBody.controlType(t) != SpecialBody.TYPE_CTRL_NONE) {
                Goal goal = new GoalTrace(t);
                dire.addInter(goal, Directive.MASK_FIXUP_MOVE);
                dire.addInterTrace(goal, Directive.MASK_FIXUP_MOVE);
            } else {
                Goal in = new Goal(((FoyerTrace) en.store.foyer).ATOM_SYS_IN);
                dire.addInterTrace(in, Directive.MASK_FIXUP_MOVE);
                if ((dire.flags & AbstractDefined.MASK_DEFI_NBCV) == 0 && t instanceof SkelVar)
                    t = new SkelCompound(en.store.foyer.ATOM_CALL, t);
                if (!(t instanceof AbstractSkel))
                    throw new EngineMessage(EngineMessage.typeError(
                            EngineMessage.OP_TYPE_CALLABLE, t), Display.DISPLAY_CONST);
                Goal goal = new GoalTrace(t);
                dire.addInter(goal, Directive.MASK_FIXUP_MOVE);
                dire.addInterTrace(goal, Directive.MASK_FIXUP_MOVE);
                Goal out = new Goal(((FoyerTrace) en.store.foyer).ATOM_SYS_OUT);
                out.back = goal;
                dire.addInterTrace(out, Directive.MASK_FIXUP_MOVE);
            }
            b = SpecialBody.bodyToRestSkel(b, en);
        }
    }

}
