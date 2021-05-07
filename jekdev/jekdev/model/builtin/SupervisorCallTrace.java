package jekdev.model.builtin;

import jekdev.model.pretty.FoyerTrace;
import jekpro.frequent.standard.SupervisorCall;
import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.builtin.SpecialBody;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Goal;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>This class provides basic functions to compile traced terms.</p>
 *
 * @author Copyright 2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.3.8 (a fast and small prolog interpreter)
 */
public class SupervisorCallTrace extends SupervisorCall {

    /**
     * <p>Convert a body to intermediate form.</p>
     *
     * @param dire The directive.
     * @param b    The body skeleton.
     * @param c    The body display.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public final void bodyToInterTrace(Directive dire, Object b,
                                       Display c, Engine en)
            throws EngineMessage {
        if (dire instanceof Clause) {
            Goal at = new Goal(((FoyerTrace) en.store.foyer).ATOM_SYS_AT);
            at.back = dire;
            dire.addInterTrace(at, Directive.MASK_FIXUP_MOVE);
        }
        BindUniv bc;
        while (b instanceof SkelVar &&
                (bc = c.bind[((SkelVar) b).id]).display != null) {
            b = bc.skel;
            c = bc.display;
        }
        while (!SpecialBody.noBody(b)) {
            Object t = SpecialBody.bodyToGoalSkel(b);
            Display d = c;
            while (t instanceof SkelVar &&
                    (bc = d.bind[((SkelVar) t).id]).display != null) {
                t = bc.skel;
                d = bc.display;
            }
            if (SpecialBody.alterType(t) != SpecialBody.TYPE_ALTR_NONE) {
                t = disjToAlter(dire, t, d, en);
                Goal goal = new GoalTrace(t);
                dire.addInter(goal, Directive.MASK_FIXUP_MOVE);
                dire.addInterTrace(goal, Directive.MASK_FIXUP_MOVE);
            } else if (SpecialBody.sequenType(t) != SpecialBody.TYPE_SEQN_NONE) {
                t = conjToSequen(dire, t, d, en);
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
                if ((flags & MASK_CALL_MLTI) != 0 && SupervisorCopy.getVar(t) != null) {
                    SkelVar sv = SkelVar.valueOf(countvar);
                    countvar++;
                    last.bind[sv.id].bindUniv(t, d, en);
                    t = sv;
                }
                Goal goal = new GoalTrace(t);
                dire.addInter(goal, Directive.MASK_FIXUP_MOVE);
                dire.addInterTrace(goal, Directive.MASK_FIXUP_MOVE);
                Goal out = new Goal(((FoyerTrace) en.store.foyer).ATOM_SYS_OUT);
                out.back = goal;
                dire.addInterTrace(out, Directive.MASK_FIXUP_MOVE);
            }
            b = SpecialBody.bodyToRestSkel(b, en);
            while (b instanceof SkelVar &&
                    (bc = c.bind[((SkelVar) b).id]).display != null) {
                b = bc.skel;
                c = bc.display;
            }
        }
    }

}