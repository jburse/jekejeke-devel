package jekdev.model.builtin;

import jekdev.reference.system.SpecialMode;
import jekpro.frequent.standard.SupervisorCall;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.*;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Refinement of the clause class. The class has the following
 * additional fields:</p>
 * <ul>
 * <li><b>nexttrace:</b> The alternate body that should be used during debug.</li>
 * </ul>
 *
 * @author Copyright 2010-2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.1 (a fast and small prolog interpreter)
 */
public final class ClauseTrace extends Clause {
    public Intermediate nexttrace;
    public Intermediate lasttrace;

    /**
     * <p>Create a clause trace.</p>
     *
     * @param copt The clause option flags.
     */
    public ClauseTrace(int copt) {
        super(copt);
    }

    /**
     * <p>Retrieve the next goal depending on debug mode.</p>
     *
     * @param en The engine.
     */
    public final Intermediate getNextRaw(Engine en) {
        if (SpecialMode.isDebug(en)) {
            return nexttrace;
        } else {
            return next;
        }
    }

    /**
     * <p>Convert a body to intermediate form.</p>
     *
     * @param b     The body skeleton.
     * @param en    The engine.
     * @param close The close flag.
     * @throws EngineMessage Shit happens.
     */
    public final void bodyToInterSkel(Object b, Engine en, boolean close)
            throws EngineMessage {
        GoalTrace.bodyToInterSkelTrace(this, b, en);
        if (close) {
            addInter(Success.DEFAULT, MASK_FIXUP_MARK);
            addInterTrace(Success.DEFAULT, 0);
        }
    }

    /******************************************************/
    /* Builder Utilities                                  */
    /******************************************************/

    /**
     * <p>Add a goal to the directive.</p>
     *
     * @param inter The intermediate.
     * @param mask  The flag.
     */
    public void addInterTrace(Intermediate inter, int mask) {
        if (lasttrace == null) {
            nexttrace = inter;
        } else {
            Object term = ((Goal) lasttrace).term;
            if (Directive.isAlter(term) || Directive.isGuard(term)) {
                while (Directive.isAlter(term)) {
                    SkelCompound sc = (SkelCompound) term;
                    ((Directive) sc.args[0]).addInterTrace(inter, 0);
                    term = sc.args[1];
                }
                if (Directive.isGuard(term)) {
                    SkelCompound sc = (SkelCompound) term;
                    ((Directive) sc.args[0]).addInterTrace(inter, 0);
                } else {
                    ((Directive) term).addInterTrace(inter, 0);
                }
            } else if (Directive.isSequen(term)) {
                SkelCompound sc = (SkelCompound) term;
                ((Directive) sc.args[0]).addInterTrace(inter, 0);
            }
            if (lasttrace instanceof GoalTrace) {
                ((GoalTrace) lasttrace).nexttrace = inter;
            } else {
                lasttrace.next = inter;
            }
        }
        if ((mask & MASK_FIXUP_MOVE) != 0)
            lasttrace = inter;
    }

}