package jekpro.tools.foreign;

import jekpro.model.inter.AbstractChoice;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.tools.array.Types;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

import static jekpro.tools.array.AbstractLense.MASK_METH_FUNC;

/**
 * <p>The class represents a choice point for a foreign predicate.
 * Foreign predicate objects cannot be directly constructed by the
 * application programmer. They are created by the method defineForeign()
 * in the interpreter or by the foreign/3 builtin.</p>
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
final class ChoiceForeign extends AbstractChoice {
    CallOut co;
    MemberMethodNondet del;
    Object obj;
    Object[] args;
    BindVar mark;
    Intermediate goalskel;
    DisplayClause goaldisplay;

    /**
     * <p>Creata choice foreign.</p>
     *
     * @param u The parent choice.
     */
    ChoiceForeign(AbstractChoice u) {
        super(u);
    }

    /**
     * <p>Logically evaluate a goal in a list of goals for an additional time.</p>
     * <p>The result is returned via the skel and display of the engine.</p>
     * <p>A new exception sliding window is returned via the engine display.</p>
     *
     * @param en The engine.
     * @return True if the foreign predicate succeeded, otherwise false.
     * @throws EngineException FFI error.
     * @throws EngineMessage   FFI error.
     */
    public final boolean moniNext(Engine en)
            throws EngineException, EngineMessage {
        /* remove choice point */
        en.choices = next;
        en.number--;

        en.contskel = goalskel;
        en.contdisplay = goaldisplay;
        if ((co.flags & CallOut.MASK_CALL_SPECI) == 0) {
            en.fault = null;
            en.releaseBind(mark);
            if (en.fault != null)
                throw en.fault;
        }

        Goal ir = (Goal) goalskel;
        Object term = ir.goal;
        Display ref = goaldisplay;
        if ((ir.flags & Goal.MASK_GOAL_NAKE) != 0) {
            /* inlined deref */
            BindUniv b1;
            while (term instanceof SkelVar &&
                    (b1 = ref.bind[((SkelVar) term).id]).display != null) {
                term = b1.skel;
                ref = b1.display;
            }
        }

        co.flags &= ~CallOut.MASK_CALL_FIRST;
        for (; ; ) {
            co.flags &= ~CallOut.MASK_CALL_RETRY;
            co.flags &= ~CallOut.MASK_CALL_SPECI;
            co.flags &= ~CallOut.MASK_CALL_CUTTR;

            Object res = AbstractMember.invokeMethod(del.method, obj, args, en);
            if ((del.subflags & MASK_METH_FUNC) != 0) {
                res = Types.normJava(del.encoderet, res);
            } else {
                res = Types.noretNormJava(del.encoderet, res);
            }
            if (res == null)
                return false;
            Display d = AbstractTerm.getDisplay(res);
            Object[] help;
            boolean ext = d.getAndReset();
            if (res != AbstractSkel.VOID_OBJ &&
                    !en.unifyTerm((help=((SkelCompound) term).args)[
                                    help.length - 1], ref,
                            AbstractTerm.getSkel(res), d)) {
                if ((co.flags & CallOut.MASK_CALL_RETRY) == 0)
                    return false;

                if ((co.flags & CallOut.MASK_CALL_SPECI) == 0) {
                    en.fault = null;
                    en.releaseBind(mark);
                    if (en.fault != null)
                        throw en.fault;
                }
            } else {
                if (ext)
                    BindUniv.remTab(d.bind, en);

                if ((co.flags & CallOut.MASK_CALL_RETRY) != 0) {
                    /* meta argument change */
                    if ((co.flags & CallOut.MASK_CALL_SPECI) != 0)
                        next = en.choices;
                    /* reuse choice point */
                    en.choices = this;
                    en.number++;
                }
                return true;
            }
        }
    }

    /**
     * <p>Free data used to logically evaluate a goal an additional time.</p>
     * <p>The sliding window is passed via the engine display.</p>
     * <p>The new sliding window is passed via the engine display.</p>
     *
     * @param n  The cut level.
     * @param en The engine.
     */
    public final void moniCut(int n, Engine en) {
        /* remove choice point */
        en.choices = next;
        en.number--;

        if ((co.flags & CallOut.MASK_CALL_CUTTR) == 0)
            return;

        /* backup sliding window */
        DisplayClause back = en.window;

        Intermediate r = en.contskel;
        DisplayClause u = en.contdisplay;
        en.contskel = goalskel;
        en.contdisplay = goaldisplay;
        if (en.skel != null) {
            co.setException(new InterpreterException(en.fault));
        } else {
            co.setException(null);
        }
        try {
            co.flags &= ~CallOut.MASK_CALL_FIRST;
            co.flags |= CallOut.MASK_CALL_CLEAN;
            AbstractMember.invokeMethod(del.method, obj, args, en);
            InterpreterException ie = co.getException();
            en.fault = (ie != null ? (EngineException) ie.getException() : null);
        } catch (EngineException x) {
            InterpreterException ie = co.getException();
            en.fault = (ie != null ? (EngineException) ie.getException() : null);
            if (en.fault != null) {
                en.fault = new EngineException(en.fault, x);
            } else {
                en.fault = x;
            }
        } catch (EngineMessage y) {
            InterpreterException ie = co.getException();
            en.fault = (ie != null ? (EngineException) ie.getException() : null);
            EngineException x = new EngineException(y, EngineException.fetchStack(en));
            if (en.fault != null) {
                en.fault = new EngineException(en.fault, x);
            } else {
                en.fault = x;
            }
        }

        /* restore continuation */
        en.contskel = r;
        en.contdisplay = u;

        /* restore sliding window */
        en.window = back;
    }

}
