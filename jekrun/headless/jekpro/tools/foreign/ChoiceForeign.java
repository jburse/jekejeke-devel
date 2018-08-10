package jekpro.tools.foreign;

import jekpro.model.inter.AbstractChoice;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.tools.array.Types;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.term.*;

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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
final class ChoiceForeign extends AbstractChoice {
    CallOut co;
    MemberMethodNondet del;
    Object obj;
    Object[] args;
    AbstractBind mark;
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
            en.skel = null;
            en.releaseBind(mark);
            if (en.skel != null)
                throw (EngineException) en.skel;
        }

        Goal ir = (Goal) goalskel;
        Object term = ir.goal;
        Display ref = goaldisplay;
        if ((ir.flags & Goal.MASK_GOAL_NAKE) != 0) {
            /* inlined deref */
            BindVar b1;
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

            Object res = AbstractMember.invokeMethod(del.method, obj, args);
            if ((del.subflags & MASK_METH_FUNC) != 0) {
                res = Types.normJava(del.encoderet, res);
            } else {
                res = del.noretNormJava(res);
            }
            if (res == null)
                return false;
            Display d = AbstractTerm.getDisplay(res);
            if (res != AbstractSkel.VOID_OBJ &&
                    !en.unifyTerm(((SkelCompound) term).args[
                                    ((SkelCompound) term).args.length - 1], ref,
                            AbstractTerm.getSkel(res), d)) {
                if ((co.flags & CallOut.MASK_CALL_RETRY) == 0)
                    return false;

                if ((co.flags & CallOut.MASK_CALL_SPECI) == 0) {
                    en.skel = null;
                    en.releaseBind(mark);
                    if (en.skel != null)
                        throw (EngineException) en.skel;
                }
            } else {
                Object check = AbstractTerm.getMarker(res);
                if (check != null && ((MutableBit) check).getBit()) {
                    d.remTab(en);
                    ((MutableBit) check).setBit(false);
                }
                if ((co.flags & CallOut.MASK_CALL_RETRY) != 0) {
                    /* meta argument change */
                    if ((co.flags & CallOut.MASK_CALL_SPECI) != 0)
                        next = en.choices;
                    /* reuse choice point */
                    en.choices = this;
                    en.number++;
                }
                return en.getNext();
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

        Intermediate r = en.contskel;
        DisplayClause u = en.contdisplay;
        en.contskel = goalskel;
        en.contdisplay = goaldisplay;
        DisplayClause back = (DisplayClause) en.display;
        if (en.skel != null) {
            co.setException(new InterpreterException((EngineException) en.skel));
        } else {
            co.setException(null);
        }
        try {
            co.flags &= ~CallOut.MASK_CALL_FIRST;
            co.flags |= CallOut.MASK_CALL_CLEAN;
            AbstractMember.invokeMethod(del.method, obj, args);
            InterpreterException ie = co.getException();
            en.skel = (ie != null ? (EngineException) ie.getException() : null);
        } catch (EngineException x) {
            InterpreterException ie = co.getException();
            en.skel = (ie != null ? (EngineException) ie.getException() : null);
            if (en.skel != null) {
                en.skel = new EngineException((EngineException) en.skel, x);
            } else {
                en.skel = x;
            }
        } catch (EngineMessage y) {
            InterpreterException ie = co.getException();
            en.skel = (ie != null ? (EngineException) ie.getException() : null);
            EngineException x = new EngineException(y, EngineException.fetchStack(en));
            if (en.skel != null) {
                en.skel = new EngineException((EngineException) en.skel, x);
            } else {
                en.skel = x;
            }
        }

        /* stack and sliding window */
        en.contskel = r;
        en.contdisplay = u;
        en.display = back;
    }

}
