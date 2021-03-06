package jekpro.tools.foreign;

import jekpro.model.inter.AbstractChoice;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.tools.array.AbstractLense;
import jekpro.tools.array.Types;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>The class represents a choice point for a foreign predicate.</p>
 * <p>Non-static Java method is called via invokevirtual.</p>
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
final class ChoiceVirtual extends AbstractChoice {
    CallOut co;
    MemberVirtualNondet del;
    Object obj;
    Object[] args;
    AbstractUndo mark;
    Intermediate goalskel;

    /**
     * <p>Creata choice foreign.</p>
     *
     * @param n The parent choice.
     */
    ChoiceVirtual(AbstractChoice n, CallFrame u) {
        super(n, u);
    }

    /**
     * <p>Logically evaluate a term in a list of goals for an additional time.</p>
     * <p>The result is returned via the contskel and contdisplay of the engine.</p>
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
        if ((co.flags & CallOut.MASK_CALL_SPCI) == 0) {
            en.fault = null;
            en.releaseBind(mark);
            if (en.fault != null)
                throw en.fault;
        }

        Object term = ((Goal) goalskel).term;
        Display ref = goaldisplay.disp;
        /* inlined deref */
        BindUniv b1;
        while (term instanceof SkelVar &&
                (b1 = ref.bind[((SkelVar) term).id]).display != null) {
            term = b1.skel;
            ref = b1.display;
        }

        co.flags &= ~CallOut.MASK_CALL_FRST;
        for (; ; ) {
            co.flags &= ~CallOut.MASK_CALL_RTRY;
            co.flags &= ~CallOut.MASK_CALL_SPCI;
            co.flags &= ~CallOut.MASK_CALL_CTTR;

            Object res = MemberVirtualDet.invokeVirtual(del.method, obj, args);
            if ((del.subflags & AbstractLense.MASK_METH_FUNC) != 0) {
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
                    !en.unify(AbstractTerm.getSkel(res), d,
                            (help = ((SkelCompound) term).args)[help.length - 1], ref)) {
                if ((co.flags & CallOut.MASK_CALL_RTRY) == 0)
                    return false;

                if ((co.flags & CallOut.MASK_CALL_SPCI) == 0) {
                    en.fault = null;
                    en.releaseBind(mark);
                    if (en.fault != null)
                        throw en.fault;
                }
            } else {
                if (ext)
                    d.remTab(en);

                if ((co.flags & CallOut.MASK_CALL_RTRY) != 0) {
                    /* meta argument change */
                    if ((co.flags & CallOut.MASK_CALL_SPCI) != 0)
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
     * <p>Free data used to logically evaluate a term an additional time.</p>
     * <p>The current exception is passed via the engine fault.</p>
     * <p>The new current exception is returned via the engine fault.</p>
     * <p>The current contskel and contdisplay of the engine is not changed.</p>
     *
     * @param en The engine.
     */
    public final void moniCut(Engine en) {
        /* remove choice point */
        en.choices = next;
        en.number--;

        if ((co.flags & CallOut.MASK_CALL_CTTR) == 0)
            return;

        /* backup continuation */
        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;
        en.contskel = goalskel;
        en.contdisplay = goaldisplay;
        if (en.fault != null) {
            co.setException(new InterpreterException(en.fault));
        } else {
            co.setException(null);
        }
        try {
            co.flags &= ~CallOut.MASK_CALL_FRST;
            co.flags |= CallOut.MASK_CALL_CLEN;
            MemberVirtualDet.invokeVirtual(del.method, obj, args);
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
            EngineException x = new EngineException(y,
                    EngineException.fetchStack(en));
            if (en.fault != null) {
                en.fault = new EngineException(en.fault, x);
            } else {
                en.fault = x;
            }
        }

        /* restore continuation */
        en.contskel = r;
        en.contdisplay = u;
    }

}
