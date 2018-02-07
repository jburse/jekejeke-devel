package jekpro.tools.foreign;

import jekpro.model.inter.AbstractChoice;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Frame;
import jekpro.model.molec.*;
import jekpro.model.rope.Intermediate;
import jekpro.tools.array.Types;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelCompound;

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
final class ChoiceForeign extends CallOut {
    private MemberMethodNondet del;
    private Object obj;
    private Object[] args;
    private Bind mark;

    /**
     * <p>Creata choice foreign.</p>
     *
     * @param u The parent choice.
     */
    ChoiceForeign(AbstractChoice u) {
        super(u);
    }

    /**
     * <p>Set the delegate.</p>
     *
     * @param d The delegate.
     */
    public void setDelegate(MemberMethodNondet d) {
        del = d;
    }

    /**
     * <p>Set the receiver.</p>
     *
     * @param o The receiver.
     */
    public void setReceiver(Object o) {
        obj = o;
    }

    /**
     * <p>Set the arguments.</p>
     *
     * @param a The arguments.
     */
    public void setArguments(Object[] a) {
        args = a;
    }

    /**
     * <p>Set the mark.</p>
     *
     * @param m The mark.
     */
    public void setMark(Bind m) {
        mark = m;
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

        en.contskel = getGoalSkel();
        en.contdisplay = getGoalDisplay();
        if (!getSpecial()) {
            en.skel = null;
            en.releaseBind(mark);
            if (en.skel != null) {
                en.display = getGoalDisplay();
                throw (EngineException) en.skel;
            }
        }

        Frame.callGoal(getGoalSkel(), getGoalDisplay(), en);
        Object term = en.skel;
        Display ref = en.display;
        try {
            for (; ; ) {
                setFirst(false);
                setCleanup(false);
                setRetry(false);
                setSpecial(false);
                setCutter(false);
                Object res = AbstractMember.invokeMethod(del.method, obj, args, en);
                res = Types.normJava(del.encoderet, res);
                if (res == null) {
                    return false;
                }
                if (res == AbstractSkel.VOID_OBJ ||
                        en.unifyTerm(((SkelCompound) term).args[
                                        ((SkelCompound) term).args.length - 1], ref,
                                AbstractTerm.getSkel(res), AbstractTerm.getDisplay(res))) {
                    if (getRetry()) {
                        /* meta argument change */
                        if (getSpecial())
                            next = en.choices;
                        /* reuse choice point */
                        en.choices = this;
                        en.number++;
                    }
                    return en.getNext();
                }
                if (!getRetry()) {
                    return false;
                }
                if (!getSpecial()) {
                    en.skel = null;
                    en.releaseBind(mark);
                    if (en.skel != null)
                        throw (EngineException) en.skel;
                }
            }
        } catch (EngineMessage x) {
            en.skel = getGoalSkel();
            en.display = getGoalDisplay();
            throw x;
        } catch (EngineException x) {
            en.display = getGoalDisplay();
            throw x;
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

        if (!getCutter())
            return;

        Intermediate r = en.contskel;
        DisplayClause u = en.contdisplay;
        en.contskel = getGoalSkel();
        en.contdisplay = getGoalDisplay();
        DisplayClause back = (DisplayClause) en.display;
        if (en.skel != null) {
            setException(new InterpreterException((EngineException) en.skel));
        } else {
            setException(null);
        }
        Frame.callGoal(getGoalSkel(), getGoalDisplay(), en);
        try {
            setFirst(false);
            setCleanup(true);
            AbstractMember.invokeMethod(del.method, obj, args, en);
            InterpreterException ie = getException();
            en.skel = (ie != null ? (EngineException) ie.getException() : null);
        } catch (EngineException x) {
            InterpreterException ie = getException();
            en.skel = (ie != null ? (EngineException) ie.getException() : null);
            if (en.skel != null) {
                en.skel = new EngineException((EngineException) en.skel, x);
            } else {
                en.skel = x;
            }
        } catch (EngineMessage y) {
            InterpreterException ie = getException();
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
