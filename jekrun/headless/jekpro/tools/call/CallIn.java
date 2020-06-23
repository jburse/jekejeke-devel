package jekpro.tools.call;

import jekpro.frequent.standard.SupervisorCall;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Intermediate;
import jekpro.tools.term.AbstractTerm;

/**
 * <p>The call-in object can be obtained from an interpreter by providing
 * optionally a term and op-tionally a result by the iterator() methods.
 * A call-in object is needed to interact with choice points and variable
 * bindings. Thus call-in objects are needed when Prolog goals are called.
 * The application programmer cannot subclass the call-in class.
 * </p>
 * <p>The method hasNext() can be used check whether the interactor can be
 * advanced. Initially the interactor is positioned before the first solution.
 * The method molec() effectively advances the interactor. The method throws
 * a NoSuchElementException when there was no solution. The method close()
 * closes an interactor. The method need not be called when there was
 * no solution.
 * </p>
 * <p>The method getNondet() allows checking whether there are choices and
 * potentially further solutions. Further solutions can be prevented by
 * calling the method cut() which will remove choice points. As a result
 * the method getNondet() will return false. An alternative way to close
 * an interactor is to use the method cleanup(). This method takes an initial
 * exception which will accumulate further exceptions during closing.
 * </p>
 * <p>It is possible to use call-in objects to implement meta-predicates,
 * provide the special directive is given to the CallOut argument. But because
 * separate choice points and tail recursion barrier are created for
 * invoked goals, it is not possible to program cut transparent arguments
 * for meta-predicates this way.
 * </p>
 * <p>Inside the Prolog execution of hasNext() or molec() or when these methods
 * have been completed during Java execution it is allowed to create and
 * use further call-in objects, as long as the interaction with these objects
 * is complete. For non-stack use call-in objects have to be drawn from
 * multiple interpreters inside the same thread.
 * </p>
 * <p>The method nextClose() throws an exception when the interactor failed,
 * the method hasNextClose() returns null when the interactor failed.
 * These methods will copy the result term and always close the interactor.
 * The later behaviour might be not appropriate if a variable binding
 * side effect of the term is desired. The method run() acts similar to
 * nextClose() but swallows failure and exceptions.
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
public final class CallIn implements Runnable {
    private AbstractUndo mark;
    private int snap;
    private int state;
    private Object goal;
    private boolean ext;
    private final Interpreter inter;

    private static final int STATE_FIRST = 0;
    private static final int STATE_NEXT = 1;
    private static final int STATE_FAILURE = 2;
    private static final int STATE_SUCCESS = 3;

    /**
     * <p>Create a call-in.</p>
     *
     * @param g The term, non-null.
     * @param i The call-in.
     */
    CallIn(Object g, Interpreter i) {
        if (g == null)
            throw new NullPointerException("term missing");
        goal = g;
        inter = i;
        Display ref = AbstractTerm.getDisplay(goal);
        ext = ref.getAndReset();
    }

    /**
     * <p>Retrieve the interpreter.</p>
     *
     * @return The interpreter,
     */
    public Interpreter getInter() {
        return inter;
    }

    /************************************************************/
    /* Common Unfold API External                               */
    /************************************************************/

    /**
     * <p>Check whether the interactor can be advanced. Initially the
     * interactor is positioned before the first solution.</p>
     *
     * @return True if a solution was found.
     * @throws InterpreterException Shit happens.
     */
    public boolean hasNext() throws InterpreterException {
        switch (state) {
            case STATE_FIRST:
                state = STATE_FAILURE;
                if (unfoldFirst()) {
                    state = STATE_SUCCESS;
                } else if (ext) {
                    Engine en = inter.getEngine();
                    Display ref = AbstractTerm.getDisplay(goal);
                    ref.remTab(en);
                }
                break;
            case STATE_NEXT:
                state = STATE_FAILURE;
                if (unfoldNext()) {
                    state = STATE_SUCCESS;
                } else if (ext) {
                    Engine en = inter.getEngine();
                    Display ref = AbstractTerm.getDisplay(goal);
                    ref.remTab(en);
                }
                break;
            case STATE_FAILURE:
            case STATE_SUCCESS:
                break;
            default:
                throw new IllegalArgumentException("illagel state");
        }
        return (state != STATE_FAILURE);
    }

    /**
     * <p>Effectively advance the interactor. The method throws an
     * InterpreterMessage when there was no solution.</p>
     *
     * @throws InterpreterException Shit happens.
     * @throws InterpreterMessage   No such element.
     */
    public CallIn next()
            throws InterpreterException, InterpreterMessage {
        switch (state) {
            case STATE_FIRST:
                state = STATE_FAILURE;
                if (unfoldFirst()) {
                    state = STATE_NEXT;
                } else if (ext) {
                    Engine en = inter.getEngine();
                    Display ref = AbstractTerm.getDisplay(goal);
                    ref.remTab(en);
                }
                break;
            case STATE_NEXT:
                state = STATE_FAILURE;
                if (unfoldNext()) {
                    state = STATE_NEXT;
                } else if (ext) {
                    Engine en = inter.getEngine();
                    Display ref = AbstractTerm.getDisplay(goal);
                    ref.remTab(en);
                }
                break;
            case STATE_FAILURE:
                break;
            case STATE_SUCCESS:
                state = STATE_NEXT;
                break;
            default:
                throw new IllegalArgumentException("illagel state");
        }
        if (state == STATE_FAILURE)
            throw new InterpreterMessage(InterpreterMessage.syntaxError(
                    EngineMessage.OP_SYNTAX_DIRECTIVE_FAILED));
        return this;
    }

    /**
     * <p>Close an interactor. Amounts to doing "&lt;term&gt;, !, fail", in
     * case "&lt;term&gt;" has been already successful. The method need
     * not be called when there was no solution.</p>
     *
     * @throws InterpreterException Shit happens.
     */
    public void close()
            throws InterpreterException {
        switch (state) {
            case STATE_FIRST:
                state = STATE_FAILURE;
                break;
            case STATE_NEXT:
                state = STATE_FAILURE;
                unfoldClose();
                if (ext) {
                    Engine en = inter.getEngine();
                    Display ref = AbstractTerm.getDisplay(goal);
                    ref.remTab(en);
                }
                break;
            case STATE_FAILURE:
                break;
            case STATE_SUCCESS:
                state = STATE_FAILURE;
                unfoldClose();
                if (ext) {
                    Engine en = inter.getEngine();
                    Display ref = AbstractTerm.getDisplay(goal);
                    ref.remTab(en);
                }
                break;
            default:
                throw new IllegalArgumentException("illagel state");
        }
    }

    /************************************************************/
    /* Special Unfold API External                              */
    /************************************************************/

    /**
     * <p>Check whether there are choice points.</p>
     *
     * @return True if there are choice points, otherwise false.
     */
    public boolean getNondet() {
        switch (state) {
            case STATE_FIRST:
                return true;
            case STATE_NEXT:
                return (inter.getEngine()).number != snap;
            case STATE_FAILURE:
                return false;
            case STATE_SUCCESS:
                return (inter.getEngine()).number != snap;
            default:
                throw new IllegalArgumentException("illagel state");
        }
    }

    /**
     * <p>Remove the choice points but leave bindings intact. Amounts
     * to doing "&lt;term&gt;, !", in case "&lt;term&gt;" has been already
     * successful. As a result the method getNondet() will return false.</p>
     *
     * @throws InterpreterException Shit happens.
     */
    public void cut()
            throws InterpreterException {
        switch (state) {
            case STATE_FIRST:
                state = STATE_FAILURE;
                break;
            case STATE_NEXT:
                state = STATE_FAILURE;
                unfoldCut();
                break;
            case STATE_FAILURE:
                break;
            case STATE_SUCCESS:
                state = STATE_FAILURE;
                unfoldCut();
                break;
            default:
                throw new IllegalArgumentException("illagel state");
        }
    }

    /**
     * <p>Close an interactor with an initial exception. Amounts
     * to doing "&lt;term&gt;, throw(error)", in case "&lt;term&gt;" has
     * been already successful.</p>
     *
     * @param x The input exception.
     * @return The output exception.
     */
    public InterpreterException cleanup(InterpreterException x) {
        switch (state) {
            case STATE_FIRST:
                state = STATE_FAILURE;
                return x;
            case STATE_NEXT:
                state = STATE_FAILURE;
                x = unfoldCleanup(x);
                if (ext) {
                    Engine en = inter.getEngine();
                    Display ref = AbstractTerm.getDisplay(goal);
                    ref.remTab(en);
                }
                return x;
            case STATE_FAILURE:
                return x;
            case STATE_SUCCESS:
                state = STATE_FAILURE;
                x = unfoldCleanup(x);
                if (ext) {
                    Engine en = inter.getEngine();
                    Display ref = AbstractTerm.getDisplay(goal);
                    ref.remTab(en);
                }
                return x;
            default:
                throw new IllegalArgumentException("illagel state");
        }
    }

    /************************************************************/
    /* Common Unfold API Internal                               */
    /************************************************************/

    /**
     * <p>Start searching solutions for the given term for the first time.</p>
     * <p>If failed or exception bindings are undone and exceptions are aggregated.</p>
     *
     * @return True if the term succeeded, otherwise false.
     * @throws InterpreterException Shit happens.
     */
    private boolean unfoldFirst()
            throws InterpreterException {
        Engine en = inter.getEngine();
        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;
        Engine backuse = en.visor.setInuse(en);
        Thread backthread = en.visor.setFence(Thread.currentThread());

        en.skel = AbstractTerm.getSkel(goal);
        en.display = AbstractTerm.getDisplay(goal);
        en.deref();
        mark = en.bind;
        snap = en.number;
        try {
            Directive dire = SupervisorCall.callGoal(AbstractDefined.MASK_DEFI_CALL, en);
            Display d2 = en.display;

            CallFrame ref2 = CallFrame.getFrame(d2, dire, en);
            en.contskel = dire;
            en.contdisplay = ref2;
            if (en.runLoop(snap, true)) {
                en.contskel = r;
                en.contdisplay = u;
                en.visor.setFence(backthread);
                en.visor.setInuse(backuse);
                return true;
            }
        } catch (EngineException x) {
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            en.releaseBind(mark);
            en.visor.setFence(backthread);
            en.visor.setInuse(backuse);
            throw new InterpreterException(en.fault);
        } catch (EngineMessage y) {
            EngineException x = new EngineException(y,
                    EngineException.fetchStack(en));
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            en.releaseBind(mark);
            en.visor.setFence(backthread);
            en.visor.setInuse(backuse);
            throw new InterpreterException(en.fault);
        }
        en.contskel = r;
        en.contdisplay = u;
        en.fault = null;
        en.releaseBind(mark);
        en.visor.setFence(backthread);
        en.visor.setInuse(backuse);
        if (en.fault != null)
            throw new InterpreterException(en.fault);
        return false;
    }

    /**
     * <p>Continue searching solutions for the last term.</p>
     * <p>If failed or exception bindings are undone and exceptions are aggregated.</p>
     *
     * @return True if the term succeeded again, otherwise false.
     * @throws InterpreterException Shit happens.
     */
    private boolean unfoldNext() throws InterpreterException {
        Engine en = inter.getEngine();
        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;
        Engine backuse = en.visor.setInuse(en);
        Thread backthread = en.visor.setFence(Thread.currentThread());
        try {
            if (en.runLoop(snap, false)) {
                en.contskel = r;
                en.contdisplay = u;
                en.visor.setFence(backthread);
                en.visor.setInuse(backuse);
                return true;
            }
        } catch (EngineException x) {
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            en.releaseBind(mark);
            en.visor.setFence(backthread);
            en.visor.setInuse(backuse);
            throw new InterpreterException(en.fault);
        } catch (EngineMessage y) {
            EngineException x = new EngineException(y,
                    EngineException.fetchStack(en));
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            en.releaseBind(mark);
            en.visor.setFence(backthread);
            en.visor.setInuse(backuse);
            throw new InterpreterException(en.fault);
        }
        en.contskel = r;
        en.contdisplay = u;
        en.fault = null;
        en.releaseBind(mark);
        en.visor.setFence(backthread);
        en.visor.setInuse(backuse);
        if (en.fault != null)
            throw new InterpreterException(en.fault);
        return false;
    }

    /**
     * <p>Stop searching solutions for the last term.</p>
     * <p>Bindings are undone and exceptions are aggregated.</p>
     *
     * @throws InterpreterException Shit happens.
     */
    private void unfoldClose() throws InterpreterException {
        Engine en = inter.getEngine();
        Engine backuse = en.visor.setInuse(en);
        Thread backthread = en.visor.setFence(Thread.currentThread());

        en.fault = null;
        en.cutChoices(snap);
        en.releaseBind(mark);

        en.visor.setFence(backthread);
        en.visor.setInuse(backuse);
        if (en.fault != null)
            throw new InterpreterException(en.fault);
    }

    /************************************************************/
    /* Special Unfold API Internal                              */
    /************************************************************/

    /**
     * <p>Stop searching solutions for the last term.</p>
     * <p>Bindings are left intact.</p>
     * <p>If exception bindings are undone and exceptions are aggregated.</p>
     *
     * @throws InterpreterException Shit happens.
     */
    private void unfoldCut() throws InterpreterException {
        Engine en = inter.getEngine();
        Engine backuse = en.visor.setInuse(en);
        Thread backthread = en.visor.setFence(Thread.currentThread());

        en.fault = null;
        en.cutChoices(snap);
        if (en.fault != null)
            en.releaseBind(mark);

        en.visor.setFence(backthread);
        en.visor.setInuse(backuse);
        if (en.fault != null)
            throw new InterpreterException(en.fault);
    }

    /**
     * <p>Stop searching solutions for the last term.</p>
     * <p>Bindings are undone and exceptions are aggregated.</p>
     *
     * @param e The current interpreter exception, or null.
     * @return The new current interpreter exception, or null.
     */
    private InterpreterException unfoldCleanup(InterpreterException e) {
        Engine en = inter.getEngine();
        Engine backuse = en.visor.setInuse(en);
        Thread backthread = en.visor.setFence(Thread.currentThread());

        en.fault = (e != null ? (EngineException) e.getException() : null);
        en.cutChoices(snap);
        en.releaseBind(mark);

        en.visor.setFence(backthread);
        en.visor.setInuse(backuse);
        if (en.fault != null)
            return new InterpreterException(en.fault);
        return null;
    }

    /*************************************************************/
    /* Runnable Interface                                        */
    /*************************************************************/

    /**
     * <p>Run a callin.</p>
     */
    public void run() {
        try {
            try {
                next().close();
            } catch (InterpreterMessage y) {
                InterpreterException x = new InterpreterException(y,
                        InterpreterException.fetchStack(getInter()));
                systemDeathBreak(getInter(), x);
            } catch (InterpreterException x) {
                systemDeathBreak(getInter(), x);
            }
        } catch (ThreadDeath x) {
            /* */
        } catch (Throwable x) {
            x.printStackTrace();
        }
    }

    /**
     * <p>Show the death exception.</p>
     *
     * @param inter The interpreter.
     * @param x     The death exception.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public static void systemDeathBreak(Interpreter inter, InterpreterException x)
            throws InterpreterMessage, InterpreterException {
        InterpreterMessage m;
        if ((m = x.exceptionType("error")) != null &&
                m.messageType("system_error") != null) {
            InterpreterException rest = x.causeChainRest();
            if (rest != null)
                rest.printStackTrace(inter);
        } else {
            x.printStackTrace(inter);
        }
    }

}
