package jekpro.tools.call;

import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Intermediate;
import jekpro.tools.term.AbstractTerm;

/**
 * <p>The call-in object can be obtained from an interpreter by providing
 * optionally a goal and op-tionally a result by the iterator() methods.
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
 * side effect of the goal is desired. The method run() acts similar to
 * nextClose() but swallows failure and exceptions.
 * </p>
 *
 * @author Copyright 2010-2015, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.1 (a fast and small prolog interpreter)
 */
public final class CallIn {
    private AbstractBind mark;
    private int snap;
    private int state;
    private final Object goal;
    private final Interpreter inter;

    private static final int STATE_FIRST = 0;
    private static final int STATE_NEXT = 1;
    private static final int STATE_FAILURE = 2;
    private static final int STATE_SUCCESS = 3;

    /**
     * <p>Create a call-in.</p>
     *
     * @param g The goal, non-null.
     * @param i The call-in.
     */
    CallIn(Object g, Interpreter i) {
        if (g == null)
            throw new NullPointerException("goal missing");
        goal = g;
        inter = i;
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
                if (unfoldFirst(goal))
                    state = STATE_SUCCESS;
                break;
            case STATE_NEXT:
                state = STATE_FAILURE;
                if (unfoldNext())
                    state = STATE_SUCCESS;
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
                if (unfoldFirst(goal))
                    state = STATE_NEXT;
                break;
            case STATE_NEXT:
                state = STATE_FAILURE;
                if (unfoldNext())
                    state = STATE_NEXT;
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
     * <p>Close an interactor. Amounts to doing "&lt;goal&gt;, !, fail", in
     * case "&lt;goal&gt;" has been already successful. The method need
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
                break;
            case STATE_FAILURE:
                break;
            case STATE_SUCCESS:
                state = STATE_FAILURE;
                unfoldClose();
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
                return ((Engine) inter.getEngine()).number != snap;
            case STATE_FAILURE:
                return false;
            case STATE_SUCCESS:
                return ((Engine) inter.getEngine()).number != snap;
            default:
                throw new IllegalArgumentException("illagel state");
        }
    }

    /**
     * <p>Remove the choice points but leave bindings intact. Amounts
     * to doing "&lt;goal&gt;, !", in case "&lt;goal&gt;" has been already
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
                state = STATE_NEXT;
                break;
            case STATE_FAILURE:
                break;
            case STATE_SUCCESS:
                state = STATE_FAILURE;
                unfoldCut();
                state = STATE_SUCCESS;
                break;
            default:
                throw new IllegalArgumentException("illagel state");
        }
    }

    /**
     * <p>Close an interactor with an initial exception. Amounts
     * to doing "&lt;goal&gt;, throw(error)", in case "&lt;goal&gt;" has
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
                return unfoldCleanup(x);
            case STATE_FAILURE:
                return x;
            case STATE_SUCCESS:
                state = STATE_FAILURE;
                return unfoldCleanup(x);
            default:
                throw new IllegalArgumentException("illagel state");
        }
    }

    /************************************************************/
    /* Common Unfold API Internal                               */
    /************************************************************/

    /**
     * <p>Start searching solutions for the given goal for the first time.</p>
     * <p>If failed or exception bindings are undone and exceptions are aggregated.</p>
     *
     * @param goal The goal.
     * @return True if the goal succeeded, otherwise false.
     * @throws InterpreterException Shit happens.
     */
    private boolean unfoldFirst(Object goal)
            throws InterpreterException {
        Engine en = (Engine) inter.getEngine();
        Intermediate r = en.contskel;
        DisplayClause u = en.contdisplay;
        Engine backuse = en.visor.setInuse(en);
        Thread backthread = en.visor.setFence(Thread.currentThread());
        en.skel = AbstractTerm.getSkel(goal);
        en.display = AbstractTerm.getDisplay(goal);
        en.deref();
        mark = en.bind;
        snap = en.number;
        try {
            boolean multi = en.wrapGoal();
            BindCount[] ref = en.display;
            Clause clause = en.store.foyer.CLAUSE_CALL;
            DisplayClause ref2 = new DisplayClause();
            ref2.bind = BindCount.newBindClause(clause.dispsize);
            ref2.addArgument(en.skel, ref, en);
            if (multi)
                BindCount.remTab(ref, en);
            ref2.setEngine(en);
            en.contskel = clause.getNextRaw(en);
            en.contdisplay = ref2;
            if (en.runFirst(snap)) {
                en.contskel = r;
                en.contdisplay = u;
                en.visor.setFence(backthread);
                en.visor.setInuse(backuse);
                return true;
            }
        } catch (EngineMessage x) {
            en.contskel = r;
            en.contdisplay = u;
            en.fault = new EngineException(x, EngineException.fetchStack(en));
            en.releaseBind(mark);
            en.visor.setFence(backthread);
            en.visor.setInuse(backuse);
            throw new InterpreterException(en.fault);
        } catch (EngineException x) {
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
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
     * <p>Continue searching solutions for the last goal.</p>
     * <p>If failed or exception bindings are undone and exceptions are aggregated.</p>
     *
     * @return True if the goal succeeded again, otherwise false.
     * @throws InterpreterException Shit happens.
     */
    private boolean unfoldNext() throws InterpreterException {
        Engine en = (Engine) inter.getEngine();
        Intermediate r = en.contskel;
        DisplayClause u = en.contdisplay;
        Engine backuse = en.visor.setInuse(en);
        Thread backthread = en.visor.setFence(Thread.currentThread());
        try {
            if (en.runNext(snap)) {
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
     * <p>Stop searching solutions for the last goal.</p>
     * <p>Bindings are undone and exceptions are aggregated.</p>
     *
     * @throws InterpreterException Shit happens.
     */
    private void unfoldClose() throws InterpreterException {
        Engine en = (Engine) inter.getEngine();
        Engine backuse = en.visor.setInuse(en);
        Thread backthread = en.visor.setFence(Thread.currentThread());
        en.window = null;
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
     * <p>Stop searching solutions for the last goal.</p>
     * <p>Bindings are left intact.</p>
     * <p>If exception bindings are undone and exceptions are aggregated.</p>
     *
     * @throws InterpreterException Shit happens.
     */
    private void unfoldCut() throws InterpreterException {
        Engine en = (Engine) inter.getEngine();
        Engine backuse = en.visor.setInuse(en);
        Thread backthread = en.visor.setFence(Thread.currentThread());

        en.window = null;
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
     * <p>Stop searching solutions for the last goal.</p>
     * <p>Bindings are undone and exceptions are aggregated.</p>
     *
     * @param e The current interpreter exception, or null.
     * @return The new current interpreter exception, or null.
     */
    private InterpreterException unfoldCleanup(InterpreterException e) {
        Engine en = (Engine) inter.getEngine();
        Engine backuse = en.visor.setInuse(en);
        Thread backthread = en.visor.setFence(Thread.currentThread());

        en.window = null;
        en.fault = (e != null ? (EngineException) e.getException() : null);
        en.cutChoices(snap);
        en.releaseBind(mark);

        en.visor.setFence(backthread);
        en.visor.setInuse(backuse);
        if (en.fault != null)
            return new InterpreterException(en.fault);
        return null;
    }

}
