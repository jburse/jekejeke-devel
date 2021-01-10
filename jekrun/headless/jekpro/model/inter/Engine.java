package jekpro.model.inter;

import jekpro.frequent.standard.SupervisorCall;
import jekpro.model.molec.*;
import jekpro.model.pretty.Store;
import jekpro.model.rope.Directive;
import jekpro.reference.arithmetic.SpecialCompare;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.structure.SpecialLexical;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.term.*;
import matula.util.data.ListArray;
import matula.util.wire.AbstractLivestock;

import java.util.Comparator;

/**
 * <p>The class provides an engine.</p>
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
public class Engine extends StackElement implements Comparator<Object> {
    public Object skel;
    public Display display;
    public Store store;
    public final Supervisor visor;
    public AbstractUndo bind;
    public AbstractChoice choices;
    public int number;
    public Object proxy;
    public EngineException fault;

    /**
     * <p>Create a new engine.</p>
     *
     * @param s The store.
     * @param v The supervisor.
     */
    public Engine(Store s, Supervisor v) {
        if (s.foyer != v.source)
            throw new IllegalArgumentException("store mismatch");
        store = s;
        visor = v;
    }

    /**
     * <p>Dereference the term given by the current skeleton
     * and display, return the result in the current skeleton
     * and display.</p>
     */
    public final void deref() {
        BindUniv b;
        while (skel instanceof SkelVar &&
                (b = display.bind[((SkelVar) skel).id]).display != null) {
            skel = b.skel;
            display = b.display;
        }
    }

    /*****************************************************************/
    /* Trampolin Interpreter                                         */
    /*****************************************************************/

    /**
     * <p>Start searching solutions for the given term for the first time.</p>
     * <p>The term is passed via skel and display of this engine.</p>
     * <p>In case of exception, the choice points are already removed.</p>
     *
     * @param snap  The choice barrier.
     * @param found The backtracking flag.
     * @return True if the term list succeeded, otherwise false.
     * @throws EngineException Shit happens.
     */
    public boolean runLoop(int snap, boolean found)
            throws EngineException, EngineMessage {
        for (; ; ) {
            if (found) {
                if (contskel != null) {
                    if (hasCont())
                        retireCont();
                    contskel = contskel.getNextRaw(this);
                    found = contskel.resolveNext(this);
                } else {
                    break;
                }
            } else {
                if (snap < number) {
                    found = choices.moniNext(this);
                } else {
                    break;
                }
            }
        }
        return found;
    }

    /**
     * <p>Release the variable binding list to the given marker.</p>
     * <p>The current exception is passed via the engine skel.</p>
     * <p>The new current exception is returned via the engine skel.</p>
     *
     * @param mark The marker.
     */
    public final void releaseBind(AbstractUndo mark) {
        while (bind != mark)
            bind.unbind(this);
    }

    /**
     * <p>Prune the choices.</p>
     * <p>The current exception is passed via the engine fault.</p>
     * <p>The new current exception is returned via the engine fault.</p>
     *
     * @param n The cut number.
     */
    public final void cutChoices(int n) {
        while (n < number) {
            AbstractChoice choice = choices;

            choice.moniCut(this);
            choice.replayNext(this);
        }
    }

    /***************************************************************/
    /* Suspend Handling                                            */
    /***************************************************************/

    /**
     * <p>Check whether the suspension queue is non-empty.</p>
     *
     * @return True if the suspension queue is non-empty.
     */
    public final boolean hasCont() {
        return (visor.cont != null &&
                (visor.flags & Supervisor.MASK_VISOR_NOCNT) == 0);
    }

    /**
     * <p>Dequeue all goals from the suspension queue.</p>
     * <p>And prepend the goals to the current continuation.</p>
     */
    public final void retireCont()
            throws EngineMessage {
        ListArray<BindUniv> list = UndoCont.bindCont(this);
        for (int i = list.size() - 1; i >= 0; i--) {
            BindUniv bc = list.get(i);
            skel = bc.skel;
            display = bc.display;
            deref();

            Directive dire = SupervisorCall.callGoal(0, this);
            Display d3 = display;

            CallFrame ref2 = CallFrame.getFrame(d3, dire, this);
            contskel = dire;
            contdisplay = ref2;
        }
    }

    /*************************************************************************/
    /* Expression Evaluation                                                 */
    /*************************************************************************/

    /**
     * <p>Arithmetically evaluate the given term.</p>
     * <p>The result is passed via the skel and display of the engine.</p>
     * <p>There is the invariant that the result is instance checked.</p>
     * <p>The continuation is passed via the contskel and contdisplay of the engine.</p>
     *
     * @param alfa The term.
     * @param d1   The display of the term.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final void computeExpr(Object alfa, Display d1)
            throws EngineMessage, EngineException {
        BindUniv b;
        while (alfa instanceof SkelVar &&
                (b = d1.bind[((SkelVar) alfa).id]).display != null) {
            alfa = b.skel;
            d1 = b.display;
        }
        if (!(alfa instanceof AbstractSkel)) {
            skel = alfa;
            display = Display.DISPLAY_CONST;
            return;
        }
        CachePredicate cp;
        if (alfa instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) alfa;
            cp = CachePredicate.getPredicate(sc.sym, sc.args.length + 1, this);
        } else if (alfa instanceof SkelAtom) {
            SkelAtom sa = (SkelAtom) alfa;
            cp = CachePredicate.getPredicate(sa, 1, this);
        } else {
            EngineMessage.checkInstantiated(alfa);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_CALLABLE, alfa));
        }
        if (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0) {
            SkelAtom sa = StackElement.callableToName(alfa);
            int arity = StackElement.callableToArity(alfa);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_EVALUABLE,
                    SpecialPred.indicatorToColonSkel(sa, arity, this)));
        }
        AbstractDelegate fun = cp.pick.del;
        if (fun == null) {
            SkelAtom sa = StackElement.callableToName(alfa);
            int arity = StackElement.callableToArity(alfa);
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_CODE,
                    SpecialPred.indicatorToColonSkel(sa, arity, this)));
        }
        skel = alfa;
        display = d1;
        fun.moniEvaluate(this);
    }

    /*****************************************************************/
    /* Lexical Comparator                                            */
    /*****************************************************************/

    /**
     * <p>Compare two objects.</p>
     *
     * @param o1 The first object.
     * @param o2 The second object.
     * @return <0 o1 < o2, 0 o1 = o2, >0 o1 > o2
     * @throws ArithmeticException Incomparable reference.
     */
    public final int compare(Object o1, Object o2)
            throws ArithmeticException {
        return SpecialLexical.compareTerm(AbstractTerm.getSkel(o1), AbstractTerm.getDisplay(o1),
                AbstractTerm.getSkel(o2), AbstractTerm.getDisplay(o2), this);
    }

    /***********************************************************/
    /* Some Convenience                                        */
    /***********************************************************/

    /**
     * <p>Retrieve the current interpreter.</p>
     *
     * @return The current interpreter or null.
     */
    public static Engine getEngine() {
        Thread thread = Thread.currentThread();
        Supervisor s = (Supervisor) AbstractLivestock.currentLivestock(thread);
        return (s != null ? s.inuse : null);
    }

}
