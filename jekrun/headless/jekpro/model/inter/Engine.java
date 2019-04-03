package jekpro.model.inter;

import jekpro.frequent.standard.EngineCopy;
import jekpro.frequent.standard.SpecialFind;
import jekpro.model.molec.*;
import jekpro.model.pretty.Store;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Intermediate;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.ListArray;

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
public class Engine extends StackElement {
    public Object skel;
    public Display display;
    public Store store;
    public final Supervisor visor;
    public AbstractUndo bind;
    public AbstractChoice choices;
    public int serno;
    public int number;
    public EngineCopy enginecopy;
    public EngineWrap enginewrap;
    public Object proxy;
    public EngineException fault;
    public CallFrame window;

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

    /**
     * <p>Unify two terms. As a side effect bindings are established.</p>
     * <p>Tail recursion implementation.</p>
     *
     * @param alfa The first skeleton.
     * @param d1   The first display.
     * @param beta The second skeleton.
     * @param d2   The second display.
     * @return True if the two terms unify, otherwise false.
     * @throws EngineException Shit happens.
     */
    public final boolean unifyTerm(Object alfa, Display d1,
                                   Object beta, Display d2)
            throws EngineException {
        for (; ; ) {
            if (alfa instanceof SkelVar) {
                // combined check and deref
                BindUniv b1;
                if ((b1 = d1.bind[((SkelVar) alfa).id]).display != null) {
                    alfa = b1.skel;
                    d1 = b1.display;
                    continue;
                }
                for (; ; ) {
                    if (beta instanceof SkelVar) {
                        // combined check and deref
                        BindUniv b2;
                        if ((b2 = d2.bind[((SkelVar) beta).id]).display != null) {
                            beta = b2.skel;
                            d2 = b2.display;
                            continue;
                        }
                        if (alfa == beta && d1 == d2)
                            return true;
                        return b2.bindAttr(alfa, d1, d2, this);
                    }
                    return b1.bindAttr(beta, d2, d1, this);
                }
            }
            for (; ; ) {
                // combined check and deref
                if (beta instanceof SkelVar) {
                    BindUniv b;
                    if ((b = d2.bind[((SkelVar) beta).id]).display != null) {
                        beta = b.skel;
                        d2 = b.display;
                        continue;
                    }
                    return b.bindAttr(alfa, d1, d2, this);
                }
                break;
            }
            if (!(alfa instanceof SkelCompound))
                return alfa.equals(beta);
            if (!(beta instanceof SkelCompound))
                return false;
            Object[] t1 = ((SkelCompound) alfa).args;
            Object[] t2 = ((SkelCompound) beta).args;
            if (t1.length != t2.length)
                return false;
            if (!((SkelCompound) alfa).sym.equals(((SkelCompound) beta).sym))
                return false;
            int i = 0;
            for (; i < t1.length - 1; i++)
                if (!unifyTerm(t1[i], d1, t2[i], d2))
                    return false;
            alfa = t1[i];
            beta = t2[i];
        }
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
    public boolean runLoop2(int snap, boolean found)
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
     * <p>Prune the choices.</p>
     * <p>The current exception and the sliding window are passed via
     * the skel and display of this engine.</p>
     * <p>The new current exception and the sliding window are returned via
     * the skel and display of this engine.</p>
     *
     * @param n The cut number.
     */
    public final void cutChoices(int n) {
        while (n < number)
            choices.moniCut(n, this);
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
            throws EngineMessage, EngineException {
        ListArray<BindUniv> list = UndoCont.bindCont(this);
        createComma(list, this);
        Display d2 = display;
        boolean ext = d2.getAndReset();
        boolean multi = wrapGoal();
        if (multi && ext)
            d2.remTab(this);
        Display ref = display;
        Directive dire = store.foyer.CLAUSE_CONT;
        DisplayClause d3 = new DisplayClause(dire.size);
        d3.bind[0].bindUniv(skel, ref, this);
        if (multi || ext)
            ref.remTab(this);
        CallFrame ref2 = CallFrame.getFrame(d3, dire, this);
        contskel = dire;
        contdisplay = ref2;
    }

    /**
     * <p>Create the comma list.</p>
     * <p>Result is returned in skel and display of the engine.</p>
     *
     * @param temp The list of solutions or null.
     * @param en   The engine.
     */
    private static void createComma(ListArray<BindUniv> temp, Engine en) {
        BindUniv val = temp.get(temp.size() - 1);
        en.skel = val.skel;
        en.display = val.display;
        for (int i = temp.size() - 2; i >= 0; i--) {
            Object t = en.skel;
            Display d = en.display;
            val = temp.get(i);
            SpecialFind.pairValue(en.store.foyer.CELL_COMMA,
                    val.skel, val.display, t, d, en);
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
                    SpecialQuali.indicatorToColonSkel(sa, arity, this)));
        }
        AbstractDelegate fun = cp.pick.del;
        if (fun == null) {
            SkelAtom sa = StackElement.callableToName(alfa);
            int arity = StackElement.callableToArity(alfa);
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_CODE,
                    SpecialQuali.indicatorToColonSkel(sa, arity, this)));
        }
        skel = alfa;
        display = d1;
        fun.moniEvaluate(this);
    }

    /****************************************************************************/
    /* Execution Helpers                                                        */
    /****************************************************************************/

    /**
     * <p>Search the given term once and close it.</p>
     * <p>Throw a warning when it fails.</p>
     * <p>The term is passed via skel and display.</p>
     *
     * @throws EngineException Shit happens.
     */
    public final void invokeChecked()
            throws EngineException {
        Intermediate r = contskel;
        CallFrame u = contdisplay;
        boolean backignore = visor.setIgnore(false);
        boolean backverify = visor.setVerify(false);
        AbstractUndo mark = bind;
        int snap = number;
        try {
            boolean multi = wrapGoal();
            Display ref = display;
            Directive dire = store.foyer.CLAUSE_CALL;
            DisplayClause d2 = new DisplayClause(dire.size);
            d2.bind[0].bindUniv(skel, ref, this);
            if (multi)
                ref.remTab(this);
            CallFrame ref2 = CallFrame.getFrame(d2, dire, this);
            contskel = dire;
            contdisplay = ref2;
            if (!runLoop2(snap, true))
                throw new EngineMessage(EngineMessage.syntaxError(
                        EngineMessage.OP_SYNTAX_DIRECTIVE_FAILED));
        } catch (EngineException x) {
            contskel = r;
            contdisplay = u;
            window = contdisplay;
            fault = x;
            cutChoices(snap);
            window = null;
            releaseBind(mark);
            visor.setVerify(backverify);
            visor.setIgnore(backignore);
            throw fault;
        } catch (EngineMessage y) {
            EngineException x = new EngineException(y,
                    EngineException.fetchStack(this));
            contskel = r;
            contdisplay = u;
            window = contdisplay;
            fault = x;
            cutChoices(snap);
            window = null;
            releaseBind(mark);
            visor.setVerify(backverify);
            visor.setIgnore(backignore);
            throw fault;
        }
        contskel = r;
        contdisplay = u;
        window = contdisplay;
        fault = null;
        cutChoices(snap);
        window = null;
        releaseBind(mark);
        visor.setVerify(backverify);
        visor.setIgnore(backignore);
        if (fault != null)
            throw fault;
    }

    /**
     * <p>Prepare a term for execution.</p>
     * <p>Goal is updated in the skel and the display of this engine.</p>
     *
     * @return True if new display is returned, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public final boolean wrapGoal()
            throws EngineException, EngineMessage {
        Object t = skel;
        Display d = display;
        EngineMessage.checkInstantiated(t);
        EngineWrap ew = enginewrap;
        if (ew == null) {
            ew = new EngineWrap();
            enginewrap = ew;
        }
        ew.countvar = 0;
        ew.flags = 0;
        ew.last = Display.DISPLAY_CONST;
        ew.countGoal(t, d, this);
        if ((ew.flags & EngineWrap.MASK_WRAP_CHNG) == 0) {
            skel = t;
            display = d;
            return false;
        }
        if ((ew.flags & EngineWrap.MASK_WRAP_MLTI) != 0)
            ew.last = new Display(ew.countvar);
        ew.countvar = 0;
        skel = ew.replaceGoalAndWrap(t, d, this);
        display = ew.last;
        ew.last = Display.DISPLAY_CONST;
        return ((ew.flags & EngineWrap.MASK_WRAP_MLTI) != 0);
    }

}
