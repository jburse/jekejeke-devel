package jekpro.model.inter;

import jekpro.frequent.standard.EngineCopy;
import jekpro.model.molec.*;
import jekpro.model.pretty.Store;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.reference.runtime.SpecialQuali;
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
public class Engine implements Comparator<Object> {
    public Object skel;
    public BindCount[] display;
    public Intermediate contskel;
    public Display contdisplay;
    public Store store;
    public final Supervisor visor;
    public AbstractBind bind;
    public AbstractChoice choices;
    public int serno;
    public int number;
    public EngineCopy enginecopy;
    public EngineWrap enginewrap;
    public Object proxy;
    public EngineException fault;
    public Display window;

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
        BindVar b;
        while (skel instanceof SkelVar &&
                (b = display[((SkelVar) skel).id]).display != null) {
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
    public final boolean unifyTerm(Object alfa, BindCount[] d1,
                                   Object beta, BindCount[] d2)
            throws EngineException {
        for (; ; ) {
            if (alfa instanceof SkelVar) {
                // combined check and deref
                BindVar b1;
                if ((b1 = d1[((SkelVar) alfa).id]).display != null) {
                    alfa = b1.skel;
                    d1 = b1.display;
                    continue;
                }
                for (; ; ) {
                    if (beta instanceof SkelVar) {
                        // combined check and deref
                        BindVar b2;
                        if ((b2 = d2[((SkelVar) beta).id]).display != null) {
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
                    BindVar b;
                    if ((b = d2[((SkelVar) beta).id]).display != null) {
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
    public final void releaseBind(AbstractBind mark) {
        while (bind != mark)
            bind.unbind(this);
    }

    /*****************************************************************/
    /* BindCount[] Management                                        */
    /*****************************************************************/

    /**
     * <p>New bind.</p>
     *
     * @param i The current last alloc.
     * @param n The max last alloc.
     */
    public static int newBind(int i, int n, BindCount[] u) {
        do {
            if (u[i] == null)
                u[i] = new BindCount();
            i++;
        } while (i < n);
        return i;
    }

    /*****************************************************************/
    /* Trampolin Interpreter                                         */
    /*****************************************************************/

    /**
     * <p>Start searching solutions for the given goal for the first time.</p>
     * <p>The goal is passed via skel and display of this engine.</p>
     * <p>In case of exception, the choice points are already removed.</p>
     *
     * @param snap The choice barrier.
     * @return True if the goal list succeeded, otherwise null.
     * @throws EngineException Shit happens.
     */
    public boolean runFirst(int snap)
            throws EngineException {
        boolean found = true;
        try {
            while (found) {
                if (contskel != null) {
                    if (contskel instanceof Goal) {
                        AbstractDelegate del = ((Goal)contskel).resolveGoal(this);
                        found = del.moniFirst(this);
                    } else {
                        ((Clause)contskel).resolveCont(this);
                    }
                } else {
                    break;
                }
            }
            while (!found && snap < number) {
                found = choices.moniNext(this);
                while (found) {
                    if (contskel != null) {
                        if (contskel instanceof Goal) {
                            AbstractDelegate del = ((Goal)contskel).resolveGoal(this);
                            found = del.moniFirst(this);
                        } else {
                            ((Clause)contskel).resolveCont(this);
                        }
                    } else {
                        break;
                    }
                }
            }
        } catch (EngineException x) {
            window = contdisplay;
            fault = x;
            cutChoices(snap);
            window = null;
            throw fault;
        } catch (EngineMessage y) {
            EngineException x = new EngineException(y, EngineException.fetchStack(this));
            window = contdisplay;
            fault = x;
            cutChoices(snap);
            window = null;
            throw fault;
        }
        return found;
    }

    /**
     * <p>Continue searching solutions for the last goal.</p>
     * <p>In case of exception, the choice points are already removed.</p>
     *
     * @param snap The choice barrier.
     * @return True if the goal list succeeded, otherwise false.
     * @throws EngineException Shit happens.
     */
    public boolean runNext(int snap)
            throws EngineException {
        boolean found = false;
        try {
            while (!found && snap < number) {
                found = choices.moniNext(this);
                while (found) {
                    if (contskel != null) {
                        if (contskel instanceof Goal) {
                            AbstractDelegate del = ((Goal)contskel).resolveGoal(this);
                            found = del.moniFirst(this);
                        } else {
                            ((Clause)contskel).resolveCont(this);
                        }
                    } else {
                        break;
                    }
                }
            }
        } catch (EngineException x) {
            window = contdisplay;
            fault = x;
            cutChoices(snap);
            window = null;
            throw fault;
        } catch (EngineMessage y) {
            EngineException x = new EngineException(y, EngineException.fetchStack(this));
            window = contdisplay;
            fault = x;
            cutChoices(snap);
            window = null;
            throw fault;
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

    /**
     * <p>Retrieve the next goal depending on debug mode.</p>
     * <p>The next goal is placed in the skel and display of the engine.</p>
     *
     * @return Always true.
     */
    public final boolean getNextRaw() {
        contskel = contskel.getNextRaw(this);
        return true;
    }

    /**
     * <p>Prepare from continuation queue or retrieve the next goal.</p>
     * <p>Differnt result depending on debug mode and wakeup mute.</p>
     * <p>The next goal is placed in the skel and display of the engine.</p>
     *
     * @return Always true.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public final boolean getNext()
            throws EngineMessage, EngineException {
        return (contskel.getRetire(this) ?
                retireCont() :
                getNextRaw());
    }

    /***************************************************************/
    /* Suspend Handling                                            */
    /***************************************************************/

    /**
     * <p>Check the continuation queue of the engine.</p>
     *
     * @return True if the queue is non-empty, otherwise false.
     */
    public final boolean hasCont() {
        if (visor.cont != null &&
                (visor.flags & Supervisor.MASK_VISOR_NOCNT) == 0)
            return true;
        return false;
    }

    /**
     * <p>Dequeue the oldest goal from the suspension queue.</p>
     *
     * @return True if sucessful, false otherwise.
     */
    public final boolean retireCont()
            throws EngineMessage, EngineException {
        ListArray<BindVar> list = BindCont.bindCont(this);
        boolean ext = contCount(list, this);
        skel = contAlloc(list, ext, this);
        BindCount[] d2 = display;
        boolean multi = wrapGoal();
        if (multi && ext)
            BindCount.remTab(d2, this);
        BindCount[] ref = display;
        Clause clause = store.foyer.CLAUSE_CONT;
        Display ref2 = new Display();
        ref2.bind = BindCount.newBindClause(clause.dispsize);
        ref2.addArgument(skel, ref, this);
        if (multi || ext)
            BindCount.remTab(ref, this);
        ref2.setEngine(this);
        contskel = clause.getNextRaw(this);
        contdisplay = ref2;
        return true;
    }

    /**
     * <p>Count the needed variable place holders.</p>
     *
     * @param list The continuation queue.
     * @param en   The engine.
     * @return True if new display is returned, otherwise false.
     */
    private static boolean contCount(ListArray<BindVar> list, Engine en) {
        int countvar = 0;
        BindCount[] last = BindCount.DISPLAY_CONST;
        boolean multi = false;
        for (int i = list.size() - 1; i >= 0; i--) {
            BindVar bv = list.get(i);
            en.skel = bv.skel;
            en.display = bv.display;
            en.deref();
            if (EngineCopy.getVar(en.skel) != null) {
                countvar++;
                if (last == BindCount.DISPLAY_CONST) {
                    last = en.display;
                } else if (last != en.display) {
                    multi = true;
                }
            }
        }
        if (multi)
            last = BindCount.newBind(countvar);
        en.display = last;
        return multi;
    }

    /**
     * <p>Unpack the arguments and bind the needed variable
     * place holders.</p>
     *
     * @param list  The continuation queue.
     * @param multi The multi flag.
     * @param en    The engine.
     * @return The new conjunction.
     */
    private static Object contAlloc(ListArray<BindVar> list, boolean multi,
                                    Engine en) {
        BindCount[] d3 = en.display;
        int countvar = 0;
        Object res = null;
        for (int i = list.size() - 1; i >= 0; i--) {
            BindVar bv = list.get(i);
            en.skel = bv.skel;
            en.display = bv.display;
            en.deref();
            Object temp;
            if (multi && EngineCopy.getVar(en.skel) != null) {
                SkelVar sv = SkelVar.valueOf(countvar);
                countvar++;
                d3[sv.id].bindVar(en.skel, en.display, en);
                temp = sv;
            } else {
                temp = en.skel;
            }
            if (res == null) {
                res = temp;
            } else {
                res = new SkelCompound(en.store.foyer.ATOM_COMMA, temp, res);
            }
        }
        en.display = d3;
        return res;
    }

    /*************************************************************************/
    /* Expression Evaluation                                                 */
    /*************************************************************************/

    /**
     * <p>Arithmetically evaluate the given term.</p>
     * <p>The result is passed via the skel and display of the engine.</p>
     * <p>There is the invariant that the result is derefered.</p>
     * <p>There is the invariant that the result is instance checked.</p>
     * <p>There is the invariant that the result is a reference or a number.</p>
     * <p>The continuation is passed via the contskel and contdisplay of the engine.</p>
     *
     * @param alfa The term.
     * @param d1   The display of the term.
     * @return True if new display is returned, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean computeExpr(Object alfa, BindCount[] d1)
            throws EngineMessage, EngineException {
        BindVar b;
        while (alfa instanceof SkelVar &&
                (b = d1[((SkelVar) alfa).id]).display != null) {
            alfa = b.skel;
            d1 = b.display;
        }
        if (!(alfa instanceof AbstractSkel)) {
            skel = alfa;
            display = BindCount.DISPLAY_CONST;
            return false;
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
            SkelAtom sa = Frame.callableToName(alfa);
            int arity = Frame.callableToArity(alfa);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_EVALUABLE,
                    SpecialQuali.indicatorToColonSkel(sa, arity, this)));
        }
        AbstractDelegate fun = cp.pick.del;
        if (fun == null) {
            SkelAtom sa = Frame.callableToName(alfa);
            int arity = Frame.callableToArity(alfa);
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_CODE,
                    SpecialQuali.indicatorToColonSkel(sa, arity, this)));
        }
        skel = alfa;
        display = d1;
        return fun.moniEvaluate(this);
    }

    /****************************************************************************/
    /* Execution Helpers                                                        */
    /****************************************************************************/

    /**
     * <p>Search the given goal once and close it.</p>
     * <p>Throw a warning when it fails.</p>
     * <p>The goal is passed via skel and display.</p>
     *
     * @throws EngineException Shit happens.
     */
    public final void invokeChecked()
            throws EngineException {
        Intermediate r = contskel;
        Display u = contdisplay;
        boolean backignore = visor.setIgnore(false);
        boolean backverify = visor.setVerify(false);
        AbstractBind mark = bind;
        int snap = number;
        try {
            boolean multi = wrapGoal();
            BindCount[] ref = display;
            Clause clause = store.foyer.CLAUSE_CALL;
            Display ref2 = new Display();
            ref2.bind = BindCount.newBindClause(clause.dispsize);
            ref2.addArgument(skel, ref, this);
            if (multi)
                BindCount.remTab(ref, this);
            ref2.setEngine(this);
            contskel = clause.getNextRaw(this);
            contdisplay = ref2;
            if (!runFirst(snap))
                throw new EngineMessage(EngineMessage.syntaxError(
                        EngineMessage.OP_SYNTAX_DIRECTIVE_FAILED));

        } catch (EngineMessage x) {
            contskel = r;
            contdisplay = u;
            fault = new EngineException(x, EngineException.fetchStack(this));
            releaseBind(mark);
            visor.setVerify(backverify);
            visor.setIgnore(backignore);
            throw fault;
        } catch (EngineException x) {
            contskel = r;
            contdisplay = u;
            fault = x;
            releaseBind(mark);
            visor.setVerify(backverify);
            visor.setIgnore(backignore);
            throw fault;
        }
        contskel = r;
        contdisplay = u;
        window = null;
        fault = null;
        cutChoices(snap);
        releaseBind(mark);
        visor.setVerify(backverify);
        visor.setIgnore(backignore);
        if (fault != null)
            throw fault;
    }

    /**
     * <p>Prepare a goal for execution.</p>
     * <p>Goal is updated in the skel and the display of this engine.</p>
     *
     * @return True if new display is returned, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public final boolean wrapGoal()
            throws EngineException, EngineMessage {
        Object t = skel;
        BindCount[] d = display;
        EngineMessage.checkInstantiated(t);
        EngineWrap ew = enginewrap;
        if (ew == null) {
            ew = new EngineWrap();
            enginewrap = ew;
        }
        ew.countvar = 0;
        ew.flags = 0;
        ew.last = BindCount.DISPLAY_CONST;
        ew.countGoal(t, d, this);
        if ((ew.flags & EngineWrap.MASK_WRAP_CHNG) == 0) {
            skel = t;
            display = d;
            return false;
        }
        if ((ew.flags & EngineWrap.MASK_WRAP_MLTI) != 0)
            ew.last = BindCount.newBind(ew.countvar);
        ew.countvar = 0;
        skel = ew.replaceGoalAndWrap(t, d, this);
        display = ew.last;
        ew.last = BindCount.DISPLAY_CONST;
        return ((ew.flags & EngineWrap.MASK_WRAP_MLTI) != 0);
    }

    /**
     * <p>Implementation of the comparator interface.</p>
     *
     * @param m1 The first molec.
     * @param m2 The second molec.
     * @return The comparison result.
     */
    public final int compare(Object m1, Object m2) throws ArithmeticException {
        return SpecialLexical.compareTerm(AbstractTerm.getSkel(m1), AbstractTerm.getDisplay(m1),
                AbstractTerm.getSkel(m2), AbstractTerm.getDisplay(m2), this);
    }

}