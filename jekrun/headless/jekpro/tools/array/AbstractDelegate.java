package jekpro.tools.array;

import jekpro.frequent.standard.EngineCopy;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>This is the base class for all predicate delegates. It also
 * alreaddy implements the automatic tunneling of predicates to
 * evaluable functions, and the automatic bridging from evaluable
 * functions to predicates.</p>
 * <p>The following flags are provided:</p>
 * <ul>
 * <li><b>MASK_DELE_VIRT:</b> The delegate expects a receiver
 * object in the first argument.</li>
 * <li><b>MASK_DELE_ARIT:</b> The delegate primarily implements
 * an evaluable function.</li>
 * <li><b>MASK_DELE_MULT:</b> The delegate implements
 * a multifile predicate.</li>
 * </ul>
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
public abstract class AbstractDelegate {
    public final static int MASK_DELE_VIRT = 0x00000001;
    public final static int MASK_DELE_ARIT = 0x00000002;
    public final static int MASK_DELE_MULT = 0x00000004;
    public final static int MASK_DELE_NOBR = 0x00000008;

    public int subflags;

    /**
     * <p>Promote a predicate to a builtin.</p>
     *
     * @param pick The predicate.
     * @param del  The builtin delegate.
     * @return The promotion result.
     */
    public static AbstractDelegate promoteBuiltin(Predicate pick,
                                                  AbstractDelegate del) {
        AbstractDelegate fun = pick.del;
        if (fun != null)
            return fun;
        synchronized (pick) {
            fun = pick.del;
            if (fun != null)
                return fun;
            pick.del = del;
        }
        return del;
    }

    /**
     * <p>Shrink this predicate from the store for a source.</p>
     *
     * @param pick  The predicate.
     * @param scope The source.
     * @throws EngineMessage Shit happens.
     */
    public abstract void shrinkPredicate(Predicate pick, AbstractSource scope)
            throws EngineMessage;

    /**
     * <p>Release this predicate from the store.</p>
     *
     * @param pick The predicate.
     */
    public abstract void releasePredicate(Predicate pick);

    /********************************************************************/
    /* Tunneling Evaluable Functions                                    */
    /********************************************************************/

    /**
     * <p>Delegate a predicate.</p>
     * <p>The term is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the contskel and contdisplay of the engine.</p>
     * <p>The new continuation is returned via the contskel and contdisplay of the engine.</p>
     *
     * @param en The interpreter.
     * @return True if the term succeeded, otherwise false.
     * @throws EngineMessage   FFI error.
     * @throws EngineException FFI error.
     */
    public boolean moniFirst(Engine en)
            throws EngineException, EngineMessage {
        SkelCompound temp = (SkelCompound) en.skel;
        Display ref = en.display;

        Object expr = AbstractDelegate.tunnelArgs(temp);
        en.computeExpr(expr, ref);
        Display d = en.display;
        boolean multi = d.getAndReset();
        if (!en.unifyTerm(temp.args[temp.args.length - 1], ref, en.skel, d))
            return false;
        if (multi)
            BindUniv.remTab(d.bind, en);
        return true;
    }

    /**
     * <p>Check and shrink the arguments.</p>
     *
     * @param temp The term skeleton.
     * @return The arguments.
     */
    private static Object tunnelArgs(SkelCompound temp) {
        int n = temp.args.length - 1;
        if (n > 0) {
            Object[] args = new Object[n];
            System.arraycopy(temp.args, 0, args, 0, n);
            return new SkelCompound(temp.sym, args);
        } else {
            return temp.sym;
        }
    }

    /********************************************************************/
    /* Bridging Predicates                                              */
    /********************************************************************/

    /**
     * <p>Delegate an evaluable function.</p>
     * <p>The evaluable is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the contskel and contdisplay of the engine.</p>
     * <p>The result is passed via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @throws EngineMessage   FFI error.
     * @throws EngineException FFI error.
     */
    public void moniEvaluate(Engine en)
            throws EngineMessage, EngineException {
        Object temp = en.skel;
        Display ref = en.display;

        Object[] args = computeArgs(temp, ref, en);
        SkelAtom sa = StackElement.callableToName(temp);
        ref = bridgeCount(args);
        en.display = ref;
        en.skel = bridgeAlloc(sa, args, ref, en);
        temp = args[args.length - 1];

        AbstractDelegate.invokeOther(en);

        ref.flags |= Display.MASK_DPTM_MLTI;
        en.display = ref;
        en.skel = temp;
    }

    /**
     * <p>Build the arguments array. The arguments of the
     * term are computed if necessary and packed.</p>
     *
     * @param temp The skeleton.
     * @param ref  The display.
     * @param en   The engine.
     * @return The arguments array.
     * @throws EngineMessage FFI error.
     */
    private Object[] computeArgs(Object temp, Display ref,
                                 Engine en)
            throws EngineException, EngineMessage {
        if (!(temp instanceof SkelCompound))
            return new Object[1];
        Object[] help = ((SkelCompound) temp).args;
        Object[] args = new Object[help.length + 1];
        int i = 0;
        if ((subflags & MASK_DELE_VIRT) != 0) {
            en.skel = help[0];
            en.display = ref;
            en.deref();
            args[i] = AbstractTerm.createMolec(en.skel, en.display);
            i++;
        }
        for (; i < help.length; i++) {
            en.computeExpr(help[i], ref);
            args[i] = AbstractTerm.createMolec(en.skel, en.display);
        }
        return args;
    }

    /**
     * <p>Count the needed variable place holders.</p>
     *
     * @param args The computed arguments.
     * @return The count.
     */
    private static Display bridgeCount(Object[] args) {
        int countvar = 0;
        int n = args.length - 1;
        for (int i = 0; i < n; i++) {
            Object temp = AbstractTerm.getSkel(args[i]);
            if (EngineCopy.getVar(temp) != null)
                countvar++;
        }
        return new Display(countvar + 1);
    }

    /**
     * <p>Unpack the arguments and bind the needed variable
     * place holders.</p>
     *
     * @param sa   The symbol.
     * @param args The computed arguments.
     * @param ref  The new display.
     * @param en   The engine.
     */
    private static SkelCompound bridgeAlloc(SkelAtom sa,
                                            Object[] args, Display ref,
                                            Engine en) {
        SkelVar[] vars = SkelVar.valueOfArray(ref.bind.length);
        int countvar = 0;
        int n = args.length - 1;
        for (int i = 0; i < n; i++) {
            Object obj = args[i];
            Object temp = AbstractTerm.getSkel(obj);
            if (EngineCopy.getVar(temp) != null) {
                Display d = AbstractTerm.getDisplay(obj);
                SkelVar sv = vars[countvar];
                countvar++;
                boolean ext = d.getAndReset();
                ref.bind[sv.id].bindUniv(temp, d, en);
                if (ext)
                    BindUniv.remTab(d.bind, en);
                args[i] = sv;
            } else {
                args[i] = temp;
            }
        }
        args[n] = vars[countvar];
        return new SkelCompound(sa, args, vars);
    }

    /**
     * <p>Invoke the other predicate and remove choice points.</p>
     *
     * @param en The engine.
     * @throws EngineException FFI error.
     * @throws EngineMessage   FFI error.
     */
    private static void invokeOther(Engine en)
            throws EngineException, EngineMessage {
        Intermediate r = en.contskel;
        DisplayClause u = en.contdisplay;
        int snap = en.number;
        boolean multi = en.wrapGoal();
        Display ref = en.display;
        Clause clause = en.store.foyer.CLAUSE_CALL;
        DisplayClause ref2 = new DisplayClause(clause.dispsize);
        ref2.def = clause;
        ref2.bind[0].bindUniv(en.skel, ref, en);
        if (multi)
            BindUniv.remTab(ref.bind, en);
        ref2.setEngine(en);
        en.contskel = clause;
        en.contdisplay = ref2;
        if (!en.runLoop(snap, true))
            throw new EngineMessage(EngineMessage.evaluationError(
                    EngineMessage.OP_EVALUATION_PARTIAL_FUNCTION));
        en.contskel = r;
        en.contdisplay = u;
        en.window = null;
        en.fault = null;
        en.cutChoices(snap);
        if (en.fault != null)
            throw en.fault;
    }

    /***************************************************************/
    /* Special Predicates                                          */
    /***************************************************************/

    /**
     * <p>Compute a hash code.</p>
     *
     * @return The hash code of this delegate.
     */
    public abstract int hashCode();

    /**
     * <p>Compare with another delegate.</p>
     *
     * @param o The other delegate.
     * @return True if this delegate equals the other delegate, otherwise false.
     */
    public abstract boolean equals(Object o);

    /**
     * <p>Generate the spec of this delegate.</p>
     *
     * @param source The source, not null.
     * @return The spec.
     * @throws EngineMessage FFI error.
     */
    public abstract Object toSpec(AbstractSource source)
            throws EngineMessage;

    /***************************************************************/
    /* Foreign Predicates                                          */
    /***************************************************************/

    /**
     * <p>Retrieve a name guess.</p>
     *
     * @return The name guess, or null.
     */
    public abstract String getFun();

    /**
     * <p>Retrieve an length guess.</p>
     *
     * @return The length guess, or -1.
     */
    public abstract int getArity();

}