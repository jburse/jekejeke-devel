package jekpro.tools.array;

import jekpro.frequent.standard.EngineCopy;
import jekpro.model.builtin.SpecialBody;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.DisplayClause;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Goal;
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public abstract class AbstractDelegate {
    public final static int MASK_DELE_VIRT = 0x00000001;
    public final static int MASK_DELE_ARIT = 0x00000002;
    public final static int MASK_DELE_MULT = 0x00000004;

    public int subflags;

    /**
     * <p>Promote a predicate to a builtin.</p>
     *
     * @param pick The predicate.
     * @param d    The builtin delegate.
     */
    public static void promoteBuiltin(Predicate pick,
                                      AbstractDelegate d) {
        if (pick.del != null)
            return;
        synchronized (pick) {
            if (pick.del != null)
                return;
            pick.del = d;
        }
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
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the contskel and contdisplay of the engine.</p>
     * <p>The new continuation is returned via the contskel and contdisplay of the engine.</p>
     *
     * @param en The interpreter.
     * @return True if the goal succeeded, otherwise false.
     * @throws EngineMessage   FFI error.
     * @throws EngineException FFI error.
     */
    public boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        SkelCompound temp = (SkelCompound) en.skel;
        Display ref = en.display;

        Object expr = AbstractDelegate.tunnelArgs(temp);
        boolean multi = en.computeExpr(expr, ref);
        Display d = en.display;
        if (!en.unifyTerm(temp.args[temp.args.length - 1], ref, en.skel, d))
            return false;
        if (multi)
            d.remTab(en);
        return en.getNext();
    }

    /**
     * <p>Check and shrink the arguments.</p>
     *
     * @param temp The goal skeleton.
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
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The result is passed via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @throws EngineMessage   FFI error.
     * @throws EngineException FFI error.
     */
    public boolean moniEvaluate(Engine en)
            throws EngineMessage, EngineException {
        Object temp = en.skel;
        Display ref = en.display;

        Object[] args = computeArgs(temp, ref, en);
        SkelAtom sa = SpecialBody.callableToName(temp);
        ref = bridgeCount(args);
        en.display = ref;
        en.skel = bridgeAlloc(sa, args, ref, en);
        temp = args[args.length - 1];

        AbstractDelegate.invokeOther(en);

        en.display = ref;
        en.skel = temp;
        return true;
    }

    /**
     * <p>Build the arguments array. The arguments of the
     * goal are computed if necessary and packed.</p>
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
            boolean multi = en.computeExpr(help[i], ref);
            Display ref2 = en.display;
            Object val = AbstractTerm.createMolec(en.skel, ref2);
            if (EngineCopy.getVar(en.skel) != null)
                AbstractTerm.setMarker(val, Boolean.valueOf(multi));
            args[i] = val;
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
                Display ref2 = AbstractTerm.getDisplay(obj);
                boolean multi = ((Boolean) AbstractTerm.getMarker(obj)).booleanValue();
                SkelVar sv = vars[countvar];
                countvar++;
                ref.bind[sv.id].bindVar(temp, ref2, en);
                if (multi)
                    ref2.remTab(en);
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
        Goal r = (Goal) en.contskel;
        DisplayClause u = en.contdisplay;
        int snap = en.number;
        boolean multi = en.wrapGoal();
        Display ref = en.display;
        Clause clause = en.store.foyer.CLAUSE_CALL;
        DisplayClause ref2 = new DisplayClause(clause.dispsize);
        ref2.addArgument(en.skel, ref, en);
        if (multi)
            ref.remTab(en);
        ref2.setEngine(en);
        en.contskel = clause.getNextRaw(en);
        en.contdisplay = ref2;
        if (!en.runFirst(snap))
            throw new EngineMessage(EngineMessage.evaluationError(
                    EngineMessage.OP_EVALUATION_PARTIAL_FUNCTION));
        en.contskel = r;
        en.contdisplay = u;
        en.skel = null;
        en.display = null;
        en.cutChoices(snap);
        if (en.skel != null)
            throw (EngineException) en.skel;
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
     * @param source The source.
     * @param en     The engine.
     * @return The spec.
     * @throws EngineMessage FFI error.
     */
    public abstract Object toSpec(AbstractSource source, Engine en)
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