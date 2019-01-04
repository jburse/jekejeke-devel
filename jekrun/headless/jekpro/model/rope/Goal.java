package jekpro.model.rope;

import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.*;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.wire.AbstractLivestock;

/**
 * <p>The class provides the goal intermediate code.</p>
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
public class Goal extends Intermediate {
    public final static int MASK_GOAL_NAKE = 0x00000010;
    public final static int MASK_GOAL_CEND = 0x00000020;

    public final Object goal;
    public final int[] uniargs;
    public final int endalloc;
    public final Clause def;
    public int endgc;

    /**
     * <p>Create a goal.</p>
     *
     * @param t  The goal.
     * @param u  The uniargs;
     * @param n  The next.
     * @param f2 The endalloc.
     * @param f3 The flags.
     * @param c  The parent clause.
     */
    public Goal(Object t, int[] u, Intermediate n,
                int f2, int f3, Clause c) {
        next = n;
        goal = t;
        uniargs = u;
        endalloc = f2;
        if (t instanceof SkelVar)
            f3 |= Goal.MASK_GOAL_NAKE;
        flags = f3;
        def = c;
    }

    /**
     * <p>Retrieve the clause.</p>
     *
     * @return The clause.
     */
    public final Clause getClause() {
        return def;
    }

    /**
     * <p>Resolve the current goal.</p>
     *
     * @param en The engine.
     * @return The delegate.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public final boolean resolveNext(Engine en)
            throws EngineException, EngineMessage {
        DisplayClause u = en.contdisplay;
        if ((((u.flags & DisplayClause.MASK_DPCL_MORE) != 0) ?
                u.number + 1 : u.number) >= en.number) {
            int n = endgc;
            int i = u.lastgc;
            if (i < n)
                u.lastgc = def.disposeBind(i, n, u.bind, en);
        }

        int n = endalloc;
        int i = u.lastalloc;
        if (i < n)
            u.lastalloc = Engine.newBind(i, n, u.bind);

        if (uniargs != null)
            unifyBody(u, en);
        if ((flags & Intermediate.MASK_INTER_NLST) == 0) {
            DisplayClause u1 = u.contdisplay;
            if (u1 != null &&
                    (u.contskel.flags & Goal.MASK_GOAL_CEND) != 0 &&
                    u1.number >= en.number) {
                Clause clause = u.contskel.getClause();
                n = ((clause.flags & Clause.MASK_CLAUSE_NBDY) != 0 ? 0 : clause.dispsize);
                i = u1.lastgc;
                if (i < n)
                    u1.lastgc = clause.disposeBind(i, n, u1.bind, en);
                u.contskel = u1.contskel;
                u.contdisplay = u1.contdisplay;
            }
        }
        if (en.visor.signal != null &&
                (en.visor.flags & AbstractLivestock.MASK_LIVESTOCK_NOSG) == 0)
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        /* current goal */
        Object alfa = goal;
        Display d1 = u;
        if ((flags & Goal.MASK_GOAL_NAKE) != 0) {
            /* inlined deref */
            BindVar b;
            while (alfa instanceof SkelVar &&
                    (b = d1.bind[((SkelVar) alfa).id]).display != null) {
                alfa = b.skel;
                d1 = b.display;
            }
        }
        CachePredicate cp;
        if (alfa instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) alfa;
            cp = CachePredicate.getPredicate(sc.sym, sc.args.length, en);
        } else if (alfa instanceof SkelAtom) {
            SkelAtom sa = (SkelAtom) alfa;
            cp = CachePredicate.getPredicate(sa, 0, en);
        } else {
            EngineMessage.checkInstantiated(alfa);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_CALLABLE, alfa));
        }
        if (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0) {
            SkelAtom sa = StackElement.callableToName(alfa);
            int arity = StackElement.callableToArity(alfa);
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_PROCEDURE,
                    SpecialQuali.indicatorToColonSkel(sa, arity, en)));
        }
        AbstractDelegate fun = cp.pick.del;
        if (fun == null) {
            SkelAtom sa = StackElement.callableToName(alfa);
            int arity = StackElement.callableToArity(alfa);
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_BODY,
                    SpecialQuali.indicatorToColonSkel(sa, arity, en)));
        }
        en.skel = alfa;
        en.display = d1;
        return fun.moniFirst(en);
    }

    /***********************************************************/
    /* Unify Helper                                            */
    /***********************************************************/

    /**
     * <p>Unify the head of the clause with the given goal.</p>
     * <p>Only the deferred variables are instantiated.</p>
     *
     * @param u  The continuation display.
     * @param en The engine.
     */
    private void unifyBody(DisplayClause u, Engine en) {
        Goal ir = (Goal) u.contskel;
        Object alfa = ir.goal;
        Display ref = u.contdisplay;
        if ((ir.flags & Goal.MASK_GOAL_NAKE) != 0) {
            /* inlined deref */
            BindVar b;
            while (alfa instanceof SkelVar &&
                    (b = ref.bind[((SkelVar) alfa).id]).display != null) {
                alfa = b.skel;
                ref = b.display;
            }
        }
        Object[] t1 = ((SkelCompound) alfa).args;
        Object[] t2 = ((SkelCompound) def.head).args;
        int[] arr = uniargs;
        for (int i = 0; i < arr.length; i++) {
            int n = arr[i];
            alfa = t1[n];
            Display d1 = ref;
            BindVar b;
            while (alfa instanceof SkelVar &&
                    (b = d1.bind[((SkelVar) alfa).id]).display != null) {
                alfa = b.skel;
                d1 = b.display;
            }
            b = u.bind[((SkelVar) t2[n]).id];
            b.bindVar(alfa, d1, en);
        }
    }

}
