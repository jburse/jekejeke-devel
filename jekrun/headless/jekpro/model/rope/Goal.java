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
 * <p>The class provides the term intermediate code.</p>
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

    public final int[] uniargs;

    /**
     * <p>Create a term.</p>
     *
     * @param t  The term.
     * @param u  The uniargs;
     * @param n  The next.
     * @param f3 The flags.
     */
    public Goal(Object t, int[] u, Intermediate n, int f3) {
        next = n;
        term = t;
        uniargs = u;
        if (t instanceof SkelVar)
            f3 |= Goal.MASK_GOAL_NAKE;
        flags = f3;
    }

    /**
     * <p>Resolve the current term.</p>
     *
     * @param en The engine.
     * @return The delegate.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public final boolean resolveNext(Engine en)
            throws EngineException, EngineMessage {
        DisplayClause u = en.contdisplay;

        if (uniargs != null)
            Goal.unifyBody(uniargs, u, en);
        if ((flags & Intermediate.MASK_INTER_NLST) == 0 &&
                (u.contskel.flags & Goal.MASK_GOAL_CEND) != 0) {
            DisplayClause u1 = u.contdisplay;
            if (u1 != null && u1.number >= en.number) {
                if ((u1.flags & DisplayClause.MASK_DPCL_LTGC) == 0) {
                    Clause clause = u1.def;
                    if ((clause.flags & Clause.MASK_CLAUSE_NBDY) == 0 && clause.dispsize > 0)
                        BindUniv.remTab(u1.bind, en);
                    u1.flags |= DisplayClause.MASK_DPCL_LTGC;
                }
                u.contskel = u1.contskel;
                u.contdisplay = u1.contdisplay;
            }
        }
        if (en.visor.signal != null &&
                (en.visor.flags & AbstractLivestock.MASK_LIVESTOCK_NOSG) == 0)
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        /* current term */
        Object alfa = term;
        Display d1 = u;
        if ((flags & Goal.MASK_GOAL_NAKE) != 0) {
            /* inlined deref */
            BindUniv b;
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
     * <p>Unify the term of the clause with the given term.</p>
     * <p>Only the deferred variables are instantiated.</p>
     *
     * @param u  The continuation display.
     * @param en The engine.
     */
    private static void unifyBody(int[] arr, DisplayClause u, Engine en) {
        Intermediate ir = u.contskel;
        Object alfa = ir.term;
        Display ref = u.contdisplay;
        if ((ir.flags & Goal.MASK_GOAL_NAKE) != 0) {
            /* inlined deref */
            BindUniv bc;
            while (alfa instanceof SkelVar &&
                    (bc = ref.bind[((SkelVar) alfa).id]).display != null) {
                alfa = bc.skel;
                ref = bc.display;
            }
        }
        Object[] t1 = ((SkelCompound) alfa).args;
        Object[] t2 = ((SkelCompound) u.def.term).args;
        BindUniv[] b = u.bind;
        for (int i = 0; i < arr.length; i++) {
            int k = arr[i];
            alfa = t1[k];
            Display d1 = ref;
            BindUniv bc;
            while (alfa instanceof SkelVar &&
                    (bc = d1.bind[((SkelVar) alfa).id]).display != null) {
                alfa = bc.skel;
                d1 = bc.display;
            }
            bc = b[((SkelVar) t2[k]).id];
            bc.bindUniv(alfa, d1, en);
        }
    }

}
