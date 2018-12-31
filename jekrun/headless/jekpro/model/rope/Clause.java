package jekpro.model.rope;

import jekpro.frequent.experiment.InterfaceReference;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.BindCount;
import jekpro.model.molec.BindVar;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import matula.util.data.ListArray;

/**
 * <p>The class provides the clause intermediate code.</p>
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
public class Clause extends Intermediate implements InterfaceReference {
    public final static int MASK_CLAUSE_ASSE = 0x00000010;

    public final static int MASK_CLAUSE_STOP = 0x00000100;
    public final static int MASK_CLAUSE_NBDY = 0x00000200;
    public final static int MASK_CLAUSE_NHED = 0x00000400;

    public Object head;
    public int dispsize;
    public int[] intargs;
    public int[] remtab;
    public int size;
    public AbstractDefined del;
    public Named[] vars;
    public int endgc;

    /**
     * <p>Create a clause.</p>
     *
     * @param copt The clause option flags.
     */
    public Clause(int copt) {
        if ((copt & AbstractDefined.MASK_DEFI_NLST) != 0)
            flags |= Intermediate.MASK_INTER_NLST;
        if ((copt & AbstractDefined.MASK_DEFI_STOP) != 0)
            flags |= Clause.MASK_CLAUSE_STOP;
        if ((copt & AbstractDefined.MASK_DEFI_NBDY) != 0)
            flags |= Clause.MASK_CLAUSE_NBDY;
        if ((copt & AbstractDefined.MASK_DEFI_NHWK) != 0)
            flags |= Intermediate.MASK_INTER_MUTE;
        if ((copt & AbstractDefined.MASK_DEFI_NHED) != 0)
            flags |= Clause.MASK_CLAUSE_NHED;
    }

    /**
     * <p>Retrieve the clause.</p>
     *
     * @return The clause.
     */
    public final Clause getClause() {
        return this;
    }

    /**
     * <p>Move to continuation.</p>
     *
     * @param en The engine.
     * @return Always true.
     */
    public final boolean resolveNext(Engine en) {
        Display u = en.contdisplay;
        if ((((u.flags & Display.MASK_DPCL_MORE) != 0) ?
                u.number + 1 : u.number) >= en.number) {
            int n = endgc;
            int i = u.lastgc;
            if (i < n)
                u.lastgc = disposeBind(i, n, u.bind, en);
        }

        if ((flags & Clause.MASK_CLAUSE_STOP) != 0) {
            en.contskel = null;
            en.contdisplay = null;
            return true;
        } else {
            en.contskel = u.contskel;
            en.contdisplay = u.contdisplay;
            return en.getNextRaw();
        }
    }

    /***********************************************************/
    /* Dispose Helper                                          */
    /***********************************************************/

    /**
     * <p>Dispose bind.</p>
     *
     * @param i  The current last gc.
     * @param n  The max last gc.
     * @param b  The display clause.
     * @param en The engine.
     */
    public final int disposeBind(int i, int n,
                                 BindCount[] b, Engine en) {
        do {
            int k = remtab[i];
            BindCount bc = b[k];
            if ((--bc.refs) == 0) {
                b[k] = null;
                if (bc.display != null)
                    BindVar.unbind(bc, en);
            }
            i++;
        } while (i < n);
        return i;
    }

    /***********************************************************/
    /* Convert To Intermediate                                 */
    /***********************************************************/

    /**
     * <p>Convert a vector of goals to a list of goals.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param vec    The goal list.
     * @param vars   The helper.
     * @param remtab The removal tab.
     * @param en     The engine.
     * @return The remaining display size.
     */
    protected int vectorToList(ListArray<Object> vec,
                               OptimizationVar[] vars, int[] remtab,
                               Engine en) {
        Intermediate end = this;
        if ((flags & Clause.MASK_CLAUSE_NBDY) == 0)
            ((Clause) end).endgc = dispsize;
        if (vec == null) {
            next = end;
            return dispsize;
        }
        int f2 = 0;
        if ((flags & MASK_INTER_NLST) != 0)
            f2 |= MASK_INTER_NLST;
        if ((flags & MASK_INTER_MUTE) != 0)
            f2 |= MASK_INTER_MUTE;
        int f3 = 0;
        if ((flags & Clause.MASK_CLAUSE_STOP) == 0)
            f3 |= Goal.MASK_GOAL_CEND;
        int i = vec.size() - 1;
        int size = dispsize;
        int size2 = dispsize;
        for (; i >= 0; i--) {
            size = OptimizationArray.sweepMaxGoal(i, false, size, vars, remtab);
            Object t = vec.get(i);
            int[] args = OptimizationArray.tempBind(t, i, vars);

            /* normal code */
            end = new Goal(t, args, end, size2, f2 | f3, this);
            if ((flags & Clause.MASK_CLAUSE_NBDY) == 0)
                ((Goal) end).endgc = size;

            size2 = OptimizationArray.sweepMinGoal(i, -1, size2, vars);
            if (args != null)
                f2 |= MASK_INTER_NLST;
            f3 &= ~Goal.MASK_GOAL_CEND;
        }
        next = end;
        return size2;
    }

    /**
     * <p>Create a clause.</p>
     *
     * @param copt The clause option flags.
     * @param en   The engine.
     * @return The clause.
     */
    public static Clause createClause(int copt, Engine en) {
        if ((copt & AbstractDefined.MASK_DEFI_NIST) == 0) {
            return en.store.foyer.createClause(copt);
        } else {
            return new Clause(copt);
        }
    }

    /**
     * <p>Analyze the clause.</p>
     *
     * @param molec The term.
     * @param en    The engine or null.
     */
    public void analyzeBody(Object molec, Engine en) {
        /* detect the veriables and the body */
        OptimizationVar[] vars = OptimizationVar.createHelper(molec);
        size = vars.length;
        ListArray<Object> body = PreClause.clauseToBody(molec);

        if (vars.length != 0) {
            /* analyze the variables */
            OptimizationVar.setStructureAndMinArg(head, this, vars);
            if (body != null) {
                for (int i = 0; i < body.size(); i++)
                    OptimizationVar.setMaxGoal(body.get(i), i, vars);
                for (int i = body.size() - 1; i >= 0; i--)
                    OptimizationVar.setMinBody(body.get(i), i, vars);
            }

            if (vars.length != 1) {
                /* shuffle the variables */
                OptimizationArray.sortAndDisplaceMinGoal(vars);
                remtab = OptimizationArray.sortMaxGoal(vars);
            } else {
                remtab = OptimizationArray.zeroInt(1);
            }
        } else {
            remtab = OptimizationArray.zeroInt(0);
        }

        dispsize = OptimizationArray.sweepMaxGoal(-1, true, size, vars, remtab);

        /* build the clause */
        int k = vectorToList(body, vars, remtab, en);
        intargs = OptimizationArray.unifyArgs(head, k, vars);
    }

    /**********************************************************/
    /* InterfaceReference Protocol                            */
    /**********************************************************/

    /**
     * <p>Assert this reference.</p>
     *
     * @param flags The flags.
     * @param en    The engine.
     * @return True if the reference was asserted, otherwise false.
     */
    public boolean assertRef(int flags, Engine en)
            throws EngineMessage {
        if (del == null)
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_DIRECTIVE,
                    this), BindCount.DISPLAY_CONST);
        return del.assertClause(this, flags, en);
    }

    /**
     * <p>Retract this reference.</p>
     *
     * @param en The engine.
     * @return True if the reference was retracted, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean retractRef(Engine en)
            throws EngineMessage {
        if (del == null)
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_DIRECTIVE,
                    this), BindCount.DISPLAY_CONST);
        return del.retractClause(this, en);
    }


    /**
     * <p>Clause this reference.</p>
     * <p>The result is returned in the skeleton and display.</p>
     *
     * @param en The engine.
     * @return True if new display is returned, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean clauseRef(Engine en)
            throws EngineMessage {
        en.skel = PreClause.intermediateToClause(head, next, en);
        en.display = (size != 0 ? BindCount.newBind(size) : BindCount.DISPLAY_CONST);
        return (size != 0);
    }

    /**************************************************************/
    /* Accessibility Checks                                       */
    /**************************************************************/

    /**
     * <p>Check whether a source is inside the ancestors.</p>
     *
     * @param alfa The source.
     * @param en   The engine.
     * @return True if the source is inside the ancestors, false otherwise.
     */
    public static boolean ancestorSource(AbstractSource alfa, Engine en) {
        return (alfa == null ||
                en.store.ancestorStore(alfa.getStore()));
    }

}
