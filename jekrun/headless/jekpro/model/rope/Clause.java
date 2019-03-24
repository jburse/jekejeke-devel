package jekpro.model.rope;

import jekpro.frequent.experiment.InterfaceReference;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.DisplayClause;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelVar;
import matula.util.data.ListArray;
import matula.util.data.MapHashLink;

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
    public final static int MASK_CLAUSE_NOBR = 0x00000800;

    public int dispsize;
    public int[] intargs;
    public int size;
    public AbstractDefined del;
    public MapHashLink<String, SkelVar> vars;

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
        if ((copt & AbstractDefined.MASK_DEFI_NHED) != 0)
            flags |= Clause.MASK_CLAUSE_NHED;
        if ((copt & AbstractDelegate.MASK_DELE_NOBR) != 0)
            flags |= Clause.MASK_CLAUSE_NOBR;
    }

    /**
     * <p>Move to continuation.</p>
     *
     * @param en The engine.
     * @return Always true.
     */
    public final boolean resolveNext(Engine en) {
        DisplayClause u = en.contdisplay;
        if ((((u.flags & DisplayClause.MASK_DPCL_MORE) != 0) ?
                u.number + 1 : u.number) >= en.number) {
            if ((u.flags & DisplayClause.MASK_DPCL_LTGC) == 0) {
                if ((flags & Clause.MASK_CLAUSE_NBDY) == 0 && dispsize > 0)
                    u.remTab(en);
                u.flags |= DisplayClause.MASK_DPCL_LTGC;
            }
        }

        if ((flags & Clause.MASK_CLAUSE_STOP) != 0) {
            en.contskel = null;
            en.contdisplay = null;
        } else {
            en.contskel = u.contskel;
            en.contdisplay = u.contdisplay;
        }
        return true;
    }

    /***********************************************************/
    /* Convert To Intermediate                                 */
    /***********************************************************/

    /**
     * <p>Convert a vector of goals to a list of goals.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param vec  The term list.
     * @param vars The helper.
     * @param en   The engine.
     */
    protected void vectorToList(ListArray<Object> vec,
                                OptimizationVar[] vars,
                                Engine en) {
        Intermediate end = this;
        if (vec == null) {
            next = end;
            return;
        }
        int f2 = 0;
        if ((flags & MASK_INTER_NLST) != 0)
            f2 |= MASK_INTER_NLST;
        int f3 = 0;
        if ((flags & Clause.MASK_CLAUSE_STOP) == 0)
            f3 |= Goal.MASK_GOAL_CEND;
        int i = vec.size() - 1;
        for (; i >= 0; i--) {
            Object t = vec.get(i);
            int[] args = OptimizationArray.tempBind(t, i, vars);

            /* normal code */
            end = new Goal(t, args, end, f2 | f3);

            if (args != null)
                f2 |= MASK_INTER_NLST;
            f3 &= ~Goal.MASK_GOAL_CEND;
        }
        next = end;
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
        /* create the helper */
        OptimizationVar[] vars = OptimizationVar.createHelper(molec);
        size = vars.length;

        /* mark the helper */
        ListArray<Object> body = PreClause.clauseToBody(molec);
        if (vars.length != 0) {
            /* analyze the variables */
            OptimizationVar.setStructureAndMinArg(term, this, vars);
            if (body != null) {
                for (int i = 0; i < body.size(); i++)
                    OptimizationVar.setMaxGoal(body.get(i), i, vars);
                for (int i = body.size() - 1; i >= 0; i--)
                    OptimizationVar.setMinBody(body.get(i), i, vars);
            }
        }
        dispsize = OptimizationArray.sortAndDisplaceMinGoal(vars);

        /* build the clause */
        vectorToList(body, vars, en);
        intargs = OptimizationArray.unifyArgs(term, vars);
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
                    this), Display.DISPLAY_CONST);
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
                    this), Display.DISPLAY_CONST);
        return del.retractClause(this, en);
    }


    /**
     * <p>Clause this reference.</p>
     * <p>The result is returned in the skeleton and display.</p>
     *
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public void clauseRef(Engine en)
            throws EngineMessage {
        Object val = PreClause.intermediateToClause(this, en);
        en.skel = val;
        en.display = AbstractSkel.createMarker(val);
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
