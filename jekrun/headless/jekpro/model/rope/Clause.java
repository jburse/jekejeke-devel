package jekpro.model.rope;

import jekpro.frequent.experiment.InterfaceReference;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.CallFrame;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelVar;
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
    public final static int MASK_CLAUSE_STOP = 0x00000020;
    public final static int MASK_CLAUSE_NHED = 0x00000040;
    public final static int MASK_CLAUSE_NOBR = 0x00000040;

    public int size;
    public int sizerule;
    public int[] intargs;
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
            flags |= Intermediate.MASK_INTER_NBDY;
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
        CallFrame u = en.contdisplay;
        Display d = u.disp;
        if ((flags & MASK_INTER_NBDY) == 0 &&
                (((d.flags & Display.MASK_DISP_MORE) != 0) ?
                u.number + 1 : u.number) >= en.number)
            d.lastCollect(en);

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
     * <p>Convert a body to intermediate form.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param body The term list, or null.
     * @param en   The engine.
     */
    public void bodyToInter(Object body, Engine en) {
        int f3 = 0;
        if ((flags & Intermediate.MASK_INTER_NLST) != 0)
            f3 |= Intermediate.MASK_INTER_NLST;
        if ((flags & Intermediate.MASK_INTER_NBDY) != 0)
            f3 |= Intermediate.MASK_INTER_NBDY;
        Goal back = null;
        while (body != null) {
            Object term = PreClause.bodyToGoal(body);
            if (term != null) {

                Goal goal = new Goal(term, f3);
                if (back == null) {
                    next = goal;
                } else {
                    back.next = goal;
                }
                back = goal;

            }
            body = PreClause.bodyToRest(body);
        }

        if (back == null) {
            next = this;
        } else {
            back.next = this;
            if ((flags & Clause.MASK_CLAUSE_STOP) == 0)
                back.flags |= Goal.MASK_GOAL_CEND;
        }
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
     * <p>Analyze a clause.</p>
     *
     * @param rule The rule.
     * @param body The body.
     */
    public void analyzeClause(Object rule, Object body) {
        Optimization[] vars = Optimization.createHelper(rule);

        if (vars.length != 0) {
            Optimization.setHead(term, this, vars);
            if (body != null)
                Optimization.setBody(body, vars);
            sizerule = Optimization.sortExtra(vars);
        }

        intargs = Optimization.unifyArgs(term, vars);
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
        Object val = PreClause.interToClause(this, en);
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
