package jekpro.frequent.misc;

import jekpro.model.inter.Engine;
import jekpro.model.molec.AbstractUndo;
import jekpro.model.molec.UndoSerno;
import jekpro.tools.call.Interpreter;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;
import jekpro.tools.term.TermVar;

/**
 * <p>Provides the methods for the module misc/residue.</p>
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
public final class ForeignResidue {

    /**********************************************************/
    /* Attributed Variables Enumeration                       */
    /**********************************************************/

    /**
     * <p>Retrieve the current mark.</p>
     *
     * @param inter The interpreter.
     * @return The current mark.
     */
    public static AbstractUndo sysCurrentMark(Interpreter inter) {
        Engine en = inter.getEngine();
        return en.bind;
    }

    /**
     * <p>Retrieve the attributed variables from a mark.</p>
     *
     * @param inter The interpreter.
     * @param mark  The mark.
     * @return The list.
     */
    public static AbstractTerm sysMarkAttrs(Interpreter inter, AbstractUndo mark) {
        Knowledgebase know = inter.getKnowledgebase();
        AbstractTerm res = know.getTermNil();

        Engine en = inter.getEngine();
        for (AbstractUndo bind = en.bind; bind != mark; bind = bind.next) {
            if (!(bind instanceof UndoSerno))
                continue;
            TermVar var = ((UndoSerno) bind).getUniv().getAttr();
            if (var == null)
                continue;
            res = new TermCompound(inter, know.getTermCons(), var, res);
        }
        return res;
    }

}