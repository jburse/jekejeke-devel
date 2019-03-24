package jekpro.model.builtin;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.rope.Clause;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides built-in predicates for body conversion.</p>
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
public final class SpecialBody extends AbstractSpecial {
    private final static int SPECIAL_CALL = 0;

//    private final static int SPECIAL_WRAP_GOAL = 4;

    /**
     * <p>Create a body special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialBody(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a term in a list of goals for the first time.</p>
     * <p>The term is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineException, EngineMessage {
        switch (id) {
            case SPECIAL_CALL:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                boolean multi = en.wrapGoal();
                ref = en.display;
                Clause clause = en.store.foyer.CLAUSE_CONT;
                DisplayClause ref2 = new DisplayClause(clause.dispsize);
                ref2.def = clause;
                ref2.bind[0].bindUniv(en.skel, ref, en);
                if (multi)
                    BindUniv.remTab(ref.bind, en);
                ref2.setEngine(en);
                en.contskel = clause;
                en.contdisplay = ref2;
                return true;
            /*
            case SPECIAL_WRAP_GOAL:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                en.wrapGoalSite();
                if (en.unifyTerm(en.skel,en.display,temp[1],ref,r,u)) {
                    en.skel = r.getNext(en);
                    en.display = u;
                    return true;
                }
                return false;
            */
            default:
                throw new IllegalArgumentException(
                        AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /*************************************************************/
    /* Replace Context & Site                                    */
    /*************************************************************/


}
