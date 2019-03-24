package jekpro.frequent.advanced;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelCompound;
import matula.util.data.SetEntry;

/**
 * <p>Provides built-in predicates for the module sequence.</p>
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
public final class SpecialSequence extends AbstractSpecial {
    private final static int SPECIAL_PIVOT_NEW = 0;
    private final static int SPECIAL_PIVOT_SET = 1;
    private final static int SPECIAL_PIVOT_GET = 2;

    /**
     * <p>Create a sequence special.</p>
     *
     * @param i The id.
     */
    public SpecialSequence(int i) {
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
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case SPECIAL_PIVOT_NEW:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                if (!en.unifyTerm(temp[0], ref, new SetEntry(), Display.DISPLAY_CONST))
                    return false;
                return true;
            case SPECIAL_PIVOT_SET:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                SetEntry pivot = derefAndCastPivot(temp[0], ref);
                pivot.value = AbstractSkel.copySkel(temp[1], ref, en);
                return true;
            case SPECIAL_PIVOT_GET:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pivot = derefAndCastPivot(temp[0], ref);
                Object val = pivot.value;
                if (val == null)
                    return false;
                Display d = AbstractSkel.createMarker(val);
                boolean multi = d.getAndReset();
                if (!en.unifyTerm(temp[1], ref, val, d))
                    return false;
                if (multi)
                    BindUniv.remTab(d.bind, en);
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Cast a pivot.</p>
     *
     * @param m The term skel.
     * @param d The term display.
     * @return The pivot.
     * @throws EngineMessage Shit happens.
     */
    public static SetEntry derefAndCastPivot(Object m, Display d)
            throws EngineMessage {
        m = SpecialUniv.derefAndCastRef(m, d);
        if (m instanceof SetEntry) {
            return (SetEntry) m;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_REF, m), d);
        }
    }

}