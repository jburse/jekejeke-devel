package jekpro.model.builtin;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.reflect.SpecialPred;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides built-in predicates for the module special.</p>
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
public final class SpecialSpecial extends AbstractSpecial {
    private final static int SPECIAL_SET_PREDICATE_PROPERTY = 0;
    private final static int SPECIAL_RESET_PREDICATE_PROPERTY = 1;
    private final static int SPECIAL_SYS_NEUTRAL_PREDICATE = 5;

    /**
     * <p>Create a special special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialSpecial(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
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
        try {
            switch (id) {
                case SPECIAL_SET_PREDICATE_PROPERTY:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    Predicate pick = SpecialPred.indicatorToPredicate(temp[0], ref, en);
                    Predicate.checkExistentPredicate(pick, temp[0], ref);

                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    EngineMessage.checkCallable(en.skel, en.display);
                    SpecialPred.addPredProp(en.skel, en.display, pick, en);
                    return true;
                case SPECIAL_RESET_PREDICATE_PROPERTY:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    pick = SpecialPred.indicatorToPredicate(temp[0], ref, en);
                    Predicate.checkExistentPredicate(pick, temp[0], ref);

                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    EngineMessage.checkCallable(en.skel, en.display);
                    SpecialPred.removePredProp(en.skel, en.display, pick, en);
                    return true;
                case SPECIAL_SYS_NEUTRAL_PREDICATE:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    Predicate.indicatorToPredicateDefined(temp[0],
                            ref, en, CachePredicate.MASK_CACH_CRTE);
                    return true;
                default:
                    throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
            }
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
        }
    }

}
