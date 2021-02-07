package jekpro.frequent.standard;

import jekpro.frequent.experiment.SpecialRef;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Clause;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides built-in predicates for the module expand.</p>
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
 * Jekejek
 */
public final class SpecialExpand extends AbstractSpecial {
    private final static int SPECIAL_SYS_COMPILEZ = 0;
    private final static int SPECIAL_SYS_COMPILEZ_OPT = 1;

    /**
     * <p>Create a session special.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialExpand(int i) {
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
            case SPECIAL_SYS_COMPILEZ:
                AbstractDefined.enhanceKnowledgebase(AbstractDefined.OPT_PERF_CNLT |
                        AbstractDefined.OPT_ACTI_BOTT, en);
                return true;
            case SPECIAL_SYS_COMPILEZ_OPT:
                AbstractDefined.enhanceKnowledgebase(AbstractDefined.OPT_PERF_CNLT |
                        AbstractDefined.OPT_ACTI_BOTT | AbstractDefined.OPT_ARGS_ASOP, en);
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

}