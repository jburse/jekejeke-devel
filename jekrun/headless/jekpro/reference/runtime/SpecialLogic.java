package jekpro.reference.runtime;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.BindCount;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.rope.Clause;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides built-in predicates for logic predicates.</p>
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
public final class SpecialLogic extends AbstractSpecial {
    private final static int SPECIAL_SYS_LOCAL_CUT = 0;
    private final static int SPECIAL_SYS_SOFT_LOCAL_CUT = 1;
    private final static int SPECIAL_SYS_SAFE = 2;

    /**
     * <p>Create a logic special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialLogic(int i) {
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
     */
    public final boolean moniFirst(Engine en)
            throws EngineException {
        switch (id) {
            case SPECIAL_SYS_LOCAL_CUT:
                Display u = en.contdisplay;
                en.window = u;
                en.fault = null;
                en.cutChoices(u.number);
                en.window = null;
                if (en.fault != null)
                    throw en.fault;
                return en.getNextRaw();
            case SPECIAL_SYS_SOFT_LOCAL_CUT:
                u = en.contdisplay;
                if ((((u.flags & Display.MASK_DPCL_MORE) != 0) ?
                        u.number + 1 : u.number) >= en.number) {
                    en.window = u;
                    en.fault = null;
                    en.cutChoices(u.number);
                    en.window = null;
                    if (en.fault != null)
                        throw en.fault;
                } else {
                    u.flags |= Display.MASK_DPCL_SOFT;
                }
                return en.getNextRaw();
            case SPECIAL_SYS_SAFE:
                Object[] temp = ((SkelCompound) en.skel).args;
                BindCount[] ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                Clause clause = en.store.foyer.CLAUSE_CONT;
                Display ref2 = new Display();
                ref2.bind = BindCount.newBindClause(clause.dispsize);
                ref2.addArgument(en.skel, en.display, en);
                ref2.setEngine(en);
                en.contskel = clause.getNextRaw(en);
                en.contdisplay = ref2;
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

}