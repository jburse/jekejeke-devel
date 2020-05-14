package jekmin.reference.experiment;

import jekpro.frequent.standard.ChoiceAtomic;
import jekpro.frequent.standard.SpecialSignal;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Supervisor;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.tools.term.SkelCompound;
import matula.util.data.ListArray;

/**
 * <p>Provides built-ins for the module experiment/cont.</p>
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
public final class SpecialCont extends AbstractSpecial {
    private final static int SPECIAL_CONT_PUSH = 0;
    private final static int SPECIAL_CONT_POP = 1;
    private final static int SPECIAL_SYS_RIPPLE = 2;

    /**
     * <p>Create a suspension queue special.</p>
     *
     * @param i The id.
     */
    public SpecialCont(int i) {
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
            case SPECIAL_CONT_PUSH:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                BindUniv bv = new BindUniv();
                bv.bindUniv(en.skel, en.display, en);
                SpecialCont.pushCont(bv, en);
                return true;
            case SPECIAL_CONT_POP:
                SpecialCont.popCont(en);
                return true;
            case SPECIAL_SYS_RIPPLE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!SpecialSignal.invokeAtomic(en, ChoiceAtomic.MASK_FLAGS_VRFY))
                    return false;
                return true;
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /**********************************************************/
    /* Push/Pop Cont                                          */
    /**********************************************************/

    /**
     * <p>Push a goal on the suspension queue.</p>
     *
     * @param bv The bind var.
     * @param en The engine.
     */
    private static void pushCont(BindUniv bv, Engine en) {
        Supervisor visor = en.visor;
        if (visor.cont == null)
            visor.cont = new ListArray<BindUniv>();
        visor.cont.add(bv);
    }

    /**
     * <p>Pop a goal from the suspension queue.</p>
     *
     * @param en The engine.
     */
    private static void popCont(Engine en) {
        Supervisor visor = en.visor;
        visor.cont.remove(visor.cont.size() - 1);
        if (visor.cont.size() == 0)
            visor.cont = null;
    }

}
