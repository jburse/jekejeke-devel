package jekpro.reference.runtime;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides built-in predicates for qualified evaluation.</p>
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
public final class EvaluableQuali extends AbstractSpecial {
    private final static int EVALUABLE_COLON = 0;
    private final static int EVALUABLE_COLONCOLON = 1;

    /**
     * <p>Create an evaluable quali.</p>
     *
     * @param i The index.
     */
    public EvaluableQuali(int i) {
        super(i);
        switch (i) {
            case EVALUABLE_COLON:
                subflags |= MASK_DELE_VIRT;
                subflags |= MASK_DELE_ARIT;
                break;
            case EVALUABLE_COLONCOLON:
                subflags |= MASK_DELE_ARIT;
                break;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Arithmetically evaluate an evaluable.</p>
     * <p>The evaluable is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The result is passed via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public final boolean moniEvaluate(Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case EVALUABLE_COLON:
                SkelCompound temp = (SkelCompound) en.skel;
                Display ref = en.display;
                Object obj = SpecialQuali.slashToClass(temp.args[0], ref, false, true, en);
                SkelAtom mod = SpecialQuali.modToAtom(obj, temp.args[0], ref, en);
                SpecialQuali.colonToCallable(temp.args[1], ref, true, en);
                SpecialQuali.colonToRoutine(mod, temp.sym, true, en);
                return en.computeExpr(en.skel, en.display);
            case EVALUABLE_COLONCOLON:
                temp = (SkelCompound) en.skel;
                ref = en.display;

                en.skel = temp.args[0];
                en.display = ref;
                en.computeExpr(en.skel, en.display);
                Object recv = en.skel;
                Display d2 = en.display;

                obj = SpecialQuali.slashToClass(recv, d2, true, true, en);
                mod = SpecialQuali.objToAtom(obj, recv, d2, en);
                boolean ext = SpecialQuali.colonToCallable(temp.args[1], ref, true, en);
                SpecialQuali.colonToMethod(mod, temp.sym, recv, d2, true, ext, en);
                return en.computeExpr(en.skel, en.display);
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

}
