package jekdev.reference.inspection;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.runtime.SpecialDynamic;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

/**
 * <p>This module provides built-ins for the module notation.</p>
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
public final class SpecialNotation extends AbstractSpecial {
    private final static int SPECIAL_SYS_SLASH_TO_MODULE = 0;
    private final static int SPECIAL_SYS_MODULE_TO_SLASH = 1;
    private final static int SPECIAL_SYS_COLON_TO_CALLABLE = 2;
    private final static int SPECIAL_SYS_CALLABLE_TO_COLON = 3;
    private final static int SPECIAL_SYS_COLON_TO_INDICATOR = 4;
    private final static int SPECIAL_SYS_INDICATOR_TO_COLON = 5;

    /**
     * <p>Create a base direct access builtin.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialNotation(int i) {
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
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case SPECIAL_SYS_SLASH_TO_MODULE:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Object obj = SpecialQuali.slashToClass(temp[0], ref, false, true, en);
                if (!en.unifyTerm(temp[1], ref, obj, Display.DISPLAY_CONST))
                    return false;
                return true;
            case SPECIAL_SYS_MODULE_TO_SLASH:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
                obj = SpecialDynamic.moduleToSlashSkel(sa.fun, src);
                if (!en.unifyTerm(temp[1], ref, obj, Display.DISPLAY_CONST))
                    return false;
                return true;
            case SPECIAL_SYS_COLON_TO_CALLABLE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                SpecialQuali.colonToCallable(temp[0], ref, true, en);
                Display d = en.display;
                boolean multi = d.getAndReset();
                if (!en.unifyTerm(temp[1], ref, en.skel, d))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
            case SPECIAL_SYS_CALLABLE_TO_COLON:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                sa = StackElement.callableToName(en.skel);
                obj = SpecialDynamic.callableToColonSkel(en.skel, (sa != null ? sa.scope : null), en);
                if (!en.unifyTerm(temp[1], ref, obj, en.display))
                    return false;
                return true;
            case SPECIAL_SYS_COLON_TO_INDICATOR:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                Integer arity = SpecialQuali.colonToIndicator(temp[0], ref, en);
                obj = new SkelCompound(new SkelAtom(Foyer.OP_SLASH), en.skel, arity);
                if (!en.unifyTerm(temp[1], ref, obj, Display.DISPLAY_CONST))
                    return false;
                return true;
            case SPECIAL_SYS_INDICATOR_TO_COLON:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                int arityint = StoreKey.derefAndCastIndicator(temp[0], ref, en);
                sa = (SkelAtom) en.skel;
                src = (sa.scope != null ? sa.scope : en.store.user);
                obj = SpecialQuali.indicatorToColonSkel(sa.fun, src, arityint, en);
                if (!en.unifyTerm(temp[1], ref, obj, Display.DISPLAY_CONST))
                    return false;
                return true;
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

}
