package jekpro.reference.runtime;

import jekpro.frequent.basic.SpecialProxy;
import jekpro.frequent.standard.SupervisorCall;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.rope.Directive;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.PositionKey;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides built-in predicates for the module logic.</p>
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
    private final static int SPECIAL_CALL_COLON = 0;
    private final static int SPECIAL_CALL_COLONCOLON = 1;
    private final static int SPECIAL_SYS_REPLACE_SITE = 2;

    /**
     * <p>Create a logic special.</p>
     *
     * @param i The id.
     */
    public SpecialLogic(int i) {
        super(i);
        switch (i) {
            case SPECIAL_CALL_COLON:
            case SPECIAL_CALL_COLONCOLON:
                subflags |= MASK_DELE_VIRT;
                break;
            case SPECIAL_SYS_REPLACE_SITE:
                break;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
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
            case SPECIAL_CALL_COLON:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                SkelAtom sym = ((SkelCompound) en.skel).sym;

                Object obj = EvaluableLogic.slashToClass(temp[0], ref, 0, en);
                SkelAtom mod = modToAtom(obj, temp[0], ref, en);
                colonToCallable(temp[1], ref, true, en);
                EvaluableLogic.colonToRoutine(mod, sym, true, en);

                Directive dire = SupervisorCall.callGoal2(AbstractDefined.MASK_DEFI_TRAN, en);
                Display d = en.display;

                CallFrame ref2 = CallFrame.getFrame(d, dire, en);
                en.contskel = dire;
                en.contdisplay = ref2;
                return true;
            case SPECIAL_CALL_COLONCOLON:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                sym = ((SkelCompound) en.skel).sym;

                en.skel = temp[0];
                en.display = ref;
                en.deref();
                Object recv = en.skel;
                Display d2 = en.display;

                obj = EvaluableLogic.slashToClass(recv, d2,
                        CacheModule.MASK_MODULE_CMPD, en);
                mod = objToAtom(obj, recv, d2, en);
                colonToCallable(temp[1], ref, true, en);
                EvaluableLogic.colonToMethod(mod, sym, recv, d2, true, en);

                dire = SupervisorCall.callGoal2(AbstractDefined.MASK_DEFI_TRAN, en);
                d = en.display;

                ref2 = CallFrame.getFrame(d, dire, en);
                en.contskel = dire;
                en.contdisplay = ref2;
                return true;
            case SPECIAL_SYS_REPLACE_SITE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;

                en.skel = temp[2];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                obj = en.skel;
                d = en.display;

                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SkelAtom sa2 = StackElement.callableToName(en.skel);

                SkelAtom sa = StackElement.callableToName(obj);
                sa = makeAtom(sa.fun, sa2, en);
                obj = StackElement.callableFromName(obj, sa);
                if (!BindUniv.unifyTerm(obj, d, temp[0], ref, en))
                    return false;
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /****************************************************************/
    /* Name Helper                                                  */
    /****************************************************************/

    /**
     * <p>Retrieve the module name.</p>
     *
     * @param mod The object.
     * @param t   The slash skeleton.
     * @param d   The slash display.
     * @param en  The engine.
     * @return The nodule name.
     * @throws EngineMessage Shit happens.
     */
    static SkelAtom objToAtom(Object mod, Object t, Display d,
                              Engine en)
            throws EngineMessage {
        if (!(mod instanceof AbstractSkel) &&
                !(mod instanceof Number)) {
            /* reference */
            mod = SpecialProxy.refClassOrProxy(mod);
            if (mod == null)
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_UNKNOWN_PROXY, t), d);
            mod = SpecialProxy.classOrProxyName(mod, en);
            if (mod == null)
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_CLASS, t), d);
        } else {
            /* atom */
        }
        return (SkelAtom) mod;
    }

    /**
     * <p>Retrieve the module atom.</p>
     *
     * @param mod The object.
     * @param t   The slash skeleton.
     * @param d   The slash display.
     * @param en  The engine.
     * @return The nodule atom.
     * @throws EngineMessage Shit happens.
     */
    public static SkelAtom modToAtom(Object mod, Object t, Display d, Engine en)
            throws EngineMessage {
        if (!(mod instanceof AbstractSkel) &&
                !(mod instanceof Number)) {
            /* reference */
            mod = SpecialProxy.classOrProxyName(mod, en);
            if (mod == null)
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_CLASS, t), d);
        } else {
            /* atom */
        }
        return (SkelAtom) mod;
    }


    /**
     * <p>Create a new atom for a given site.</p>
     *
     * @param fun The name of the atom.
     * @param sa2 The call-site, or null.
     * @param en  The engine.
     * @return The new atom.
     */
    public static SkelAtom makeAtom(String fun, SkelAtom sa2, Engine en) {
        AbstractSource scope = (sa2 != null ? sa2.scope : null);
        PositionKey pos = (sa2 != null ? sa2.getPosition() : null);

        int m = (pos != null ? SkelAtom.MASK_ATOM_POSI : 0);
        sa2 = en.store.foyer.createAtom(fun, scope, m);
        sa2.setPosition(pos);

        return sa2;
    }

    /************************************************************/
    /* Callable Colon                                           */
    /************************************************************/

    /**
     * <p>Convert a colon to a callable.</p>
     * <p>The result is return in skel and display of the engine.</p>
     * <p>A qualified callable has the following syntax.</p>
     * <pre>
     *     colon --> module ":" colon
     *             | receiver "::" colon
     *             | term.
     * </pre>
     *
     * @param t    The colon skeleton.
     * @param d    The colon display.
     * @param comp The compound flag.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void colonToCallable(Object t, Display d,
                                       boolean comp,
                                       Engine en)
            throws EngineMessage {
        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(EvaluableLogic.OP_COLON)) {
            SkelCompound temp = (SkelCompound) t;
            Object obj = EvaluableLogic.slashToClass(temp.args[0], d, 0, en);
            SkelAtom mod = modToAtom(obj, temp.args[0], d, en);
            colonToCallable(temp.args[1], d, comp, en);
            EvaluableLogic.colonToRoutine(mod, temp.sym, comp, en);
        } else if (comp && t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(EvaluableLogic.OP_COLONCOLON)) {
            SkelCompound temp = (SkelCompound) t;

            en.skel = temp.args[0];
            en.display = d;
            en.deref();
            Object recv = en.skel;
            Display d2 = en.display;

            Object obj = EvaluableLogic.slashToClass(recv, d2,
                    CacheModule.MASK_MODULE_CMPD, en);
            SkelAtom mod = objToAtom(obj, recv, d2, en);
            colonToCallable(temp.args[1], d, comp, en);
            EvaluableLogic.colonToMethod(mod, temp.sym, recv, d2, comp, en);
        }
    }

}