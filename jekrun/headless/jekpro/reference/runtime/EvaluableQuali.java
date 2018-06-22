package jekpro.reference.runtime;

import jekpro.frequent.basic.SpecialProxy;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.tools.term.AbstractSkel;
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
        subflags |= MASK_DELE_VIRT;
        subflags |= MASK_DELE_ARIT;
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
    public final void moniEvaluate(Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case EVALUABLE_COLON:
                SkelCompound temp = (SkelCompound) en.skel;
                Display ref = en.display;
                Object obj = SpecialQuali.slashToClass(temp.args[0], ref, false, true, en);
                String fun;
                /* reference */
                if (!(obj instanceof AbstractSkel) &&
                        !(obj instanceof Number)) {
                    fun = SpecialProxy.classOrProxyName(obj);
                    if (fun == null)
                        throw new EngineMessage(EngineMessage.domainError(
                                EngineMessage.OP_DOMAIN_CLASS, temp.args[0]), ref);
                /* atom */
                } else {
                    fun = ((SkelAtom) obj).fun;
                }
                SpecialQuali.colonToCallable(temp.args[1], ref, true, en);
                if (en.skel instanceof SkelCompound) {
                    SkelCompound sc2 = (SkelCompound) en.skel;
                    en.skel = new SkelCompound(CacheFunctor.getFunctor(sc2.sym, fun,
                            temp.sym, en), sc2.args, sc2.vars);
                } else if (en.skel instanceof SkelAtom) {
                    SkelAtom sa = (SkelAtom) en.skel;
                    en.skel = CacheFunctor.getFunctor(sa, fun, temp.sym, en);
                } else {
                    EngineMessage.checkInstantiated(en.skel);
                    throw new EngineMessage(EngineMessage.typeError(
                            EngineMessage.OP_TYPE_CALLABLE,
                            en.skel), en.display);
                }
                en.computeExpr(en.skel, en.display);
                return;
            case EVALUABLE_COLONCOLON:
                temp = (SkelCompound) en.skel;
                ref = en.display;
                obj = SpecialQuali.slashToClass(temp.args[0], ref, true, true, en);
                /* reference */
                if (!(obj instanceof AbstractSkel) &&
                        !(obj instanceof Number)) {
                    obj = SpecialProxy.refClassOrProxy(obj);
                    if (obj == null)
                        throw new EngineMessage(EngineMessage.domainError(
                                EngineMessage.OP_DOMAIN_UNKNOWN_PROXY, temp.args[0]), ref);
                    fun = SpecialProxy.classOrProxyName(obj);
                    if (fun == null)
                        throw new EngineMessage(EngineMessage.domainError(
                                EngineMessage.OP_DOMAIN_CLASS, temp.args[0]), ref);
                /* atom */
                } else {
                    fun = ((SkelAtom) obj).fun;
                }
                SpecialQuali.colonToCallable(temp.args[1], ref, true, en);
                if (en.skel instanceof SkelCompound) {
                    SkelCompound sc2 = (SkelCompound) en.skel;
                    Display d3 = en.display;

                    en.skel = temp.args[0];
                    en.display = ref;
                    en.deref();
                    Object recv = en.skel;
                    Display d2 = en.display;

                    boolean multi = SpecialQuali.prependCount(recv, d2,
                            sc2.args, d3, en);
                    en.skel = new SkelCompound(CacheFunctor.getFunctor(sc2.sym, fun,
                            temp.sym, en), SpecialQuali.prependAlloc(recv, d2,
                            sc2.args, d3, multi, en));
                } else if (en.skel instanceof SkelAtom) {
                    SkelAtom sa = (SkelAtom) en.skel;

                    en.skel = temp.args[0];
                    en.display = ref;
                    en.deref();
                    Object recv = en.skel;
                    Display d2 = en.display;

                    en.skel = new SkelCompound(CacheFunctor.getFunctor(sa, fun,
                            temp.sym, en), recv);
                    en.display = d2;
                } else {
                    EngineMessage.checkInstantiated(en.skel);
                    throw new EngineMessage(EngineMessage.typeError(
                            EngineMessage.OP_TYPE_CALLABLE,
                            en.skel), en.display);
                }
                en.computeExpr(en.skel, en.display);
                return;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

}
