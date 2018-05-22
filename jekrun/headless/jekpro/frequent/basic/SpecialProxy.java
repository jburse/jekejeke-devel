package jekpro.frequent.basic;

import jekpro.model.builtin.SpecialSpecial;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.CacheSubclass;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.tools.proxy.BranchAPI;
import jekpro.tools.proxy.InterfaceHandler;
import jekpro.tools.proxy.InterfaceSlots;
import jekpro.tools.proxy.InterfaceState;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationHandler;

/**
 * <p>Provides built-in predicates for the module proxy.</p>
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
public final class SpecialProxy extends AbstractSpecial {
    private final static int SPECIAL_SYS_PROXY_HANDLER = 0;
    private final static int SPECIAL_SYS_PROXY_STATE = 1;
    private final static int SPECIAL_SYS_ASSIGNABLE_FROM = 2;

    private final static Class[] SIG_INVOKE = new Class[]{InvocationHandler.class};

    /**
     * <p>Create a foreign special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialProxy(int i) {
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
        switch (id) {
            case SPECIAL_SYS_PROXY_HANDLER:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Object obj = SpecialQuali.slashToClass(temp[0], ref, false, en);
                SkelAtom sa;
                if (!(obj instanceof AbstractSkel) &&
                        !(obj instanceof Number)) {
                    /* reference */
                    String fun = BranchAPI.classOrProxyName(obj);
                    if (fun == null)
                        throw new EngineMessage(EngineMessage.domainError(
                                EngineMessage.OP_DOMAIN_CLASS, temp[0]), ref);
                    sa = new SkelAtom(fun);
                } else {
                    /* atom */
                    sa = (SkelAtom) obj;
                }
                obj = SpecialProxy.newProxyHandler(CacheSubclass.getBase(sa, en));
                if (!en.unifyTerm(temp[1], ref, obj, Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_PROXY_STATE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                obj = SpecialQuali.slashToClass(temp[0], ref, false, en);
                if (!(obj instanceof AbstractSkel) &&
                        !(obj instanceof Number)) {
                    /* reference */
                    String fun = BranchAPI.classOrProxyName(obj);
                    if (fun == null)
                        throw new EngineMessage(EngineMessage.domainError(
                                EngineMessage.OP_DOMAIN_CLASS, temp[0]), ref);
                    sa = new SkelAtom(fun);
                } else {
                    /* atom */
                    sa = (SkelAtom) obj;
                }
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                Number num = EngineMessage.castInteger(en.skel, en.display);
                EngineMessage.checkNotLessThanZero(num);
                int size = EngineMessage.castIntValue(num);
                obj = SpecialProxy.newProxyState(CacheSubclass.getBase(sa, en), size);
                if (!en.unifyTerm(temp[2], ref, obj, Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_ASSIGNABLE_FROM:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                obj = SpecialQuali.slashToClass(temp[0], ref, false, en);
                String fun;
                if (!(obj instanceof AbstractSkel) &&
                        !(obj instanceof Number)) {
                    /* reference */
                    fun = BranchAPI.classOrProxyName(obj);
                    if (fun == null)
                        throw new EngineMessage(EngineMessage.domainError(
                                EngineMessage.OP_DOMAIN_CLASS, temp[0]), ref);
                } else {
                    /* atom */
                    fun = ((SkelAtom) obj).fun;
                }
                obj = SpecialQuali.slashToClass(temp[1], ref, false, en);
                if (!(obj instanceof AbstractSkel) &&
                        !(obj instanceof Number)) {
                    /* reference */
                    String fun2 = BranchAPI.classOrProxyName(obj);
                    if (fun2 == null)
                        throw new EngineMessage(EngineMessage.domainError(
                                EngineMessage.OP_DOMAIN_CLASS, temp[1]), ref);
                    sa = new SkelAtom(fun2);
                } else {
                    /* atom */
                    sa = (SkelAtom) obj;
                }
                if (!CacheSubclass.getSubclass(sa, fun, en))
                    return false;
                return en.getNextRaw();
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Instantiate the Java proxy class of the given Prolog text.</p>
     *
     * @param scope The Prolog text.
     * @return The instance.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static Object newProxyHandler(AbstractSource scope)
            throws EngineMessage, EngineException {
        InterfaceHandler handler = scope.defineHandler();
        Class clazz = handler.defineGener();
        if (InterfaceSlots.class.isAssignableFrom(clazz))
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_PROXY,
                    SpecialSpecial.constructorToCallable(new Class[]{})));
        Constructor constr = SpecialSpecial.getDeclaredConstructor(clazz, SIG_INVOKE);
        return scope.getStore().foyer.getFactory().newInstance(constr, new Object[]{handler});
    }

    /**
     * <p>Instantiate the Java proxy class of the given Prolog text.</p>
     *
     * @param scope The Prolog text.
     * @param size  The size.
     * @return The instance.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static Object newProxyState(AbstractSource scope, int size)
            throws EngineMessage, EngineException {
        InterfaceHandler handler = scope.defineHandler();
        Class clazz = handler.defineGener();
        if (!InterfaceSlots.class.isAssignableFrom(clazz))
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_PROXY,
                    SpecialSpecial.constructorToCallable(new Class[]{Integer.TYPE})));
        Constructor constr = SpecialSpecial.getDeclaredConstructor(clazz, SIG_INVOKE);
        InterfaceState state = handler.createState(size);
        return scope.getStore().foyer.getFactory().newInstance(constr, new Object[]{state});
    }

}
