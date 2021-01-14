package jekpro.frequent.basic;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Store;
import jekpro.reference.runtime.EvaluableLogic;
import jekpro.reference.runtime.SpecialLogic;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.array.Types;
import jekpro.tools.foreign.AutoClass;
import jekpro.tools.proxy.ProxyHandler;
import jekpro.tools.proxy.ProxyPivot;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.config.AbstractRuntime;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SpecialProxy extends AbstractSpecial {
    private final static int SPECIAL_SYS_NEW_INSTANCE = 0;
    private final static int SPECIAL_SYS_ASSIGNABLE_FROM = 2;
    private final static int SPECIAL_SYS_SYS_GET_CLASS = 3;

    /**
     * <p>Create a foreign special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialProxy(int i) {
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
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineException, EngineMessage {
        try {
            switch (id) {
                case SPECIAL_SYS_NEW_INSTANCE:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    Object obj = EvaluableLogic.slashToClass(temp[0], ref, 0, en);
                    SkelAtom sa = SpecialLogic.modToAtom(obj, temp[0], ref, en);
                    obj = SpecialProxy.newInstance(CacheSubclass.getBase(sa, en), en);
                    if (!BindUniv.unifyTerm(obj, Display.DISPLAY_CONST, temp[1], ref, en))
                        return false;
                    return true;
                case SPECIAL_SYS_ASSIGNABLE_FROM:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    obj = EvaluableLogic.slashToClass(temp[0], ref, 0, en);
                    SkelAtom mod = SpecialLogic.modToAtom(obj, temp[0], ref, en);
                    obj = EvaluableLogic.slashToClass(temp[1], ref, 0, en);
                    sa = SpecialLogic.modToAtom(obj, temp[1], ref, en);
                    if (!CacheSubclass.getSubclass(sa, mod, en))
                        return false;
                    return true;
                case SPECIAL_SYS_SYS_GET_CLASS:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    Object m = SpecialUniv.derefAndCastRef(temp[0], ref);
                    obj = SpecialProxy.refClassOrProxy(m);
                    if (obj == null)
                        throw new EngineMessage(EngineMessage.domainError(
                                EngineMessage.OP_DOMAIN_UNKNOWN_PROXY, m));
                    if (!BindUniv.unifyTerm(obj, Display.DISPLAY_CONST, temp[1], ref, en))
                        return false;
                    return true;
                default:
                    throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
            }
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
        }
    }

    /****************************************************************/
    /* Proxy Creation                                               */
    /****************************************************************/

    /**
     * <p>Instantiate the Java proxy class of the given Prolog text.</p>
     *
     * @param scope The Prolog text.
     * @param en    The engine.
     * @return The instance.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static Object newInstance(AbstractSource scope, Engine en)
            throws EngineMessage, EngineException {
        ProxyHandler handler = defineHandler(scope, en);
        Constructor constr = handler.getProxyConstr();
        if (handler.getPivotFlag()) {
            ProxyPivot state = new ProxyPivot(handler);
            return AutoClass.invokeNew(constr, state);
        } else {
            return AutoClass.invokeNew(constr, handler);
        }
    }

    /**
     * <p>Define a handler.</p>
     *
     * @param scope The Prolog text.
     * @param en    The engine.
     * @return The handler.
     * @throws EngineMessage Shit happens.
     */
    private static ProxyHandler defineHandler(AbstractSource scope, Engine en)
            throws EngineMessage {
        if (!(scope instanceof InterfaceProxyable))
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_CREATE,
                    EngineMessage.OP_PERMISSION_PROXY,
                    scope.getPathAtom()));
        InterfaceProxyable proxable = (InterfaceProxyable) scope;
        ProxyHandler handler = proxable.getHandler();
        if (handler != null)
            return handler;
        synchronized (proxable) {
            handler = proxable.getHandler();
            if (handler != null)
                return handler;
            handler = new ProxyHandler();
            handler.setSource(scope, en);
            proxable.setHandler(handler);
        }
        return handler;
    }

    /****************************************************************/
    /* Proxy Name                                                   */
    /****************************************************************/

    /**
     * <p>Retrieve the class or proxy of a reference.</p>
     * <p>Only proxies based on our sources are recognized.</p>
     *
     * @param obj The reference.
     * @return The class or proxy.
     */
    public static Object refClassOrProxy(Object obj) {
        if (!(obj instanceof Proxy))
            return obj.getClass();
        InvocationHandler iv = Proxy.getInvocationHandler(obj);
        if (iv instanceof ProxyPivot)
            iv = ((ProxyPivot) iv).getHandler();
        if (iv instanceof ProxyHandler)
            return ((ProxyHandler) iv).getSource();
        return null;
    }

    /**
     * <p>Retrieve the name of a class or proxy.</p>
     * <p>Only proxies based on our sources are recognized.</p>
     *
     * @param obj The class or proxy.
     * @param en  The engine.
     * @return The name.
     */
    public static SkelAtom classOrProxyName(Object obj, Engine en) {
        if (obj instanceof Class) {
            Class clazz = (Class) obj;
            Store store = inSomeChain(clazz.getClassLoader(), en);
            if (store == null)
                return null;
            String s1 = AbstractRuntime.classToString(clazz);
            return new SkelAtom(s1, store.user);
        }
        if (obj instanceof AbstractSource) {
            AbstractSource src = (AbstractSource) obj;
            Store store = src.getStore();
            String s1 = src.getFullName();
            return new SkelAtom(s1, store.user);
        }
        return null;
    }

    /**
     * <p>Check whether a class loader is in some store.</p>
     *
     * @param what The class loader.
     * @param en   The engine.
     */
    private static Store inSomeChain(ClassLoader what, Engine en) {
        Store[] stores = en.store.foyer.snapshotStores();
        for (int i = 0; i < stores.length; i++) {
            Store store = stores[i];
            ClassLoader stop = (store.parent != null ? store.parent.loader : null);
            if (AbstractRuntime.inChain(store.loader, stop, what))
                return store;
        }
        return null;
    }

}
