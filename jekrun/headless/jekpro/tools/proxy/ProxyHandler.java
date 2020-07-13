package jekpro.tools.proxy;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Supervisor;
import jekpro.model.molec.Display;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Store;
import jekpro.tools.call.AbstractAuto;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;
import matula.util.wire.AbstractLivestock;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

/**
 * <p>Java code for the instantiation example.</p>
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
public final class ProxyHandler implements InvocationHandler {
    private final AbstractSource src;
    private Class gener;
    private final MapHash<Method, ExecutorMethod> execs =
            new MapHash<Method, ExecutorMethod>();

    /**
     * <p>Create a prolog handler for the given Prolog text.</p>
     *
     * @param s The source.
     */
    public ProxyHandler(AbstractSource s) {
        src = s;
    }

    /**
     * <p>Retrieve the source.</p>
     *
     * @return The source.
     */
    public AbstractSource getSource() {
        return src;
    }

    /**************************************************************/
    /* Invoke Prolog                                              */
    /**************************************************************/

    /**
     * <p>Invoke by executing a Prolog term.</p>
     * <p>See also java.beans.EventHandler for more ideas.</p>
     *
     * @param method The method.
     * @param proxy  The proxy object.
     * @param args   The arguments.
     * @return The return value.
     */
    public Object invoke(Object proxy, Method method, Object[] args) {
        Supervisor s = (Supervisor) AbstractLivestock.currentLivestock(Thread.currentThread());
        Engine en = (s != null ? s.inuse : null);
        Interpreter inter = (en != null ? (Interpreter) en.proxy : null);
        if (inter == null) {
            Knowledgebase know = (Knowledgebase) src.getStore().proxy;
            inter = know.iterable();
        }
        if (method.getDeclaringClass() == Object.class) {
            String name = method.getName();
            if (name.equals("hashCode")) {
                return Integer.valueOf(System.identityHashCode(proxy));
            } else if (name.equals("equals")) {
                return (proxy == args[0] ? Boolean.TRUE : Boolean.FALSE);
            } else if (name.equals("toString")) {
                return proxy.getClass().getName() + "@" + Integer.toHexString(proxy.hashCode());
            }
        } else if (method.getDeclaringClass() == InterfaceSlots.class) {
            String name = method.getName();
            if (name.equals("at")) {
                return at(proxy, ((Integer) args[0]).intValue());
            } else if (name.equals("set_at")) {
                set_at(proxy, ((Integer) args[0]).intValue(),
                        (AbstractTerm) args[1], inter.getEngine());
                return null;
            } else if (name.equals("length")) {
                return Integer.valueOf(length(proxy));
            }
        }
        ExecutorMethod exe = findExecutor(method);
        try {
            return exe.runGoal(proxy, args, inter);
        } catch (InterpreterException x) {
            throw new RuntimeWrap(x);
        } catch (InterpreterMessage x) {
            throw new RuntimeWrap(x);
        }
    }

    /**
     * <p>Find an executor for a given method.</p>
     *
     * @param method The method.
     * @return The executor.
     */
    private ExecutorMethod findExecutor(Method method) {
        ExecutorMethod exe;
        synchronized (this) {
            exe = execs.get(method);
            if (exe != null)
                return exe;
            exe = createMethod(method);
            exe.setSource(getSource());
            execs.add(method, exe);
        }
        return exe;
    }

    /**
     * <p>Create an executor.</p>
     *
     * @param method The method.
     * @return The executor, or null.
     */
    private ExecutorMethod createMethod(Method method) {
        ExecutorMethod exe = new ExecutorMethod(method);
        if (!exe.encodeSignature())
            return null;
        return exe;
    }

    /**************************************************************/
    /* Instance Handling                                          */
    /**************************************************************/

    /**
     * <p>Create a new state and initialize it.</p>
     *
     * @param size The size.
     * @return The initialized state.
     */
    public ProxyState createState(int size) {
        ProxyState state = new ProxyState(this, size);
        for (int i = 0; i < size; i++)
            state.set_at(i, src.getStore().foyer.ATOM_NIL);
        return state;
    }

    /**
     * <p>Retrieve the data.</p>
     * <p>Will instantiate the template.</p>
     *
     * @param proxy The proxy.
     * @param idx   The index.
     * @return The data.
     */
    private static AbstractTerm at(Object proxy, int idx) {
        ProxyState state = (ProxyState) Proxy.getInvocationHandler(proxy);
        if (idx < 0 || state.length() <= idx)
            throw new ArrayIndexOutOfBoundsException();
        Object m = state.at(idx);
        Display ref = AbstractSkel.createMarker(m);
        return AbstractTerm.createTermWrapped(m, ref);
    }

    /**
     * <p>Set the data.</p>
     * <p>Will store a template.</p>
     *
     * @param proxy The proxy object.
     * @param idx   The index.
     * @param data  The data.
     * @param en    The engine.
     */
    private static void set_at(Object proxy, int idx,
                               AbstractTerm data, Engine en) {
        ProxyState state = (ProxyState) Proxy.getInvocationHandler(proxy);
        if (idx < 0 || state.length() <= idx)
            throw new ArrayIndexOutOfBoundsException();
        Object val = AbstractSkel.copySkel(AbstractTerm.getSkel(data),
                AbstractTerm.getDisplay(data), en);
        state.set_at(idx, val);
    }

    /**
     * <p>Retrieve the length.</p>
     *
     * @param proxy The proxy object.
     * @return The length.
     */
    private static int length(Object proxy) {
        ProxyState state = (ProxyState) Proxy.getInvocationHandler(proxy);
        return state.length();
    }

    /**************************************************************/
    /* Class Handling                                             */
    /**************************************************************/

    /**
     * <p>Define the generated proxy class.</p>
     *
     * @return The generated proxy class.
     */
    public Class defineGener() {
        Class res = gener;
        if (res != null)
            return res;
        synchronized (this) {
            res = gener;
            if (res != null)
                return res;
            res = createProxy();
            gener = res;
        }
        return res;
    }

    /**
     * <p>Create a generated proxy class.</p>
     *
     * @return The generated proxy class.
     */
    private Class createProxy() {
        ListArray<Class> list = new ListArray<Class>();
        MapEntry<AbstractSource, Integer>[] deps = src.snapshotDeps();
        for (int i = 0; i < deps.length; i++) {
            MapEntry<AbstractSource, Integer> entry = deps[i];
            if (!(entry.key instanceof AbstractAuto))
                continue;
            int flags = entry.value.intValue();
            if ((flags & AbstractSource.MASK_IMPT_AUTO) == 0 ||
                    (flags & AbstractSource.MASK_IMPT_MODL) == 0 ||
                    (flags & AbstractSource.MASK_IMPT_REEX) == 0)
                continue;
            Class clazz = ((AbstractAuto) entry.key).getAuto();
            if (clazz == null || !clazz.isInterface())
                continue;
            list.add(clazz);
        }

        Class[] interfaces = new Class[list.size()];
        list.toArray(interfaces);

        Store store = src.getStore();
        return Proxy.getProxyClass(store.loader, interfaces);
    }

}
