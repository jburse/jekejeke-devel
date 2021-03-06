package jekpro.tools.proxy;

import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.reference.reflect.SpecialForeign;
import jekpro.tools.call.AbstractAuto;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;

import java.lang.reflect.*;

/**
 * <p>Common proxy invocation handler.</p>
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
    private AbstractSource src;
    private MapHash<Method, ProxyExecutor> execs;
    private Constructor constr;
    private boolean pivot;

    /**
     * <p>Set the source.</p>
     *
     * @param scope The Prolog text.
     * @param en    The engine.
     * @throws EngineMessage Shit happens.
     */
    public void setSource(AbstractSource scope, Engine en)
            throws EngineMessage {
        src = scope;
        Class clazz = createProxyClass();
        execs = createProxyExecs(clazz, en);
        constr = SpecialForeign.getDeclaredConstructor(clazz, InvocationHandler.class);
        pivot = InterfacePivot.class.isAssignableFrom(clazz);
    }

    /**
     * <p>Retrieve the source.</p>
     *
     * @return The source.
     */
    public AbstractSource getSource() {
        return src;
    }

    /**
     * <p>Retrieve the proxy constructor.</p>
     *
     * @return The proxy constructor.
     */
    public Constructor getProxyConstr() {
        return constr;
    }

    /**
     * <p>Retrieve the pivot flag.</p>
     *
     * @return The pivot flag.
     */
    public boolean getPivotFlag() {
        return pivot;
    }

    /**************************************************************/
    /* Class & Executors Initialization                           */
    /**************************************************************/

    /**
     * <p>Create the proxy class.</p>
     *
     * @return The proxy class.
     */
    private Class createProxyClass() {
        ListArray<Class> list = new ListArray<>();
        MapEntry<AbstractSource, Integer>[] deps = src.snapshotDeps();
        for (int i = 0; i < deps.length; i++) {
            MapEntry<AbstractSource, Integer> entry = deps[i];
            int flags = entry.value.intValue();
            if ((flags & AbstractSource.MASK_IMPT_AUTO) == 0 ||
                    (flags & AbstractSource.MASK_IMPT_MODL) == 0 ||
                    (flags & AbstractSource.MASK_IMPT_REEX) == 0)
                continue;
            if (!(entry.key instanceof AbstractAuto))
                continue;
            Class clazz = ((AbstractAuto) entry.key).getAuto();
            if (clazz == null || !clazz.isInterface())
                continue;
            list.add(clazz);
        }

        Class[] interfaces = new Class[list.size()];
        list.toArray(interfaces);

        ClassLoader loader = src.getStore().loader;
        return Proxy.getProxyClass(loader, interfaces);
    }

    /**
     * <p>Create the proxy executors.</p>
     *
     * @param clazz The proxy class.
     * @param en    The engine.
     * @return The proxy executors.
     */
    public MapHash<Method, ProxyExecutor> createProxyExecs(Class clazz, Engine en) {
        MapHash<Method, ProxyExecutor> map = new MapHash<>();
        Class[] interfaces = clazz.getInterfaces();

        for (int i = 0; i < interfaces.length; i++) {
            Method[] list = interfaces[i].getMethods();
            for (int j = 0; j < list.length; j++) {
                Method method = list[j];
                if (Modifier.isStatic(method.getModifiers()))
                    continue;
                if (map.getEntry(method) != null)
                    continue;
                ProxyExecutor exec = new ProxyExecutor();
                if (!exec.encodeSignature(method, en))
                    continue;
                if (!exec.encodeSpecial(method, en))
                    continue;
                exec.setHandler(method, this);
                map.add(method, exec);
            }
        }
        return map;
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
     * @throws Throwable Exception of the method.
     */
    public Object invoke(Object proxy, Method method, Object[] args)
            throws Throwable {
        Interpreter inter = Interpreter.getInter();
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
        }
        ProxyExecutor exec = execs.get(method);
        try {
            return exec.runGoal(proxy, args, inter);
        } catch (InterpreterException x) {
            throw new RuntimeWrap(x);
        } catch (InterpreterMessage x) {
            throw new RuntimeWrap(x);
        }
    }

    /**
     * <p>Some testing.</p>
     *
     * @param args Not used.
     */
    /*
    public static void main(String[] args) {
        Object obj = Proxy.newProxyInstance(ProxyHandler.class.getClassLoader(),
                new Class[]{InterfacePivot.class},
                new InvocationHandler() {
                    @Override
                    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
                        return null;
                    }
                });
        Class clazz = InterfacePivot.class;
        System.out.println("clazz=" + clazz);
        Method[] methods = clazz.getDeclaredMethods();
        for (int i = 0; i < methods.length; i++) {
            Method method = methods[i];
            boolean stat = Modifier.isStatic(method.getModifiers());
            System.out.println("methods[" + i + "]=" + methods[i] + ", static=" + stat);
        }

        System.out.println();

        clazz = obj.getClass();
        System.out.println("clazz=" + clazz);
        methods = clazz.getDeclaredMethods();
        for (int i = 0; i < methods.length; i++) {
            Method method = methods[i];
            boolean stat = Modifier.isStatic(method.getModifiers());
            System.out.println("methods[" + i + "]=" + methods[i] + ", static=" + stat);
        }

    }
    */

}
