package matula.util.swing;

import matula.util.system.ForeignUri;

import java.lang.reflect.*;

/**
 * <p>The ctrl-c runner.</p>
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
public final class InterruptHandler implements InvocationHandler {
    private static final String OP_INTERRUPT = "INT";

    private final Runnable runnable;
    private Object backsignal;
    private static Constructor sigNew;
    private static Method sigHand;
    private static Constructor proxyNew;

    static {
        try {
            Class<?> sigClass = Class.forName("sun.misc.Signal");
            sigNew = sigClass.getConstructor(String.class);

            Class<?> sigHandClass = Class.forName("sun.misc.SignalHandler");
            sigHand = sigClass.getMethod("handle", sigClass, sigHandClass);

            Class<?> proxyClass = Proxy.getProxyClass(sigHandClass.getClassLoader(), sigHandClass);
            proxyNew = proxyClass.getConstructor(InvocationHandler.class);
        } catch (ClassNotFoundException e) {
            sigNew = null;
            sigHand = null;
            proxyNew = null;
        } catch (NoSuchMethodException e) {
            sigNew = null;
            sigHand = null;
            proxyNew = null;
        }
    }

    /**
     * <p>Create an interrupt handler.</p>
     *
     * @param r The runnable.
     */
    public InterruptHandler(Runnable r) {
        runnable = r;
    }

    /**
     * <p>Create SignalHandler via reflection.</p>
     *
     * @return The signal handler.
     */
    private Object refHandler() {
        Object res;
        try {
            res = proxyNew.newInstance(this);
        } catch (InvocationTargetException x) {
            throw new RuntimeException(ForeignUri.SHOULDNT_HAPPEN, x);
        } catch (IllegalAccessException x) {
            throw new RuntimeException(ForeignUri.SHOULDNT_HAPPEN, x);
        } catch (InstantiationException x) {
            throw new RuntimeException(ForeignUri.SHOULDNT_HAPPEN, x);
        }
        return res;
    }

    /**
     * <p>Call Signal.handle via reflection.</p>
     *
     * @param sig  The signal.
     * @param hand The new signal handler.
     * @return The old signal handler.
     */
    private Object refHandle(String sig, Object hand) {
        Object res;
        try {
            Object sigObj = sigNew.newInstance(sig);
            res = sigHand.invoke(null, sigObj, hand);
        } catch (InvocationTargetException x) {
            throw new RuntimeException(ForeignUri.SHOULDNT_HAPPEN, x);
        } catch (IllegalAccessException x) {
            throw new RuntimeException(ForeignUri.SHOULDNT_HAPPEN, x);
        } catch (InstantiationException x) {
            throw new RuntimeException(ForeignUri.SHOULDNT_HAPPEN, x);
        }
        return res;
    }


    /**
     * <p>Install ctrl break handler.</p>
     */
    public final void installRunnable() {
        if (proxyNew == null)
            return;
        if (backsignal == null)
            backsignal = refHandle(OP_INTERRUPT, refHandler());
    }

    /**
     * <p>Deinstall ctrl break handler.</p>
     */
    public final void deinstallRunnable() {
        if (proxyNew == null)
            return;
        if (backsignal != null) {
            refHandle(OP_INTERRUPT, backsignal);
            backsignal = null;
        }
    }

    /**
     * <p>Invocation of the proxy object.</p>
     *
     * @param proxy  The proxy object.
     * @param method The method.
     * @param args   The arguments
     * @return The return value.
     */
    public Object invoke(Object proxy, Method method, Object[] args) {
        runnable.run();
        return null;
    }

}
