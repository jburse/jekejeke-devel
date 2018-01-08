package jekpro.platform.swing;

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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class InterruptHandler implements InvocationHandler {
    private static final String OP_INTERRUPT = "INT";

    private final Runnable runnable;
    private Object backsignal;

    /**
     * <p>Create an interrupt handler.</p>
     *
     * @param r The runnable.
     */
    public InterruptHandler(Runnable r) {
        runnable = r;
    }

    /**
     * <p>Call Signal.handle via reflection.</p>
     *
     * @param sig  The signal.
     * @param hand The new signal handler.
     * @return The old signal handler.
     * @throws ClassNotFoundException    Shit happens.
     * @throws NoSuchMethodException     Shit happens.
     * @throws InvocationTargetException Shit happens.
     * @throws IllegalAccessException    Shit happens.
     * @throws InstantiationException    Shit happens.
     */
    private Object refHandle(String sig, Object hand)
            throws ClassNotFoundException, NoSuchMethodException,
            InvocationTargetException, IllegalAccessException,
            InstantiationException {
        Class<?> sigClass = Class.forName("sun.misc.Signal");
        Constructor sigNew = sigClass.getConstructor(String.class);
        Object sigObj = sigNew.newInstance(sig);

        Class<?> sigHandClass = Class.forName("sun.misc.SignalHandler");
        Method sigHand = sigClass.getMethod("handle", sigClass, sigHandClass);
        return sigHand.invoke(null, sigObj, hand);
    }

    /**
     * <p>Create SignalHandler via reflection.</p>
     *
     * @return The signal handler.
     * @throws ClassNotFoundException    Shit happens.
     * @throws NoSuchMethodException     Shit happens.
     * @throws InvocationTargetException Shit happens.
     * @throws IllegalAccessException    Shit happens.
     * @throws InstantiationException    Shit happens.
     */
    private Object refHandler() throws ClassNotFoundException,
            NoSuchMethodException, InvocationTargetException,
            IllegalAccessException, InstantiationException {
        Class<?> sigHandClass = Class.forName("sun.misc.SignalHandler");
        Class<?> proxyClass = Proxy.getProxyClass(sigHandClass.getClassLoader(), sigHandClass);
        Constructor proxyNew = proxyClass.getConstructor(InvocationHandler.class);
        return proxyNew.newInstance(this);
    }

    /**
     * <p>Install ctrl break handler.</p>
     */
    public final void installRunnable() {
        try {
            if (backsignal == null) {
                backsignal = refHandle(OP_INTERRUPT, refHandler());
            }
        } catch (ClassNotFoundException e) {
            /* */
        } catch (NoSuchMethodException e) {
            /* */
        } catch (InvocationTargetException e) {
            /* */
        } catch (IllegalAccessException e) {
            /* */
        } catch (InstantiationException e) {
            /* */
        }
    }

    /**
     * <p>Deinstall ctrl break handler.</p>
     */
    public final void deinstallRunnable() {
        try {
            if (backsignal != null) {
                refHandle(OP_INTERRUPT, backsignal);
                backsignal = null;
            }
        } catch (ClassNotFoundException e) {
            /* */
        } catch (NoSuchMethodException e) {
            /* */
        } catch (InvocationTargetException e) {
            /* */
        } catch (IllegalAccessException e) {
            /* */
        } catch (InstantiationException e) {
            /* */
        }
    }

    /**
     * <p>Invocation of the proxy object.</p>
     *
     * @param proxy  The proxy object.
     * @param method The method.
     * @param args   The arguments
     * @return The return value.
     * @throws Throwable The exception.
     */
    public Object invoke(Object proxy, Method method, Object[] args)
            throws Throwable {
        runnable.run();
        return null;
    }

}
