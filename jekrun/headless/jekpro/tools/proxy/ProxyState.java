package jekpro.tools.proxy;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

/**
 * <p>Per instance invocation handler.</p>
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
public final class ProxyState implements InvocationHandler {
    private final ProxyHandler handler;
    private final Object[] template;

    /**
     * <p>Create a data by a handler.</p>
     *
     * @param h    The handler.
     * @param size The size.
     */
    ProxyState(ProxyHandler h, int size) {
        handler = h;
        template = new Object[size];
    }

    /**
     * <p>Retrieve the handler.</p>
     *
     * @return The handler.
     */
    public ProxyHandler getHandler() {
        return handler;
    }

    /**
     * <p>Retrieve the template skeleton.</p>
     *
     * @param idx The index.
     * @return The template skeleton.
     */
    Object at(int idx) {
        return template[idx];
    }

    /**
     * <p>Set the template skeleton.</p>
     *
     * @param idx  The index.
     * @param data The template skeleton.
     */
    void set_at(int idx, Object data) {
        template[idx] = data;
    }

    /**
     * <p>Retrieve the length.</p>
     *
     * @return The length.
     */
    int length() {
        return template.length;
    }

    /**
     * <p>Invoke by executing a Prolog goal.</p>
     *
     * @param proxy  The proxy object.
     * @param method The method.
     * @param args   The arguments.
     * @return The return value.
     */
    public Object invoke(Object proxy, Method method, Object[] args)
            throws Throwable {
        return handler.invoke(proxy, method, args);
    }

}
