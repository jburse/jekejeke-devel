package jekpro.tools.proxy;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;

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
public final class ProxyPivot implements InvocationHandler {
    private ProxyHandler handler;
    private Object template;

    /**
     * <p>Set the handler.</p>
     *
     * @param h The handler.
     */
    void setHandler(ProxyHandler h) {
        handler = h;
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
     * <p>Retrieve the value.</p>
     *
     * @return The value.
     */
    AbstractTerm value() {
        Object m = template;
        if (m == null)
            return null;
        Display ref = AbstractSkel.createMarker(m);
        return AbstractTerm.createTermWrapped(m, ref);
    }

    /**
     * <p>Set the value.</p>
     *
     * @param data The value.
     * @param en   The engine.
     */
    void set_value(AbstractTerm data, Engine en) {
        Display ref = AbstractTerm.getDisplay(data);
        Object val = AbstractTerm.getSkel(data);
        template = AbstractSkel.copySkel(val, ref, en);
    }

    /**
     * <p>Invoke by executing a Prolog term.</p>
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
