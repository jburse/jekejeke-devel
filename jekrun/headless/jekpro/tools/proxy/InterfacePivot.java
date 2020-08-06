package jekpro.tools.proxy;

import jekpro.tools.call.Interpreter;
import jekpro.tools.term.AbstractTerm;

import java.lang.reflect.Proxy;

/**
 * <p>The pivot interface for a state-full proxy. The Java proxy
 * class then understands the methods of the Java interface
 * “InterfacePivot” and realizes a getter and setter for the
 * pivot state. The corresponding Prolog module needs to re-export
 * this interface directly or indirectly.</p>
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
public interface InterfacePivot {

    /**
     * <p>Retrieve the value.</p>
     *
     * @return The value.
     */
    default AbstractTerm value() {
        ProxyPivot state = (ProxyPivot) Proxy.getInvocationHandler(this);
        return state.value();
    }

    /**
     * <p>Set the value.</p>
     *
     * @param inter The interpreter.
     * @param data  The value.
     */
    default void set_value(Interpreter inter, AbstractTerm data) {
        ProxyPivot state = (ProxyPivot) Proxy.getInvocationHandler(this);
        state.set_value(data, inter.getEngine());
    }

}