package jekpro.tools.proxy;

import jekpro.tools.term.AbstractTerm;

/**
 * <p>The slots interface for a state-full proxy. The interface allows
 * accessing and modifying the slots of a proxy instance. The slots
 * are indexed starting with 1. The number of the slots is specified
 * when creating the proxy instance.
 * </p>
 * <p>The methods at() and set_at() tolerate an index that is outside
 * the range 1..size where size is the number of slots and indicate a
 * failure. The method length() returns the size of the given proxy
 * instance.
 * </p>
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
public interface InterfaceSlots {

    /**
     * <p>Retrieve the data.</p>
     * <p>Will instantiate the template.</p>
     *
     * @param idx The index.
     * @return The data.
     */
    AbstractTerm at(int idx);

    /**
     * <p>Set the data.</p>
     * <p>Will store a template.</p>
     *
     * @param idx  The index.
     * @param data The data.
     */
    void set_at(int idx, AbstractTerm data);

    /**
     * <p>Retrieve the length.</p>
     *
     * @return The length.
     */
    int length();

}
