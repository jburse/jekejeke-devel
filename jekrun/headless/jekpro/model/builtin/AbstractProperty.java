package jekpro.model.builtin;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Store;

/**
 * <p>Abstract base class for properties.</p>
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
 * Only to be distributed with programs that add sgnificant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public class AbstractProperty {

    protected final int id;

    /**
     * <p>Create a flag.</p>
     *
     * @param i The id of the flag.
     */
    protected AbstractProperty(int i) {
        id = i;
    }

    /************************************************************/
    /* Store Properties                                         */
    /************************************************************/

    /**
     * <p>Retrieve all the store properties.</p>
     *
     * @param store The store.
     * @param en    The engine.
     * @return The properties.
     */
    public Object[] getStoreProp(Store store, Engine en) {
        throw new IllegalArgumentException("not implemented");
    }

    /**
     * <p>Set a store property.</p>
     *
     * @param store The store.
     * @param m     The property skeleton.
     * @param d     The property display.
     * @param en    The engine.
     * @throws EngineMessage Shit happens.
     */
    public void setStoreProp(Store store, Object m, Display d, Engine en)
            throws EngineMessage {
        throw new IllegalArgumentException("not implemented");
    }

    /**
     * <p>Reset a store property.</p>
     *
     * @param store The store.
     * @param m     The property skeleton.
     * @param d     The property display.
     * @param en    The engine.
     * @throws EngineMessage Shit happens.
     */
    public void resetStoreProp(Store store, Object m, Display d, Engine en)
            throws EngineMessage {
        throw new IllegalArgumentException("not implemented");
    }


}