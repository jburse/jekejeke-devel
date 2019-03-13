package jekpro.model.builtin;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;

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
public class AbstractProperty<T> {

    protected final int id;

    /**
     * <p>Create a property.</p>
     *
     * @param i The id of the property.
     */
    protected AbstractProperty(int i) {
        id = i;
    }

    /************************************************************/
    /* Object Properties                                        */
    /************************************************************/

    /**
     * <p>Retrieve all the object properties.</p>
     *
     * @param obj The object.
     * @param en  The engine.
     * @return The properties.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public Object[] getObjProps(T obj, Engine en)
            throws EngineException, EngineMessage {
        throw new IllegalArgumentException("not implemented");
    }

    /**
     * <p>Set a object property.</p>
     *
     * @param obj The object.
     * @param m   The property skeleton.
     * @param d   The property display.
     * @param en  The engine.
     * @return True if property could be set, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean setObjProp(T obj, Object m, Display d, Engine en)
            throws EngineMessage {
        throw new IllegalArgumentException("not implemented");
    }

    /**
     * <p>Reset a object property.</p>
     *
     * @param obj The object.
     * @param m   The property skeleton.
     * @param d   The property display.
     * @param en  The engine.
     * @return True if property could be set, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean resetObjProp(T obj, Object m, Display d, Engine en)
            throws EngineMessage {
        throw new IllegalArgumentException("not implemented");
    }

    /**
     * <p>Retrieve all the object properties.</p>
     *
     * @param obj The object.
     * @param en  The engine.
     * @return The properties.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public Object[] hasObjProp(T obj, Object m, Display d, Engine en)
            throws EngineException, EngineMessage {
        throw new IllegalArgumentException("not implemented");
    }

}