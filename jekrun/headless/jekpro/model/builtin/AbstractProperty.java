package jekpro.model.builtin;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.runtime.SpecialLogic;
import jekpro.tools.term.AbstractTerm;

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
public abstract class AbstractProperty<T> {
    public static final int MASK_PROP_SHOW = 0x00000001;
    public static final int MASK_PROP_DEFL = 0x00000002;
    public static final int MASK_PROP_SUPR = 0x00000004;

    /* predicate property formatting */
    public static final int MASK_PROP_SLCF = 0x00000010;
    public static final int MASK_PROP_PRJF = 0x00000020;
    public static final int MASK_PROP_DELE = 0x00000040;
    public static final int MASK_PROP_MODI = 0x00000080;

    /* predicate property display */
    public static final int MASK_PROP_SETP = 0x00000100;
    public static final int MASK_PROP_META = 0x00000200;

    protected final int id;
    private int flags;

    /**
     * <p>Create a property.</p>
     *
     * @param i The id of the property.
     */
    protected AbstractProperty(int i) {
        id = i;
    }

    /**
     * <p>Create a property.</p>
     *
     * @param i The id of the property.
     * @param f The flags.
     */
    protected AbstractProperty(int i, int f) {
        id = i;
        flags = f;
    }

    /**
     * <p>Retrieve the flags.</p>
     *
     * @return The flags.
     */
    public int getFlags() {
        return flags;
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
    public abstract Object[] getObjProps(T obj, Engine en)
            throws EngineException, EngineMessage;

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
    public abstract boolean setObjProp(T obj, Object m, Display d, Engine en)
            throws EngineMessage;

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
    public abstract boolean resetObjProp(T obj, Object m, Display d, Engine en)
            throws EngineMessage;

    /**
     * <p>Check whether object has a property.</p>
     *
     * @param obj The object.
     * @param m   The property skeleton.
     * @param d   The property display.
     * @param en  The engine.
     * @return True if object has property.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public Object[] hasObjProp(T obj, Object m, Display d, Engine en)
            throws EngineException, EngineMessage {
        throw new IllegalArgumentException("not implemented");
    }

    /**
     * <p>Retrieve all the objects for a property.</p>
     *
     * @param en The engine.
     * @param m  The property skeleton.
     * @param d  The property display.
     * @return The properties.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public T[] idxObjProp(Object m, Display d, Engine en)
            throws EngineException, EngineMessage {
        throw new IllegalArgumentException("not implemented");
    }

    /********************************************************************/
    /* Collection Utilities                                             */
    /********************************************************************/

    /**
     * <p>Cons an array of values to the given term.</p>
     * <p>The tail is passed in skeleton and display.</p>
     * <p>The result is returned in skeleton and display.</p>
     *
     * @param molecs The molecs.
     * @param en     The engine.
     */
    public static void consArray(Object[] molecs, Engine en) {
        if (molecs == null)
            return;
        for (int i = molecs.length - 1; i >= 0; i--) {
            Object t4 = en.skel;
            Display d2 = en.display;
            Object elem = molecs[i];
            Object val = AbstractTerm.getSkel(elem);
            Display ref = AbstractTerm.getDisplay(elem);
            SpecialLogic.pairValue(en.store.foyer.CELL_CONS,
                    val, ref, t4, d2, en);
        }
    }

}