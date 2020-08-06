package jekdev.reference.inspection;

import jekpro.model.builtin.AbstractFlag;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.StoreKey;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.MapHash;

/**
 * <p>Store properties on development environment level.</p>
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
public final class PropertyTraceFrame extends AbstractProperty<StackElement> {
    public final static MapHash<StoreKey, AbstractProperty<StackElement>> DEFAULT
            = new MapHash<StoreKey, AbstractProperty<StackElement>>();

    public final static String OP_SYS_PARENT_FRAME = "sys_parent_frame";
    public final static String OP_SYS_CALL_GOAL = "sys_call_goal";

    private static final int PROP_SYS_PARENT_FRAME = 0;
    private static final int PROP_SYS_CALL_GOAL = 1;

    static {
        DEFAULT.add(new StoreKey(OP_SYS_PARENT_FRAME, 1), new PropertyTraceFrame(PROP_SYS_PARENT_FRAME));
        DEFAULT.add(new StoreKey(OP_SYS_CALL_GOAL, 1), new PropertyTraceFrame(PROP_SYS_CALL_GOAL));
    }

    /**
     * <p>Create a frame property.</p>
     *
     * @param i The id of the frame property.
     */
    private PropertyTraceFrame(int i) {
        super(i);
    }

    /**
     * <p>Retrieve all the frame properties.</p>
     *
     * @param frame The frame.
     * @param en    The engine.
     * @return The properties.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public Object[] getObjProps(StackElement frame, Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case PROP_SYS_PARENT_FRAME:
                frame = StackElement.skipNoTrace(frame.contdisplay, en);
                Object val = (frame != null ? frame : new SkelAtom(AbstractFlag.OP_NULL));
                val = new SkelCompound(new SkelAtom(OP_SYS_PARENT_FRAME), val);
                return new Object[]{AbstractTerm.createMolec(val, Display.DISPLAY_CONST)};
            case PROP_SYS_CALL_GOAL:
                StackElement.callGoal(frame.contskel, frame.contdisplay, en);
                val = new SkelCompound(new SkelAtom(OP_SYS_CALL_GOAL), en.skel);
                return new Object[]{AbstractTerm.createMolec(val, en.display)};
            default:
                throw new IllegalArgumentException("illegal prop");
        }
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
    public boolean setObjProp(StackElement obj, Object m, Display d, Engine en)
            throws EngineMessage {
        if (id < PROP_SYS_PARENT_FRAME || PROP_SYS_CALL_GOAL < id)
            throw new IllegalArgumentException("illegal prop");
        return false;
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
    public boolean resetObjProp(StackElement obj, Object m, Display d, Engine en)
            throws EngineMessage {
        if (id < PROP_SYS_PARENT_FRAME || PROP_SYS_CALL_GOAL < id)
            throw new IllegalArgumentException("illegal prop");
        return false;
    }

}