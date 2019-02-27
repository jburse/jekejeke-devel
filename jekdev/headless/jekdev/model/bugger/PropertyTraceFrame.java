package jekdev.model.bugger;

import jekpro.frequent.standard.EngineCopy;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.Engine;
import jekpro.model.inter.InterfaceStack;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.Display;
import jekpro.model.molec.DisplayClause;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.ReadOpts;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Intermediate;
import jekpro.model.rope.PreClause;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.ListArray;
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
public final class PropertyTraceFrame extends AbstractProperty<InterfaceStack> {
    public final static String OP_SYS_CLAUSE_REF = "sys_clause_ref";
    public final static String OP_VARIABLES = "variables";
    public final static String OP_SYS_PARENT_FRAME = "sys_parent_frame";
    public final static String OP_SYS_CALL_GOAL = "sys_call_goal";

    private static final int PROP_SYS_CLAUSE_REF = 0;
    private static final int PROP_VARIABLES = 1;
    private static final int PROP_SYS_PARENT_FRAME = 2;
    private static final int PROP_SYS_CALL_GOAL = 3;

    /**
     * <p>Create a frame property.</p>
     *
     * @param i The id of the frame property.
     */
    private PropertyTraceFrame(int i) {
        super(i);
    }

    /**
     * <p>Define the frame properties.</p>
     *
     * @return The frame properties.
     */
    static MapHash<StoreKey, AbstractProperty<InterfaceStack>> defineFrameProps() {
        MapHash<StoreKey, AbstractProperty<InterfaceStack>> frameprops = new MapHash<StoreKey, AbstractProperty<InterfaceStack>>();
        frameprops.add(new StoreKey(OP_SYS_CLAUSE_REF, 2), new PropertyTraceFrame(PROP_SYS_CLAUSE_REF));
        frameprops.add(new StoreKey(OP_VARIABLES, 1), new PropertyTraceFrame(PROP_VARIABLES));
        frameprops.add(new StoreKey(OP_SYS_PARENT_FRAME, 1), new PropertyTraceFrame(PROP_SYS_PARENT_FRAME));
        frameprops.add(new StoreKey(OP_SYS_CALL_GOAL, 1), new PropertyTraceFrame(PROP_SYS_CALL_GOAL));
        return frameprops;
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
    public Object[] getObjProp(InterfaceStack frame, Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case PROP_SYS_CLAUSE_REF:
                Clause def = frame.getContSkel().getClause();
                Object val = new SkelCompound(new SkelAtom(OP_SYS_CLAUSE_REF), def);
                return new Object[]{AbstractTerm.createMolec(val, Display.DISPLAY_CONST)};
            case PROP_VARIABLES:
                val = callVariables(frame.getContSkel(), frame.getContDisplay(), en);
                val = new SkelCompound(new SkelAtom(ReadOpts.OP_VARIABLES), val);
                return new Object[]{AbstractTerm.createMolec(val, frame.getContDisplay())};
            case PROP_SYS_PARENT_FRAME:
                frame = StackElement.skipNoTrace(frame.getContDisplay(), en);
                val = (frame != null ? frame : new SkelAtom(AbstractFlag.OP_NULL));
                val = new SkelCompound(new SkelAtom(OP_SYS_PARENT_FRAME), val);
                return new Object[]{AbstractTerm.createMolec(val, Display.DISPLAY_CONST)};
            case PROP_SYS_CALL_GOAL:
                StackElement.callGoal(frame.getContSkel(), frame.getContDisplay(), en);
                val = new SkelCompound(new SkelAtom(OP_SYS_CALL_GOAL), en.skel);
                return new Object[]{AbstractTerm.createMolec(val, en.display)};
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /*************************************************************/
    /* Helpers for possible non-allocated variables              */
    /*************************************************************/

    /**
     * <p>Determine the possibly non-allocated variables.</p>+
     *
     * @param r  The continuation skeleton.
     * @param dc The continuation display.
     * @param en The engine.
     * @return The variable list.
     * @throws EngineMessage Shit happens.
     */
    private static Object callVariables(Intermediate r, DisplayClause dc, Engine en)
            throws EngineMessage {
        Clause def = r.getClause();
        Object t = PreClause.intermediateToClause(def, en);
        ListArray<SkelVar> copy = termToList(t, dc);
        return consSet(copy, en.store);
    }

    /**
     * <p>Make a list of the term variables.</p>
     * <p>Will only include those variables that are allocated.</p>
     *
     * @param val The term.
     * @param d   The display.
     * @return The term variables.
     */
    private static ListArray<SkelVar> termToList(Object val, Display d) {
        Object var = EngineCopy.getVar(val);
        if (var == null)
            return null;
        ListArray<SkelVar> copy = null;
        if (var instanceof SkelVar) {
            SkelVar sv = (SkelVar) var;
            copy = addToList(sv, d, copy);
        } else {
            SkelVar[] temp = (SkelVar[]) var;
            copy = null;
            for (int i = 0; i < temp.length; i++) {
                SkelVar sv = temp[i];
                copy = addToList(sv, d, copy);
            }
        }
        return copy;
    }

    /**
     * <p>Add a variable to the list.</p>
     *
     * @param sv   The variable.
     * @param d    The display.
     * @param copy The list.
     * @return The list
     */
    private static ListArray<SkelVar> addToList(SkelVar sv, Display d,
                                                ListArray<SkelVar> copy) {
        if (d == null || sv.id >= d.bind.length || d.bind[sv.id] == null)
            return copy;
        if (copy == null)
            copy = new ListArray<SkelVar>();
        copy.add(sv);
        return copy;
    }

    /**
     * <p>Convert a variable set into a variable list.</p>
     *
     * @param mvs   The variable set.
     * @param store The store.
     */
    private static Object consSet(ListArray<SkelVar> mvs, Store store) {
        Object res = store.foyer.ATOM_NIL;
        if (mvs == null)
            return res;
        for (int i = mvs.size() - 1; i >= 0; i--)
            res = new SkelCompound(store.foyer.ATOM_CONS, mvs.get(i), res);
        return res;
    }

}