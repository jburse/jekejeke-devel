package jekdev.reference.inspection;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.ReadOpts;
import jekpro.model.pretty.SkelAtomAnno;
import jekpro.model.pretty.StoreKey;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.PositionKey;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.MapHash;

/**
 * <p>Callable properties on development environment level.</p>
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
public final class PropertyTraceCallable extends AbstractProperty<Object> {
    public final static MapHash<StoreKey, AbstractProperty<Object>> DEFAULT =
            new MapHash<>();

    public final static String OP_SYS_HINT = "sys_hint";
    public final static String OP_SYS_FILLERS = "sys_fillers";
    public final static String OP_SOURCE_FILE = "source_file";
    public final static String OP_LINE_NO = "line_no";

    private static final int PROP_SYS_HINT = 0;
    private static final int PROP_SYS_FILLERS = 1;
    private static final int PROP_SOURCE_FILE = 2;
    private static final int PROP_LINE_NO = 3;

    static {
        DEFAULT.add(new StoreKey(OP_SYS_HINT, 1), new PropertyTraceCallable(PROP_SYS_HINT));
        DEFAULT.add(new StoreKey(OP_SYS_FILLERS, 1), new PropertyTraceCallable(PROP_SYS_FILLERS));
        DEFAULT.add(new StoreKey(OP_SOURCE_FILE, 1), new PropertyTraceCallable(PROP_SOURCE_FILE));
        DEFAULT.add(new StoreKey(OP_LINE_NO, 1), new PropertyTraceCallable(PROP_LINE_NO));
    }

    /**
     * <p>Create a callable property.</p>
     *
     * @param i The id of the atom property.
     */
    private PropertyTraceCallable(int i) {
        super(i);
    }

    /**
     * <p>Retrieve all the object properties.</p>
     *
     * @param obj The object.
     * @param en  The engine.
     * @return The properties.
     */
    public Object[] getObjProps(Object obj, Engine en) {
        switch (id) {
            case PROP_SYS_HINT:
                Object t = AbstractTerm.getSkel(obj);
                SkelAtom sa = StackElement.callableToName(t);
                if (!(sa instanceof SkelAtomAnno))
                    return AbstractBranch.FALSE_PROPERTY;
                int hint = ((SkelAtomAnno) sa).getHint();
                if (hint == 0)
                    return AbstractBranch.FALSE_PROPERTY;
                return new Object[]{AbstractTerm.createMolec(
                        new SkelCompound(new SkelAtom(OP_SYS_HINT),
                                new SkelAtom(SkelAtom.valueOf(hint))), Display.DISPLAY_CONST)};
            case PROP_SYS_FILLERS:
                t = AbstractTerm.getSkel(obj);
                sa = StackElement.callableToName(t);
                if (!(sa instanceof SkelAtomAnno))
                    return AbstractBranch.FALSE_PROPERTY;
                String[][] fillers = ((SkelAtomAnno) sa).getFillers();
                if (fillers == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return fillerToVals(fillers, en);
            case PROP_SOURCE_FILE:
                t = AbstractTerm.getSkel(obj);
                sa = StackElement.callableToName(t);
                PositionKey pos = sa.getPosition();
                if (pos == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return new Object[]{AbstractTerm.createMolec(
                        new SkelCompound(new SkelAtom(OP_SOURCE_FILE),
                                new SkelAtom(pos.getOrigin())), Display.DISPLAY_CONST)};
            case PROP_LINE_NO:
                t = AbstractTerm.getSkel(obj);
                sa = StackElement.callableToName(t);
                pos = sa.getPosition();
                if (pos == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return new Object[]{AbstractTerm.createMolec(
                        new SkelCompound(new SkelAtom(ReadOpts.OP_LINE_NO),
                                Integer.valueOf(pos.getLineNo())), Display.DISPLAY_CONST)};
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
    public boolean setObjProp(Object obj, Object m, Display d, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_SYS_HINT:
            case PROP_SYS_FILLERS:
            case PROP_SOURCE_FILE:
            case PROP_LINE_NO:
                return false;
            default:
                throw new IllegalArgumentException("illegal prop");
        }
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
    public boolean resetObjProp(Object obj, Object m, Display d, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_SYS_HINT:
            case PROP_SYS_FILLERS:
            case PROP_SOURCE_FILE:
            case PROP_LINE_NO:
                return false;
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /****************************************************************/
    /* Deref Utility                                                */
    /****************************************************************/

    /**
     * <p>Convert fillers into values.</p>
     *
     * @param fillers The fillers.
     * @param en      The engine.
     * @return The values.
     */
    private static Object[] fillerToVals(String[][] fillers, Engine en) {
        Object[] molecs = new Object[fillers.length];
        for (int i = 0; i < fillers.length; i++) {
            Object val = en.store.foyer.ATOM_NIL;
            String[] filler = fillers[i];
            if (filler != null) {
                for (int j = filler.length - 1; j >= 0; j--) {
                    SkelAtom help = new SkelAtom(filler[j]);
                    val = new SkelCompound(en.store.foyer.ATOM_CONS, help, val);
                }
            }
            molecs[i] = AbstractTerm.createMolec(new SkelCompound(
                    new SkelAtom(OP_SYS_FILLERS), val), Display.DISPLAY_CONST);
        }
        return molecs;
    }

}