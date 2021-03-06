package jekdev.reference.inspection;

import jekdev.model.pretty.StoreTrace;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.array.Types;
import jekpro.tools.term.*;
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
public final class PropertyTraceStore extends AbstractProperty<Store> {
    public final static MapHash<StoreKey, AbstractProperty<Store>> DEFAULT
            = new MapHash<>();

    private final static String OP_SYS_BREAK = "sys_break";
    private final static String OP_SYS_NAME = "sys_name";
    private final static String OP_SYS_PARENT = "sys_parent";
    private final static String OP_SYS_LASTMOD = "sys_lastmod";

    private static final int PROP_SYS_BREAK = 0;
    private static final int PROP_SYS_NAME = 1;
    private static final int PROP_SYS_PARENT = 2;
    private static final int PROP_SYS_LASTMOD = 3;

    static {
        DEFAULT.add(new StoreKey(OP_SYS_BREAK, 2), new PropertyTraceStore(PROP_SYS_BREAK));
        DEFAULT.add(new StoreKey(OP_SYS_NAME, 1), new PropertyTraceStore(PROP_SYS_NAME));
        DEFAULT.add(new StoreKey(OP_SYS_PARENT, 1), new PropertyTraceStore(PROP_SYS_PARENT));
        DEFAULT.add(new StoreKey(OP_SYS_LASTMOD, 1), new PropertyTraceStore(PROP_SYS_LASTMOD));
    }

    /**
     * <p>Create a store property.</p>
     *
     * @param i The id of the store property.
     */
    private PropertyTraceStore(int i) {
        super(i);
    }

    /**
     * <p>Retrieve a store property.</p>
     *
     * @param store The store.
     * @param en    The engine.
     * @return The property.
     */
    public Object[] getObjProps(Store store, Engine en) {
        switch (id) {
            case PROP_SYS_BREAK:
                StoreTrace storetrace = (StoreTrace) store;
                ListArray<PositionKey> breakpoints = storetrace.snapshotBreakPoints();
                Object[] res = new Object[breakpoints.size()];
                for (int i = 0; i < breakpoints.size(); i++) {
                    PositionKey breakpoint = breakpoints.get(i);
                    Object val = new SkelCompound(new SkelAtom(OP_SYS_BREAK),
                            new SkelAtom(breakpoint.getOrigin()),
                            Integer.valueOf(breakpoint.getLineNo()));
                    res[i] = TermAtomic.createMolec(val, Display.DISPLAY_CONST);
                }
                return res;
            case PROP_SYS_NAME:
                Object val = new SkelCompound(new SkelAtom(OP_SYS_NAME),
                        new SkelAtom(store.name));
                return new Object[]{TermAtomic.createMolec(val, Display.DISPLAY_CONST)};
            case PROP_SYS_PARENT:
                Store parent = store.parent;
                val = new SkelCompound(new SkelAtom(OP_SYS_PARENT),
                        parent != null ? (Knowledgebase) parent.proxy : new SkelAtom(AbstractFlag.OP_NULL));
                return new Object[]{TermAtomic.createMolec(val, Display.DISPLAY_CONST)};
            case PROP_SYS_LASTMOD:
                storetrace = (StoreTrace) store;
                val = new SkelCompound(new SkelAtom(OP_SYS_LASTMOD),
                        TermAtomic.normBigInteger(storetrace.getLastModified()));
                return new Object[]{TermAtomic.createMolec(val, Display.DISPLAY_CONST)};
            default:
                throw new IllegalArgumentException("illegal prop");
        }
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
    public boolean setObjProp(Store store, Object m, Display d, Engine en)
            throws EngineMessage {
        try {
            switch (id) {
                case PROP_SYS_BREAK:
                    PositionKey pos = PropertyTraceStore.derefAndCastPositionKey(m, d, en);
                    StoreTrace storetrace = (StoreTrace) store;
                    storetrace.addBreakPoint(pos);
                    return true;
                case PROP_SYS_NAME:
                    return false;
                case PROP_SYS_PARENT:
                    return false;
                case PROP_SYS_LASTMOD:
                    long lastmod = PropertyTraceStore.derefAndCastLastMod(m, d, en);
                    storetrace = (StoreTrace) store;
                    storetrace.setLastModified(lastmod);
                    return true;
                default:
                    throw new IllegalArgumentException("illegal prop");
            }
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
        }
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
    public boolean resetObjProp(Store store, Object m, Display d, Engine en)
            throws EngineMessage {
        try {
            switch (id) {
                case PROP_SYS_BREAK:
                    PositionKey pos = PropertyTraceStore.derefAndCastPositionKey(m, d, en);
                    StoreTrace storetrace = (StoreTrace) store;
                    storetrace.removeBreakPoint(pos);
                    return true;
                case PROP_SYS_NAME:
                    return false;
                case PROP_SYS_PARENT:
                    return false;
                case PROP_SYS_LASTMOD:
                    return false;
                default:
                    throw new IllegalArgumentException("illegal prop");
            }
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
        }
    }

    /****************************************************************/
    /* Deref Utility                                                */
    /****************************************************************/

    /**
     * <p>Deref and cast to position key.</p>
     *
     * @param m  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The position key.
     * @throws EngineMessage      Shit happens.
     * @throws ClassCastException Shit happens.
     */
    private static PositionKey derefAndCastPositionKey(Object m, Display d, Engine en)
            throws EngineMessage, ClassCastException {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 2 &&
                ((SkelCompound) m).sym.fun.equals(OP_SYS_BREAK)) {
            SkelCompound sc = (SkelCompound) m;
            String orig = SpecialUniv.derefAndCastString(sc.args[0], d);
            Number num = SpecialEval.derefAndCastInteger(sc.args[1], d);
            SpecialEval.checkNotLessThanZero(num);
            int line = SpecialEval.castIntValue(num);
            return new PositionKey(orig, line);
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Deref and cast to position key.</p>
     *
     * @param m  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The position key.
     * @throws EngineMessage      Shit happens.
     * @throws ClassCastException Shit happens.
     */
    private static long derefAndCastLastMod(Object m, Display d, Engine en)
            throws EngineMessage {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 1 &&
                ((SkelCompound) m).sym.fun.equals(OP_SYS_LASTMOD)) {
            SkelCompound sc = (SkelCompound) m;
            Number num = SpecialEval.derefAndCastInteger(sc.args[0], d);
            return SpecialEval.castLongValue(num);
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

}