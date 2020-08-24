package jekpro.reference.reflect;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.MapHash;

/**
 * <p>Callable properties on runtime library level.</p>
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
public final class PropertyCallable extends AbstractProperty<Object> {
    public final static MapHash<StoreKey, AbstractProperty<Object>> DEFAULT =
            new MapHash<StoreKey, AbstractProperty<Object>>();

    public final static String OP_SYS_CONTEXT = "sys_context";

    private static final int PROP_SYS_CONTEXT = 0;

    static {
        DEFAULT.add(new StoreKey(OP_SYS_CONTEXT, 1), new PropertyCallable(PROP_SYS_CONTEXT));
    }

    /**
     * <p>Create an atom property.</p>
     *
     * @param i The id of the atom property.
     */
    private PropertyCallable(int i) {
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
            case PROP_SYS_CONTEXT:
                Object t = AbstractTerm.getSkel(obj);
                SkelAtom sa = StackElement.callableToName(t);
                sa = (sa.scope != null ? sa.scope.getPathAtom() : new SkelAtom(""));
                return new Object[]{AbstractTerm.createMolec(
                        new SkelCompound(new SkelAtom(OP_SYS_CONTEXT), sa),
                        Display.DISPLAY_CONST)};
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
            case PROP_SYS_CONTEXT:
                AbstractSource src = derefAndCastContext(m, d, en);
                Object t = AbstractTerm.getSkel(obj);
                SkelAtom sa = StackElement.callableToName(t);
                sa = new SkelAtom(sa.fun, src);
                en.skel = StackElement.callableFromName(t, sa);
                en.display = AbstractTerm.getDisplay(obj);
                return true;
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
            case PROP_SYS_CONTEXT:
                Object t = AbstractTerm.getSkel(obj);
                SkelAtom sa = StackElement.callableToName(t);
                sa = new SkelAtom(sa.fun);
                en.skel = StackElement.callableFromName(t, sa);
                en.display = AbstractTerm.getDisplay(obj);
                return true;
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /****************************************************************/
    /* Deref Utility                                                */
    /****************************************************************/

    /**
     * <p>Deref and cast to context.</p>
     *
     * @param m  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The scope.
     * @throws EngineMessage      Shit happens.
     */
    private static AbstractSource derefAndCastContext(Object m, Display d, Engine en)
            throws EngineMessage {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 1 &&
                ((SkelCompound) m).sym.fun.equals(OP_SYS_CONTEXT)) {
            SkelCompound sc = (SkelCompound) m;
            return PropertyCallable.derefAndCastScope(sc.args[0], d, en);
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Deref and cast to scope.</p>
     *
     * @param m  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The position key.
     * @throws EngineMessage      Shit happens.
     */
    public static AbstractSource derefAndCastScope(Object m, Display d, Engine en)
            throws EngineMessage {
        SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(m, d);
        AbstractSource scope;
        if (!"".equals(sa.fun)) {
            scope = (sa.scope != null ? sa.scope : en.store.user);
            scope = scope.getStore().getSource(sa.fun);
            AbstractSource.checkExistentSource(scope, sa);
        } else {
            scope = null;
        }
        return scope;
    }

}