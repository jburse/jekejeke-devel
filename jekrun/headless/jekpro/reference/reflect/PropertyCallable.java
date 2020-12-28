package jekpro.reference.reflect;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.bootload.ForeignPath;
import jekpro.reference.runtime.SpecialSession;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.MapHash;
import matula.util.data.MapHashLink;

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
            new MapHash<>();

    public final static String OP_SYS_CONTEXT = "sys_context";
    public final static String OP_SYS_VARIABLE_NAMES = "sys_variable_names";

    private static final int PROP_SYS_CONTEXT = 0;
    private static final int PROP_SYS_VARIABLE_NAMES = 1;

    static {
        DEFAULT.add(new StoreKey(OP_SYS_CONTEXT, 1), new PropertyCallable(PROP_SYS_CONTEXT));
        DEFAULT.add(new StoreKey(OP_SYS_VARIABLE_NAMES, 1), new PropertyCallable(PROP_SYS_VARIABLE_NAMES));
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
            case PROP_SYS_VARIABLE_NAMES:
                Display d = AbstractTerm.getDisplay(obj);
                t = SpecialSession.hashToAssoc(d.vars, d, en);
                return new Object[]{AbstractTerm.createMolec(
                        new SkelCompound(new SkelAtom(OP_SYS_VARIABLE_NAMES), t), d)};
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
            case PROP_SYS_VARIABLE_NAMES:
                MapHash<BindUniv, String> print = derefAndCastAssoc(m, d, en);
                t = AbstractTerm.getSkel(obj);
                Display d2 = AbstractTerm.getDisplay(obj);
                MapHashLink<String, SkelVar> res = collectNames(t, d2, print);
                Display ref;
                if (res != null) {
                    ref = new Display(d2.bind.length);
                    ref.vars = res;
                } else {
                    ref = Display.valueOf(d2.bind.length);
                }
                if (d2.bind.length != 0)
                    ref.marker = true;
                en.skel = t;
                en.display = ref;
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
     * @return True if property could be reset, otherwise false.
     */
    public boolean resetObjProp(Object obj, Object m, Display d, Engine en) {
        switch (id) {
            case PROP_SYS_CONTEXT:
                Object t = AbstractTerm.getSkel(obj);
                SkelAtom sa = StackElement.callableToName(t);
                sa = new SkelAtom(sa.fun);
                en.skel = StackElement.callableFromName(t, sa);
                en.display = AbstractTerm.getDisplay(obj);
                return true;
            case PROP_SYS_VARIABLE_NAMES:
                t = AbstractTerm.getSkel(obj);
                Display d2 = AbstractTerm.getDisplay(obj);
                Display ref = Display.valueOf(d2.bind.length);
                if (d2.bind.length != 0)
                    ref.marker = true;
                en.skel = t;
                en.display = ref;
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
     * @throws EngineMessage Shit happens.
     */
    private static AbstractSource derefAndCastContext(Object m,
                                                      Display d, Engine en)
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
            return ForeignPath.derefAndCastScope(sc.args[0], d, en);
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Deref and cast to assoc.</p>
     *
     * @param m  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The print map.
     * @throws EngineMessage Shit happens.
     */
    private static MapHash<BindUniv, String> derefAndCastAssoc(Object m,
                                                               Display d, Engine en)
            throws EngineMessage {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 1 &&
                ((SkelCompound) m).sym.fun.equals(OP_SYS_VARIABLE_NAMES)) {
            SkelCompound sc = (SkelCompound) m;
            return SupervisorCopy.assocToMapUniv(sc.args[0], d, en);
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /****************************************************************/
    /* Create Hash                                                  */
    /****************************************************************/

    /**
     * <p>Make a copy of the given variable names.</p>
     * <p>Only copy terms that are bound to a variable.</p>
     * <p>Only copy variables that already exist in rule.</p>
     *
     * @param m     The term skeleton.
     * @param d     The term display.
     * @param print The print map.
     * @return The named copy.
     */
    private static MapHashLink<String, SkelVar> collectNames(Object m, Display d,
                                                             MapHash<BindUniv, String> print) {
        Object var = SupervisorCopy.getVar(m);
        if (print == null || var == null)
            return null;
        MapHashLink<String, SkelVar> copy = null;
        SkelVar v;
        if (var instanceof SkelVar) {
            v = (SkelVar) var;
        } else {
            SkelVar[] temp = (SkelVar[]) var;
            int i = 0;
            for (; i < temp.length - 1; i++) {
                v = temp[i];
                String name = print.get(d.bind[v.id]);
                if (name == null)
                    continue;
                if (copy == null)
                    copy = new MapHashLink<>();
                copy.add(name, v);
            }
            v = temp[i];
        }
        String name = print.get(d.bind[v.id]);
        if (name == null)
            return copy;
        if (copy == null)
            copy = new MapHashLink<>();
        copy.add(name, v);
        return copy;
    }

}