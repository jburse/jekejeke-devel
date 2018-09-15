package jekpro.reference.reflect;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Frame;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.molec.OperatorSearch;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.AbstractStore;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Operator;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermCompound;
import matula.util.data.MapEntry;

/**
 * <p>Provides built-in predicates for the operators.</p>
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
public final class SpecialOper extends AbstractSpecial {
    private final static int SPECIAL_SYS_CURRENT_OPER = 2;
    private final static int SPECIAL_SYS_CURRENT_OPER_CHK = 3;
    private final static int SPECIAL_SYS_OPER_PROPERTY = 4;
    private final static int SPECIAL_SYS_OPER_PROPERTY_CHK = 5;

    private final static int SPECIAL_RESET_OPER_PROPERTY = 7;

    public final static String OP_FULL_NAME = "full_name";
    private final static String OP_NSPL = "nspl";
    private final static String OP_NSPR = "nspr";
    private final static String OP_LEVEL = "level";
    private final static String OP_MODE = "mode";
    public final static String OP_VISIBLE = "visible";
    public final static String OP_OVERRIDE = "override";
    public final static String OP_SYS_CONTEXT = "sys_context";
    public final static String OP_SYS_USAGE = "sys_usage";
    public final static String OP_SYS_PORTRAY = "sys_portray";
    public final static String OP_SYS_ALIAS = "sys_alias";

    public final static String OP_FX = "fx";
    public final static String OP_FY = "fy";
    public final static String OP_XFX = "xfx";
    public final static String OP_XFY = "xfy";
    public final static String OP_YFX = "yfx";
    public final static String OP_XF = "xf";
    public final static String OP_YF = "yf";

    private final static String OP_PREFIX = "prefix";
    private final static String OP_INFIX = "infix";
    private final static String OP_POSTFIX = "postfix";

    /**
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case SPECIAL_SYS_CURRENT_OPER:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                if (!en.unifyTerm(temp[0], ref,
                        currentOpers(en), Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_CURRENT_OPER_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                Operator op = operToOperator(temp[0], ref, en);
                if (op == null)
                    return false;
                return en.getNextRaw();
            case SPECIAL_SYS_OPER_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                op = operToOperator(temp[0], ref, en);
                if (op == null)
                    return false;
                operToProperties(op, en);
                if (!en.unifyTerm(temp[1], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_OPER_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                op = operToOperator(temp[0], ref, en);
                if (op == null)
                    return false;
                StoreKey prop = StoreKey.propToStoreKey(temp[1], ref, en);
                operToProperty(prop, op, en);
                if (!en.unifyTerm(temp[2], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_RESET_OPER_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                op = operToOperator(temp[0], ref, en);
                Operator.checkExistentOperator(op, temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                removeOperProp(en.skel, en.display, op, en);
                return en.getNextRaw();
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**************************************************************/
    /* Oper Enumeration & Lookup                                  */
    /**************************************************************/

    /**
     * <p>Create a prolog list with the public syntax operators.</p>
     *
     * @param en The engine.
     * @return The prolog list of the public syntax operators.
     * @throws EngineMessage Shit happens.
     */
    private static Object currentOpers(Engine en)
            throws EngineMessage {
        AbstractStore store = en.store;
        Object res = en.store.foyer.ATOM_NIL;
        while (store != null) {
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int j = 0; j < sources.length; j++) {
                AbstractSource base = sources[j].value;
                MapEntry<String, Operator>[] opers = base.snapshotOper();
                for (int i = opers.length - 1; i >= 0; i--) {
                    Operator oper = opers[i].value;
                    if (!OperatorSearch.visibleOper(oper, en.store.user))
                        continue;
                    SkelAtom sa = new SkelAtom(oper.getKey(), en.store.user);
                    Object val = SpecialOper.operToColonSkel(oper.getType(), sa, en);
                    res = new SkelCompound(en.store.foyer.ATOM_CONS, val, res);
                }
            }
            store = store.parent;
        }
        return res;
    }

    /**
     * <p>Lookup an operator from a compound.</p>
     *
     * @param t  The compound skeleton.
     * @param d  The compound display.
     * @param en The engine copy.
     * @return The operator.
     * @throws EngineMessage Shit happends.
     */
    public static Operator operToOperator(Object t, Display d,
                                          Engine en)
            throws EngineMessage, EngineException {
        int type = colonToOper(t, d, en);
        SkelAtom sa = (SkelAtom) en.skel;
        Operator op = OperatorSearch.getOper(sa.scope, sa.fun, type, en);
        en.skel = sa;
        return op;
    }

    /****************************************************************************/
    /* High-Level Operator Property Access                                      */
    /****************************************************************************/

    /**
     * <p>Create a prolog list for the properties of the given operator.</p>
     * <p>Result is returned in skeleton and display.</p>
     *
     * @param op The operator.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void operToProperties(Operator op,
                                        Engine en)
            throws EngineMessage {
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        StoreKey[] keys = listOperProp();
        for (int j = keys.length - 1; j >= 0; j--) {
            StoreKey key = keys[j];
            Object t = en.skel;
            Display d = en.display;
            Object[] vals = getOperProp(op, key, en);
            en.skel = t;
            en.display = d;
            AbstractProperty.consArray(vals, en);
        }
    }

    /**
     * <p>Create a prolog list for the property of the given operator.</p>
     * <p>Result is returned in skeleton and display.</p>
     *
     * @param key The property.
     * @param op  The operator.
     * @param en  The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void operToProperty(StoreKey key, Operator op,
                                      Engine en)
            throws EngineMessage {
        Object[] vals = getOperProp(op, key, en);
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        AbstractProperty.consArray(vals, en);
    }

    /**
     * <p>Reset an operator property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param t  The value skeleton.
     * @param d  The value display.
     * @param op The operator.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void removeOperProp(Object t, Display d, Operator op,
                                      Engine en)
            throws EngineMessage {
        StoreKey prop = Frame.callableToStoreKey(t);
        Object[] vals = getOperProp(op, prop, en);
        vals = AbstractProperty.removeValue(vals, AbstractTerm.createMolec(t, d));
        setOperProp(prop, op, vals, en);
    }

    /**
     * <p>Set an operator property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param t  The value skeleton.
     * @param d  The value display.
     * @param op The operator.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void addOperProp(Object t, Display d, Operator op,
                                   Engine en)
            throws EngineMessage {
        StoreKey prop = Frame.callableToStoreKey(t);
        Object[] vals = getOperProp(op, prop, en);
        vals = AbstractProperty.addValue(vals, AbstractTerm.createMolec(t, d));
        setOperProp(prop, op, vals, en);
    }

    /**************************************************************/
    /* Properties Interface                                       */
    /**************************************************************/

    // The property keys
    private static final StoreKey KEY_FULL_NAME = new StoreKey(OP_FULL_NAME, 1);
    private static final StoreKey KEY_NSPL = new StoreKey(OP_NSPL, 0);
    private static final StoreKey KEY_NSPR = new StoreKey(OP_NSPR, 0);
    private static final StoreKey KEY_LEVEL = new StoreKey(OP_LEVEL, 1);
    private static final StoreKey KEY_MODE = new StoreKey(OP_MODE, 1);
    private static final StoreKey KEY_VISIBLE = new StoreKey(OP_VISIBLE, 1);
    private static final StoreKey KEY_OVERRIDE = new StoreKey(OP_OVERRIDE, 0);
    private static final StoreKey KEY_SYS_USAGE = new StoreKey(OP_SYS_USAGE, 1);
    private static final StoreKey KEY_SYS_PORTRAY = new StoreKey(OP_SYS_PORTRAY, 1);
    private static final StoreKey KEY_SYS_ALIAS = new StoreKey(OP_SYS_ALIAS, 1);

    private static StoreKey[] OP_OPER_PROPS = {
            KEY_NSPL,
            KEY_NSPR,
            KEY_VISIBLE,
            KEY_LEVEL,
            KEY_MODE,
            KEY_FULL_NAME,
            KEY_OVERRIDE,
            KEY_SYS_USAGE,
            KEY_SYS_PORTRAY,
            KEY_SYS_ALIAS};

    /**
     * <p>Create a syntax special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialOper(int i) {
        super(i);
    }

    /**
     * <p>Retrieve the list of operator properties.</p>
     *
     * @return The operator properties.
     */
    public static StoreKey[] listOperProp() {
        return OP_OPER_PROPS;
    }

    /**
     * <p>Retrieve some property of an operator.</p>
     *
     * @param oper The operator.
     * @param prop The property.
     * @param en   The engine.
     * @return The values.
     * @throws EngineMessage Shit happends.
     */
    public static Object[] getOperProp(Operator oper, StoreKey prop,
                                       Engine en)
            throws EngineMessage {
        if (KEY_FULL_NAME.equals(prop)) {
            Object val = new SkelAtom(oper.getKey());
            return new Object[]{new TermCompound(new SkelCompound(
                    new SkelAtom(OP_FULL_NAME), val))};
        } else if (KEY_NSPL.equals(prop)) {
            if ((oper.getBits() & Operator.MASK_OPER_NSPL) != 0) {
                return new Object[]{new SkelAtom(OP_NSPL)};
            } else {
                return AbstractBranch.FALSE_PROPERTY;
            }
        } else if (KEY_NSPR.equals(prop)) {
            if ((oper.getBits() & Operator.MASK_OPER_NSPR) != 0) {
                return new Object[]{new SkelAtom(OP_NSPR)};
            } else {
                return AbstractBranch.FALSE_PROPERTY;
            }
        } else if (KEY_LEVEL.equals(prop)) {
            int level = oper.getLevel();
            if (level != 0) {
                Object val = Integer.valueOf(oper.getLevel());
                return new Object[]{new TermCompound(
                        new SkelCompound(new SkelAtom(OP_LEVEL), val))};
            } else {
                return AbstractBranch.FALSE_PROPERTY;
            }
        } else if (KEY_MODE.equals(prop)) {
            int flags = oper.getBits();
            if ((flags & Operator.MASK_OPER_DEFI) != 0) {
                Object val = new SkelAtom(leftRightTypeToAtom(
                        flags & Operator.MASK_OPER_MODE, oper.getType()));
                return new Object[]{new TermCompound(
                        new SkelCompound(new SkelAtom(OP_MODE), val))};
            } else {
                return AbstractBranch.FALSE_PROPERTY;
            }
        } else if (KEY_VISIBLE.equals(prop)) {
            int flags = oper.getBits();
            if ((flags & Operator.MASK_OPER_VSPR) != 0) {
                return new Object[]{new TermCompound(new SkelCompound(
                        new SkelAtom(OP_VISIBLE),
                        new SkelAtom(AbstractSource.OP_PRIVATE)))};
            } else if ((flags & Operator.MASK_OPER_VSPU) != 0) {
                return new Object[]{new TermCompound(new SkelCompound(
                        new SkelAtom(OP_VISIBLE),
                        new SkelAtom(AbstractSource.OP_PUBLIC)))};
            } else {
                return AbstractBranch.FALSE_PROPERTY;
            }
        } else if (KEY_OVERRIDE.equals(prop)) {
            if ((oper.getBits() & Operator.MASK_OPER_OVRD) != 0) {
                return new Object[]{new SkelAtom(OP_OVERRIDE)};
            } else {
                return AbstractBranch.FALSE_PROPERTY;
            }
        } else if (KEY_SYS_USAGE.equals(prop)) {
            AbstractSource src = oper.getScope();
            if (!Clause.ancestorSource(src, en))
                return AbstractBranch.FALSE_PROPERTY;
            return new Object[]{new TermCompound(new SkelCompound(
                    new SkelAtom(SpecialOper.OP_SYS_USAGE),
                    new SkelAtom(oper.getScope().getPath())))};
        } else if (KEY_SYS_PORTRAY.equals(prop)) {
            String portray = oper.getPortray();
            if (portray != null) {
                SkelAtom val = new SkelAtom(portray);
                return new Object[]{new TermCompound(new SkelCompound(
                        new SkelAtom(OP_SYS_PORTRAY), val))};
            } else {
                return AbstractBranch.FALSE_PROPERTY;
            }
        } else if (KEY_SYS_ALIAS.equals(prop)) {
            String alias = oper.getAlias();
            if (alias != null) {
                SkelAtom val = new SkelAtom(alias);
                return new Object[]{new TermCompound(new SkelCompound(
                        new SkelAtom(OP_SYS_ALIAS), val))};
            } else {
                return AbstractBranch.FALSE_PROPERTY;
            }
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                    StoreKey.storeKeyToPropSkel(prop.getFun(), prop.getArity())));
        }
    }

    /**
     * <p>Set some property of an operator.</p>
     *
     * @param prop The property.
     * @param op   The operator.
     * @param vals The values.
     * @param en   The engine.
     * @throws EngineMessage Shit happends.
     */
    public static void setOperProp(StoreKey prop, Operator op,
                                   Object[] vals, Engine en)
            throws EngineMessage {
        try {
            if (KEY_FULL_NAME.equals(prop)) {
                /* can't modify */
            } else if (KEY_NSPL.equals(prop)) {
                if (vals.length != 0) {
                    op.setBit(Operator.MASK_OPER_NSPL);
                } else {
                    op.resetBit(Operator.MASK_OPER_NSPL);
                }
                return;
            } else if (KEY_NSPR.equals(prop)) {
                if (vals.length != 0) {
                    op.setBit(Operator.MASK_OPER_NSPR);
                } else {
                    op.resetBit(Operator.MASK_OPER_NSPR);
                }
                return;
            } else if (KEY_LEVEL.equals(prop)) {
                int level;
                if (vals.length != 0) {
                    Object molec = vals[vals.length - 1];
                    SkelCompound sc = (SkelCompound) AbstractTerm.getSkel(molec);
                    Number num = SpecialEval.derefAndCastInteger(sc.args[0], AbstractTerm.getDisplay(molec));
                    SpecialEval.checkNotLessThanZero(num);
                    level = SpecialEval.castIntValue(num);
                    SpecialOper.checkOperatorLevel(level);
                    if (vals.length > 1)
                        throw new EngineMessage(EngineMessage.permissionError(
                                EngineMessage.OP_PERMISSION_ADD,
                                EngineMessage.OP_PERMISSION_VALUE,
                                new SkelCompound(new SkelAtom(OP_LEVEL),
                                        Integer.valueOf(level))));
                } else {
                    level = 0;
                }
                op.setLevel(level);
                return;
            } else if (KEY_MODE.equals(prop)) {
                int leftright;
                if (vals.length != 0) {
                    Object molec = vals[vals.length - 1];
                    SkelCompound sc = (SkelCompound) AbstractTerm.getSkel(molec);
                    String fun = SpecialUniv.derefAndCastString(sc.args[0], AbstractTerm.getDisplay(molec));
                    leftright = SpecialOper.atomToLeftRight(fun);
                    if (vals.length > 1)
                        throw new EngineMessage(EngineMessage.permissionError(
                                EngineMessage.OP_PERMISSION_ADD,
                                EngineMessage.OP_PERMISSION_VALUE,
                                new SkelCompound(new SkelAtom(OP_MODE),
                                        new SkelAtom(fun))));
                } else {
                    leftright = 0;
                }
                op.resetBit(Operator.MASK_OPER_MODE);
                op.setBit(leftright);
                return;
            } else if (KEY_VISIBLE.equals(prop)) {
                String fun;
                if (vals.length != 0) {
                    Object molec = vals[vals.length - 1];
                    SkelCompound sc = (SkelCompound) AbstractTerm.getSkel(molec);
                    fun = SpecialUniv.derefAndCastString(sc.args[0], AbstractTerm.getDisplay(molec));
                    if (vals.length > 1)
                        throw new EngineMessage(EngineMessage.permissionError(
                                EngineMessage.OP_PERMISSION_ADD,
                                EngineMessage.OP_PERMISSION_VALUE,
                                new SkelCompound(new SkelAtom(OP_VISIBLE),
                                        new SkelAtom(fun))));
                } else {
                    fun = null;
                }
                if (AbstractSource.OP_PRIVATE.equals(fun)) {
                    op.setBit(Operator.MASK_OPER_VSPR);
                    op.resetBit(Operator.MASK_OPER_VSPU);
                } else if (AbstractSource.OP_PUBLIC.equals(fun)) {
                    op.setBit(Operator.MASK_OPER_VSPU);
                    op.resetBit(Operator.MASK_OPER_VSPR);
                } else if (fun == null) {
                    op.resetBit(Operator.MASK_OPER_VSPR);
                    op.resetBit(Operator.MASK_OPER_VSPU);
                } else {
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_PROPERTY_VALUE,
                            new SkelAtom(fun)));
                }
                return;
            } else if (KEY_OVERRIDE.equals(prop)) {
                if (vals.length != 0) {
                    op.setBit(Operator.MASK_OPER_OVRD);
                } else {
                    op.resetBit(Operator.MASK_OPER_OVRD);
                }
                return;
            } else if (KEY_SYS_USAGE.equals(prop)) {
                /* can't modify */
            } else if (KEY_SYS_PORTRAY.equals(prop)) {
                String fun;
                if (vals.length != 0) {
                    Object molec = vals[vals.length - 1];
                    SkelCompound sc = (SkelCompound) AbstractTerm.getSkel(molec);
                    fun = SpecialUniv.derefAndCastString(sc.args[0], AbstractTerm.getDisplay(molec));
                    if (vals.length > 1)
                        throw new EngineMessage(EngineMessage.permissionError(
                                EngineMessage.OP_PERMISSION_ADD,
                                EngineMessage.OP_PERMISSION_VALUE,
                                new SkelCompound(new SkelAtom(OP_SYS_PORTRAY),
                                        new SkelAtom(fun))));
                } else {
                    fun = null;
                }
                op.setPortray(fun);
                return;
            } else if (KEY_SYS_ALIAS.equals(prop)) {
                String fun;
                if (vals.length != 0) {
                    Object molec = vals[vals.length - 1];
                    SkelCompound sc = (SkelCompound) AbstractTerm.getSkel(molec);
                    fun = SpecialUniv.derefAndCastString(sc.args[0], AbstractTerm.getDisplay(molec));
                    if (vals.length > 1)
                        throw new EngineMessage(EngineMessage.permissionError(
                                EngineMessage.OP_PERMISSION_ADD,
                                EngineMessage.OP_PERMISSION_VALUE,
                                new SkelCompound(new SkelAtom(OP_SYS_ALIAS),
                                        new SkelAtom(fun))));
                } else {
                    fun = null;
                }
                op.setAlias(fun);
                return;
            } else {
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                        StoreKey.storeKeyToPropSkel(prop.getFun(), prop.getArity())));
            }
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_PROPERTY,
                    StoreKey.storeKeyToPropSkel(prop.getFun(), prop.getArity())));
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
        }
    }

    /*************************************************************************/
    /* Mode Conversions                                                      */
    /*************************************************************************/

    /**
     * <p>Convert a leftright and type to an atom</p>
     *
     * @param leftright The leftright.
     * @param type      The type.
     * @return The atom.
     */
    public static String leftRightTypeToAtom(int leftright, int type) {
        switch (type) {
            case Operator.TYPE_PREFIX:
                if ((leftright & Operator.MASK_OPER_RGHT) != 0) {
                    return OP_FX;
                } else {
                    return OP_FY;
                }
            case Operator.TYPE_INFIX:
                if ((leftright & Operator.MASK_OPER_LEFT) != 0) {
                    if ((leftright & Operator.MASK_OPER_RGHT) != 0) {
                        return OP_XFX;
                    } else {
                        return OP_XFY;
                    }
                } else {
                    return OP_YFX;
                }
            case Operator.TYPE_POSTFIX:
                if ((leftright & Operator.MASK_OPER_LEFT) != 0) {
                    return OP_XF;
                } else {
                    return OP_YF;
                }
            default:
                throw new IllegalArgumentException("illegal type");
        }
    }

    /**
     * <p>Decode an operator left right.</p>
     * <p>The following syntax is recognized:</p>
     * <pre>
     *     mode = "fx" | "fy" | "xfx" | "xfy" | "yfx" | "xf" | "yf".
     * </pre>
     *
     * @param modestr The mode string.
     * @return The left right.
     * @throws EngineMessage Shit happens.
     */
    public static int atomToLeftRight(String modestr)
            throws EngineMessage {
        int leftright;
        if (modestr.equals(OP_FX)) {
            leftright = Operator.MASK_OPER_DEFI + Operator.MASK_OPER_RGHT;
        } else if (modestr.equals(OP_FY)) {
            leftright = Operator.MASK_OPER_DEFI;
        } else if (modestr.equals(OP_XFX)) {
            leftright = Operator.MASK_OPER_DEFI + Operator.MASK_OPER_LEFT + Operator.MASK_OPER_RGHT;
        } else if (modestr.equals(OP_XFY)) {
            leftright = Operator.MASK_OPER_DEFI + Operator.MASK_OPER_LEFT;
        } else if (modestr.equals(OP_YFX)) {
            leftright = Operator.MASK_OPER_DEFI + Operator.MASK_OPER_RGHT;
        } else if (modestr.equals(OP_XF)) {
            leftright = Operator.MASK_OPER_DEFI + Operator.MASK_OPER_LEFT;
        } else if (modestr.equals(OP_YF)) {
            leftright = Operator.MASK_OPER_DEFI;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_OPERATOR_SPECIFIER,
                    new SkelAtom(modestr)));
        }
        return leftright;
    }

    /**
     * <p>Decode an operator type.</p>
     * <p>The following syntax is recognized:</p>
     * <pre>
     *     mode = "fx" | "fy" | "xfx" | "xfy" | "yfx" | "xf" | "yf".
     * </pre>
     *
     * @param modestr The mode string.
     * @return The type.
     * @throws EngineMessage Shit happens.
     */
    public static int atomToType(String modestr)
            throws EngineMessage {
        int type;
        if (modestr.equals(OP_FX)) {
            type = Operator.TYPE_PREFIX;
        } else if (modestr.equals(OP_FY)) {
            type = Operator.TYPE_PREFIX;
        } else if (modestr.equals(OP_XFX)) {
            type = Operator.TYPE_INFIX;
        } else if (modestr.equals(OP_XFY)) {
            type = Operator.TYPE_INFIX;
        } else if (modestr.equals(OP_YFX)) {
            type = Operator.TYPE_INFIX;
        } else if (modestr.equals(OP_XF)) {
            type = Operator.TYPE_POSTFIX;
        } else if (modestr.equals(OP_YF)) {
            type = Operator.TYPE_POSTFIX;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_OPERATOR_SPECIFIER,
                    new SkelAtom(modestr)));
        }
        return type;
    }

    /*************************************************************************/
    /* Operatpr Indicator Conversions                                        */
    /*************************************************************************/

    /**
     * <p>Convert a colon oper to a type and key.</p>
     * <p>The key is returned in engine skel and display.</p>
     * <p>The following syntax is used:</p>
     * <pre>
     * oper    --> prefix(name)
     *           | postfix(name)
     *           | infix(name).
     *
     * name    --> module ":" name
     *           | atom.
     * </pre>
     * *
     *
     * @param t  The oper skeleton.
     * @param d  The oper display.
     * @param en The engine.
     * @return The type.
     * @throws EngineMessage Shit happends.
     */
    public static int colonToOper(Object t, Display d, Engine en)
            throws EngineMessage {
        int type = opToType(t, d, en);
        SpecialQuali.colonToCallable(en.skel, en.display, false, en);
        if (en.skel instanceof SkelAtom) {
            /* ok */
        } else {
            EngineMessage.checkInstantiated(en.skel);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_ATOM, en.skel), en.display);
        }
        return type;
    }

    /**
     * <p>Convert colon oper to a type and colon atom.</p>
     * <p>The colon atom is returned in engine skel and display.</p>
     *
     * @param t  The oper skeleton.
     * @param d  The oper display.
     * @param en The engine.
     * @return The type.
     * @throws EngineMessage Shit happends.
     */
    public static int opToType(Object t, Display d, Engine en)
            throws EngineMessage {
        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;
        int type;
        if ((t instanceof SkelCompound) &&
                ((SkelCompound) t).args.length == 1 &&
                ((SkelCompound) t).sym.fun.equals(OP_PREFIX)) {
            type = Operator.TYPE_PREFIX;
        } else if ((t instanceof SkelCompound) &&
                ((SkelCompound) t).args.length == 1 &&
                ((SkelCompound) t).sym.fun.equals(OP_INFIX)) {
            type = Operator.TYPE_INFIX;
        } else if ((t instanceof SkelCompound) &&
                ((SkelCompound) t).args.length == 1 &&
                ((SkelCompound) t).sym.fun.equals(OP_POSTFIX)) {
            type = Operator.TYPE_POSTFIX;
        } else {
            EngineMessage.checkInstantiated(en.skel);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_OPER_INDICATOR, t), d);
        }
        en.skel = ((SkelCompound) t).args[0];
        return type;
    }

    /**
     * <p>Convert a type and fun to a colon.</p>
     *
     * @param type The type.
     * @param sa   The name.
     * @param en   The engine.
     * @return The compound.
     * @throws EngineMessage Shit happens.
     */
    public static Object operToColonSkel(int type, SkelAtom sa, Engine en)
            throws EngineMessage {
        Object s = Clause.callableToColonSkel(sa, en);

        return typeToOpSkel(s, type);
    }

    /**
     * <p>Convert a type to a string.</p>
     *
     * @param key  The key.
     * @param type The type.
     * @return The string.
     */
    public static Object typeToOpSkel(Object key, int type) {
        String optype;
        switch (type) {
            case Operator.TYPE_PREFIX:
                optype = OP_PREFIX;
                break;
            case Operator.TYPE_INFIX:
                optype = OP_INFIX;
                break;
            case Operator.TYPE_POSTFIX:
                optype = OP_POSTFIX;
                break;
            default:
                throw new IllegalArgumentException("illegal type");
        }
        return new SkelCompound(new SkelAtom(optype), key);
    }

    /*************************************************************************/
    /* Level Conversions                                                     */
    /*************************************************************************/

    /**
     * <p>Check the operator level.</p>
     *
     * @param level The operator level.
     */
    public static void checkOperatorLevel(int level)
            throws EngineMessage {
        if (level < 0 || level > Operator.LEVEL_HIGH)
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_OPERATOR_PRIORITY,
                    Integer.valueOf(level)));
    }

}
