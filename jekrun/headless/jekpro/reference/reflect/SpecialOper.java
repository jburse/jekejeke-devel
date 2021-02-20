package jekpro.reference.reflect;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.Operator;
import jekpro.reference.runtime.SpecialDynamic;
import jekpro.reference.runtime.SpecialLogic;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.comp.sharik.AbstractTracking;
import matula.util.config.AbstractBundle;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;

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
    public final static int SPECIAL_SYS_NEUTRAL_OPER = 0;
    private final static int SPECIAL_SYS_CURRENT_OPER = 2;
    private final static int SPECIAL_SYS_CURRENT_OPER_CHK = 3;
    private final static int SPECIAL_SYS_OPER_PROPERTY = 4;
    private final static int SPECIAL_SYS_OPER_PROPERTY_CHK = 5;
    private final static int SPECIAL_SYS_OPER_PROPERTY_IDX = 6;
    public final static int SPECIAL_SET_OPER_PROPERTY = 7;
    private final static int SPECIAL_RESET_OPER_PROPERTY = 8;
    private final static int SPECIAL_SYS_SYNTAX_PROPERTY_IDX = 9;
    private final static int SPECIAL_SYS_SYNTAX_PROPERTY_CHK = 10;

    public final static Operator[] FALSE_OPERS = new Operator[]{};

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
     * <p>Create a syntax special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialOper(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a term in a list of goals for the first time.</p>
     * <p>The term is passed via the skel and display of the engine.</p>
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
            case SPECIAL_SYS_NEUTRAL_OPER:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                SpecialOper.operToOperatorDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_CRTE);
                return true;
            case SPECIAL_SYS_CURRENT_OPER:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                if (!en.unify(currentOpers(en), Display.DISPLAY_CONST, temp[0], ref))
                    return false;
                return true;
            case SPECIAL_SYS_CURRENT_OPER_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                Operator oper = SpecialOper.operToOperator(temp[0], ref, en);
                if (oper == null)
                    return false;
                return true;
            case SPECIAL_SYS_OPER_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                oper = SpecialOper.operToOperator(temp[0], ref, en);
                if (oper == null)
                    return false;
                operToProperties(oper, en);
                Display d = en.display;
                boolean multi = d.getAndReset();
                if (!en.unify(en.skel, d, temp[1], ref))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
            case SPECIAL_SYS_OPER_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                oper = SpecialOper.operToOperator(temp[0], ref, en);
                if (oper == null)
                    return false;
                StoreKey prop = AbstractProperty.propToStoreKey(temp[1], ref, en);
                SpecialOper.operToProperty(oper, prop, en);
                d = en.display;
                multi = d.getAndReset();
                if (!en.unify(en.skel, d, temp[2], ref))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
            case SPECIAL_SYS_OPER_PROPERTY_IDX:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                if (!en.unify(SpecialOper.propertyToOperators(en.skel, en.display, en), Display.DISPLAY_CONST, temp[1], ref))
                    return false;
                return true;
            case SPECIAL_SET_OPER_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                oper = SpecialOper.operToOperator(temp[0], ref, en);
                Operator.checkExistentOperator(oper, temp[0], ref);

                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialOper.setOperProp(oper, en.skel, en.display, en);
                return true;
            case SPECIAL_RESET_OPER_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                oper = SpecialOper.operToOperator(temp[0], ref, en);
                Operator.checkExistentOperator(oper, temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialOper.resetOperProp(oper, en.skel, en.display, en);
                return true;
            case SPECIAL_SYS_SYNTAX_PROPERTY_IDX:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                if (!en.unify(propertyToSyntax(en.skel, en.display, en), Display.DISPLAY_CONST, temp[1], ref))
                    return false;
                return true;
            case SPECIAL_SYS_SYNTAX_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                oper = SpecialOper.operToOperatorDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_UCHK);
                if (oper == null)
                    return false;
                prop = AbstractProperty.propToStoreKey(temp[1], ref, en);
                SpecialOper.operToProperty(oper, prop, en);
                d = en.display;
                multi = d.getAndReset();
                if (!en.unify(en.skel, d, temp[2], ref))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
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
        Store store = en.store;
        Object res = en.store.foyer.ATOM_NIL;
        while (store != null) {
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int j = 0; j < sources.length; j++) {
                AbstractSource base = sources[j].value;
                Operator[] opers = base.snapshotOper();
                res = consOperators(opers, res, en);
            }
            store = store.parent;
        }
        return res;
    }

    /**
     * <p>Collect and filter operator indicators.</p>
     *
     * @param opers The operators.
     * @param res   The old predicate indicators.
     * @param en    The engine.
     * @return The new predicate indicators.
     * @throws EngineMessage Shit happens.
     */
    private static Object consOperators(Operator[] opers, Object res,
                                        Engine en)
            throws EngineMessage {
        for (int i = opers.length - 1; i >= 0; i--) {
            Operator oper = opers[i];
            if (!OperatorSearch.visibleOper(oper, en.store.user))
                continue;
            Object val = SpecialOper.operToColonSkel(oper.getName(),
                    oper.getSource().getFullName(), oper.getType(), en);
            res = new SkelCompound(en.store.foyer.ATOM_CONS, val, res);
        }
        return res;
    }

    /***********************************************************************/
    /* High-Level Operator Property Access I                               */
    /***********************************************************************/

    /**
     * <p>Create a prolog list for the properties of the given operator.</p>
     * <p>Result is returned in skeleton and display.</p>
     *
     * @param oper The operator.
     * @param en   The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static void operToProperties(Operator oper,
                                        Engine en)
            throws EngineMessage, EngineException {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot
                = en.store.foyer.snapshotTrackings();
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        for (int i = snapshot.length - 1; i >= 0; i--) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            MapHashLink<StoreKey, AbstractProperty<Operator>> props = branch.getOperProps();
            for (MapEntry<StoreKey, AbstractProperty<Operator>> entry2 =
                 (props != null ? props.getLastEntry() : null);
                 entry2 != null; entry2 = props.predecessor(entry2)) {
                AbstractProperty<Operator> prop = entry2.value;
                Object t = en.skel;
                Display d = en.display;
                Object[] vals = prop.getObjProps(oper, en);
                en.skel = t;
                en.display = d;
                AbstractProperty.consArray(vals, en);
            }
        }
    }

    /**
     * <p>Create a prolog list for the property of the given operator.</p>
     * <p>Result is returned in skeleton and display.</p>
     *
     * @param oper The operator.
     * @param sk   The property.
     * @param en   The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static void operToProperty(Operator oper, StoreKey sk,
                                      Engine en)
            throws EngineMessage, EngineException {
        AbstractProperty<Operator> prop = SpecialOper.findOperProperty(sk, en);
        Object[] vals = prop.getObjProps(oper, en);
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        AbstractProperty.consArray(vals, en);
    }

    /***********************************************************************/
    /* High-Level Operator Property Access II                              */
    /***********************************************************************/

    /**
     * <p>Set an operator property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param oper The operator.
     * @param m    The value skeleton.
     * @param d    The value display.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void setOperProp(Operator oper,
                                   Object m, Display d, Engine en)
            throws EngineMessage {
        StoreKey sk = StackElement.callableToStoreKey(m);
        AbstractProperty<Operator> prop = findOperProperty(sk, en);
        if (!prop.setObjProp(oper, m, d, en))
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_PROPERTY,
                    sk.storeKeyToSkel()));
    }

    /**
     * <p>Reset an operator property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param oper The operator.
     * @param m    The value skeleton.
     * @param d    The value display.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void resetOperProp(Operator oper,
                                     Object m, Display d, Engine en)
            throws EngineMessage {
        StoreKey sk = StackElement.callableToStoreKey(m);
        AbstractProperty<Operator> prop = SpecialOper.findOperProperty(sk, en);
        if (!prop.resetObjProp(oper, m, d, en))
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_PROPERTY,
                    sk.storeKeyToSkel()));
    }

    /**
     * <p>Retrieve the operators for a property.</p>
     *
     * @param t  The value skeleton.
     * @param d  The value display.
     * @param en The engine.
     */
    private static Object propertyToOperators(Object t, Display d,
                                              Engine en)
            throws EngineMessage, EngineException {
        StoreKey sk = StackElement.callableToStoreKey(t);
        AbstractProperty<Operator> prop = findOperProperty(sk, en);
        Operator[] vals = prop.idxObjProp(t, d, en);
        Object res = en.store.foyer.ATOM_NIL;
        res = consOperators(vals, res, en);
        return res;
    }

    /**
     * <p>Find a operator property.</p>
     * <p>Throws a domain error for undefined display properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param sk The property.
     * @param en The engine.
     * @return The operator property.
     * @throws EngineMessage Shit happens.
     */
    public static AbstractProperty<Operator> findOperProperty(StoreKey sk,
                                                              Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot
                = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            MapHashLink<StoreKey, AbstractProperty<Operator>> props = branch.getOperProps();
            AbstractProperty<Operator> prop = (props != null ? props.get(sk) : null);
            if (prop != null)
                return prop;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                sk.storeKeyToSkel()));
    }

    /*************************************************************************/
    /* Mode Conversions                                                      */
    /*************************************************************************/

    /**
     * <p>Convert a mode and type to an atom</p>
     *
     * @param leftright The leftright.
     * @param type      The type.
     * @return The atom.
     */
    public static String modeTypeToAtom(int leftright, int type) {
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
     * <p>Decode an operator mode.</p>
     * <p>The following syntax is recognized:</p>
     * <pre>
     *     mode = "fx" | "fy" | "xfx" | "xfy" | "yfx" | "xf" | "yf".
     * </pre>
     *
     * @param modestr The mode string.
     * @return The operator mode.
     * @throws EngineMessage Shit happens.
     */
    public static int atomToMode(String modestr)
            throws EngineMessage {
        int mode;
        if (modestr.equals(OP_FX)) {
            mode = Operator.MASK_OPER_RGHT;
        } else if (modestr.equals(OP_FY)) {
            mode = 0;
        } else if (modestr.equals(OP_XFX)) {
            mode = Operator.MASK_OPER_LEFT | Operator.MASK_OPER_RGHT;
        } else if (modestr.equals(OP_XFY)) {
            mode = Operator.MASK_OPER_LEFT;
        } else if (modestr.equals(OP_YFX)) {
            mode = Operator.MASK_OPER_RGHT;
        } else if (modestr.equals(OP_XF)) {
            mode = Operator.MASK_OPER_LEFT;
        } else if (modestr.equals(OP_YF)) {
            mode = 0;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_OPERATOR_SPECIFIER,
                    new SkelAtom(modestr)));
        }
        return mode;
    }

    /**
     * <p>Decode an operator type.</p>
     * <p>The following syntax is recognized:</p>
     * <pre>
     *     mode = "fx" | "fy" | "xfx" | "xfy" | "yfx" | "xf" | "yf".
     * </pre>
     *
     * @param modestr The mode string.
     * @return The operator type.
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
        SpecialLogic.colonToCallable(en.skel, en.display, false, en);
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
        Object s = SpecialDynamic.callableToColonSkel(sa, en);
        return new SkelCompound(typeToOp(type), s);
    }

    /**
     * <p>Convert a type and fun to a colon.</p>
     *
     * @param fun  The name.
     * @param mod  The full name.
     * @param type The type.
     * @param en   The engine.
     * @return The compound.
     * @throws EngineMessage Shit happens.
     */
    public static Object operToColonSkel(String fun, String mod,
                                         int type, Engine en)
            throws EngineMessage {
        Object s = SpecialDynamic.callableToColonSkel(new SkelAtom(fun), mod, en);
        return new SkelCompound(typeToOp(type), s);
    }

    /**
     * <p>Convert a type to a string.</p>
     *
     * @param type The type.
     * @return The string.
     */
    public static SkelAtom typeToOp(int type) {
        switch (type) {
            case Operator.TYPE_PREFIX:
                return new SkelAtom(OP_PREFIX);
            case Operator.TYPE_INFIX:
                return new SkelAtom(OP_INFIX);
            case Operator.TYPE_POSTFIX:
                return new SkelAtom(OP_POSTFIX);
            default:
                throw new IllegalArgumentException("illegal type");
        }
    }

    /**********************************************************/
    /* Level Conversions                                      */
    /**********************************************************/

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

    /**********************************************************/
    /* Moved From Debugger                                    */
    /**********************************************************/

    /**
     * <p>Retrieve the operators for a property.</p>
     *
     * @param t  The value skeleton.
     * @param d  The value display.
     * @param en The engine.
     */
    private static Object propertyToSyntax(Object t, Display d,
                                           Engine en)
            throws EngineMessage, EngineException {
        StoreKey sk = StackElement.callableToStoreKey(t);
        AbstractProperty<Operator> prop = findOperProperty(sk, en);
        Operator[] vals = prop.idxObjProp(t, d, en);
        Object res = en.store.foyer.ATOM_NIL;
        res = consSyntax(vals, res, en);
        return res;
    }

    /**
     * <p>Collect and filter operator indicators.</p>
     *
     * @param opers The operators.
     * @param res   The old predicate indicators.
     * @param en    The engine.
     * @return The new predicate indicators.
     * @throws EngineMessage Shit happens.
     */
    public static Object consSyntax(Operator[] opers, Object res,
                                    Engine en)
            throws EngineMessage {
        for (int i = opers.length - 1; i >= 0; i--) {
            Operator oper = opers[i];
            Object val = operToColonSkel(oper.getName(),
                    oper.getSource().getFullName(), oper.getType(), en);
            res = new SkelCompound(en.store.foyer.ATOM_CONS, val, res);
        }
        return res;
    }

    /**************************************************************/
    /* Operator Lookup                                            */
    /**************************************************************/

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
        Operator oper = OperatorSearch.getOper(sa, type, en);
        en.skel = sa;
        return oper;
    }

    /**
     * <p>Lookup an operator from a compound and possibly create it.</p>
     *
     * @param t    The compound skeleton.
     * @param d    The compound display.
     * @param en   The engine copy.
     * @param copt The create flag.
     * @return The operator.
     * @throws EngineMessage Shit happends.
     */
    public static Operator operToOperatorDefined(Object t, Display d,
                                                 Engine en, int copt)
            throws EngineMessage, EngineException {
        int type = colonToOper(t, d, en);
        SkelAtom sa = (SkelAtom) en.skel;
        Operator oper = OperatorSearch.getOperDefined(sa, type, en, copt);
        en.skel = sa;
        AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
        if (oper == null || ((copt & CachePredicate.MASK_CACH_UCHK) == 0 &&
                !OperatorSearch.visibleOper(oper, src)))
            return null;
        return oper;
    }

}
