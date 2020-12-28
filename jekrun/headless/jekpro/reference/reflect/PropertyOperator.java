package jekpro.reference.reflect;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Operator;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.array.Types;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.ListArray;
import matula.util.data.MapHashLink;

/**
 * <p>This class provides operator properties.</p>
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
public final class PropertyOperator extends AbstractProperty<Operator> {
    public final static MapHashLink<StoreKey, AbstractProperty<Operator>> DEFAULT
            = new MapHashLink<>();

    private final static String OP_OP = "op";
    private final static String OP_SYS_PORTRAY = "sys_portray";
    private final static String OP_SYS_ALIAS = "sys_alias";
    private final static String OP_SYS_TABR = "sys_tabr";
    private final static String OP_SYS_NSPL = "sys_nspl";
    private final static String OP_SYS_NSPR = "sys_nspr";
    private final static String OP_SYS_NEWR = "sys_newr";

    private static final int PROP_VISIBLE = 0;
    private static final int PROP_OP = 1;
    private static final int PROP_SYS_USAGE = 3;
    private static final int PROP_SYS_PORTRAY = 4;
    private static final int PROP_SYS_ALIAS = 5;
    private static final int PROP_SYS_TABR = 6;
    private static final int PROP_SYS_NSPL = 7;
    private static final int PROP_SYS_NSPR = 8;
    private static final int PROP_SYS_NEWR = 9;
    private static final int PROP_FULL_NAME = 10;

    static {
        DEFAULT.add(new StoreKey(PropertyPredicate.OP_VISIBLE, 1), new PropertyOperator(PROP_VISIBLE,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_SUPR |
                        AbstractProperty.MASK_PROP_PRJF));
        DEFAULT.add(new StoreKey(OP_OP, 2), new PropertyOperator(PROP_OP,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_META));
        DEFAULT.add(new StoreKey(PropertyPredicate.OP_SYS_USAGE, 1), new PropertyOperator(PROP_SYS_USAGE));
        DEFAULT.add(new StoreKey(OP_SYS_PORTRAY, 1), new PropertyOperator(PROP_SYS_PORTRAY,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_SETP));
        DEFAULT.add(new StoreKey(OP_SYS_ALIAS, 1), new PropertyOperator(PROP_SYS_ALIAS,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_SETP));
        DEFAULT.add(new StoreKey(OP_SYS_TABR, 0), new PropertyOperator(PROP_SYS_TABR));
        DEFAULT.add(new StoreKey(OP_SYS_NSPL, 0), new PropertyOperator(PROP_SYS_NSPL));
        DEFAULT.add(new StoreKey(OP_SYS_NSPR, 0), new PropertyOperator(PROP_SYS_NSPR));
        DEFAULT.add(new StoreKey(OP_SYS_NEWR, 0), new PropertyOperator(PROP_SYS_NEWR));
        DEFAULT.add(new StoreKey(PropertyPredicate.OP_FULL_NAME, 1), new PropertyOperator(PROP_FULL_NAME));
    }

    /**
     * <p>Create an operator property.</p>
     *
     * @param i The id of the operator property.
     */
    private PropertyOperator(int i) {
        super(i);
    }

    /**
     * <p>Create an operator property.</p>
     *
     * @param i The id of the operator property.
     * @param f The flags.
     */
    public PropertyOperator(int i, int f) {
        super(i, f);
    }

    /**
     * <p>Retrieve all the operator properties.</p>
     *
     * @param oper The operator.
     * @param en   The engine.
     * @return The properties.
     */
    public Object[] getObjProps(Operator oper, Engine en) {
        switch (id) {
            case PROP_VISIBLE:
                int flags = oper.getBits();
                if ((flags & Operator.MASK_OPER_VSPR) != 0) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(PropertyPredicate.OP_VISIBLE),
                            new SkelAtom(AbstractSource.OP_PRIVATE)), Display.DISPLAY_CONST)};
                } else if ((flags & Operator.MASK_OPER_VSPU) != 0) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(PropertyPredicate.OP_VISIBLE),
                            new SkelAtom(AbstractSource.OP_PUBLIC)), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_OP:
                flags = oper.getLevel();
                if (flags != 0) {
                    Object val = Integer.valueOf(flags);
                    Object val2 = new SkelAtom(SpecialOper.modeTypeToAtom(
                            oper.getBits() & Operator.MASK_OPER_MODE, oper.getType()));
                    return new Object[]{AbstractTerm.createMolec(
                            new SkelCompound(new SkelAtom(OP_OP), val, val2), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_USAGE:
                AbstractSource src = oper.getScope();
                if (src == null)
                    return AbstractBranch.FALSE_PROPERTY;
                if (!Clause.ancestorSource(src, en))
                    return AbstractBranch.FALSE_PROPERTY;
                return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                        new SkelAtom(PropertyPredicate.OP_SYS_USAGE),
                        src.getPathAtom()), Display.DISPLAY_CONST)};
            case PROP_SYS_PORTRAY:
                String str = oper.getPortray();
                if (str != null) {
                    Object val = new SkelAtom(str);
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(OP_SYS_PORTRAY), val), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_ALIAS:
                str = oper.getAlias();
                if (str != null) {
                    Object val = new SkelAtom(str);
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(OP_SYS_ALIAS), val), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_TABR:
                if ((oper.getBits() & Operator.MASK_OPER_TABR) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_TABR)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_NSPL:
                if ((oper.getBits() & Operator.MASK_OPER_NSPL) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_NSPL)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_NSPR:
                if ((oper.getBits() & Operator.MASK_OPER_NSPR) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_NSPR)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_NEWR:
                if ((oper.getBits() & Operator.MASK_OPER_NEWR) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_NEWR)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_FULL_NAME:
                Object val = new SkelAtom(oper.getKey());
                return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                        new SkelAtom(PropertyPredicate.OP_FULL_NAME), val), Display.DISPLAY_CONST)};
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /**
     * <p>Set an operator property.</p>
     *
     * @param oper The operator.
     * @param m    The property skeleton.
     * @param d    The property display.
     * @param en   The engine.
     * @return True if property could be set, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean setObjProp(Operator oper, Object m, Display d, Engine en)
            throws EngineMessage {
        try {
            switch (id) {
                case PROP_VISIBLE:
                    int flags = PropertyOperator.derefAndCastVisible(m, d, en);
                    oper.resetBit(Operator.MASK_OPER_VISI);
                    oper.setBit(flags);
                    return true;
                case PROP_OP:
                    flags = PropertyOperator.derefAndCastOp(m, d, en);
                    oper.setLevel(flags & 0xFFFF);
                    oper.resetBit(Operator.MASK_OPER_MODE);
                    oper.setBit(flags >> 16);
                    return true;
                case PROP_SYS_USAGE:
                    /* can't modify */
                    return false;
                case PROP_SYS_PORTRAY:
                    String str = PropertyOperator.derefAndCastPortray(m, d, en);
                    oper.setPortray(str);
                    return true;
                case PROP_SYS_ALIAS:
                    str = PropertyOperator.derefAndCastAlias(m, d, en);
                    oper.setAlias(str);
                    return true;
                case PROP_SYS_TABR:
                    oper.setBit(Operator.MASK_OPER_TABR);
                    return true;
                case PROP_SYS_NSPL:
                    oper.setBit(Operator.MASK_OPER_NSPL);
                    return true;
                case PROP_SYS_NSPR:
                    oper.setBit(Operator.MASK_OPER_NSPR);
                    return true;
                case PROP_SYS_NEWR:
                    oper.setBit(Operator.MASK_OPER_NEWR);
                    return true;
                case PROP_FULL_NAME:
                    /* can't modify */
                    return false;
                default:
                    throw new IllegalArgumentException("illegal prop");
            }
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
        }
    }

    /**
     * <p>Reset an operator property.</p>
     *
     * @param oper The operator.
     * @param m    The property skeleton.
     * @param d    The property display.
     * @param en   The engine.
     * @return True if property could be set, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean resetObjProp(Operator oper, Object m, Display d, Engine en)
            throws EngineMessage {
        try {
            switch (id) {
                case PROP_VISIBLE:
                    oper.resetBit(Operator.MASK_OPER_VISI);
                    return true;
                case PROP_OP:
                    oper.setLevel(0);
                    oper.resetBit(Operator.MASK_OPER_MODE);
                    return true;
                case PROP_SYS_USAGE:
                    /* can't modify */
                    return false;
                case PROP_SYS_PORTRAY:
                    oper.setPortray(null);
                    return true;
                case PROP_SYS_ALIAS:
                    oper.setAlias(null);
                    return true;
                case PROP_SYS_TABR:
                    oper.resetBit(Operator.MASK_OPER_TABR);
                    return true;
                case PROP_SYS_NSPL:
                    oper.resetBit(Operator.MASK_OPER_NSPL);
                    return true;
                case PROP_SYS_NSPR:
                    oper.resetBit(Operator.MASK_OPER_NSPR);
                    return true;
                case PROP_SYS_NEWR:
                    oper.resetBit(Operator.MASK_OPER_NEWR);
                    return true;
                case PROP_FULL_NAME:
                    /* can't modify */
                    return false;
                default:
                    throw new IllegalArgumentException("illegal prop");
            }
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
        }
    }

    /**
     * <p>Retrieve operators for a property.</p>
     *
     * @param m  The value skeleton.
     * @param d  The value display.
     * @param en The engine.
     * @return The operators, or null.
     * @throws EngineMessage Shit happens.
     */
    public Operator[] idxObjProp(Object m, Display d, Engine en)
            throws EngineMessage {
        if (id == PROP_SYS_USAGE) {
            AbstractSource src = PropertyPredicate.derefAndCastDef(m, d,
                    PropertyPredicate.OP_SYS_USAGE, en);
            if (src == null || !Clause.ancestorSource(src, en))
                return SpecialOper.FALSE_OPERS;
            Operator[] snapshot = src.snapshotOpersInv();
            ListArray<Operator> res = null;
            for (int i = 0; i < snapshot.length; i++) {
                Operator oper = snapshot[i];
                if (!Clause.ancestorSource(oper.getSource(), en))
                    continue;
                if (res == null)
                    res = new ListArray<>();
                res.add(oper);
            }
            if (res == null)
                return SpecialOper.FALSE_OPERS;
            Operator[] vals = new Operator[res.size()];
            res.toArray(vals);
            return vals;
        } else {
            if (id < PROP_VISIBLE || id > PROP_FULL_NAME)
                throw new IllegalArgumentException("illegal prop");
            return null;
        }
    }

    /****************************************************************/
    /* Deref Utility                                                */
    /****************************************************************/

    /**
     * <p>Deref and cast to operator visibility.</p>
     *
     * @param m  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The operator visibility.
     * @throws EngineMessage      Shit happens.
     * @throws ClassCastException Shit happens.
     */
    private static int derefAndCastVisible(Object m, Display d, Engine en)
            throws EngineMessage, ClassCastException {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 1 &&
                ((SkelCompound) m).sym.fun.equals(PropertyPredicate.OP_VISIBLE)) {
            m = ((SkelCompound) m).args[0];
            String fun = SpecialUniv.derefAndCastString(m, d);
            return PropertyOperator.atomToVisible(fun);
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Deref and cast to operator portray.</p>
     *
     * @param m  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The operator portray.
     * @throws EngineMessage      Shit happens.
     * @throws ClassCastException Shit happens.
     */
    private static String derefAndCastPortray(Object m, Display d, Engine en)
            throws EngineMessage, ClassCastException {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 1 &&
                ((SkelCompound) m).sym.fun.equals(OP_SYS_PORTRAY)) {
            m = ((SkelCompound) m).args[0];
            return SpecialUniv.derefAndCastString(m, d);
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Deref and cast to operator alias.</p>
     *
     * @param m  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The operator alias.
     * @throws EngineMessage      Shit happens.
     * @throws ClassCastException Shit happens.
     */
    private static String derefAndCastAlias(Object m, Display d, Engine en)
            throws EngineMessage, ClassCastException {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 1 &&
                ((SkelCompound) m).sym.fun.equals(OP_SYS_ALIAS)) {
            m = ((SkelCompound) m).args[0];
            return SpecialUniv.derefAndCastString(m, d);
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Deref and cast to operator level.</p>
     *
     * @param m  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The operator level.
     * @throws EngineMessage      Shit happens.
     * @throws ClassCastException Shit happens.
     */
    private static int derefAndCastOp(Object m, Display d, Engine en)
            throws EngineMessage, ClassCastException {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 2 &&
                ((SkelCompound) m).sym.fun.equals(OP_OP)) {
            Object[] temp = ((SkelCompound) m).args;
            Number num = SpecialEval.derefAndCastInteger(temp[0], d);
            SpecialEval.checkNotLessThanZero(num);
            int level = SpecialEval.castIntValue(num);
            SpecialOper.checkOperatorLevel(level);
            String fun = SpecialUniv.derefAndCastString(temp[1], d);
            int mode = SpecialOper.atomToMode(fun);
            return level + (mode << 16);
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /****************************************************************/
    /* Atoms Utility                                                */
    /****************************************************************/


    /**
     * <p>Decode an operator visibility.</p>
     * <p>The following syntax is recognized:</p>
     * <pre>
     *     mode = "private" | "public"".
     * </pre>
     *
     * @param fun The visibility string.
     * @return The operator visibility.
     * @throws EngineMessage Shit happens.
     */
    private static int atomToVisible(String fun)
            throws EngineMessage {
        int flags;
        if (AbstractSource.OP_PRIVATE.equals(fun)) {
            flags = Operator.MASK_OPER_VSPR;
        } else if (AbstractSource.OP_PUBLIC.equals(fun)) {
            flags = Operator.MASK_OPER_VSPU;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_PROPERTY_VALUE,
                    new SkelAtom(fun)));
        }
        return flags;
    }

}