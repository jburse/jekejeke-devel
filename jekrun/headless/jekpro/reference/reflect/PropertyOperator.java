package jekpro.reference.reflect;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Operator;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.MapHash;

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
    private final static String OP_NSPL = "nspl";
    private final static String OP_NSPR = "nspr";
    private final static String OP_LEVEL = "level";
    private final static String OP_MODE = "mode";
    private final static String OP_SYS_PORTRAY = "sys_portray";
    private final static String OP_SYS_ALIAS = "sys_alias";

    private static final int PROP_FULL_NAME = 0;
    private static final int PROP_NSPL = 1;
    private static final int PROP_NSPR = 2;
    private static final int PROP_LEVEL = 3;
    private static final int PROP_MODE = 4;
    private static final int PROP_VISIBLE = 5;
    private static final int PROP_OVERRIDE = 6;
    private static final int PROP_SYS_USAGE = 7;
    private static final int PROP_SYS_PORTRAY = 8;
    private static final int PROP_SYS_ALIAS = 9;

    /**
     * <p>Create an operator property.</p>
     *
     * @param i The id of the operator property.
     */
    private PropertyOperator(int i) {
        super(i);
    }

    /**
     * <p>Define the operator properties.</p>
     *
     * @return The operator properties.
     */
    public static MapHash<StoreKey, AbstractProperty<Operator>> defineOperProps() {
        MapHash<StoreKey, AbstractProperty<Operator>> operprops = new MapHash<StoreKey, AbstractProperty<Operator>>();
        operprops.add(new StoreKey(PropertyPredicate.OP_FULL_NAME, 1), new PropertyOperator(PROP_FULL_NAME));
        operprops.add(new StoreKey(OP_NSPL, 0), new PropertyOperator(PROP_NSPL));
        operprops.add(new StoreKey(OP_NSPR, 0), new PropertyOperator(PROP_NSPR));
        operprops.add(new StoreKey(OP_LEVEL, 1), new PropertyOperator(PROP_LEVEL));
        operprops.add(new StoreKey(OP_MODE, 1), new PropertyOperator(PROP_MODE));
        operprops.add(new StoreKey(PropertyPredicate.OP_VISIBLE, 1), new PropertyOperator(PROP_VISIBLE));
        operprops.add(new StoreKey(PropertyPredicate.OP_OVERRIDE, 0), new PropertyOperator(PROP_OVERRIDE));
        operprops.add(new StoreKey(PropertyPredicate.OP_SYS_USAGE, 1), new PropertyOperator(PROP_SYS_USAGE));
        operprops.add(new StoreKey(OP_SYS_PORTRAY, 1), new PropertyOperator(PROP_SYS_PORTRAY));
        operprops.add(new StoreKey(OP_SYS_ALIAS, 1), new PropertyOperator(PROP_SYS_ALIAS));
        return operprops;
    }

    /**
     * <p>Retrieve all the operator properties.</p>
     *
     * @param oper The operator.
     * @param en   The engine.
     * @return The properties.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public Object[] getObjProps(Operator oper, Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case PROP_FULL_NAME:
                Object val = new SkelAtom(oper.getKey());
                return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                        new SkelAtom(PropertyPredicate.OP_FULL_NAME), val), Display.DISPLAY_CONST)};
            case PROP_NSPL:
                if ((oper.getBits() & Operator.MASK_OPER_NSPL) != 0) {
                    return new Object[]{new SkelAtom(OP_NSPL)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_NSPR:
                if ((oper.getBits() & Operator.MASK_OPER_NSPR) != 0) {
                    return new Object[]{new SkelAtom(OP_NSPR)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_LEVEL:
                int flags = oper.getLevel();
                if (flags != 0) {
                    val = Integer.valueOf(oper.getLevel());
                    return new Object[]{AbstractTerm.createMolec(
                            new SkelCompound(new SkelAtom(OP_LEVEL), val), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_MODE:
                flags = oper.getBits();
                if ((flags & Operator.MASK_OPER_DEFI) != 0) {
                    val = new SkelAtom(SpecialOper.modeTypeToAtom(
                            flags & Operator.MASK_OPER_MODE, oper.getType()));
                    return new Object[]{AbstractTerm.createMolec(
                            new SkelCompound(new SkelAtom(OP_MODE), val), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_VISIBLE:
                flags = oper.getBits();
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
            case PROP_OVERRIDE:
                if ((oper.getBits() & Operator.MASK_OPER_OVRD) != 0) {
                    return new Object[]{new SkelAtom(PropertyPredicate.OP_OVERRIDE)};
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
                    val = new SkelAtom(str);
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(OP_SYS_PORTRAY), val), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_ALIAS:
                str = oper.getAlias();
                if (str != null) {
                    val = new SkelAtom(str);
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(OP_SYS_ALIAS), val), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
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
                case PROP_FULL_NAME:
                    /* can't modify */
                    return false;
                case PROP_NSPL:
                    oper.setBit(Operator.MASK_OPER_NSPL);
                    return true;
                case PROP_NSPR:
                    oper.setBit(Operator.MASK_OPER_NSPR);
                    return true;
                case PROP_LEVEL:
                    int level = PropertyOperator.derefAndCastLevel(m, d, en);
                    oper.setLevel(level);
                    return true;
                case PROP_MODE:
                    level = PropertyOperator.derefAndCastMode(m, d, en);
                    oper.resetBit(Operator.MASK_OPER_MODE);
                    oper.setBit(level);
                    return true;
                case PROP_VISIBLE:
                    level = PropertyOperator.derefAndCastVisible(m, d, en);
                    oper.resetBit(Operator.MASK_OPER_VISI);
                    oper.setBit(level);
                    return true;
                case PROP_OVERRIDE:
                    oper.setBit(Operator.MASK_OPER_OVRD);
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
                default:
                    throw new IllegalArgumentException("illegal prop");
            }
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
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
                case PROP_FULL_NAME:
                    /* can't modify */
                    return false;
                case PROP_NSPL:
                    oper.resetBit(Operator.MASK_OPER_NSPL);
                    return true;
                case PROP_NSPR:
                    oper.resetBit(Operator.MASK_OPER_NSPR);
                    return true;
                case PROP_LEVEL:
                    oper.setLevel(0);
                    return true;
                case PROP_MODE:
                    oper.resetBit(Operator.MASK_OPER_MODE);
                    return true;
                case PROP_VISIBLE:
                    oper.resetBit(Operator.MASK_OPER_VISI);
                    return true;
                case PROP_OVERRIDE:
                    oper.resetBit(Operator.MASK_OPER_OVRD);
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
                default:
                    throw new IllegalArgumentException("illegal prop");
            }
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
        }
    }

    /****************************************************************/
    /* Deref Utility                                                */
    /****************************************************************/

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
    private static int derefAndCastLevel(Object m, Display d, Engine en)
            throws EngineMessage, ClassCastException {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 1 &&
                ((SkelCompound) m).sym.fun.equals(OP_LEVEL)) {
            m = ((SkelCompound) m).args[0];
            Number num = SpecialEval.derefAndCastInteger(m, d);
            SpecialEval.checkNotLessThanZero(num);
            int level = SpecialEval.castIntValue(num);
            SpecialOper.checkOperatorLevel(level);
            return level;
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Deref and cast to operator mode.</p>
     *
     * @param m  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The operator mode.
     * @throws EngineMessage      Shit happens.
     * @throws ClassCastException Shit happens.
     */
    private static int derefAndCastMode(Object m, Display d, Engine en)
            throws EngineMessage, ClassCastException {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 1 &&
                ((SkelCompound) m).sym.fun.equals(OP_MODE)) {
            m = ((SkelCompound) m).args[0];
            String fun = SpecialUniv.derefAndCastString(m, d);
            return SpecialOper.atomToMode(fun);
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

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