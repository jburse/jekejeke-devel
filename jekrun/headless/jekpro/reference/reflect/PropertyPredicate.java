package jekpro.reference.reflect;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.Clause;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;

/**
 * <p>This class provides predicate properties.</p>
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
public final class PropertyPredicate extends AbstractProperty<Predicate> {
    public final static String OP_VISIBLE = "visible";

    public final static String OP_OVERRIDE = "override";
    private final static String OP_SYS_MULTIFILE = "sys_multifile";
    private final static String OP_DISCONTIGUOUS = "discontiguous";
    private final static String OP_SYS_STYLE_CHECK = "sys_style_check";
    private final static String OP_SYS_PUBLIC = "sys_public";
    private final static String OP_SYS_PRIVATE = "sys_private";
    private final static String OP_SYS_META_PREDICATE = "sys_meta_predicate";
    private final static String OP_SYS_META_FUNCTION = "sys_meta_function";
    private final static String OP_SYS_DYNAMIC = "sys_dynamic";
    private final static String OP_SYS_THREAD_LOCAL = "sys_thread_local";
    private final static String OP_SYS_GROUP_LOCAL = "sys_group_local";

    private final static String OP_MULTIFILE = "multifile";
    private final static String OP_VIRTUAL = "virtual";
    private final static String OP_SYS_ARITHMETIC = "sys_arithmetic";
    private final static String OP_AUTOMATIC = "automatic";
    private final static String OP_SYS_NOBARRIER = "sys_nobarrier";

    private final static String OP_SYS_NOEXPAND = "sys_noexpand";
    private final static String OP_SYS_NOMACRO = "sys_nomacro";
    private final static String OP_SYS_BODY = "sys_body";
    private final static String OP_SYS_RULE = "sys_rule";
    private final static String OP_SYS_NOTRACE = "sys_notrace";

    private final static String OP_META_PREDICATE = "meta_predicate";
    private final static String OP_META_FUNCTION = "meta_function";

    private final static String OP_BUILT_IN = "built_in";
    private final static String OP_STATIC = "static";
    private final static String OP_DYNAMIC = "dynamic";
    private final static String OP_THREAD_LOCAL = "thread_local";
    private final static String OP_GROUP_LOCAL = "group_local";

    public final static String OP_FULL_NAME = "full_name";
    public final static String OP_SYS_USAGE = "sys_usage";
    private final static String OP_SYS_NOBODY = "sys_nobody";
    private final static String OP_SYS_NOSTACK = "sys_nostack";
    private final static String OP_SYS_NOHEAD = "sys_nohead";

    public final static int PROP_VISIBLE = 0;

    public final static int PROP_OVERRIDE = 1;
    public final static int PROP_SYS_MULTIFILE = 2;
    public final static int PROP_DISCONTIGUOUS = 3;
    public final static int PROP_SYS_STYLE_CHECK = 4;
    public final static int PROP_SYS_PUBLIC = 5;
    public final static int PROP_SYS_PRIVATE = 6;
    public final static int PROP_SYS_META_PREDICATE = 7;
    public final static int PROP_SYS_META_FUNCTION = 8;
    public final static int PROP_SYS_DYNAMIC = 9;
    public final static int PROP_SYS_THREAD_LOCAL = 10;
    public final static int PROP_SYS_GROUP_LOCAL = 11;

    public final static int PROP_MULTIFILE = 12;
    public final static int PROP_VIRTUAL = 13;
    public final static int PROP_SYS_ARITHMETIC = 14;
    public final static int PROP_AUTOMATIC = 15;
    public final static int PROP_SYS_NOBARRIER = 16;

    public final static int PROP_SYS_NOEXPAND = 17;
    public final static int PROP_SYS_NOMACRO = 18;
    public final static int PROP_SYS_BODY = 19;
    public final static int PROP_SYS_RULE = 20;
    public final static int PROP_SYS_NOTRACE = 21;

    public final static int PROP_META_PREDICATE = 22;
    public final static int PROP_META_FUNCTION = 23;

    public final static int PROP_BUILT_IN = 24;
    public final static int PROP_STATIC = 25;
    public final static int PROP_DYNAMIC = 26;
    public final static int PROP_THREAD_LOCAL = 27;
    public final static int PROP_GROUP_LOCAL = 28;

    public final static int PROP_FULL_NAME = 29;
    public final static int PROP_SYS_USAGE = 30;
    public final static int PROP_SYS_NOBODY = 31;
    public final static int PROP_SYS_NOSTACK = 32;
    public final static int PROP_SYS_NOHEAD = 33;

    /**
     * <p>Create a predicate property.</p>
     *
     * @param i The id of the source property.
     */
    private PropertyPredicate(int i) {
        super(i);
    }

    /**
     * <p>Create a predicate property.</p>
     *
     * @param i The id of the source property.
     * @param f The flags.
     */
    private PropertyPredicate(int i, int f) {
        super(i, f);
    }

    /**
     * <p>Define the predicate properties.</p>
     *
     * @return The predicate properties.
     */
    public static MapHash<StoreKey, AbstractProperty<Predicate>> definePredProps() {
        MapHash<StoreKey, AbstractProperty<Predicate>> predprops = new MapHash<StoreKey, AbstractProperty<Predicate>>();
        predprops.add(new StoreKey(OP_VISIBLE, 1), new PropertyPredicate(PROP_VISIBLE,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_SUPR |
                        AbstractProperty.MASK_PROP_PRJF | AbstractProperty.MASK_PROP_MODI));

        predprops.add(new StoreKey(OP_OVERRIDE, 1), new PropertyPredicate(PROP_OVERRIDE,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_SLCF |
                        AbstractProperty.MASK_PROP_MODI));
        predprops.add(new StoreKey(OP_SYS_MULTIFILE, 1), new PropertyPredicate(PROP_SYS_MULTIFILE));
        predprops.add(new StoreKey(OP_DISCONTIGUOUS, 1), new PropertyPredicate(PROP_DISCONTIGUOUS,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_SLCF |
                        AbstractProperty.MASK_PROP_MODI));
        predprops.add(new StoreKey(OP_SYS_STYLE_CHECK, 1), new PropertyPredicate(PROP_SYS_STYLE_CHECK));
        predprops.add(new StoreKey(OP_SYS_PUBLIC, 1), new PropertyPredicate(PROP_SYS_PUBLIC));
        predprops.add(new StoreKey(OP_SYS_PRIVATE, 1), new PropertyPredicate(PROP_SYS_PRIVATE));
        predprops.add(new StoreKey(OP_SYS_META_PREDICATE, 1), new PropertyPredicate(PROP_SYS_META_PREDICATE));
        predprops.add(new StoreKey(OP_SYS_META_FUNCTION, 1), new PropertyPredicate(PROP_SYS_META_FUNCTION));
        predprops.add(new StoreKey(OP_SYS_DYNAMIC, 1), new PropertyPredicate(PROP_SYS_DYNAMIC));
        predprops.add(new StoreKey(OP_SYS_THREAD_LOCAL, 1), new PropertyPredicate(PROP_SYS_THREAD_LOCAL));
        predprops.add(new StoreKey(OP_SYS_GROUP_LOCAL, 1), new PropertyPredicate(PROP_SYS_GROUP_LOCAL));

        predprops.add(new StoreKey(OP_MULTIFILE, 0), new PropertyPredicate(PROP_MULTIFILE,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_MODI));
        predprops.add(new StoreKey(OP_VIRTUAL, 0), new PropertyPredicate(PROP_VIRTUAL,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_MODI));
        predprops.add(new StoreKey(OP_SYS_ARITHMETIC, 0), new PropertyPredicate(PROP_SYS_ARITHMETIC));
        predprops.add(new StoreKey(OP_AUTOMATIC, 0), new PropertyPredicate(PROP_AUTOMATIC));
        predprops.add(new StoreKey(OP_SYS_NOBARRIER, 0), new PropertyPredicate(PROP_SYS_NOBARRIER,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_SETP));

        predprops.add(new StoreKey(OP_SYS_NOEXPAND, 0), new PropertyPredicate(PROP_SYS_NOEXPAND,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_SETP));
        predprops.add(new StoreKey(OP_SYS_NOMACRO, 0), new PropertyPredicate(PROP_SYS_NOMACRO,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_SETP));
        predprops.add(new StoreKey(OP_SYS_BODY, 0), new PropertyPredicate(PROP_SYS_BODY,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_SETP));
        predprops.add(new StoreKey(OP_SYS_RULE, 0), new PropertyPredicate(PROP_SYS_RULE,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_SETP));
        predprops.add(new StoreKey(OP_SYS_NOTRACE, 0), new PropertyPredicate(PROP_SYS_NOTRACE,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_MODI));

        predprops.add(new StoreKey(OP_META_PREDICATE, 1), new PropertyPredicate(PROP_META_PREDICATE,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_META));
        predprops.add(new StoreKey(OP_META_FUNCTION, 1), new PropertyPredicate(PROP_META_FUNCTION,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_META));

        predprops.add(new StoreKey(OP_BUILT_IN, 0), new PropertyPredicate(PROP_BUILT_IN,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_DELE));
        predprops.add(new StoreKey(OP_STATIC, 0), new PropertyPredicate(PROP_STATIC,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_DEFL |
                        AbstractProperty.MASK_PROP_DELE));
        predprops.add(new StoreKey(OP_DYNAMIC, 0), new PropertyPredicate(PROP_DYNAMIC,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_DELE));
        predprops.add(new StoreKey(OP_THREAD_LOCAL, 0), new PropertyPredicate(PROP_THREAD_LOCAL,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_DELE));
        predprops.add(new StoreKey(OP_GROUP_LOCAL, 0), new PropertyPredicate(PROP_GROUP_LOCAL,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_DELE));

        predprops.add(new StoreKey(OP_FULL_NAME, 1), new PropertyPredicate(PROP_FULL_NAME));
        predprops.add(new StoreKey(OP_SYS_USAGE, 1), new PropertyPredicate(PROP_SYS_USAGE));
        predprops.add(new StoreKey(OP_SYS_NOBODY, 0), new PropertyPredicate(PROP_SYS_NOBODY));
        predprops.add(new StoreKey(OP_SYS_NOSTACK, 0), new PropertyPredicate(PROP_SYS_NOSTACK));
        predprops.add(new StoreKey(OP_SYS_NOHEAD, 0), new PropertyPredicate(PROP_SYS_NOHEAD));
        return predprops;
    }

    /**
     * <p>Retrieve all the predicate properties.</p>
     *
     * @param pick The predicate.
     * @param en   The engine.
     * @return The predicate properties.
     * @throws EngineMessage Shit happens.
     */
    public Object[] getObjProps(Predicate pick, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_VISIBLE:
                int flags = pick.getBits();
                if ((flags & Predicate.MASK_PRED_VSPR) != 0) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(OP_VISIBLE),
                            new SkelAtom(AbstractSource.OP_PRIVATE)), Display.DISPLAY_CONST)};
                } else if ((flags & Predicate.MASK_PRED_VSPU) != 0) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(OP_VISIBLE),
                            new SkelAtom(AbstractSource.OP_PUBLIC)), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_OVERRIDE:
                ListArray<Object> res = PropertyPredicate.filterDefs(pick,
                        Predicate.MASK_TRCK_OVRD, en);
                if (res == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return PropertyPredicate.snapshotToVals(
                        new SkelAtom(OP_OVERRIDE), res);
            case PROP_SYS_MULTIFILE:
                res = PropertyPredicate.filterDefs(pick,
                        Predicate.MASK_TRCK_MULT, en);
                if (res == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return PropertyPredicate.snapshotToVals(
                        new SkelAtom(OP_SYS_MULTIFILE), res);
            case PROP_DISCONTIGUOUS:
                res = PropertyPredicate.filterDefs(pick,
                        Predicate.MASK_TRCK_DISC, en);
                if (res == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return PropertyPredicate.snapshotToVals(
                        new SkelAtom(OP_DISCONTIGUOUS), res);
            case PROP_SYS_STYLE_CHECK:
                res = PropertyPredicate.filterDefs(pick,
                        Predicate.MASK_TRCK_STYL, en);
                if (res == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return PropertyPredicate.snapshotToVals(
                        new SkelAtom(OP_SYS_STYLE_CHECK), res);
            case PROP_SYS_PUBLIC:
                res = PropertyPredicate.filterDefs(pick,
                        Predicate.MASK_TRCK_VSPU, en);
                if (res == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return PropertyPredicate.snapshotToVals(
                        new SkelAtom(OP_SYS_PUBLIC), res);
            case PROP_SYS_PRIVATE:
                res = PropertyPredicate.filterDefs(pick,
                        Predicate.MASK_TRCK_VSPR, en);
                if (res == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return PropertyPredicate.snapshotToVals(
                        new SkelAtom(OP_SYS_PRIVATE), res);
            case PROP_SYS_META_PREDICATE:
                res = PropertyPredicate.filterDefs(pick,
                        Predicate.MASK_TRCK_PRED, en);
                if (res == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return PropertyPredicate.snapshotToVals(
                        new SkelAtom(OP_SYS_META_PREDICATE), res);
            case PROP_SYS_META_FUNCTION:
                res = PropertyPredicate.filterDefs(pick,
                        Predicate.MASK_TRCK_FUNC, en);
                if (res == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return PropertyPredicate.snapshotToVals(
                        new SkelAtom(OP_SYS_META_FUNCTION), res);
            case PROP_SYS_DYNAMIC:
                res = PropertyPredicate.filterDefs(pick,
                        Predicate.MASK_TRCK_DYNA, en);
                if (res == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return PropertyPredicate.snapshotToVals(
                        new SkelAtom(OP_SYS_DYNAMIC), res);
            case PROP_SYS_THREAD_LOCAL:
                res = PropertyPredicate.filterDefs(pick,
                        Predicate.MASK_TRCK_TRLC, en);
                if (res == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return PropertyPredicate.snapshotToVals(
                        new SkelAtom(OP_SYS_THREAD_LOCAL), res);
            case PROP_SYS_GROUP_LOCAL:
                res = PropertyPredicate.filterDefs(pick,
                        Predicate.MASK_TRCK_GRLC, en);
                if (res == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return PropertyPredicate.snapshotToVals(
                        new SkelAtom(OP_SYS_GROUP_LOCAL), res);

            case PROP_MULTIFILE:
                if ((pick.getBits() & Predicate.MASK_PRED_MULT) != 0) {
                    return new Object[]{new SkelAtom(Predicate.OP_MULTIFILE)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_VIRTUAL:
                if ((pick.getBits() & Predicate.MASK_PRED_VIRT) != 0) {
                    return new Object[]{new SkelAtom(Predicate.OP_VIRTUAL)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_ARITHMETIC:
                AbstractDelegate fun = pick.del;
                if (fun != null && ((fun.subflags & AbstractDelegate.MASK_DELE_ARIT) != 0)) {
                    return new Object[]{new SkelAtom(OP_SYS_ARITHMETIC)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_AUTOMATIC:
                if ((pick.getBits() & Predicate.MASK_PRED_AUTO) != 0) {
                    return new Object[]{new SkelAtom(OP_AUTOMATIC)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_NOBARRIER:
                if ((pick.getBits() & Predicate.MASK_PRED_NOBR) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_NOBARRIER)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }

            case PROP_SYS_NOEXPAND:
                if ((pick.getBits() & Predicate.MASK_PRED_NOEX) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_NOEXPAND)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_NOMACRO:
                if ((pick.getBits() & Predicate.MASK_PRED_NOMC) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_NOMACRO)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_BODY:
                if ((pick.getBits() & Predicate.MASK_PRED_BODY) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_BODY)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_RULE:
                if ((pick.getBits() & Predicate.MASK_PRED_RULE) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_RULE)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_NOTRACE:
                if ((pick.getBits() & Predicate.MASK_PRED_NOTR) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_NOTRACE)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_META_PREDICATE:
                Object t = pick.meta_predicate;
                if (t != null) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(OP_META_PREDICATE), t), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_META_FUNCTION:
                t = pick.meta_function;
                if (t != null) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(OP_META_FUNCTION), t), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }

            case PROP_BUILT_IN:
                fun = pick.del;
                if (fun != null && !(fun instanceof AbstractDefined)) {
                    return new Object[]{new SkelAtom(OP_BUILT_IN)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_STATIC:
                fun = pick.del;
                if (fun != null && (fun.subflags & AbstractDefined.MASK_DEFI_STAT) != 0) {
                    return new Object[]{new SkelAtom(OP_STATIC)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_DYNAMIC:
                fun = pick.del;
                if (fun != null && (fun.subflags & AbstractDefined.MASK_DEFI_DYNA) != 0) {
                    return new Object[]{new SkelAtom(OP_DYNAMIC)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_THREAD_LOCAL:
                fun = pick.del;
                if (fun != null && (fun.subflags & AbstractDefined.MASK_DEFI_THLC) != 0) {
                    return new Object[]{new SkelAtom(OP_THREAD_LOCAL)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_GROUP_LOCAL:
                fun = pick.del;
                if (fun != null && (fun.subflags & AbstractDefined.MASK_DEFI_GRLC) != 0) {
                    return new Object[]{new SkelAtom(OP_GROUP_LOCAL)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }

            case PROP_FULL_NAME:
                return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                new SkelAtom(OP_FULL_NAME),
                                new SkelAtom(pick.getFun(), pick.getSource().getStore().user)),
                        Display.DISPLAY_CONST)};
            case PROP_SYS_USAGE:
                res = PropertyPredicate.filterDefs(pick, 0, en);
                if (res == null)
                    return AbstractBranch.FALSE_PROPERTY;
                return PropertyPredicate.snapshotToVals(
                        new SkelAtom(OP_SYS_USAGE), res);
            case PROP_SYS_NOBODY:
                fun = pick.del;
                if ((fun instanceof AbstractDefined) &&
                        (fun.subflags & AbstractDefined.MASK_DEFI_NBDY) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_NOBODY)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_NOSTACK:
                fun = pick.del;
                if ((fun instanceof AbstractDefined) &&
                        (fun.subflags & AbstractDefined.MASK_DEFI_NLST) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_NOSTACK)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_NOHEAD:
                fun = pick.del;
                if ((fun instanceof AbstractDefined) &&
                        (fun.subflags & AbstractDefined.MASK_DEFI_NHED) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_NOHEAD)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /**
     * <p>Set a predicate property.</p>
     *
     * @param pick The predicate.
     * @param m    The property skeleton.
     * @param d    The property display.
     * @param en   The engine.
     * @return True if property could be set, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean setObjProp(Predicate pick, Object m, Display d, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_VISIBLE:
                int flags = PropertyPredicate.derefAndCastVisible(m, d, en);
                pick.resetBit(Predicate.MASK_PRED_VISI);
                pick.setBit(flags);
                return true;

            case PROP_OVERRIDE:
                AbstractSource src = derefAndCastDef(m, d, OP_OVERRIDE, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.addDef(src, Predicate.MASK_TRCK_OVRD, en);
                return true;
            case PROP_SYS_MULTIFILE:
                src = derefAndCastDef(m, d, OP_SYS_MULTIFILE, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.addDef(src, Predicate.MASK_TRCK_MULT, en);
                return true;
            case PROP_DISCONTIGUOUS:
                src = derefAndCastDef(m, d, OP_DISCONTIGUOUS, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.addDef(src, Predicate.MASK_TRCK_DISC, en);
                return true;
            case PROP_SYS_STYLE_CHECK:
                src = derefAndCastDef(m, d, OP_SYS_STYLE_CHECK, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.addDef(src, Predicate.MASK_TRCK_STYL, en);
                return true;
            case PROP_SYS_PUBLIC:
                src = derefAndCastDef(m, d, OP_SYS_PUBLIC, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.addDef(src, Predicate.MASK_TRCK_VSPU, en);
                return true;
            case PROP_SYS_PRIVATE:
                src = derefAndCastDef(m, d, OP_SYS_PRIVATE, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.addDef(src, Predicate.MASK_TRCK_VSPR, en);
                return true;
            case PROP_SYS_META_PREDICATE:
                src = derefAndCastDef(m, d, OP_SYS_META_PREDICATE, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.addDef(src, Predicate.MASK_TRCK_PRED, en);
                return true;
            case PROP_SYS_META_FUNCTION:
                src = derefAndCastDef(m, d, OP_SYS_META_FUNCTION, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.addDef(src, Predicate.MASK_TRCK_FUNC, en);
                return true;
            case PROP_SYS_DYNAMIC:
                src = derefAndCastDef(m, d, OP_SYS_DYNAMIC, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.addDef(src, Predicate.MASK_TRCK_DYNA, en);
                return true;
            case PROP_SYS_THREAD_LOCAL:
                src = derefAndCastDef(m, d, OP_SYS_THREAD_LOCAL, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.addDef(src, Predicate.MASK_TRCK_TRLC, en);
                return true;
            case PROP_SYS_GROUP_LOCAL:
                src = derefAndCastDef(m, d, OP_SYS_GROUP_LOCAL, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.addDef(src, Predicate.MASK_TRCK_GRLC, en);
                return true;

            case PROP_MULTIFILE:
                pick.setBit(Predicate.MASK_PRED_MULT);
                return true;
            case PROP_VIRTUAL:
                pick.setBit(Predicate.MASK_PRED_VIRT);
                return true;
            case PROP_SYS_ARITHMETIC:
                /* can't modify */
                return false;
            case PROP_AUTOMATIC:
                pick.setBit(Predicate.MASK_PRED_AUTO);
                return true;
            case PROP_SYS_NOBARRIER:
                pick.setBit(Predicate.MASK_PRED_NOBR);
                return true;

            case PROP_SYS_NOEXPAND:
                pick.setBit(Predicate.MASK_PRED_NOEX);
                return true;
            case PROP_SYS_NOMACRO:
                pick.setBit(Predicate.MASK_PRED_NOMC);
                return true;
            case PROP_SYS_BODY:
                pick.setBit(Predicate.MASK_PRED_BODY);
                return true;
            case PROP_SYS_RULE:
                pick.setBit(Predicate.MASK_PRED_RULE);
                return true;
            case PROP_SYS_NOTRACE:
                pick.setBit(Predicate.MASK_PRED_NOTR);
                return true;

            case PROP_META_PREDICATE:
                pick.meta_predicate = derefAndCastMeta(pick,
                        m, d, OP_META_PREDICATE, en);
                return true;
            case PROP_META_FUNCTION:
                pick.meta_function = derefAndCastMeta(pick,
                        m, d, OP_META_FUNCTION, en);
                return true;

            case PROP_BUILT_IN:
                /* can't modify */
                return false;
            case PROP_STATIC:
                /* can't modify */
                return false;
            case PROP_DYNAMIC:
                /* can't modify */
                return false;
            case PROP_THREAD_LOCAL:
                /* can't modify */
                return false;
            case PROP_GROUP_LOCAL:
                /* can't modify */
                return false;

            case PROP_FULL_NAME:
                /* can't modify */
                return false;
            case PROP_SYS_USAGE:
                /* can't modify */
                return false;
            case PROP_SYS_NOBODY:
                AbstractDelegate fun = pick.del;
                AbstractDefined.checkDefinedWrite(fun, pick, en);
                fun.subflags |= AbstractDefined.MASK_DEFI_NBDY;
                return true;
            case PROP_SYS_NOSTACK:
                fun = pick.del;
                AbstractDefined.checkDefinedWrite(fun, pick, en);
                fun.subflags |= AbstractDefined.MASK_DEFI_NLST;
                return true;
            case PROP_SYS_NOHEAD:
                fun = pick.del;
                AbstractDefined.checkDefinedWrite(fun, pick, en);
                fun.subflags |= AbstractDefined.MASK_DEFI_NHED;
                return true;
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /**
     * <p>Reset a predicate property.</p>
     *
     * @param pick The predicate.
     * @param m    The property skeleton.
     * @param d    The property display.
     * @param en   The engine.
     * @return True if property could be set, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean resetObjProp(Predicate pick, Object m, Display d, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_VISIBLE:
                pick.resetBit(Predicate.MASK_PRED_VISI);
                return true;

            case PROP_OVERRIDE:
                AbstractSource src = derefAndCastDef(m, d, OP_OVERRIDE, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.removeDef(src, Predicate.MASK_TRCK_OVRD);
                return true;
            case PROP_SYS_MULTIFILE:
                src = derefAndCastDef(m, d, OP_SYS_MULTIFILE, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.removeDef(src, Predicate.MASK_TRCK_MULT);
                return true;
            case PROP_DISCONTIGUOUS:
                src = derefAndCastDef(m, d, OP_DISCONTIGUOUS, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.removeDef(src, Predicate.MASK_TRCK_DISC);
                return true;
            case PROP_SYS_STYLE_CHECK:
                src = derefAndCastDef(m, d, OP_SYS_STYLE_CHECK, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.removeDef(src, Predicate.MASK_TRCK_STYL);
                return true;
            case PROP_SYS_PUBLIC:
                src = derefAndCastDef(m, d, OP_SYS_PUBLIC, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.removeDef(src, Predicate.MASK_TRCK_VSPU);
                return true;
            case PROP_SYS_PRIVATE:
                src = derefAndCastDef(m, d, OP_SYS_PRIVATE, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.removeDef(src, Predicate.MASK_TRCK_VSPR);
                return true;
            case PROP_SYS_META_PREDICATE:
                src = derefAndCastDef(m, d, OP_SYS_META_PREDICATE, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.removeDef(src, Predicate.MASK_TRCK_PRED);
                return true;
            case PROP_SYS_META_FUNCTION:
                src = derefAndCastDef(m, d, OP_SYS_META_FUNCTION, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.removeDef(src, Predicate.MASK_TRCK_FUNC);
                return true;
            case PROP_SYS_DYNAMIC:
                src = derefAndCastDef(m, d, OP_SYS_DYNAMIC, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.removeDef(src, Predicate.MASK_TRCK_DYNA);
                return true;
            case PROP_SYS_THREAD_LOCAL:
                src = derefAndCastDef(m, d, OP_SYS_THREAD_LOCAL, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.removeDef(src, Predicate.MASK_TRCK_TRLC);
                return true;
            case PROP_SYS_GROUP_LOCAL:
                src = derefAndCastDef(m, d, OP_SYS_GROUP_LOCAL, en);
                if (src == null || !Clause.ancestorSource(src, en))
                    return true;
                pick.removeDef(src, Predicate.MASK_TRCK_GRLC);
                return true;

            case PROP_MULTIFILE:
                pick.resetBit(Predicate.MASK_PRED_MULT);
                return true;
            case PROP_VIRTUAL:
                pick.resetBit(Predicate.MASK_PRED_VIRT);
                return true;
            case PROP_SYS_ARITHMETIC:
                /* can't modify */
                return false;
            case PROP_AUTOMATIC:
                pick.resetBit(Predicate.MASK_PRED_AUTO);
                return true;
            case PROP_SYS_NOBARRIER:
                pick.resetBit(Predicate.MASK_PRED_NOBR);
                return true;

            case PROP_SYS_NOEXPAND:
                pick.resetBit(Predicate.MASK_PRED_NOEX);
                return true;
            case PROP_SYS_NOMACRO:
                pick.resetBit(Predicate.MASK_PRED_NOMC);
                return true;
            case PROP_SYS_BODY:
                pick.resetBit(Predicate.MASK_PRED_BODY);
                return true;
            case PROP_SYS_RULE:
                pick.resetBit(Predicate.MASK_PRED_RULE);
                return true;
            case PROP_SYS_NOTRACE:
                pick.resetBit(Predicate.MASK_PRED_NOTR);
                return true;

            case PROP_META_PREDICATE:
                pick.meta_predicate = null;
                return true;
            case PROP_META_FUNCTION:
                pick.meta_function = null;
                return true;

            case PROP_BUILT_IN:
                /* can't modify */
                return false;
            case PROP_STATIC:
                /* can't modify */
                return false;
            case PROP_DYNAMIC:
                /* can't modify */
                return false;
            case PROP_THREAD_LOCAL:
                /* can't modify */
                return false;
            case PROP_GROUP_LOCAL:
                /* can't modify */
                return false;

            case PROP_FULL_NAME:
                /* can't modify */
                return false;
            case PROP_SYS_USAGE:
                /* can't modify */
                return false;
            case PROP_SYS_NOBODY:
                AbstractDelegate fun = pick.del;
                AbstractDefined.checkDefinedWrite(fun, pick, en);
                fun.subflags &= ~AbstractDefined.MASK_DEFI_NBDY;
                return true;
            case PROP_SYS_NOSTACK:
                fun = pick.del;
                AbstractDefined.checkDefinedWrite(fun, pick, en);
                fun.subflags &= ~AbstractDefined.MASK_DEFI_NLST;
                return true;
            case PROP_SYS_NOHEAD:
                fun = pick.del;
                AbstractDefined.checkDefinedWrite(fun, pick, en);
                fun.subflags &= ~AbstractDefined.MASK_DEFI_NHED;
                return true;
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /**
     * <p>Retrieve all the objects for a property.</p>
     *
     * @param en The engine.
     * @param m  The property skeleton.
     * @param d  The property display.
     * @return The properties.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public Predicate[] idxObjProp(Object m, Display d, Engine en)
            throws EngineException, EngineMessage {
        if (id == PROP_SYS_USAGE) {
            AbstractSource src = derefAndCastDef(m, d, OP_SYS_USAGE, en);
            if (src == null || !Clause.ancestorSource(src, en))
                return AbstractBranch.FALSE_PREDS;
            MapEntry<Predicate, Integer>[] snapshot = src.snapshotPredsInv();
            ListArray<Predicate> res = null;
            for (int i = 0; i < snapshot.length; i++) {
                Predicate pick = snapshot[i].key;
                if (!Clause.ancestorSource(pick.getSource(), en))
                    continue;
                if (res == null)
                    res = new ListArray<Predicate>();
                res.add(pick);
            }
            if (res == null)
                return AbstractBranch.FALSE_PREDS;
            Predicate[] vals = new Predicate[res.size()];
            res.toArray(vals);
            return vals;
        } else {
            if (id < PROP_VISIBLE || id > PROP_SYS_NOHEAD)
                throw new IllegalArgumentException("illegal prop");
            return null;
        }
    }

    /*****************************************************************/
    /* Access Helper                                                 */
    /*****************************************************************/

    /**
     * <p>Retrieve the definitions that satisfy some condition.</p>
     *
     * @param pick The predicate.
     * @param cond The condition.
     * @param en   The engine.
     * @return The definitions, or null.
     */
    public static ListArray<Object> filterDefs(Predicate pick, int cond, Engine en) {
        MapEntry<AbstractSource, Integer>[] snapshot = pick.snapshotDefs();
        ListArray<Object> res = null;
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractSource, Integer> entry = snapshot[i];
            AbstractSource src = entry.key;
            if (!Clause.ancestorSource(src, en))
                continue;
            if (cond != 0 && (entry.value.intValue() & cond) == 0)
                continue;
            if (res == null)
                res = new ListArray<Object>();
            res.add(src.getPathAtom());
        }
        return res;
    }

    /**
     * <p>Convert a snapshot to a values.</p>
     *
     * @param sa  The property name.
     * @param res The snapshot.
     * @return The values.
     */
    public static Object[] snapshotToVals(SkelAtom sa,
                                          ListArray<Object> res) {
        Object[] vals = new Object[res.size()];
        for (int i = 0; i < res.size(); i++) {
            vals[i] = AbstractTerm.createMolec(new SkelCompound(
                    sa, res.get(i)), Display.DISPLAY_CONST);
        }
        return vals;
    }

    /****************************************************************/
    /* Deref Utility                                                */
    /****************************************************************/

    /**
     * <p>Deref and cast to predicate visibility.</p>
     *
     * @param m  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The predicate visibility.
     * @throws EngineMessage Shit happens.
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
            return PropertyPredicate.atomToVisible(fun);
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Deref and cast to definition source.</p>
     *
     * @param m  The term skeleton.
     * @param d  The term display.
     * @param op The definition name.
     * @param en The engine.
     * @return The definition source.
     * @throws EngineMessage Shit happens.
     */
    public static AbstractSource derefAndCastDef(Object m, Display d,
                                                 String op, Engine en)
            throws EngineMessage {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 1 &&
                ((SkelCompound) m).sym.fun.equals(op)) {
            m = ((SkelCompound) m).args[0];
            SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(m, d);
            AbstractSource source = (sa.scope != null ? sa.scope : en.store.user);
            source = source.getStore().getSource(sa.fun);
            return source;
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Deref and cast to predicate meta.</p>
     *
     * @param pick The predicate.
     * @param m    The term skeleton.
     * @param d    The term display.
     * @param op   The meat name.
     * @param en   The engine.
     * @return The meta term.
     * @throws EngineMessage Shit happens.
     */
    private static Object derefAndCastMeta(Predicate pick, Object m, Display d,
                                           String op, Engine en)
            throws EngineMessage {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 1 &&
                ((SkelCompound) m).sym.fun.equals(op)) {
            m = ((SkelCompound) m).args[0];
            return Predicate.checkMetaSpez(pick, m, d, en);
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
     * <p>Decode a predicate visibility.</p>
     * <p>The following syntax is recognized:</p>
     * <pre>
     *     mode = "private" | "public"".
     * </pre>
     *
     * @param fun The visibility string.
     * @return The predicate visibility.
     * @throws EngineMessage Shit happens.
     */
    private static int atomToVisible(String fun)
            throws EngineMessage {
        int flags;
        if (AbstractSource.OP_PRIVATE.equals(fun)) {
            flags = Predicate.MASK_PRED_VSPR;
        } else if (AbstractSource.OP_PUBLIC.equals(fun)) {
            flags = Predicate.MASK_PRED_VSPU;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_PROPERTY_VALUE,
                    new SkelAtom(fun)));
        }
        return flags;
    }

}