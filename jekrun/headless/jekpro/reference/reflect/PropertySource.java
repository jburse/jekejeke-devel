package jekpro.reference.reflect;

import matula.comp.sharik.LicenseError;
import jekpro.frequent.system.ForeignLocale;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.builtin.Branch;
import jekpro.model.inter.Engine;
import jekpro.model.molec.CacheModule;
import jekpro.model.molec.CacheSubclass;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractFile;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.LoadForce;
import jekpro.model.rope.LoadOpts;
import jekpro.reference.bootload.ForeignPath;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.runtime.SpecialDynamic;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;
import matula.util.system.ForeignUri;

import java.io.IOException;

/**
 * <p>This class provides source properties.</p>
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
public final class PropertySource extends AbstractProperty<AbstractSource> {
    public final static MapHash<StoreKey, AbstractProperty<AbstractSource>> DEFAULT
            = new MapHash<>();

    private final static String OP_SHORT_NAME = "short_name";
    private final static String OP_SYS_CAPABILITY = "sys_capability";
    public final static String OP_EXPIRATION = "expiration";
    public final static String OP_LAST_MODIFIED = "last_modified";
    public final static String OP_VERSION_TAG = "version_tag";
    public final static String OP_DATE = "date";
    public final static String OP_MAX_AGE = "max_age";
    private final static String OP_SYS_NOTRACE = "sys_notrace";
    private final static String OP_SYS_SOURCE_PRELOAD = "sys_source_preload";
    private final static String OP_SYS_TIMING = "sys_timing";

    public final static String OP_SYS_SOURCE_VISIBLE = "sys_source_visible";
    private final static String OP_PACKAGE = "package";
    private final static String OP_USE_PACKAGE = "use_package";
    public final static String OP_SYS_SOURCE_NAME = "sys_source_name";
    public final static String OP_SYS_LINK = "sys_link";
    private final static String OP_SYS_MODULE = "sys_module";

    private static final int PROP_SHORT_NAME = 0;
    private static final int PROP_SYS_CAPABILITY = 1;
    private static final int PROP_EXPIRATION = 2;
    private static final int PROP_LAST_MODIFIED = 3;
    private static final int PROP_VERSION_TAG = 4;
    private static final int PROP_DATE = 5;
    private static final int PROP_MAX_AGE = 6;
    private static final int PROP_SYS_NOTRACE = 7;
    private static final int PROP_SYS_SOURCE_PRELOAD = 8;
    private static final int PROP_SYS_TIMING = 9;

    private static final int PROP_SYS_SOURCE_VISIBLE = 10;
    private static final int PROP_PACKAGE = 11;
    private static final int PROP_USE_PACKAGE = 12;
    private static final int PROP_SYS_SOURCE_NAME = 13;
    private static final int PROP_SYS_LINK = 14;
    private static final int PROP_SYS_MODULE = 15;

    static {
        DEFAULT.add(new StoreKey(OP_SHORT_NAME, 1), new PropertySource(PROP_SHORT_NAME));
        DEFAULT.add(new StoreKey(OP_SYS_CAPABILITY, 1), new PropertySource(PROP_SYS_CAPABILITY));
        DEFAULT.add(new StoreKey(OP_EXPIRATION, 1), new PropertySource(PROP_EXPIRATION));
        DEFAULT.add(new StoreKey(OP_LAST_MODIFIED, 1), new PropertySource(PROP_LAST_MODIFIED));
        DEFAULT.add(new StoreKey(OP_VERSION_TAG, 1), new PropertySource(PROP_VERSION_TAG));
        DEFAULT.add(new StoreKey(OP_DATE, 1), new PropertySource(PROP_DATE));
        DEFAULT.add(new StoreKey(OP_MAX_AGE, 1), new PropertySource(PROP_MAX_AGE));
        DEFAULT.add(new StoreKey(OP_SYS_NOTRACE, 0), new PropertySource(PROP_SYS_NOTRACE,
                AbstractProperty.MASK_PROP_SHOW));
        DEFAULT.add(new StoreKey(OP_SYS_SOURCE_PRELOAD, 0), new PropertySource(PROP_SYS_SOURCE_PRELOAD,
                AbstractProperty.MASK_PROP_SHOW));
        DEFAULT.add(new StoreKey(OP_SYS_TIMING, 1), new PropertySource(PROP_SYS_TIMING));

        DEFAULT.add(new StoreKey(OP_SYS_SOURCE_VISIBLE, 1), new PropertySource(PROP_SYS_SOURCE_VISIBLE,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_DEFL));
        DEFAULT.add(new StoreKey(OP_PACKAGE, 1), new PropertySource(PROP_PACKAGE, AbstractProperty.MASK_PROP_SHOW));
        DEFAULT.add(new StoreKey(OP_USE_PACKAGE, 1), new PropertySource(PROP_USE_PACKAGE, AbstractProperty.MASK_PROP_SHOW));
        DEFAULT.add(new StoreKey(OP_SYS_SOURCE_NAME, 1), new PropertySource(PROP_SYS_SOURCE_NAME,
                AbstractProperty.MASK_PROP_SHOW | AbstractProperty.MASK_PROP_DEFL));
        DEFAULT.add(new StoreKey(OP_SYS_LINK, 2), new PropertySource(PROP_SYS_LINK, AbstractProperty.MASK_PROP_SHOW));

        DEFAULT.add(new StoreKey(OP_SYS_MODULE, 1), new PropertySource(PROP_SYS_MODULE));
    }

    /**
     * <p>Create a source property.</p>
     *
     * @param i The id of the source property.
     */
    private PropertySource(int i) {
        super(i);
    }

    /**
     * <p>Create a source property.</p>
     *
     * @param i The id of the source property.
     * @param f The flags.
     */
    private PropertySource(int i, int f) {
        super(i, f);
    }

    /**
     * <p>Retrieve all the object properties.</p>
     *
     * @param src The object.
     * @param en  The engine.
     * @return The properties.
     * @throws EngineMessage Shit happens.
     */
    public Object[] getObjProps(AbstractSource src, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_SHORT_NAME:
                return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                        new SkelAtom(AbstractSource.OP_SHORT_NAME),
                        new SkelAtom(ForeignLocale.shortName(src.getPath()))), Display.DISPLAY_CONST)};
            case PROP_SYS_CAPABILITY:
                AbstractBranch capa = (AbstractBranch) src.getBranch();
                if (capa != null) {
                    AbstractFactory factory = en.store.foyer.getFactory();
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(OP_SYS_CAPABILITY),
                            new SkelAtom(factory.getReflection().branchToString(capa))), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_EXPIRATION:
                if (src instanceof AbstractFile) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(PropertySource.OP_EXPIRATION),
                            TermAtomic.normBigInteger(((AbstractFile) src).getExpiration())), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_LAST_MODIFIED:
                if (src instanceof AbstractFile) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(PropertySource.OP_LAST_MODIFIED),
                            TermAtomic.normBigInteger(((AbstractFile) src).getLastModified())), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_VERSION_TAG:
                if (src instanceof AbstractFile) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(PropertySource.OP_VERSION_TAG),
                            new SkelAtom(((AbstractFile) src).getETag())), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_DATE:
                if (src instanceof AbstractFile) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(PropertySource.OP_DATE),
                            TermAtomic.normBigInteger(((AbstractFile) src).getDate())), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_MAX_AGE:
                if (src instanceof AbstractFile) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(PropertySource.OP_MAX_AGE),
                            Integer.valueOf(((AbstractFile) src).getMaxAge())), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_NOTRACE:
                if ((src.getBits() & AbstractSource.MASK_SRC_NOTR) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_NOTRACE)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_SOURCE_PRELOAD:
                if ((src.getBits() & AbstractSource.MASK_SRC_PREL) != 0) {
                    return new Object[]{new SkelAtom(AbstractSource.OP_SYS_SOURCE_PRELOAD)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_TIMING:
                return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                        new SkelAtom(AbstractSource.OP_SYS_TIMING),
                        TermAtomic.normBigInteger(src.getTiming())), Display.DISPLAY_CONST)};

            case PROP_SYS_SOURCE_VISIBLE:
                int flags = src.getBits();
                if ((flags & AbstractSource.MASK_SRC_VSPR) != 0) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(AbstractSource.OP_SYS_SOURCE_VISIBLE),
                            new SkelAtom(AbstractSource.OP_PRIVATE)), Display.DISPLAY_CONST)};
                } else if ((flags & AbstractSource.MASK_SRC_VSPU) != 0) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(AbstractSource.OP_SYS_SOURCE_VISIBLE),
                            new SkelAtom(AbstractSource.OP_PUBLIC)), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_PACKAGE:
                MapEntry<String, Integer>[] fixes = src.snapshotFixes();
                if (fixes.length == 0)
                    return AbstractBranch.FALSE_PROPERTY;
                ListArray<Object> list = new ListArray<>();
                for (int i = 0; i < fixes.length; i++) {
                    MapEntry<String, Integer> fix = fixes[i];
                    Object val;
                    flags = fix.value.intValue();
                    if ((flags & AbstractSource.MASK_PCKG_LIBR) != 0) {
                        val = AbstractFile.osToSlashSkel(fix.key, false, null);
                        Object test = AbstractTerm.createMolec(new SkelCompound(
                                new SkelAtom(LoadOpts.OP_PACKAGE),
                                new SkelCompound(new SkelAtom(LoadOpts.OP_PREFIX_LIBRARY), val)), Display.DISPLAY_CONST);
                        list.add(test);
                    }
                    if ((flags & AbstractSource.MASK_PCKG_FRGN) != 0) {
                        val = AbstractFile.osToSlashSkel(fix.key, false, null);
                        Object test = AbstractTerm.createMolec(new SkelCompound(
                                new SkelAtom(LoadOpts.OP_PACKAGE),
                                new SkelCompound(new SkelAtom(LoadOpts.OP_PREFIX_FOREIGN), val)), Display.DISPLAY_CONST);
                        list.add(test);
                    }
                }
                Object[] vals = new Object[list.size()];
                list.toArray(vals);
                return vals;
            case PROP_USE_PACKAGE:
                fixes = src.snapshotFixes();
                if (fixes.length == 0)
                    return AbstractBranch.FALSE_PROPERTY;
                list = new ListArray<>();
                for (int i = 0; i < fixes.length; i++) {
                    MapEntry<String, Integer> fix = fixes[i];
                    Object val;
                    flags = fix.value.intValue();
                    if ((flags & AbstractSource.MASK_USES_LIBR) != 0) {
                        val = AbstractFile.osToSlashSkel(fix.key, false, null);
                        Object test = AbstractTerm.createMolec(new SkelCompound(
                                new SkelAtom(LoadOpts.OP_USE_PACKAGE),
                                new SkelCompound(new SkelAtom(LoadOpts.OP_PREFIX_LIBRARY), val)), Display.DISPLAY_CONST);
                        list.add(test);
                    }
                    if ((flags & AbstractSource.MASK_USES_FRGN) != 0) {
                        val = AbstractFile.osToSlashSkel(fix.key, false, null);
                        Object test = AbstractTerm.createMolec(new SkelCompound(
                                new SkelAtom(LoadOpts.OP_USE_PACKAGE),
                                new SkelCompound(new SkelAtom(LoadOpts.OP_PREFIX_FOREIGN), val)), Display.DISPLAY_CONST);
                        list.add(test);
                    }
                }
                vals = new Object[list.size()];
                list.toArray(vals);
                return vals;
            case PROP_SYS_SOURCE_NAME:
                String s = src.getName();
                if (s != null) {
                    s = s.replace(CacheSubclass.OP_CHAR_SYN, CacheModule.OP_CHAR_OS);
                    Object val = AbstractFile.osToSlashSkel(s, true, null);
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                            new SkelAtom(AbstractSource.OP_SYS_SOURCE_NAME), val), Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_LINK:
                MapEntry<AbstractSource, Integer>[] deps = src.snapshotDeps();
                if (deps.length == 0)
                    return AbstractBranch.FALSE_PROPERTY;
                list = new ListArray<>();
                for (int i = 0; i < deps.length; i++) {
                    MapEntry<AbstractSource, Integer> dep = deps[i];
                    flags = dep.value.intValue();
                    if ((flags & AbstractSource.MASK_IMPT_AUTO) != 0 &&
                            (flags & AbstractSource.MASK_IMPT_MODL) == 0 &&
                            (flags & AbstractSource.MASK_IMPT_REEX) == 0) {
                        Object val = new SkelAtom(LoadForce.OP_SYS_LINK_SYS_AUTO_LOAD);
                        list.add(AbstractTerm.createMolec(new SkelCompound(new SkelAtom(PropertySource.OP_SYS_LINK),
                                dep.key.getPathAtom(), val), Display.DISPLAY_CONST));
                    } else if ((flags & AbstractSource.MASK_IMPT_AUTO) != 0 &&
                            (flags & AbstractSource.MASK_IMPT_MODL) != 0 &&
                            (flags & AbstractSource.MASK_IMPT_REEX) == 0) {
                        Object val = new SkelAtom(LoadForce.OP_SYS_LINK_USE_MODULE);
                        list.add(AbstractTerm.createMolec(new SkelCompound(new SkelAtom(PropertySource.OP_SYS_LINK),
                                dep.key.getPathAtom(), val), Display.DISPLAY_CONST));
                    } else if ((flags & AbstractSource.MASK_IMPT_AUTO) != 0 &&
                            (flags & AbstractSource.MASK_IMPT_MODL) != 0 &&
                            (flags & AbstractSource.MASK_IMPT_REEX) != 0) {
                        Object val = new SkelAtom(LoadForce.OP_SYS_LINK_REEXPORT);
                        list.add(AbstractTerm.createMolec(new SkelCompound(new SkelAtom(PropertySource.OP_SYS_LINK),
                                dep.key.getPathAtom(), val), Display.DISPLAY_CONST));
                    }
                    if ((flags & AbstractSource.MASK_IMPT_RSCS) != 0) {
                        Object val = new SkelAtom(LoadForce.OP_SYS_LINK_SYS_LOAD_RESOURCE);
                        list.add(AbstractTerm.createMolec(new SkelCompound(new SkelAtom(PropertySource.OP_SYS_LINK),
                                dep.key.getPathAtom(), val), Display.DISPLAY_CONST));
                    }
                    if ((flags & AbstractSource.MASK_IMPT_HOFL) != 0) {
                        Object val = new SkelAtom(LoadForce.OP_SYS_LINK_SYS_HOME_FILE);
                        list.add(AbstractTerm.createMolec(new SkelCompound(new SkelAtom(PropertySource.OP_SYS_LINK),
                                dep.key.getPathAtom(), val), Display.DISPLAY_CONST));
                    }
                    if ((flags & AbstractSource.MASK_IMPT_PAIM) != 0) {
                        Object val = new SkelAtom(LoadForce.OP_SYS_LINK_SYS_PARENT_IMPORT);
                        list.add(AbstractTerm.createMolec(new SkelCompound(new SkelAtom(PropertySource.OP_SYS_LINK),
                                dep.key.getPathAtom(), val), Display.DISPLAY_CONST));
                    }
                }
                vals = new Object[list.size()];
                list.toArray(vals);
                return vals;
            case PROP_SYS_MODULE:
                s = src.getFullName();
                if (Branch.OP_USER.equals(s))
                    return AbstractBranch.FALSE_PROPERTY;
                Object val = SpecialDynamic.moduleToSlashSkel(s, src.getStore().user);
                return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                        new SkelAtom(OP_SYS_MODULE),
                        val), Display.DISPLAY_CONST)};
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /**
     * <p>Set a object property.</p>
     *
     * @param src The object.
     * @param m   The property skeleton.
     * @param d   The property display.
     * @param en  The engine.
     * @return True if property could be set, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean setObjProp(AbstractSource src, Object m, Display d, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_SHORT_NAME:
                /* can't modify */
                return false;
            case PROP_SYS_CAPABILITY:
                /* can't modify */
                return false;
            case PROP_EXPIRATION:
                /* can't modify */
                return false;
            case PROP_LAST_MODIFIED:
                /* can't modify */
                return false;
            case PROP_VERSION_TAG:
                /* can't modify */
                return false;
            case PROP_DATE:
                /* can't modify */
                return false;
            case PROP_MAX_AGE:
                /* can't modify */
                return false;
            case PROP_SYS_NOTRACE:
                src.setBit(AbstractSource.MASK_SRC_NOTR);
                return true;
            case PROP_SYS_SOURCE_PRELOAD:
                src.setBit(AbstractSource.MASK_SRC_PREL);
                return true;
            case PROP_SYS_TIMING:
                /* can't modify */
                return false;

            case PROP_SYS_SOURCE_VISIBLE:
                int flags = PropertySource.derefAndCastVisible(m, d, en);
                src.resetBit(AbstractSource.MASK_SRC_VISI);
                src.setBit(flags);
                return true;
            case PROP_PACKAGE:
                flags = PropertySource.derefAndCastPackage(m, d, en);
                flags = src.addFix((String) en.skel, flags);
                CacheModule.notifyFixvers(src, flags);
                return true;
            case PROP_USE_PACKAGE:
                flags = PropertySource.derefAndCastUsePackage(m, d, en);
                flags = src.addFix((String) en.skel, flags);
                CacheModule.notifyFixvers(src, flags);
                return true;
            case PROP_SYS_SOURCE_NAME:
                String fun = PropertySource.derefAndCastName(m, d, en);
                flags = (src.setName(fun) ? 0 : AbstractSource.MASK_PCKG_LIBR);
                CacheModule.notifyFixvers(src, flags);
                return true;
            case PROP_SYS_LINK:
                /* can't modify */
                return false;
            case PROP_SYS_MODULE:
                /* can't modify */
                return false;
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /**
     * <p>Reset a object property.</p>
     *
     * @param src The object.
     * @param m   The property skeleton.
     * @param d   The property display.
     * @param en  The engine.
     * @return True if property could be set, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean resetObjProp(AbstractSource src, Object m, Display d, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_SHORT_NAME:
                /* can't modify */
                return false;
            case PROP_SYS_CAPABILITY:
                /* can't modify */
                return false;
            case PROP_EXPIRATION:
                /* can't modify */
                return false;
            case PROP_LAST_MODIFIED:
                /* can't modify */
                return false;
            case PROP_VERSION_TAG:
                /* can't modify */
                return false;
            case PROP_DATE:
                /* can't modify */
                return false;
            case PROP_MAX_AGE:
                /* can't modify */
                return false;
            case PROP_SYS_NOTRACE:
                src.resetBit(AbstractSource.MASK_SRC_NOTR);
                return true;
            case PROP_SYS_SOURCE_PRELOAD:
                src.resetBit(AbstractSource.MASK_SRC_PREL);
                return true;
            case PROP_SYS_TIMING:
                /* can't modify */
                return false;

            case PROP_SYS_SOURCE_VISIBLE:
                src.resetBit(AbstractSource.MASK_SRC_VISI);
                return true;
            case PROP_PACKAGE:
                int flags = PropertySource.derefAndCastPackage(m, d, en);
                flags = src.removeFix((String) en.skel, flags);
                CacheModule.notifyFixvers(src, flags);
                return true;
            case PROP_USE_PACKAGE:
                flags = PropertySource.derefAndCastUsePackage(m, d, en);
                flags = src.removeFix((String) en.skel, flags);
                CacheModule.notifyFixvers(src, flags);
                return true;
            case PROP_SYS_SOURCE_NAME:
                flags = (src.setName(null) ? 0 : AbstractSource.MASK_PCKG_LIBR);
                CacheModule.notifyFixvers(src, flags);
                return true;
            case PROP_SYS_LINK:
                /* can't modify */
                return false;
            case PROP_SYS_MODULE:
                /* can't modify */
                return false;
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /****************************************************************/
    /* Deref Utility                                                */
    /****************************************************************/

    /**
     * <p>Deref and cast to source visibility.</p>
     *
     * @param m  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The source visibility.
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
                ((SkelCompound) m).sym.fun.equals(OP_SYS_SOURCE_VISIBLE)) {
            m = ((SkelCompound) m).args[0];
            String fun = SpecialUniv.derefAndCastString(m, d);
            return PropertySource.atomToVisible(fun);
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Deref and cast to package.</p>
     * <p>The packge name is returned in the engine skel.</p>
     *
     * @param m  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The package flags.
     * @throws EngineMessage      Shit happens.
     * @throws ClassCastException Shit happens.
     */
    private static int derefAndCastPackage(Object m, Display d, Engine en)
            throws EngineMessage, ClassCastException {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 1 &&
                ((SkelCompound) m).sym.fun.equals(OP_PACKAGE)) {
            en.skel = ((SkelCompound) m).args[0];
            en.display = d;
            en.deref();
            m = en.skel;
            d = en.display;
            int flags;
            if (m instanceof SkelCompound &&
                    ((SkelCompound) m).args.length == 1 &&
                    ((SkelCompound) m).sym.fun.equals(LoadOpts.OP_PREFIX_LIBRARY)) {
                flags = AbstractSource.MASK_PCKG_LIBR;
            } else if (m instanceof SkelCompound &&
                    ((SkelCompound) m).args.length == 1 &&
                    ((SkelCompound) m).sym.fun.equals(LoadOpts.OP_PREFIX_FOREIGN)) {
                flags = AbstractSource.MASK_PCKG_FRGN;
            } else {
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_FIX_OPTION, m), d);
            }
            m = ((SkelCompound) m).args[0];
            en.skel = AbstractFile.slashToOsString(m, d, false, en);
            return flags;
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Deref and cast to use package.</p>
     * <p>The use packge name is returned in the engine skel.</p>
     *
     * @param m  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The use package flags.
     * @throws EngineMessage      Shit happens.
     * @throws ClassCastException Shit happens.
     */
    private static int derefAndCastUsePackage(Object m, Display d, Engine en)
            throws EngineMessage, ClassCastException {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 1 &&
                ((SkelCompound) m).sym.fun.equals(OP_USE_PACKAGE)) {
            en.skel = ((SkelCompound) m).args[0];
            en.display = d;
            en.deref();
            m = en.skel;
            d = en.display;
            int flags;
            if (m instanceof SkelCompound &&
                    ((SkelCompound) m).args.length == 1 &&
                    ((SkelCompound) m).sym.fun.equals(LoadOpts.OP_PREFIX_LIBRARY)) {
                flags = AbstractSource.MASK_USES_LIBR;
            } else if (m instanceof SkelCompound &&
                    ((SkelCompound) m).args.length == 1 &&
                    ((SkelCompound) m).sym.fun.equals(LoadOpts.OP_PREFIX_FOREIGN)) {
                flags = AbstractSource.MASK_USES_FRGN;
            } else {
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_FIX_OPTION, m), d);
            }
            m = ((SkelCompound) m).args[0];
            en.skel = AbstractFile.slashToOsString(m, d, false, en);
            return flags;
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Deref and cast to source name.</p>
     *
     * @param m  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The source name.
     * @throws EngineMessage      Shit happens.
     * @throws ClassCastException Shit happens.
     */
    private static String derefAndCastName(Object m, Display d, Engine en)
            throws EngineMessage, ClassCastException {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 1 &&
                ((SkelCompound) m).sym.fun.equals(OP_SYS_SOURCE_NAME)) {
            m = ((SkelCompound) m).args[0];
            String fun = AbstractFile.slashToOsString(m, d, true, en);
            fun = fun.replace(CacheModule.OP_CHAR_OS, CacheSubclass.OP_CHAR_SYN);
            return fun;
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
            flags = AbstractSource.MASK_SRC_VSPR;
        } else if (AbstractSource.OP_PUBLIC.equals(fun)) {
            flags = AbstractSource.MASK_SRC_VSPU;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_PROPERTY_VALUE,
                    new SkelAtom(fun)));
        }
        return flags;
    }

    /****************************************************************/
    /* Listing Utility                                              */
    /****************************************************************/

    /**
     * <p>Generate a declaration skel for the source property.</p>
     *
     * @param skel   The skel.
     * @param source The source.
     * @param en     The engine.
     * @return The declaration skel.
     * @throws EngineMessage Shit happens.
     */
    public static Object shortLink(Object skel,
                                   AbstractSource source, Engine en)
            throws EngineMessage {
        try {
            SkelCompound sc = (SkelCompound) skel;
            String key = ((SkelAtom) sc.args[0]).fun;
            SkelAtom sa = (SkelAtom) sc.args[1];

            boolean rsc = sa.fun.equals(LoadForce.OP_SYS_LINK_SYS_LOAD_RESOURCE);

            int mask;
            if (rsc) {
                mask = ForeignPath.MASK_MODL_RSCS | ForeignPath.MASK_FAIL_READ;
            } else {
                mask = ForeignPath.MASK_MODL_BASE | ForeignPath.MASK_FAIL_READ;
            }
            Object spec = CacheSubclass.unfindKey(key, source, mask, en);

            if (spec instanceof SkelCompound &&
                    ((SkelCompound) spec).args.length == 1 &&
                    ((SkelCompound) spec).sym.fun.equals(LoadOpts.OP_PREFIX_LIBRARY)) {
                if (rsc) {
                    mask = ForeignPath.MASK_MODL_RSCS;
                } else {
                    mask = ForeignPath.MASK_MODL_VERB;
                }
            } else if (spec instanceof SkelCompound &&
                    ((SkelCompound) spec).args.length == 1 &&
                    ((SkelCompound) spec).sym.fun.equals(LoadOpts.OP_PREFIX_FOREIGN)) {
                mask = ForeignPath.MASK_MODL_FRGN;
            } else if (spec instanceof SkelAtom) {
                key = ((SkelAtom) spec).fun;
                if (ForeignUri.sysUriIsRelative(key)) {
                    spec = AbstractFile.osToSlashSkel(key, true, source);
                } else {
                    spec = new SkelAtom(key, source);
                }
                return new SkelCompound(sa, spec);
            } else {
                throw new IllegalArgumentException("illegal spec");
            }
            sc = (SkelCompound) spec;
            key = ((SkelAtom) sc.args[0]).fun;
            spec = CacheModule.unfindPrefix(key, source, mask);

            if (spec instanceof SkelAtom) {
                key = ((SkelAtom) spec).fun;
            } else if (spec instanceof SkelCompound &&
                    ((SkelCompound) spec).args.length == 1 &&
                    ((SkelCompound) spec).sym.fun.equals(LoadOpts.OP_PREFIX_VERBATIM)) {
                sc = (SkelCompound) spec;
                key = ((SkelAtom) sc.args[0]).fun;
            } else {
                throw new IllegalArgumentException("illegal spec");
            }

            spec = new SkelCompound(sc.sym,
                    AbstractFile.osToSlashSkel(key, true, source));
            return new SkelCompound(sa, spec);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        } catch (LicenseError x) {
            throw new EngineMessage(EngineMessage.licenseError(x.getMessage()));
        }
    }

    /**
     * <p>Generate a declaration skel for the source property.</p>
     *
     * @param skel The skel.
     * @param en   The engine.
     * @return The declaration skel.
     */
    public static Object shortModule(Object skel, Engine en) {
        SkelCompound sc = (SkelCompound) skel;
        return new SkelCompound(new SkelAtom(SpecialLoad.OP_MODULE),
                sc.args[0], en.store.foyer.ATOM_NIL);
    }

}