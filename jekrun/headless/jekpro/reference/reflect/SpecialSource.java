package jekpro.reference.reflect;

import matula.comp.sharik.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.builtin.Branch;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.Resource;
import jekpro.reference.runtime.EvaluableLogic;
import jekpro.reference.runtime.SpecialDynamic;
import jekpro.reference.runtime.SpecialLogic;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.comp.sharik.AbstractTracking;
import matula.util.config.AbstractBundle;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;

/**
 * <p>Provides built-in predicates for the source reflection.</p>
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
public final class SpecialSource extends AbstractSpecial {
    private final static int SPECIAL_SYS_CURRENT_SOURCE = 0;
    private final static int SPECIAL_SYS_CURRENT_SOURCE_CHK = 1;
    private final static int SPECIAL_SYS_SOURCE_PROPERTY = 2;
    private final static int SPECIAL_SYS_SOURCE_PROPERTY_CHK = 3;
    public final static int SPECIAL_SET_SOURCE_PROPERTY = 4;
    public final static int SPECIAL_RESET_SOURCE_PROPERTY = 5;
    private final static int SPECIAL_SYS_CURRENT_RESOURCE = 6;
    private final static int SPECIAL_SYS_CURRENT_MODULE = 7;
    private final static int SPECIAL_SYS_CURRENT_MODULE_CHK = 8;

    /**
     * <p>Create a source reflection special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialSource(int i) {
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
            case SPECIAL_SYS_CURRENT_SOURCE:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                if (!en.unify(SpecialSource.currentSources(en), Display.DISPLAY_CONST, temp[0], ref))
                    return false;
                return true;
            case SPECIAL_SYS_CURRENT_SOURCE_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                AbstractSource source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                if (source == null)
                    return false;
                return true;
            case SPECIAL_SYS_SOURCE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                if (source == null)
                    return false;
                SpecialSource.sourceToProperties(source, en);
                Display d = en.display;
                boolean multi = d.getAndReset();
                if (!en.unify(en.skel, d, temp[1], ref))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
            case SPECIAL_SYS_SOURCE_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                if (source == null)
                    return false;
                StoreKey prop = AbstractProperty.propToStoreKey(temp[1], ref, en);
                SpecialSource.sourceToProperty(source, prop, en);
                d = en.display;
                multi = d.getAndReset();
                if (!en.unify(en.skel, d, temp[2], ref))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
            case SPECIAL_SET_SOURCE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                AbstractSource.checkExistentSource(source, sa);

                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialSource.setSrcProp(source, en.skel, en.display, en);
                return true;
            case SPECIAL_RESET_SOURCE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                AbstractSource.checkExistentSource(source, sa);

                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialSource.resetSrcProp(source, en.skel, en.display, en);
                return true;
            case SPECIAL_SYS_CURRENT_RESOURCE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                if (!en.unify(SpecialSource.currentResources(en), Display.DISPLAY_CONST, temp[0], ref))
                    return false;
                return true;
            case SPECIAL_SYS_CURRENT_MODULE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                if (!en.unify(SpecialSource.currentModules(en), Display.DISPLAY_CONST, temp[0], ref))
                    return false;
                return true;
            case SPECIAL_SYS_CURRENT_MODULE_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                AbstractSource base = SpecialSource.nameToModule(temp[0], ref, en);
                if (base == null)
                    return false;
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**************************************************************/
    /* High-Level AbstractSource Enumeration                      */
    /**************************************************************/

    /**
     * <p>Create a prolog list for all the sources.</p>
     *
     * @param en The engine.
     * @return The prolog list of the user sources.
     */
    private static Object currentSources(Engine en) {
        Store store = en.store;
        Object res = en.store.foyer.ATOM_NIL;
        while (store != null) {
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int i = sources.length - 1; i >= 0; i--) {
                AbstractSource source = sources[i].value;
                res = new SkelCompound(en.store.foyer.ATOM_CONS,
                        source.getPathAtom(), res);
            }
            store = store.parent;
        }
        return res;
    }

    /***********************************************************************/
    /* High-Level AbstractSource Property Access                           */
    /***********************************************************************/

    /**
     * <p>Create a prolog list for the properties of the given source.</p>
     * <p>Result is returned in skeleton and display.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param src The source.
     * @param en  The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void sourceToProperties(AbstractSource src, Engine en)
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
            MapHash<StoreKey, AbstractProperty<AbstractSource>> props = branch.getSrcProps();
            for (MapEntry<StoreKey, AbstractProperty<AbstractSource>> entry2 =
                 (props != null ? props.getLastEntry() : null);
                 entry2 != null; entry2 = props.predecessor(entry2)) {
                AbstractProperty<AbstractSource> prop = entry2.value;
                Object t = en.skel;
                Display d = en.display;
                Object[] vals = prop.getObjProps(src, en);
                en.skel = t;
                en.display = d;
                AbstractProperty.consArray(vals, en);
            }
        }
    }

    /**
     * <p>Create a prolog list for the property of the given source.</p>
     * <p>Result is returned in skeleton and display.</p>
     *
     * @param src The source.
     * @param sk  The property.
     * @param en  The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void sourceToProperty(AbstractSource src, StoreKey sk,
                                         Engine en)
            throws EngineMessage, EngineException {
        AbstractProperty<AbstractSource> prop = SpecialSource.findSrcProperty(sk, en);
        Object[] vals = prop.getObjProps(src, en);
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        AbstractProperty.consArray(vals, en);
    }

    /***********************************************************************/
    /* High-Level Operator Property Access II                              */
    /***********************************************************************/

    /**
     * <p>Set a source property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param src The source.
     * @param m   The value skeleton.
     * @param d   The value display.
     * @param en  The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void setSrcProp(AbstractSource src,
                                  Object m, Display d, Engine en)
            throws EngineMessage {
        StoreKey sk = StackElement.callableToStoreKey(m);
        AbstractProperty<AbstractSource> prop = findSrcProperty(sk, en);
        if (!prop.setObjProp(src, m, d, en))
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_PROPERTY,
                    sk.storeKeyToSkel()));
    }

    /**
     * <p>Set a source property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param src The source.
     * @param m   The value skeleton.
     * @param d   The value display.
     * @param en  The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void resetSrcProp(AbstractSource src,
                                    Object m, Display d, Engine en)
            throws EngineMessage {
        StoreKey sk = StackElement.callableToStoreKey(m);
        AbstractProperty<AbstractSource> prop = findSrcProperty(sk, en);
        if (!prop.resetObjProp(src, m, d, en))
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_PROPERTY,
                    sk.storeKeyToSkel()));
    }

    /**
     * <p>Find a source property.</p>
     * <p>Throws a domain error for undefined display properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param sk The property.
     * @param en The engine.
     * @return The source property.
     * @throws EngineMessage Shit happens.
     */
    public static AbstractProperty<AbstractSource> findSrcProperty(StoreKey sk,
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
            MapHash<StoreKey, AbstractProperty<AbstractSource>> props = branch.getSrcProps();
            AbstractProperty<AbstractSource> prop = (props != null ? props.get(sk) : null);
            if (prop != null)
                return prop;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                sk.storeKeyToSkel()));
    }

    /**********************************************************************/
    /* Resource Handling                                                  */
    /**********************************************************************/

    /**
     * <p>Create a prolog list for all the resources.</p>
     *
     * @param en The engine.
     * @return The prolog list of the user sources.
     */
    private static Object currentResources(Engine en) {
        Store store = en.store;
        Object res = en.store.foyer.ATOM_NIL;
        while (store != null) {
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int j = 0; j < sources.length; j++) {
                AbstractSource base = sources[j].value;
                Resource[] rscs = base.snapshotResources();
                for (int i = rscs.length - 1; i >= 0; i--) {
                    Resource rsc = rscs[i];

                    int m = (rsc.getPosition() != null ? SkelAtom.MASK_ATOM_POSI : 0);
                    SkelAtom sa = en.store.foyer.createAtom(rsc.getKey(), base, m);
                    sa.setPosition(rsc.getPosition());

                    res = new SkelCompound(en.store.foyer.ATOM_CONS, sa, res);
                }
            }
            store = store.parent;
        }
        return res;
    }

    /**************************************************************/
    /* Module Enumeration & Lookup                                */
    /**************************************************************/

    /**
     * <p>Create a prolog list with the modules.</p>
     *
     * @param en The engine.
     * @return The prolog list of the modules.
     * @throws EngineMessage Shit happens.
     */
    private static Object currentModules(Engine en)
            throws EngineMessage {
        Store store = en.store;
        Object res = en.store.foyer.ATOM_NIL;
        while (store != null) {
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int i = sources.length - 1; i >= 0; i--) {
                AbstractSource base = sources[i].value;
                String s = base.getFullName();
                if (Branch.OP_USER.equals(s))
                    continue;
                Object val = SpecialDynamic.moduleToSlashSkel(s, base.getStore().user);
                res = new SkelCompound(en.store.foyer.ATOM_CONS, val, res);
            }
            store = store.parent;
        }
        return res;
    }

    /**
     * <p>Get module by name.</p>
     *
     * @param t  The name skel.
     * @param d  The name display.
     * @param en The engine.
     * @return The module.
     * @throws EngineMessage Shit happens.
     */
    private static AbstractSource nameToModule(Object t, Display d,
                                               Engine en)
            throws EngineMessage {
        Object obj = EvaluableLogic.slashToClass(t, d, 0, en);
        SkelAtom mod = SpecialLogic.modToAtom(obj, t, d, en);
        return AbstractSource.getModule(mod.fun, en.store);
    }

}
