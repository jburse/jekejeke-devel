package jekdev.reference.inspection;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractInformation;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.InterfaceStack;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.BindCount;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.SkelCompound;
import matula.comp.sharik.AbstractBundle;
import matula.comp.sharik.AbstractTracking;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;

/**
 * <p>This module provides built-ins for store access.</p>
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
public final class SpecialStore extends AbstractSpecial {
    private final static int SPECIAL_SYS_STORE_PROPERTY = 0;
    private final static int SPECIAL_SYS_STORE_PROPERTY_CHK = 1;
    private final static int SPECIAL_SET_STORE_PROPERTY = 2;
    private final static int SPECIAL_RESET_STORE_PROPERTY = 3;

    private final static String OP_DOMAIN_STORE = "store";

    /**
     * <p>Create a store special.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialStore(int i) {
        super(i);
    }

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
            case SPECIAL_SYS_STORE_PROPERTY:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Knowledgebase know = derefAndCastStore(temp[0], ref);
                boolean multi = storeToProperties((Store) know.getStore(), en);
                Display d = en.display;
                if (!en.unifyTerm(temp[1], ref, en.skel, d))
                    return false;
                if (multi)
                    BindCount.remTab(d.bind, en);
                return en.getNext();
            case SPECIAL_SYS_STORE_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                know = derefAndCastStore(temp[0], ref);
                StoreKey sk = StoreKey.propToStoreKey(temp[1], ref, en);
                multi = storeToProperty((Store) know.getStore(), sk, en);
                d = en.display;
                if (!en.unifyTerm(temp[2], ref, en.skel, d))
                    return false;
                if (multi)
                    BindCount.remTab(d.bind, en);
                return en.getNext();
            case SPECIAL_SET_STORE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                know = derefAndCastStore(temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                setStoreProp((Store) know.getStore(), en.skel, en.display, en);
                return en.getNextRaw();
            case SPECIAL_RESET_STORE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                know = derefAndCastStore(temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                resetStoreProp((Store) know.getStore(), en.skel, en.display, en);
                return en.getNextRaw();
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /*******************************************************************/
    /* Retrieve Store Property                                         */
    /*******************************************************************/

    /**
     * <p>Create a prolog list for the properties of the given store.</p>
     * <p>Result is returned in skeleton and display.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param store The store.
     * @param en    The engine.
     * @return The multi flag.
     * @throws EngineMessage Shit happens.
     * @throws EngineException Shit happens.
     */
    private static boolean storeToProperties(Store store, Engine en)
            throws EngineMessage, EngineException {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot
                = en.store.foyer.snapshotTrackings();
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        boolean multi = false;
        for (int i = snapshot.length - 1; i >= 0; i--) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            MapHash<StoreKey, AbstractProperty<Store>> props = branch.getStoreProps();
            for (MapEntry<StoreKey, AbstractProperty<Store>> entry2 =
                 (props != null ? props.getLastEntry() : null);
                 entry2 != null; entry2 = props.predecessor(entry2)) {
                AbstractProperty prop = entry2.value;
                Object t = en.skel;
                Display d = en.display;
                Object[] vals = prop.getObjProp(store, en);
                en.skel = t;
                en.display = d;
                multi = AbstractInformation.consArray(multi, vals, en);
            }
        }
        return multi;
    }

    /**
     * <p>Retrieve a store property.</p>
     * <p>Throws a domain error for undefined display properties.</p>
     * <p>Result is returned in skeleton and display.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param store The store, non null.
     * @param sk    The property.
     * @param en    The engine.
     * @return The multi flag.
     * @throws EngineMessage Shit happens.
     * @throws EngineException Shit happens.
     */
    private static boolean storeToProperty(Store store, StoreKey sk,
                                           Engine en)
            throws EngineMessage, EngineException {
        AbstractProperty<Store> prop = findStoreProperty(sk, en);
        Object[] vals = prop.getObjProp(store, en);
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        return AbstractInformation.consArray(false, vals, en);
    }

    /*******************************************************************/
    /* Set/Reset Store Property                                        */
    /*******************************************************************/

    /**
     * <p>Set a store property.</p>
     * <p>Throws a domain error for undefined display properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param store The store.
     * @param m     The term skeleton.
     * @param d     The term display.
     * @param en    The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void setStoreProp(Store store,
                                     Object m, Display d, Engine en)
            throws EngineMessage {
        StoreKey sk = StackElement.callableToStoreKey(m);
        AbstractProperty prop = findStoreProperty(sk, en);
        if (!prop.setObjProp(store, m, d, en))
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_PROPERTY,
                    StoreKey.storeKeyToPropSkel(sk.getFun(), sk.getArity())));
    }

    /**
     * <p>Set a store property.</p>
     * <p>Throws a domain error for undefined display properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param store The store.
     * @param m     The term skeleton.
     * @param d     The term display.
     * @param en    The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void resetStoreProp(Store store,
                                       Object m, Display d, Engine en)
            throws EngineMessage {
        StoreKey sk = StackElement.callableToStoreKey(m);
        AbstractProperty prop = findStoreProperty(sk, en);
        if (!prop.resetObjProp(store, m, d, en))
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_PROPERTY,
                    StoreKey.storeKeyToPropSkel(sk.getFun(), sk.getArity())));
    }

    /**
     * <p>Find a store property.</p>
     * <p>Throws a domain error for undefined display properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param sk The property.
     * @param en The engine.
     * @return The store property.
     * @throws EngineMessage Shit happens.
     */
    private static AbstractProperty<Store> findStoreProperty(StoreKey sk,
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
            MapHash<StoreKey, AbstractProperty<Store>> props = branch.getStoreProps();
            AbstractProperty prop = (props != null ? props.get(sk) : null);
            if (prop != null)
                return prop;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                StoreKey.storeKeyToPropSkel(sk.getFun(), sk.getArity())));
    }

    /*******************************************************************/
    /* Deref Utility                                                   */
    /*******************************************************************/

    /**
     * <p>Cast a store.</p>
     *
     * @param m The term skel.
     * @param d The term display.
     * @return The szore.
     * @throws EngineMessage Shit happens.
     */
    public static Knowledgebase derefAndCastStore(Object m, Display d)
            throws EngineMessage {
        m = SpecialUniv.derefAndCastRef(m, d);
        if (m instanceof Knowledgebase) {
            return (Knowledgebase) m;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    OP_DOMAIN_STORE, m), d);
        }
    }

}