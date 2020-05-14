package jekdev.reference.inspection;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.tools.call.*;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import matula.util.config.AbstractBundle;
import matula.comp.sharik.AbstractTracking;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;
import matula.util.config.ArrayEnumeration;

/**
 * The foreign predicates for the module system/group.
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
public final class ForeignStore {

    /**
     * <p>Retrieve all the properties of a knowledge base.</p>
     *
     * @param inter The interpreter.
     * @param know  The knowledge base.
     * @return The properties.
     * @throws InterpreterException Validation Error.
     * @throws InterpreterMessage   Validation Error.
     */
    public static Object sysStoreProperty(Interpreter inter,
                                          Knowledgebase know)
            throws InterpreterException, InterpreterMessage {
        Engine en = (Engine) inter.getEngine();
        try {
            storeToProperties((Store) know.getStore(), en);
            return AbstractTerm.createTerm(en.skel, en.display);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Retrieve some properties of a knowledge base.</p>
     *
     * @param inter The interpreter.
     * @param know  The knowledge base.
     * @param key   The property name and arity.
     * @return The properties.
     * @throws InterpreterException Validation Error.
     * @throws InterpreterMessage   Validation Error.
     */
    public static Object sysStorePropertyChk(Interpreter inter,
                                             Knowledgebase know, Object key)
            throws InterpreterException, InterpreterMessage {
        Engine en = (Engine) inter.getEngine();
        try {
            StoreKey sk = StoreKey.propToStoreKey(AbstractTerm.getSkel(key),
                    AbstractTerm.getDisplay(key), en);
            storeToProperty((Store) know.getStore(), sk, en);
            return AbstractTerm.createTerm(en.skel, en.display);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Set some properties of a knowledge base.</p>
     *
     * @param inter The interpreter.
     * @param know  The knowledge base.
     * @param val   The property functor and arguments.
     * @throws InterpreterMessage Validation Error.
     */
    public static void sysSetStoreProperty(Interpreter inter,
                                           Knowledgebase know, Object val)
            throws InterpreterMessage {
        Engine en = (Engine) inter.getEngine();
        try {
            setStoreProp((Store) know.getStore(),
                    AbstractTerm.getSkel(val), AbstractTerm.getDisplay(val), en);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Reset some properties of a knowledge base.</p>
     *
     * @param inter The interpreter.
     * @param know  The knowledge base.
     * @param val   The property functor and arguments.
     * @throws InterpreterMessage Validation Error.
     */
    public static void sysResetStoreProperty(Interpreter inter,
                                             Knowledgebase know, Object val)
            throws InterpreterMessage {
        Engine en = (Engine) inter.getEngine();
        try {
            resetStoreProp((Store) know.getStore(),
                    AbstractTerm.getSkel(val), AbstractTerm.getDisplay(val), en);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /****************************************************************/
    /* Managed Stores                                              */
    /****************************************************************/

    /**
     * <p>Retrieve the managed knowledge bases.</p>
     *
     * @param co    The call out.
     * @param inter The interpreter.
     * @return The managed knowledge base.
     */
    public static Knowledgebase sysCurrentStore(CallOut co,
                                                Interpreter inter) {
        ArrayEnumeration<Store> dc;
        if (co.getFirst()) {
            Foyer foyer = (Foyer) inter.getKnowledgebase().getLobby().getFoyer();
            dc = new ArrayEnumeration<Store>(foyer.snapshotStores());
            co.setData(dc);
        } else {
            dc = (ArrayEnumeration<Store>) co.getData();
        }
        if (!dc.hasMoreElements())
            return null;
        Store res = dc.nextElement();
        co.setRetry(dc.hasMoreElements());
        return (Knowledgebase) res.proxy;
    }

    /**
     * <p>Check whether the knowledge base is managed.</p>
     *
     * @param inter The interpreter.
     * @param know  The knowledge base.
     * @return True if the knowledge base is managed, otherwise false.
     */
    public static boolean sysCurrentStoreChk(Interpreter inter,
                                             Knowledgebase know) {
        Foyer foyer = (Foyer) inter.getKnowledgebase().getLobby().getFoyer();
        Store store = (Store) know.getStore();
        return (store.foyer == foyer);
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
     * @throws EngineMessage   Validation Error.
     * @throws EngineException Validation Error.
     */
    public static void storeToProperties(Store store, Engine en)
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
            MapHash<StoreKey, AbstractProperty<Store>> props = branch.getStoreProps();
            for (MapEntry<StoreKey, AbstractProperty<Store>> entry2 =
                 (props != null ? props.getLastEntry() : null);
                 entry2 != null; entry2 = props.predecessor(entry2)) {
                AbstractProperty<Store> prop = entry2.value;
                Object t = en.skel;
                Display d = en.display;
                Object[] vals = prop.getObjProps(store, en);
                en.skel = t;
                en.display = d;
                AbstractProperty.consArray(vals, en);
            }
        }
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
     * @throws EngineMessage   Validation Error.
     * @throws EngineException Validation Error.
     */
    public static void storeToProperty(Store store, StoreKey sk,
                                       Engine en)
            throws EngineMessage, EngineException {
        AbstractProperty<Store> prop = findStoreProperty(sk, en);
        Object[] vals = prop.getObjProps(store, en);
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        AbstractProperty.consArray(vals, en);
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
     * @throws EngineMessage Validation Error.
     */
    private static void setStoreProp(Store store,
                                     Object m, Display d, Engine en)
            throws EngineMessage {
        EngineMessage.checkCallable(m, d);
        StoreKey sk = StackElement.callableToStoreKey(m);
        AbstractProperty<Store> prop = findStoreProperty(sk, en);
        if (!prop.setObjProp(store, m, d, en))
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_PROPERTY,
                    StoreKey.storeKeyToSkel(sk)));
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
     * @throws EngineMessage Validation Error.
     */
    private static void resetStoreProp(Store store,
                                       Object m, Display d, Engine en)
            throws EngineMessage {
        EngineMessage.checkCallable(m, d);
        StoreKey sk = StackElement.callableToStoreKey(m);
        AbstractProperty<Store> prop = findStoreProperty(sk, en);
        if (!prop.resetObjProp(store, m, d, en))
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_PROPERTY,
                    StoreKey.storeKeyToSkel(sk)));
    }


    /**
     * <p>Find a store property.</p>
     * <p>Throws a domain error for undefined display properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param sk The property.
     * @param en The engine.
     * @return The store property.
     * @throws EngineMessage Validation Error.
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
            AbstractProperty<Store> prop = (props != null ? props.get(sk) : null);
            if (prop != null)
                return prop;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                StoreKey.storeKeyToSkel(sk)));
    }

}