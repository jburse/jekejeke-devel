package jekpro.reference.reflect;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.StoreKey;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelCompound;
import matula.comp.sharik.AbstractTracking;
import matula.util.config.AbstractBundle;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;

/**
 * <p>Provides built-in predicates for the callable reflection.</p>
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
public final class SpecialCall extends AbstractSpecial {
    private final static int SPECIAL_SYS_CALLABLE_PROPERTY = 0;
    private final static int SPECIAL_SYS_CALLABLE_PROPERTY_CHK = 1;
    private final static int SPECIAL_SET_CALLABLE_PROPERTY = 2;
    private final static int SPECIAL_RESET_CALLABLE_PROPERTY = 3;

    /**
     * <p>Create a callable reflection special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialCall(int i) {
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
            case SPECIAL_SYS_CALLABLE_PROPERTY:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialCall.callableToProperties(AbstractTerm.createMolec(en.skel, en.display), en);
                Display d = en.display;
                boolean multi = d.getAndReset();
                if (!en.unifyTerm(temp[1], ref, en.skel, d))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
            case SPECIAL_SYS_CALLABLE_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                StoreKey prop = StoreKey.propToStoreKey(temp[1], ref, en);
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialCall.callableToProperty(AbstractTerm.createMolec(en.skel, en.display), prop, en);
                d = en.display;
                multi = d.getAndReset();
                if (!en.unifyTerm(temp[2], ref, en.skel, d))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
            case SPECIAL_SET_CALLABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[2];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                Object t = en.skel;
                d = en.display;
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialCall.setCallableProp(AbstractTerm.createMolec(t, d), en.skel, en.display, en);
                if (!en.unifyTerm(temp[0], ref, en.skel, en.display))
                    return false;
                return true;
            case SPECIAL_RESET_CALLABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[2];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                t = en.skel;
                d = en.display;
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialCall.resetCallableProp(AbstractTerm.createMolec(t, d), en.skel, en.display, en);
                if (!en.unifyTerm(temp[0], ref, en.skel, en.display))
                    return false;
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /***********************************************************************/
    /* High-Level Atom Property Access                                     */
    /***********************************************************************/

    /**
     * <p>Create a prolog list for the properties of the given atom.</p>
     * <p>Result is returned in skeleton and display.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param molec The molec.
     * @param en    The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void callableToProperties(Object molec, Engine en)
            throws EngineMessage, EngineException {
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = snapshot.length - 1; i >= 0; i--) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            MapHash<StoreKey, AbstractProperty<Object>> props = branch.getCallableProps();
            for (MapEntry<StoreKey, AbstractProperty<Object>> entry2 =
                 (props != null ? props.getLastEntry() : null);
                 entry2 != null; entry2 = props.predecessor(entry2)) {
                AbstractProperty<Object> prop = entry2.value;
                Object t = en.skel;
                Display d = en.display;
                Object[] vals = prop.getObjProps(molec, en);
                en.skel = t;
                en.display = d;
                AbstractProperty.consArray(vals, en);
            }
        }
    }

    /**
     * <p>Create a prolog list for the property of the given atom.</p>
     * <p>Result is returned in skeleton and display.</p>
     *
     * @param molec The molec.
     * @param sk    The property.
     * @param en    The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void callableToProperty(Object molec, StoreKey sk,
                                           Engine en)
            throws EngineMessage, EngineException {
        AbstractProperty<Object> prop = SpecialCall.findCallableProperty(sk, en);
        Object[] vals = prop.getObjProps(molec, en);
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        AbstractProperty.consArray(vals, en);
    }

    /**
     * <p>Set a predicate property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     * <p>The result is return in skel and display of engine.</p>
     *
     * @param molec The molec.
     * @param m     The value skeleton.
     * @param d     The value display.
     * @param en    The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void setCallableProp(Object molec, Object m, Display d,
                                        Engine en)
            throws EngineMessage {
        StoreKey sk = StackElement.callableToStoreKey(m);
        AbstractProperty<Object> prop = SpecialCall.findCallableProperty(sk, en);
        if (!prop.setObjProp(molec, m, d, en))
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_PROPERTY,
                    StoreKey.storeKeyToSkel(sk)));
    }

    /**
     * <p>Reset a predicate property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     * <p>The result is return in skel and display of engine.</p>
     *
     * @param molec The molec.
     * @param m     The value skeleton.
     * @param d     The value display.
     * @param en    The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void resetCallableProp(Object molec, Object m, Display d,
                                          Engine en)
            throws EngineMessage {
        StoreKey sk = StackElement.callableToStoreKey(m);
        AbstractProperty<Object> prop = SpecialCall.findCallableProperty(sk, en);
        if (!prop.resetObjProp(molec, m, d, en))
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_PROPERTY,
                    StoreKey.storeKeyToSkel(sk)));
    }

    /**
     * <p>Find a callable property.</p>
     * <p>Throws a domain error for undefined display properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param sk The property.
     * @param en The engine.
     * @return The operator property.
     * @throws EngineMessage Shit happens.
     */
    public static AbstractProperty<Object> findCallableProperty(StoreKey sk,
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
            MapHash<StoreKey, AbstractProperty<Object>> props = branch.getCallableProps();
            AbstractProperty<Object> prop = (props != null ? props.get(sk) : null);
            if (prop != null)
                return prop;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                StoreKey.storeKeyToSkel(sk)));
    }

}