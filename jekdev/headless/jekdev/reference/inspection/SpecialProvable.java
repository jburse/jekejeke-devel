package jekdev.reference.inspection;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.builtin.SpecialBody;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Frame;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.reflect.SpecialPred;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.comp.sharik.AbstractBundle;
import matula.comp.sharik.AbstractTracking;
import matula.util.data.MapEntry;


/**
 * <p>This module provides built-ins for direct predicate access.</p>
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
public final class SpecialProvable extends AbstractSpecial {
    private final static int SPECIAL_SYS_CURRENT_PROVABLE_CHK = 1;
    private final static int SPECIAL_SYS_PROVABLE_PROPERTY = 2;

    private final static int SPECIAL_SET_PROVABLE_PROPERTY = 4;
    private final static int SPECIAL_RESET_PROVABLE_PROPERTY = 5;

    private final static int SPECIAL_SYS_CALLABLE_PROPERTY = 6;
    private final static int SPECIAL_SYS_CALLABLE_PROPERTY_CHK = 7;
    private final static int SPECIAL_SET_CALLABLE_PROPERTY = 8;
    private final static int SPECIAL_RESET_CALLABLE_PROPERTY = 9;

    /**
     * <p>Create a provable direct access builtin.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialProvable(int i) {
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
            case SPECIAL_SYS_CURRENT_PROVABLE_CHK:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Predicate pick = SpecialLoad.indicatorToProvable(temp[0], ref, en);
                if (pick == null)
                    return false;
                return en.getNextRaw();
            case SPECIAL_SYS_PROVABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialLoad.indicatorToProvable(temp[0], ref, en);
                if (pick == null)
                    return false;
                SpecialPred.predicateToProperties(pick, en);
                if (!en.unifyTerm(temp[1], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_SET_PROVABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialLoad.indicatorToProvable(temp[0], ref, en);
                Predicate.checkExistentProvable(pick, temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialPred.addPredProp(en.skel, en.display, pick, en);
                return en.getNextRaw();
            case SPECIAL_RESET_PROVABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialLoad.indicatorToProvable(temp[0], ref, en);
                Predicate.checkExistentProvable(pick, temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialPred.removePredProp(en.skel, en.display, pick, en);
                return en.getNextRaw();
            case SPECIAL_SYS_CALLABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SkelAtom sa = SpecialBody.callableToName(en.skel);
                atomToProperties(sa, en);
                if (!en.unifyTerm(temp[1], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_CALLABLE_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                sa = SpecialBody.callableToName(en.skel);
                StoreKey prop = StoreKey.propToStoreKey(temp[1], ref, en);
                atomToProperty(prop, sa, en);
                if (!en.unifyTerm(temp[2], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_SET_CALLABLE_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[2];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                Object t = en.skel;
                Display d = en.display;
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                t = addAtomProp(en.skel, en.display, t, en);
                if (!en.unifyTerm(temp[0], ref, t, d))
                    return false;
                return en.getNext();
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
                t = removeAtomProp(en.skel, en.display, t, en);
                if (!en.unifyTerm(temp[0], ref, t, d))
                    return false;
                return en.getNext();
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
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
     * @param sa The atom, or null.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void atomToProperties(SkelAtom sa, Engine en)
            throws EngineMessage {
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = snapshot.length - 1; i >= 0; i--) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractBranch branch = (AbstractBranch) entry.key;
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            StoreKey[] props = branch.listAtomProp();
            for (int j = props.length - 1; j >= 0; j--) {
                StoreKey prop = props[j];
                Object t = en.skel;
                Display d = en.display;
                Object[] vals = getPropAtom(prop, sa, en);
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
     * @param prop The property.
     * @param sa   The atom, or null.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void atomToProperty(StoreKey prop, SkelAtom sa,
                                       Engine en)
            throws EngineMessage {
        Object[] vals = getPropAtom(prop, sa, en);
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        AbstractProperty.consArray(vals, en);
    }

    /**
     * <p>Set a predicate property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param t  The value skeleton.
     * @param d  The value display.
     * @param t2 The callable skeleton.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    private static Object addAtomProp(Object t, Display d,
                                      Object t2, Engine en)
            throws EngineMessage {
        SkelAtom sa = SpecialBody.callableToName(t2);

        StoreKey prop = Frame.callableToStoreKey(t);
        Object[] vals = getPropAtom(prop, sa, en);
        vals = AbstractProperty.addValue(vals, AbstractTerm.createMolec(t, d));
        sa = setPropAtom(prop, sa, vals, en);

        return SpecialBody.callableFromName(t2, sa);
    }

    /**
     * <p>Reset a predicate property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param t  The value skeleton.
     * @param d  The value display.
     * @param t2 The callable skeleton.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    private static Object removeAtomProp(Object t, Display d,
                                         Object t2, Engine en)
            throws EngineMessage {
        SkelAtom sa = SpecialBody.callableToName(t2);

        StoreKey prop = Frame.callableToStoreKey(t);
        Object[] vals = getPropAtom(prop, sa, en);
        vals = AbstractProperty.removeValue(vals, AbstractTerm.createMolec(t, d));
        sa = setPropAtom(prop, sa, vals, en);

        return SpecialBody.callableFromName(t2, sa);
    }

    /***********************************************************************/
    /* Low-Level Atom Property Access                                      */
    /***********************************************************************/

    /**
     * <p>Retrieve an atom property.</p>
     * <p>Throws a domain error for undefined atom properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param prop The property.
     * @param sa   The atom, or null.
     * @param en   The engine.
     * @return The value.
     * @throws EngineMessage Shit happens.
     */
    public static Object[] getPropAtom(StoreKey prop, SkelAtom sa,
                                       Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractBranch branch = (AbstractBranch) entry.key;
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            Object[] vals = branch.getAtomProp(prop, sa, en);
            if (vals != null)
                return vals;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                StoreKey.storeKeyToPropSkel(prop.getFun(), prop.getArity())));
    }

    /**
     * <p>Set an atom property.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param prop The property.
     * @param skel The atom.
     * @param vals The values.
     * @param en   The engine.
     * @return The new atom.
     * @throws EngineMessage Shit happens.
     */
    private static SkelAtom setPropAtom(StoreKey prop, SkelAtom skel,
                                        Object[] vals, Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractBranch branch = (AbstractBranch) entry.key;
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            if (branch.setAtomProp(prop, skel, vals, en))
                return (SkelAtom) en.skel;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                StoreKey.storeKeyToPropSkel(prop.getFun(), prop.getArity())));
    }

}
