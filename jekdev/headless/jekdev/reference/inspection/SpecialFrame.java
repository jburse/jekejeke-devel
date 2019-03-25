package jekdev.reference.inspection;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelCompound;
import matula.comp.sharik.AbstractBundle;
import matula.comp.sharik.AbstractTracking;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;

/**
 * <p>This module provides built-ins for frame access.</p>
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
public final class SpecialFrame extends AbstractSpecial {
    private final static int SPECIAL_RULE_FRAME = 0;
    private final static int SPECIAL_SYS_FRAME_PROPERTY = 2;
    private final static int SPECIAL_SYS_FRAME_PROPERTY_CHK = 3;

    private final static String OP_DOMAIN_NOT_NULL = "not_null";
    private final static String OP_DOMAIN_STACK = "stack";


    /**
     * <p>Create a frame special.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialFrame(int i) {
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
            case SPECIAL_RULE_FRAME:
                return AbstractDefined.searchKnowledgebase(AbstractDefined.OPT_CHCK_DEFN |
                        AbstractDefined.OPT_RSLT_CREF, en);
            case SPECIAL_SYS_FRAME_PROPERTY:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                StackElement frame = derefAndCastStackElement(temp[0], ref);
                checkNotNull(frame);
                SpecialFrame.frameToProperties(frame, en);
                Display d = en.display;
                boolean multi = d.getAndReset();
                if (!en.unifyTerm(temp[1], ref, en.skel, d))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
            case SPECIAL_SYS_FRAME_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                frame = derefAndCastStackElement(temp[0], ref);
                checkNotNull(frame);
                StoreKey sk = StoreKey.propToStoreKey(temp[1], ref, en);
                SpecialFrame.frameToProperty(frame, sk, en);
                d = en.display;
                multi = d.getAndReset();
                if (!en.unifyTerm(temp[2], ref, en.skel, d))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /*******************************************************************/
    /* Retrieve Frame Property                                         */
    /*******************************************************************/

    /**
     * <p>Create a prolog list for the properties of the given frame.</p>
     * <p>Result is returned in skeleton and display.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param frame The frame, non null.
     * @param en    The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void frameToProperties(StackElement frame,
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
            MapHash<StoreKey, AbstractProperty<StackElement>> props = branch.getFrameProps();
            for (MapEntry<StoreKey, AbstractProperty<StackElement>> entry2 =
                 (props != null ? props.getLastEntry() : null);
                 entry2 != null; entry2 = props.predecessor(entry2)) {
                AbstractProperty prop = entry2.value;
                Object t = en.skel;
                Display d = en.display;
                Object[] vals = prop.getObjProps(frame, en);
                en.skel = t;
                en.display = d;
                AbstractProperty.consArray(vals, en);
            }
        }
    }

    /**
     * <p>Create a prolog list for the property of the given display.</p>
     * <p>Result is returned in skeleton and display.</p>
     *
     * @param frame The frame, non null.
     * @param sk    The property.
     * @param en    The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void frameToProperty(StackElement frame, StoreKey sk,
                                        Engine en)
            throws EngineMessage, EngineException {
        AbstractProperty<StackElement> prop = findFrameProperty(sk, en);
        Object[] vals = prop.getObjProps(frame, en);
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        AbstractProperty.consArray(vals, en);
    }

    /**
     * <p>Retrieve a frame property.</p>
     * <p>Throws a domain error for undefined display properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param sk The property.
     * @param en The engine.
     * @return The frame property.
     * @throws EngineMessage Shit happens.
     */
    private static AbstractProperty<StackElement> findFrameProperty(StoreKey sk,
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
            MapHash<StoreKey, AbstractProperty<StackElement>> props = branch.getFrameProps();
            AbstractProperty prop = (props != null ? props.get(sk) : null);
            if (prop != null)
                return prop;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                StoreKey.storeKeyToSkel(sk)));
    }

    /*******************************************************************/
    /* Deref Utility                                                   */
    /*******************************************************************/

    /**
     * <p>Check whether the object is a not null.</p>
     *
     * @param f The object.
     * @throws EngineMessage A null frame.
     */
    public static void checkNotNull(Object f)
            throws EngineMessage {
        if (f != null) {
            /* */
        } else {
            throw new EngineMessage(EngineMessage.representationError(
                    SpecialFrame.OP_DOMAIN_NOT_NULL));
        }
    }

    /**
     * <p>Cast a frame or null.</p>
     *
     * @param m The term skel.
     * @param d The term display.
     * @return The frame.
     * @throws EngineMessage Shit happens.
     */
    public static StackElement derefAndCastStackElement(Object m, Display d)
            throws EngineMessage {
        m = SpecialUniv.derefAndCastRefOrNull(m, d);
        if (m == null || m instanceof StackElement) {
            return (StackElement) m;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    OP_DOMAIN_STACK, m), d);
        }
    }

}
