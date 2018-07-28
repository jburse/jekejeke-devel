package jekpro.frequent.experiment;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.*;
import jekpro.model.molec.*;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.*;
import jekpro.frequent.standard.EngineCopy;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.comp.sharik.AbstractTracking;
import matula.util.data.MapEntry;
import matula.comp.sharik.AbstractBundle;

/**
 * <p>Provides built-in predicates for the internal database predicates.</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SpecialRef extends AbstractSpecial {
    private final static int SPECIAL_ASSERTABLE_REF = 0;
    private final static int SPECIAL_ASSUMABLE_REF = 1;
    private final static int SPECIAL_RECORDA_REF = 2;
    private final static int SPECIAL_RECORDZ_REF = 3;
    private final static int SPECIAL_ERASE_REF = 4;
    private final static int SPECIAL_COMPILED_REF = 5;
    private final static int SPECIAL_CLAUSE_REF = 6;
    private final static int SPECIAL_SYS_REF_PROPERTY = 7;
    private final static int SPECIAL_SYS_REF_PROPERTY_CHK = 8;
    private final static int SPECIAL_SET_REF_PROPERTY = 9;
    private final static int SPECIAL_RESET_REF_PROPERTY = 10;

    /**
     * <p>Create a special internal.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialRef(int i) {
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
            case SPECIAL_ASSERTABLE_REF:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Clause clause = SpecialRef.compileClause(AbstractDefined.OPT_PROM_DYNA |
                        AbstractDefined.OPT_CHCK_ASSE, en);
                if (!en.unifyTerm(temp[1], ref,
                        clause, Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_ASSUMABLE_REF:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                clause = SpecialRef.compileClause(AbstractDefined.OPT_PROM_THLC |
                        AbstractDefined.OPT_CHCK_ASSE, en);
                if (!en.unifyTerm(temp[1], ref,
                        clause, Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_RECORDA_REF:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                InterfaceReference ptr = SpecialRef.castReference(temp[0], ref);
                if (!ptr.assertRef(0, en))
                    return false;
                return en.getNextRaw();
            case SPECIAL_RECORDZ_REF:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                ptr = SpecialRef.castReference(temp[0], ref);
                if (!ptr.assertRef(AbstractDefined.OPT_ACTI_BOTT, en))
                    return false;
                return en.getNextRaw();
            case SPECIAL_ERASE_REF:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                ptr = SpecialRef.castReference(temp[0], ref);
                if (!ptr.retractRef(en))
                    return false;
                return en.getNextRaw();
            case SPECIAL_COMPILED_REF:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                ptr = SpecialRef.castReference(temp[0], ref);
                ptr.clauseRef(en);
                if (!en.unifyTerm(temp[1], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_CLAUSE_REF:
                return AbstractDefined.searchKnowledgebase(AbstractDefined.OPT_CHCK_ASSE |
                        AbstractDefined.OPT_ACTI_WRIT |
                        AbstractDefined.OPT_RSLT_CREF, en);
            case SPECIAL_SYS_REF_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                ptr = SpecialRef.castReference(temp[0], ref);
                SpecialRef.refToProperties(ptr, en);
                if (!en.unifyTerm(temp[1], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_REF_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                ptr = SpecialRef.castReference(temp[0], ref);
                StoreKey sk = StoreKey.propToStoreKey(temp[1], ref, en);
                SpecialRef.refToProperty(sk, ptr, en);
                if (!en.unifyTerm(temp[2], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_SET_REF_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                ptr = SpecialRef.castReference(temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialRef.addRefProp(en.skel, en.display, ptr, en);
                return en.getNextRaw();
            case SPECIAL_RESET_REF_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                ptr = SpecialRef.castReference(temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialRef.removeRefProp(en.skel, en.display, ptr, en);
                return en.getNextRaw();
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /****************************************************************************/
    /* Clause Compilation                                                       */
    /****************************************************************************/

    /**
     * <p>Compile a new clause.</p>
     * <p>The goal is passed via the engine skel and display.</p>
     * <p>The following flags are recognized:</p>
     * <ul>
     * <li><b>OPT_ARGS_ASOP:</b> The goal has assert options.</li>
     * <li><b>MASK_OPER_DYNA:</b> Predicate should be dynamic.</li>
     * <li><b>MASK_OPER_THRE:</b> Predicate should be thread_local.</li>
     * </ul>
     * <p>The ptr can be a rule, fact or directive.</p>
     *
     * @param flags The flags.
     * @param en    The engine.
     * @return True if the ptr ref could be unified.
     * @throws EngineMessage Shit happens.
     */
    private static Clause compileClause(int flags,
                                        Engine en)
            throws EngineMessage, EngineException {
        Object[] temp = ((SkelCompound) en.skel).args;
        Display ref = en.display;
        en.skel = temp[0];
        en.display = ref;
        en.deref();
        EngineCopy ec = en.enginecopy;
        if (ec == null) {
            ec = new EngineCopy();
            en.enginecopy = ec;
        }
        ec.vars = null;
        ec.flags = 0;
        Object molec = ec.copyTermAndWrap(en.skel, en.display, en);
        Named[] vars = null;
        if ((flags & AbstractDefined.OPT_ARGS_ASOP) != 0)
            vars = Named.decodeAssertOptions(temp[2], ref, en, ec);
        ec.vars = null;
        Object head = PreClause.clauseToHead(molec, en);
        Predicate pick = AbstractDefined.determineDefined(head, flags, en);
        Clause clause = AbstractDefined.determineClause(pick, flags, en);
        clause.vars = vars;
        clause.head = head;
        clause.analyzeBody(molec, en);
        return clause;
    }
    
    /***************************************************************/
    /* High-Level Clause Access/Modification                       */
    /***************************************************************/

    /**
     * <p>Create a prolog list for the properties of the given reference.</p>
     * <p>Result is returned in skeleton and display.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param ptr The reference.
     * @param en     The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void refToProperties(InterfaceReference ptr, Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        for (int i = snapshot.length - 1; i >= 0; i--) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;

            AbstractBranch branch = (AbstractBranch) entry.key;
            StoreKey[] props = branch.listRefProp(ptr, en);
            for (int j = props.length - 1; j >= 0; j--) {
                StoreKey prop = props[j];
                Object t = en.skel;
                Display d = en.display;
                Object[] vals = SpecialRef.getRefProp(prop, ptr, en);
                en.skel = t;
                en.display = d;
                AbstractProperty.consArray(vals, en);
            }
        }
    }

    /**
     * <p>Create a prolog list for the property of the given reference.</p>
     * <p>Result is returned in skeleton and display.</p>
     *
     * @param prop   The property.
     * @param ptr The ptr.
     * @param en     The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void refToProperty(StoreKey prop,
                                      InterfaceReference ptr, Engine en)
            throws EngineMessage {
        Object[] vals = SpecialRef.getRefProp(prop, ptr, en);
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        AbstractProperty.consArray(vals, en);
    }

    /**
     * <p>Set a ptr property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param temp   The value skeleton.
     * @param ref    The value display.
     * @param ptr The ptr.
     * @param en     The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void addRefProp(Object temp, Display ref,
                                   InterfaceReference ptr, Engine en)
            throws EngineMessage {
        StoreKey prop = Frame.callableToStoreKey(temp);
        Object[] vals = SpecialRef.getRefProp(prop, ptr, en);
        vals = AbstractProperty.addValue(vals, AbstractTerm.createMolec(temp, ref));
        SpecialRef.setRefProp(prop, vals, ptr, en);
    }

    /**
     * <p>Reset a ptr property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param temp   The value skeleton.
     * @param ref    The value display.
     * @param ptr The ptr.
     * @param en     The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void removeRefProp(Object temp, Display ref,
                                      InterfaceReference ptr, Engine en)
            throws EngineMessage {
        StoreKey prop = Frame.callableToStoreKey(temp);
        Object[] vals = SpecialRef.getRefProp(prop, ptr, en);
        vals = AbstractProperty.removeValue(vals, AbstractTerm.createMolec(temp, ref));
        SpecialRef.setRefProp(prop, vals, ptr, en);
    }
    
    /***************************************************************/
    /* Low-Level Clause Access/Modification                        */
    /***************************************************************/

    /**
     * <p>Retrieve a reference property.</p>
     * <p>Throws a domain error for undefined ptr properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param prop   The property.
     * @param ptr The reference.
     * @param en     The engine.
     * @return The value.
     * @throws EngineMessage Shit happens.
     */
    private static Object[] getRefProp(StoreKey prop,
                                       InterfaceReference ptr, Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            Object[] vals = branch.getRefProp(prop, ptr, en);
            if (vals != null)
                return vals;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                StoreKey.storeKeyToPropSkel(prop.getFun(), prop.getArity())));
    }

    /**
     * <p>Set a reference property.</p>
     * <p>Throws a domain error for undefined values properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param prop   The property.
     * @param vals2  The values, non null.
     * @param ptr The reference.
     * @param en     The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void setRefProp(StoreKey prop, Object[] vals2,
                                   InterfaceReference ptr, Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            if (branch.setRefProp(prop, vals2, ptr, en))
                return;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                StoreKey.storeKeyToPropSkel(prop.getFun(), prop.getArity())));
    }
    
    /*******************************************************************/
    /* Check InterfaceReference                                                 */
    /*******************************************************************/

    /**
     * <p>Cast a ptr.</p>
     *
     * @param m The skel.
     * @return The ptr.
     * @throws EngineMessage Shit happens.
     */
    public static InterfaceReference castReference(Object m, Display d)
            throws EngineMessage {
        BindVar b;
        while (m instanceof SkelVar &&
                (b = d.bind[((SkelVar) m).id]).display != null) {
            m = b.skel;
            d = b.display;
        }
        if (m instanceof InterfaceReference) {
            return (InterfaceReference) m;
        } else {
            EngineMessage.checkRef(m, d);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_REF, m));
        }
    }

}
