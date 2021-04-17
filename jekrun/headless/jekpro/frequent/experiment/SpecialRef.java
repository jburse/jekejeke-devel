package jekpro.frequent.experiment;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.*;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.PrologReader;
import jekpro.model.pretty.ReadOpts;
import jekpro.model.rope.Clause;
import jekpro.reference.reflect.PropertyCallable;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.runtime.SpecialDynamic;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.MapHash;
import matula.util.data.MapHashLink;

/**
 * <p>Provides built-in predicates for the reference predicates.</p>
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
public final class SpecialRef extends AbstractSpecial {
    private final static int SPECIAL_ASSERTABLE_REF = 0;
    private final static int SPECIAL_ASSUMABLE_REF = 1;
    private final static int SPECIAL_RECORDA_REF = 2;
    private final static int SPECIAL_RECORDZ_REF = 3;
    private final static int SPECIAL_ERASE_REF = 4;
    private final static int SPECIAL_COMPILED_REF = 5;
    private final static int SPECIAL_CLAUSE_REF = 6;
    private final static int SPECIAL_SYS_RULE_REF = 7;

    /**
     * <p>Create a special internal.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialRef(int i) {
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
     * @throws EngineMessage   Validation Error.
     * @throws EngineException Validation Error.
     */
    public final boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case SPECIAL_ASSERTABLE_REF:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Clause clause = SpecialRef.compileClause(AbstractDefined.OPT_PROM_DYNA |
                        AbstractDefined.OPT_CHCK_ASSE, en);
                if (!en.unify(clause, Display.DISPLAY_CONST, temp[1], ref))
                    return false;
                return true;
            case SPECIAL_ASSUMABLE_REF:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                clause = SpecialRef.compileClause(AbstractDefined.OPT_PROM_THLC |
                        AbstractDefined.OPT_CHCK_ASSE, en);
                if (!en.unify(clause, Display.DISPLAY_CONST, temp[1], ref))
                    return false;
                return true;
            case SPECIAL_RECORDA_REF:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                InterfaceReference ptr = SpecialRef.derefAndCastPtr(temp[0], ref);
                if (!ptr.assertRef(0, en))
                    return false;
                return true;
            case SPECIAL_RECORDZ_REF:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                ptr = SpecialRef.derefAndCastPtr(temp[0], ref);
                if (!ptr.assertRef(AbstractDefined.OPT_ACTI_BOTT, en))
                    return false;
                return true;
            case SPECIAL_ERASE_REF:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                ptr = SpecialRef.derefAndCastPtr(temp[0], ref);
                if (!ptr.retractRef(en))
                    return false;
                return true;
            case SPECIAL_COMPILED_REF:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                ptr = SpecialRef.derefAndCastPtr(temp[0], ref);
                ptr.clauseRef(en);
                Display d = en.display;
                boolean multi = d.getAndReset();
                if (!en.unify(en.skel, d, temp[1], ref))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
            case SPECIAL_CLAUSE_REF:
                return SpecialDynamic.searchKnowledgebase(AbstractDefined.OPT_CHCK_ASSE |
                        AbstractDefined.OPT_ACTI_WRIT | AbstractDefined.OPT_RSLT_CREF, en);
            case SPECIAL_SYS_RULE_REF:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                Predicate pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_UCHK);
                if (pick == null)
                    return false;
                if (!(pick.del instanceof AbstractDefined))
                    return false;

                return ((AbstractDefined) pick.del).listFirst(temp, ref,
                        AbstractDefined.OPT_RSLT_CREF, en);
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /****************************************************************************/
    /* Clause Compilation                                                       */
    /****************************************************************************/

    /**
     * <p>Compile a new clause.</p>
     * <p>The term is passed via the engine skel and display.</p>
     * <p>The following flags are recognized:</p>
     * <ul>
     * <li><b>OPT_ARGS_ASOP:</b> The term has assert options.</li>
     * <li><b>MASK_OPER_DYNA:</b> Predicate should be dynamic.</li>
     * <li><b>MASK_OPER_THRE:</b> Predicate should be thread_local.</li>
     * </ul>
     * <p>The ptr can be a rule, fact or directive.</p>
     *
     * @param flags The flags.
     * @param en    The engine.
     * @return The new clause..
     * @throws EngineMessage Validation Error.
     */
    public static Clause compileClause(int flags, Engine en)
            throws EngineMessage, EngineException {
        Object[] temp = ((SkelCompound) en.skel).args;
        Display ref = en.display;
        SupervisorCopy ec = en.visor.getCopy();
        if ((flags & AbstractDefined.OPT_ARGS_ASOP) != 0) {
            ec.vars = null;
            ec.anon = null;
            ec.flags = SupervisorCopy.MASK_COPY_SING;
            Object molec = ec.copyTermNew(temp[0], ref);
            MapHash<BindUniv, String> print =
                    SpecialRef.decodeAssertOptions(temp[2], ref, en);
            MapHashLink<String, SkelVar> vars =
                    SupervisorCopy.copyVarsUniv(ec.vars, print);
            MapHashLink<String, SkelVar> anon =
                    SupervisorCopy.copyVarsUniv(ec.anon, print);
            ec.vars = null;
            ec.anon = null;
            Object term = Clause.clauseToHead(molec, en);
            PrologReader.checkSingleton(term, anon, en);
            Clause clause = Clause.determineCompiled(flags, term, molec, en);
            clause.vars = vars;
            return clause;
        } else {
            ec.vars = null;
            ec.flags = 0;
            Object molec = ec.copyTermNew(temp[0], ref);
            ec.vars = null;
            Object term = Clause.clauseToHead(molec, en);
            Clause clause = Clause.determineCompiled(flags, term, molec, en);
            return clause;
        }
    }

    /**************************************************************/
    /* Helper                                                     */
    /**************************************************************/

    /**
     * <p>Cast a ptr.</p>
     *
     * @param m The skel.
     * @param d The display.
     * @return The ptr.
     * @throws EngineMessage Validation Error.
     */
    public static InterfaceReference derefAndCastPtr(Object m, Display d)
            throws EngineMessage {
        m = SpecialUniv.derefAndCastRef(m, d);
        if (m instanceof InterfaceReference) {
            return ((InterfaceReference) m);
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_REF, m));
        }
    }

    /**
     * <p>Decode the given assert options.</p>
     * <p>Will look for the following option:</p>
     * <ul>
     * <li><b>variable_names:</b> The variables names with singletons (input).</li>
     * </ul>
     *
     * @param t  The assert options skel.
     * @param d  The assert options display.
     * @param en The engine.
     * @return The assert options.
     * @throws EngineMessage Validation Error.
     */
    public static MapHash<BindUniv, String> decodeAssertOptions(Object t, Display d,
                                                                Engine en)
            throws EngineMessage {
        MapHash<BindUniv, String> vars = null;
        en.skel = t;
        en.display = d;
        en.deref();
        while (en.skel instanceof SkelCompound &&
                ((SkelCompound) en.skel).args.length == 2 &&
                ((SkelCompound) en.skel).sym.fun.equals(Foyer.OP_CONS)) {
            Object[] mc = ((SkelCompound) en.skel).args;
            d = en.display;
            en.skel = mc[0];
            en.deref();
            if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(ReadOpts.OP_VARIABLE_NAMES)) {
                vars = PropertyCallable.assocToMapUniv(((SkelCompound) en.skel).args[0], d, en);
            } else {
                EngineMessage.checkInstantiated(en.skel);
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_ASSERT_OPTION,
                        en.skel), en.display);
            }
            en.skel = mc[1];
            en.display = d;
            en.deref();
        }
        if (en.skel instanceof SkelAtom &&
                ((SkelAtom) en.skel).fun.equals(Foyer.OP_NIL)) {
            /* */
        } else {
            EngineMessage.checkInstantiated(en.skel);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_LIST,
                    en.skel), en.display);
        }
        return vars;
    }

}
