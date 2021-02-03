package jekpro.model.rope;

import jekpro.frequent.experiment.InterfaceReference;
import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractLocator;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.reference.runtime.SpecialDynamic;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.MapHashLink;

/**
 * <p>The class provides the clause intermediate code.</p>
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
public class Clause extends Directive implements InterfaceReference {
    public final static String OP_TURNSTILE = ":-";

    public final static int MASK_CLAUSE_ASSE = 0x00000010;

    public Object head;
    public int sizerule;
    public int[] intargs;
    public AbstractDefined del;
    public MapHashLink<String, SkelVar> vars;
    public int size;

    /**
     * <p>Create a clause.</p>
     *
     * @param copt The clause option flags.
     */
    public Clause(int copt) {
        super(copt);
    }

    /**
     * <p>Create a clause.</p>
     *
     * @param copt The clause option flags.
     * @param en   The engine.
     * @return The clause.
     */
    public static Clause createClause(int copt, Engine en) {
        if ((copt & AbstractDefined.MASK_DEFI_NIST) == 0) {
            return en.store.foyer.createClause(copt);
        } else {
            return new Clause(copt);
        }
    }

    /***********************************************************/
    /* Convert To Intermediate Form                            */
    /***********************************************************/

    /**
     * <p>Check style and assert the clause.</p>
     *
     * @param hopt  The flags.
     * @param term The head skeleton.
     * @param molec The clause skeleton.
     * @param en    The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static Clause determineCompiled(int hopt, Object term,
                                           Object molec, Engine en)
            throws EngineMessage, EngineException {
        /* process head */
        if (term == null)
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_DIRECTIVE,
                    molec), AbstractSkel.createDisplay(molec));
        AbstractDefined fun = determineDefined(term, hopt, en);
        Clause clause = Clause.createClause(fun.subflags, en);
        clause.size = SupervisorCopy.displaySize(molec);
        clause.head = SupervisorCopy.adornTermSkel(term);
        clause.del = fun;

        /* process body */
        term = Clause.clauseToBody(molec, en);
        Optimization[] vars = Optimization.createHelper(molec);
        if (vars.length != 0) {
            Optimization.setHead(clause.head, clause.flags, vars);
            Optimization.setBody(term, vars);
            clause.sizerule = Optimization.sortExtra(vars);
        }
        if ((clause.flags & AbstractDefined.MASK_DEFI_NHST) == 0) {
            clause.intargs = Optimization.unifyArgsLinear(clause.head, vars);
        } else {
            clause.intargs = Optimization.unifyArgsTerm(clause.head, vars);
        }
        clause.bodyToInterSkel(term, en, true);

        return clause;
    }

    /**
     * <p>Determine the defined of a term.</p>
     *
     * @param head The term, can be null.
     * @param hopt The term options flag.
     * @param en   The engine.
     * @return The predicate.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static AbstractDefined determineDefined(Object head, int hopt,
                                                    Engine en)
            throws EngineMessage, EngineException {
        if (head == null)
            return null;
        CachePredicate cp;
        int copt = CachePredicate.MASK_CACH_CRTE;
        if ((hopt & AbstractDefined.OPT_PROM_MASK) != AbstractDefined.OPT_PROM_STAT)
            copt |= CachePredicate.MASK_CACH_NSTS;
        if ((hopt & AbstractDefined.OPT_PROM_MASK) == AbstractDefined.OPT_PROM_STAT)
            copt |= CachePredicate.MASK_CACH_LOCA;
        SkelAtom sa;
        if (head instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) head;
            sa = sc.sym;
            cp = CachePredicate.getPredicateDefined(sa,
                    sc.args.length, en, copt);
        } else if (head instanceof SkelAtom) {
            sa = (SkelAtom) head;
            cp = CachePredicate.getPredicateDefined(sa,
                    0, en, copt);
        } else {
            EngineMessage.checkInstantiated(head);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_CALLABLE, head));
        }
        Predicate pick = cp.pick;
        AbstractDelegate fun;
        switch ((hopt & AbstractDefined.OPT_PROM_MASK)) {
            case AbstractDefined.OPT_PROM_STAT:
                fun = AbstractDefined.promoteStatic(pick, en.store);
                break;
            case AbstractDefined.OPT_PROM_DYNA:
                fun = AbstractDefined.promoteDynamic(pick, en.store);
                break;
            case AbstractDefined.OPT_PROM_THLC:
                fun = AbstractDefined.promoteThreadLocal(pick, en.store);
                break;
            default:
                throw new IllegalArgumentException("illegal promo");
        }
        /* check predicate modify */
        switch ((hopt & AbstractDefined.OPT_CHCK_MASK)) {
            case AbstractDefined.OPT_CHCK_DEFN:
                AbstractDefined.checkDefinedWrite(fun, pick);
                break;
            case AbstractDefined.OPT_CHCK_ASSE:
                AbstractDefined.checkAssertableWrite(fun, pick);
                break;
            default:
                throw new IllegalArgumentException("illegal check");
        }
        if ((hopt & AbstractDefined.OPT_PROM_MASK) == AbstractDefined.OPT_PROM_STAT) {
            AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
            AbstractLocator locator = src.locator;
            if (locator != null)
                locator.addPosition(sa.getPosition(), pick, AbstractLocator.MASK_LOC_STAT);
        }
        if ((hopt & AbstractDefined.OPT_STYL_MASK) == AbstractDefined.OPT_STYL_DECL)
            Predicate.checkPredicateBody(pick, sa, en);
        return (AbstractDefined) fun;
    }

    /**
     * <p>Determine the term of a term.</p>
     *
     * @param molec The term.
     * @param en    The engine.
     * @return The term.
     * @throws EngineMessage Shit happens.
     */
    public static Object clauseToHead(Object molec, Engine en)
            throws EngineMessage {
        if (molec instanceof SkelCompound &&
                ((SkelCompound) molec).args.length == 2 &&
                ((SkelCompound) molec).sym.fun.equals(OP_TURNSTILE)) {
            SkelCompound sc = (SkelCompound) molec;
            return SpecialDynamic.colonToCallableSkel(sc.args[0], en);
        } else if (molec instanceof SkelCompound &&
                ((SkelCompound) molec).args.length == 1 &&
                ((SkelCompound) molec).sym.fun.equals(OP_TURNSTILE)) {
            return null;
        } else {
            return SpecialDynamic.colonToCallableSkel(molec, en);
        }
    }

    /**
     * <p>Convert a term into a body.</p>
     *
     * @param molec The clause.
     * @param en    The engine.
     * @return The body.
     */
    static Object clauseToBody(Object molec, Engine en) {
        if (molec instanceof SkelCompound &&
                ((SkelCompound) molec).args.length == 2 &&
                ((SkelCompound) molec).sym.fun.equals(OP_TURNSTILE)) {
            SkelCompound sc = (SkelCompound) molec;
            return sc.args[1];
        } else if (molec instanceof SkelCompound &&
                ((SkelCompound) molec).args.length == 1 &&
                ((SkelCompound) molec).sym.fun.equals(OP_TURNSTILE)) {
            SkelCompound sc = (SkelCompound) molec;
            return sc.args[0];
        } else {
            return en.store.foyer.ATOM_TRUE;
        }
    }

    /**************************************************************/
    /* Convert from Intermediate Form                             */
    /**************************************************************/

    /**
     * <p>Convert a clause to a term.</p>
     *
     * @param clause The clause.
     * @param en     The engine.
     * @return The term.
     * @throws EngineMessage Shit happens.
     */
    public static Object interToClause(Clause clause, Engine en)
            throws EngineMessage {
        Object body = interToBodySkel(clause, clause.last, en);
        if (clause.head != null) {
            Object chead = SpecialDynamic.callableToColonSkel(clause.head, en);
            if (body instanceof SkelAtom &&
                    ((SkelAtom) body).fun.equals(Foyer.OP_TRUE)) {
                return chead;
            } else {
                return new SkelCompound(new SkelAtom(OP_TURNSTILE), chead, body);
            }
        } else {
            return new SkelCompound(new SkelAtom(OP_TURNSTILE), body);
        }
    }

    /**********************************************************/
    /* InterfaceReference Protocol                            */
    /**********************************************************/

    /**
     * <p>Assert this reference.</p>
     *
     * @param flags The flags.
     * @param en    The engine.
     * @return True if the reference was asserted, otherwise false.
     */
    public boolean assertRef(int flags, Engine en)
            throws EngineMessage {
        return del.assertClause(this, flags, en);
    }

    /**
     * <p>Retract this reference.</p>
     *
     * @param en The engine.
     * @return True if the reference was retracted, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean retractRef(Engine en)
            throws EngineMessage {
        return del.retractClause(this, en);
    }

    /**
     * <p>Clause this reference.</p>
     * <p>The result is returned in the skeleton and display.</p>
     *
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public void clauseRef(Engine en)
            throws EngineMessage {
        Object val = interToClause(this, en);
        en.skel = val;
        en.display = AbstractSkel.createMarker(val);
    }

    /**************************************************************/
    /* Accessibility Checks                                       */
    /**************************************************************/

    /**
     * <p>Check whether a source is inside the ancestors.</p>
     *
     * @param alfa The source.
     * @param en   The engine.
     * @return True if the source is inside the ancestors, false otherwise.
     */
    public static boolean ancestorSource(AbstractSource alfa, Engine en) {
        return (alfa == null ||
                en.store.ancestorStore(alfa.getStore()));
    }

}
