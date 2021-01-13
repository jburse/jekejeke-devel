package jekdev.reference.debug;

import jekpro.model.inter.*;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.PrologWriter;
import jekpro.model.rope.*;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.AssocSorted;
import matula.util.regex.IgnoreCase;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>Provides a special predicate for intermediate code listing.</p>
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
public final class SpecialFriendly extends AbstractSpecial {
    private final static int SPECIAL_SYS_VM_DISASSEMBLE = 0;
    private final static int SPECIAL_SYS_VM_COLLECT = 1;
    private final static int SPECIAL_SYS_MAP_NEW = 2;
    private final static int SPECIAL_SYS_MAP_SHOW = 3;

    private final static String CODE_UNIFY_TERM = " unify_term";
    private final static String CODE_UNIFY_MIXED = " unify_mixed";
    private final static String CODE_UNIFY_LINEAR = " unify_linear";
    private final static String CODE_UNIFY_COMBO = " unify_combo";

    private final static String CODE_CALL_GOAL = " call_goal";
    private final static String CODE_LAST_GOAL = " last_goal";

    private final static String CODE_TRY_FLOW = " try_flow";
    private final static String CODE_RETRY_FLOW = " retry_flow";
    private final static String CODE_TRUST_FLOW = " trust_flow";

    private final static String CODE_THEN_FLOW = " then_flow";
    private final static String CODE_SOFT_THEN_FLOW = " soft_then_flow";

    private final static String OP_DOMAIN_MAP = "map";

    /**
     * <p>Create a index dump special.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialFriendly(int i) {
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
            case SPECIAL_SYS_VM_DISASSEMBLE:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Predicate pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_UCHK);
                if (pick == null)
                    return false;

                SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(temp[1], ref);
                AbstractSource source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                if (source == null)
                    return false;
                if (pick.getDef(source) == null)
                    return false;

                Number beta = SpecialEval.derefAndCastInteger(temp[2], ref);
                SpecialEval.checkNotLessThanZero(beta);
                int flags = SpecialEval.castIntValue(beta);

                Object obj = en.visor.curoutput;
                LoadOpts.checkTextWrite(obj);
                Writer wr = (Writer) obj;
                PrologWriter pw = en.store.foyer.createWriter(Foyer.IO_TERM);
                pw.setSource(en.visor.peekStack());
                pw.setEngineRaw(en);
                pw.setFlags(pw.getFlags() | PrologWriter.FLAG_QUOT);
                pw.setWriter(wr);
                SpecialFriendly.disassemblePredicate(pw, pick, source, flags, en);
                SpecialLoad.newLineFlush(wr);
                return true;
            case SPECIAL_SYS_VM_COLLECT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_UCHK);
                if (pick == null)
                    return false;

                sa = SpecialUniv.derefAndCastStringWrapped(temp[1], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                if (source == null)
                    return false;
                if (pick.getDef(source) == null)
                    return false;

                AssocSorted<String, Integer> map = derefAndCastMap(temp[2], ref);
                SpecialFriendly.collectPredicate(pick, source, map, en);
                return true;
            case SPECIAL_SYS_MAP_NEW:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                map = new AssocSorted<>(IgnoreCase.DEFAULT_TERTIARY);
                if (!BindUniv.unifyTerm(map, Display.DISPLAY_CONST, temp[0], ref, en))
                    return false;
                return true;
            case SPECIAL_SYS_MAP_SHOW:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                map = derefAndCastMap(temp[0], ref);
                obj = en.visor.curoutput;
                LoadOpts.checkTextWrite(obj);
                wr = (Writer) obj;
                SpecialFriendly.mapShow(wr, map);
                return true;
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /****************************************************************/
    /* List & Hooked                                                */
    /****************************************************************/

    /**
     * <p>Disassemble the given predicate.</p>
     *
     * @param pw     The prolog writer.
     * @param pick   The predicate.
     * @param source The source.
     * @param flags  The flags.
     * @param en     The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void disassemblePredicate(PrologWriter pw, Predicate pick,
                                          AbstractSource source, int flags,
                                          Engine en)
            throws EngineMessage, EngineException {
        try {
            if (!(pick.del instanceof AbstractDefined))
                return;
            FriendlyPrinter fp = new FriendlyPrinter();
            fp.flags = flags;
            fp.pw = pw;
            /* flesh out clauses */
            Clause[] list = ((AbstractDefined) pick.del).listClauses(en);
            for (int i = 0; i < list.length; i++) {
                Clause clause = list[i];
                SkelAtom sa = StackElement.callableToName(clause.head);
                if (source != sa.scope)
                    continue;
                Object t = PreClause.interToClause(clause, en);
                pw.setSource(source);
                pw.setFlags(pw.getFlags() | (PrologWriter.FLAG_NEWL | PrologWriter.FLAG_MKDT));
                pw.setSpez(PrologWriter.SPEZ_META);
                pw.setOffset(-1);
                Display ref = SpecialLoad.showClause(pw, t, clause.vars, en,
                        SpecialLoad.MASK_SHOW_NANO | SpecialLoad.MASK_SHOW_NRBD);
                pw.setSource(en.visor.peekStack());
                pw.setFlags(pw.getFlags() & ~(PrologWriter.FLAG_NEWL | PrologWriter.FLAG_MKDT));
                pw.setSpez(0);
                disassembleClause(clause, ref, fp);
            }
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Disassemble a clause.</p>
     *
     * @param clause The clause.
     * @param ref    The display.
     * @param fp     The firendly printer.
     * @throws IOException     IO error.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    private static void disassembleClause(Clause clause, Display ref,
                                       FriendlyPrinter fp)
            throws IOException, EngineMessage, EngineException {
        fp.count = 0;
        Writer wr = fp.pw.getWriter();
        if (clause.intargs != null) {
            for (int l = 0; l < clause.intargs.length; l++) {
                int n = clause.intargs[l];
                switch (n) {
                    case Optimization.UNIFY_SKIP:
                        break;
                    case Optimization.UNIFY_LINEAR:
                    case Optimization.UNIFY_MIXED:
                    case Optimization.UNIFY_TERM:
                        fp.friendlyCount();
                        if (n == Optimization.UNIFY_TERM) {
                            wr.write(SpecialFriendly.CODE_UNIFY_TERM);
                        } else if (n == Optimization.UNIFY_MIXED) {
                            wr.write(SpecialFriendly.CODE_UNIFY_MIXED);
                        } else {
                            wr.write(SpecialFriendly.CODE_UNIFY_LINEAR);
                        }
                        wr.write(" _");
                        wr.write(Integer.toString(l));
                        wr.write(", ");
                        fp.pw.unparseStatement(((SkelCompound) clause.head).args[l], ref);
                        wr.write('\n');
                        wr.flush();
                        break;
                    default:
                        fp.friendlyCount();
                        wr.write(SpecialFriendly.CODE_UNIFY_COMBO);
                        wr.write(" _");
                        wr.write(Integer.toString(n));
                        wr.write(", _");
                        wr.write(Integer.toString(l));
                        wr.write('\n');
                        wr.flush();
                        break;
                }
            }
        }
        disassembleBody(clause, ref, fp);
    }

    /**
     * <p>Disassemble a goal list.</p>
     *
     * @param dire The directive.
     * @param ref  The display.
     * @param fp   The disassemble printer.
     * @throws IOException     IO error.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    private static void disassembleBody(Directive dire,
                                     Display ref,
                                     FriendlyPrinter fp)
            throws IOException, EngineException, EngineMessage {
        Intermediate temp = dire;
        if (fp.lastDirective(dire) != null) {
            do {
                if (temp instanceof Directive) {
                    temp = fp.nextDirective((Directive) temp);
                } else {
                    temp = fp.nextGoal(temp);
                }
                Object branch = ((Goal) temp).term;
                int type;
                if (Directive.isAlter(branch) || Directive.isGuard(branch)) {
                    while (Directive.isAlter(branch)) {
                        SkelCompound sc = (SkelCompound) branch;
                        Directive help = (Directive) sc.args[0];
                        disassembleBranch(help, ref, fp, branch == ((Goal) temp).term);
                        branch = sc.args[1];
                    }
                    if (Directive.isGuard(branch)) {
                        SkelCompound sc = (SkelCompound) branch;
                        Directive help = (Directive) sc.args[0];
                        disassembleBranch(help, ref, fp, branch == ((Goal) temp).term);
                    } else {
                        Writer wr = fp.pw.getWriter();
                        fp.friendlyCount();
                        wr.write(SpecialFriendly.CODE_TRUST_FLOW);
                        wr.write('\n');
                        wr.flush();
                        fp.level++;
                        disassembleBody((Directive) branch, ref, fp);
                        fp.level--;
                    }
                } else if (Directive.isSequen(branch)) {
                    SkelCompound sc = (SkelCompound) branch;
                    Directive help = (Directive) sc.args[0];
                    disassembleBody(help, ref, fp);
                } else if ((type = Directive.controlType(branch)) == Directive.TYPE_CTRL_BEGN
                        || type == Directive.TYPE_CTRL_SBGN) {
                    /* */
                } else if (type == Directive.TYPE_CTRL_CMMT
                        || type == Directive.TYPE_CTRL_SCMT) {
                    Writer wr = fp.pw.getWriter();
                    fp.friendlyCount();
                    if (type == Directive.TYPE_CTRL_CMMT) {
                        wr.write(SpecialFriendly.CODE_THEN_FLOW);
                    } else {
                        wr.write(SpecialFriendly.CODE_SOFT_THEN_FLOW);
                    }
                    wr.write('\n');
                    wr.flush();
                } else {
                    Writer wr = fp.pw.getWriter();
                    fp.friendlyCount();
                    if ((temp.flags & Goal.MASK_GOAL_CEND) == 0) {
                        wr.write(SpecialFriendly.CODE_CALL_GOAL);
                    } else {
                        wr.write(SpecialFriendly.CODE_LAST_GOAL);
                    }
                    wr.write(' ');
                    fp.pw.unparseStatement(branch, ref);
                    wr.write('\n');
                    wr.flush();
                }
            } while (temp != fp.lastDirective(dire));
        }
    }

    /**
     * <p>Disassemble a try/retry goal list.</p>
     *
     * @param help  The branch.
     * @param ref   The display.
     * @param fp    The firendly printer.
     * @param first The first flag.
     * @throws IOException     IO error.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    private static void disassembleBranch(Directive help,
                                       Display ref,
                                       FriendlyPrinter fp,
                                       boolean first)
            throws IOException, EngineException, EngineMessage {
        Writer wr = fp.pw.getWriter();
        fp.friendlyCount();
        if (first) {
            wr.write(SpecialFriendly.CODE_TRY_FLOW);
        } else {
            wr.write(SpecialFriendly.CODE_RETRY_FLOW);
        }
        wr.write('\n');
        wr.flush();
        fp.level++;
        disassembleBody(help, ref, fp);
        fp.level--;
    }

    /****************************************************************/
    /* Summary & Report                                             */
    /****************************************************************/

    /**
     * <p>Report the given predicate.</p>
     *
     * @param pick   The predicate.
     * @param source The source.
     * @param map    The statistics map.
     * @param en     The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void collectPredicate(Predicate pick,
                                        AbstractSource source,
                                        AssocSorted<String, Integer> map,
                                        Engine en)
            throws EngineMessage {
        if (!(pick.del instanceof AbstractDefined))
            return;
        FriendlyReport fp = new FriendlyReport();
        fp.map = map;
        /* flesh out clauses */
        Clause[] list = ((AbstractDefined) pick.del).listClauses(en);
        for (int i = 0; i < list.length; i++) {
            Clause clause = list[i];
            SkelAtom sa = StackElement.callableToName(clause.head);
            if (source != sa.scope)
                continue;
            collectClause(clause, fp);
        }
    }

    /**
     * <p>Report a clause.</p>
     *
     * @param clause The clause.
     */
    private static void collectClause(Clause clause, FriendlyReport fp) {
        if (clause.intargs != null) {
            for (int l = 0; l < clause.intargs.length; l++) {
                int n = clause.intargs[l];
                switch (n) {
                    case Optimization.UNIFY_SKIP:
                        break;
                    case Optimization.UNIFY_LINEAR:
                    case Optimization.UNIFY_MIXED:
                    case Optimization.UNIFY_TERM:
                        if (n == Optimization.UNIFY_TERM) {
                            fp.increment(SpecialFriendly.CODE_UNIFY_TERM);
                        } else if (n == Optimization.UNIFY_MIXED) {
                            fp.increment(SpecialFriendly.CODE_UNIFY_MIXED);
                        } else {
                            fp.increment(SpecialFriendly.CODE_UNIFY_LINEAR);
                        }
                        break;
                    default:
                        fp.increment(SpecialFriendly.CODE_UNIFY_COMBO);
                        break;
                }
            }
        }
        collectBody(clause, fp);
    }

    /**
     * <p>Report a goal list.</p>
     *
     * @param dire The directive.
     * @param fp   The collect printer.
     */
    private static void collectBody(Directive dire,
                                   FriendlyReport fp) {
        Intermediate temp = dire;
        if (dire.last != null) {
            do {
                temp = temp.next;
                Object branch = ((Goal) temp).term;
                int type;
                if (Directive.isAlter(branch) || Directive.isGuard(branch)) {
                    while (Directive.isAlter(branch)) {
                        SkelCompound sc = (SkelCompound) branch;
                        Directive help = (Directive) sc.args[0];
                        collectBranch(help, fp, branch == ((Goal) temp).term);
                        branch = sc.args[1];
                    }
                    if (Directive.isGuard(branch)) {
                        SkelCompound sc = (SkelCompound) branch;
                        Directive help = (Directive) sc.args[0];
                        collectBranch(help, fp, branch == ((Goal) temp).term);
                    } else {
                        fp.increment(SpecialFriendly.CODE_TRUST_FLOW);
                        collectBody((Directive) branch, fp);
                    }
                } else if (Directive.isSequen(branch)) {
                    SkelCompound sc = (SkelCompound) branch;
                    Directive help = (Directive) sc.args[0];
                    collectBody(help, fp);
                } else if ((type = Directive.controlType(branch)) == Directive.TYPE_CTRL_BEGN
                        || type == Directive.TYPE_CTRL_SBGN) {
                    /* */
                } else if (type == Directive.TYPE_CTRL_CMMT
                        || type == Directive.TYPE_CTRL_SCMT) {
                    if (type == Directive.TYPE_CTRL_CMMT) {
                        fp.increment(SpecialFriendly.CODE_THEN_FLOW);
                    } else {
                        fp.increment(SpecialFriendly.CODE_SOFT_THEN_FLOW);
                    }
                } else {
                    if ((temp.flags & Goal.MASK_GOAL_CEND) == 0) {
                        fp.increment(SpecialFriendly.CODE_CALL_GOAL);
                    } else {
                        fp.increment(SpecialFriendly.CODE_LAST_GOAL);
                    }
                }
            } while (temp != dire.last);
        }
    }

    /**
     * <p>Report a try/retry goal list.</p>
     *
     * @param help  The branch.
     * @param fp    The firendly printer.
     * @param first The first flag.
     */
    private static void collectBranch(Directive help,
                                     FriendlyReport fp,
                                     boolean first) {
        if (first) {
            fp.increment(SpecialFriendly.CODE_TRY_FLOW);
        } else {
            fp.increment(SpecialFriendly.CODE_RETRY_FLOW);
        }
        collectBody(help, fp);
    }

    /**************************************************************/
    /* Helper                                                     */
    /**************************************************************/

    /**
     * <p>Cast a map.</p>
     *
     * @param m The term skel.
     * @param d The term display.
     * @return The map.
     * @throws EngineMessage Shit happens.
     */
    public static AssocSorted derefAndCastMap(Object m, Display d)
            throws EngineMessage {
        m = SpecialUniv.derefAndCastRef(m, d);
        if (m instanceof AssocSorted) {
            return (AssocSorted) m;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    OP_DOMAIN_MAP, m), d);
        }
    }
    
    /**
     * <p>Show a statistics map.</p>
     *
     * @param wr  The writer.
     * @param map The statistics map.
     * @throws EngineMessage Shit happens.
     */
    private static void mapShow(Writer wr, AssocSorted<String, Integer> map)
            throws EngineMessage {
        try {
            FriendlyReport fp = new FriendlyReport();
            fp.map = map;
            fp.mapShow(wr);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }
    
}
