package jekdev.reference.debug;

import jekpro.model.builtin.SpecialBody;
import jekpro.model.inter.*;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.PrologReader;
import jekpro.model.pretty.PrologWriter;
import jekpro.model.rope.*;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.reflect.PropertyCallable;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.structure.EngineVars;
import jekpro.reference.structure.SpecialUniv;
import jekpro.reference.structure.SpecialVars;
import jekpro.tools.term.*;
import matula.util.data.*;
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
    private final static int SPECIAL_SYS_HISTOGRAM_NEW = 0;
    private final static int SPECIAL_SYS_HISTOGRAM_SHOW = 1;
    private final static int SPECIAL_SYS_VM_DISASSEMBLE_REF = 2;
    private final static int SPECIAL_SYS_VM_COLLECT_REF = 3;

    private final static String CODE_UNIFY_HEAD = "unify_head";
    private final static String CODE_UNIFY_TERM = "unify_term";
    private final static String CODE_UNIFY_COMBO = "unify_combo";

    private final static String CODE_GOAL_CALL = "goal_call";
    private final static String CODE_GOAL_LAST = "goal_last";

    private final static String CODE_FLOW_TRY = "flow_try";
    private final static String CODE_FLOW_RETRY = "flow_retry";
    private final static String CODE_FLOW_TRUST = "flow_trust";

    private final static String CODE_CUT_THEN = "cut_then";
    private final static String CODE_CUT_SOFT_THEN = "cut_soft_then";

    private final static String OP_DOMAIN_MAP = "map";
    private final static String OP_DOMAIN_CLAUSE = "clause";

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
            case SPECIAL_SYS_HISTOGRAM_NEW:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                AssocSorted<String, FriendlyReport> map = new AssocSorted<>(IgnoreCase.DEFAULT_TERTIARY);
                if (!en.unify(map, Display.DISPLAY_CONST, temp[0], ref))
                    return false;
                return true;
            case SPECIAL_SYS_HISTOGRAM_SHOW:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                map = derefAndCastMap(temp[0], ref);

                SpecialFriendly.histogramShow(map, en);
                return true;
            case SPECIAL_SYS_VM_DISASSEMBLE_REF:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;

                Clause clause = derefAndCastClause(temp[0], ref);

                en.skel = temp[1];
                en.display = ref;
                en.deref();
                Display d2 = en.display;

                MapHash<BindUniv, String> print = PropertyCallable.assocToMapUniv(temp[2], ref, en);

                Number beta = SpecialEval.derefAndCastInteger(temp[3], ref);
                SpecialEval.checkNotLessThanZero(beta);
                int flags = SpecialEval.castIntValue(beta);

                SpecialFriendly.disassembleClause(clause, d2, print, flags, en);
                return true;
            case SPECIAL_SYS_VM_COLLECT_REF:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;

                clause = derefAndCastClause(temp[0], ref);
                map = derefAndCastMap(temp[1], ref);
                SpecialFriendly.collectClause(clause, map);
                return true;
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Disassemble the given predicate.</p>
     *
     * @param clause The clause.
     * @param d2 The display.
     * @param print The print map.
     * @param flags  The flags.
     * @param en     The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void disassembleClause(Clause clause, Display d2,
                                          MapHash<BindUniv, String> print,
                                          int flags, Engine en)
            throws EngineMessage, EngineException {
        try {
            Object obj = en.visor.curoutput;
            LoadOpts.checkTextWrite(obj);
            Writer wr = (Writer) obj;

            SkelAtom sa = StackElement.callableToName(clause.head);
            PrologWriter pw = en.store.foyer.createWriter(Foyer.IO_TERM);
            pw.setDefaults(sa.scope);
            pw.setEngine(en);
            pw.setFlags(pw.getFlags() | PrologWriter.FLAG_QUOT);
            pw.setWriter(wr);
            pw.setPrintMap(print);
            FriendlyPrinter fp = new FriendlyPrinter();

            fp.flags = flags;
            fp.pw = pw;
            fp.ref = d2;
            disassembleClause(clause, fp);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Show a statistics map.</p>
     *
     * @param map The statistics map.
     * @param en  The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void histogramShow(AssocSorted<String, FriendlyReport> map, Engine en)
            throws EngineMessage {
        try {
            Object obj = en.visor.curoutput;
            LoadOpts.checkTextWrite(obj);
            Writer wr = (Writer) obj;

            FriendlyReport.show(map, wr);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /****************************************************************/
    /* List & Hooked                                                */
    /****************************************************************/

    /**
     * <p>Disassemble a clause.</p>
     *
     * @param clause The clause.
     * @param fp     The firendly printer.
     * @throws IOException     IO error.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    private static void disassembleClause(Clause clause,
                                          FriendlyPrinter fp)
            throws IOException, EngineMessage, EngineException {
        fp.count = 0;
        Writer wr = fp.pw.getWriter();
        short[] arr = clause.intargs;
        if (arr == null) {
            fp.friendlyCount();
            wr.write(SpecialFriendly.CODE_UNIFY_HEAD);
            wr.write('\n');
            wr.flush();
        } else {
            int l = 0;
            while (l < arr.length) {
                switch (arr[l++]) {
                    case Optimization.UNIFY_TERM:
                        int k = arr[l++];
                        fp.friendlyCount();
                        wr.write(SpecialFriendly.CODE_UNIFY_TERM);
                        wr.write(" _");
                        wr.write(Integer.toString(k));
                        wr.write(", ");
                        fp.pw.unparseStatement(((SkelCompound) clause.head).args[k], fp.ref);
                        wr.write('\n');
                        wr.flush();
                        break;
                    case Optimization.UNIFY_COMBO:
                        fp.friendlyCount();
                        wr.write(SpecialFriendly.CODE_UNIFY_COMBO);
                        wr.write(" _");
                        wr.write(Integer.toString(arr[l++]));
                        wr.write(", _");
                        wr.write(Integer.toString(arr[l++]));
                        wr.write('\n');
                        wr.flush();
                        break;
                    default:
                        throw new IllegalArgumentException("illegal code");
                }
            }
        }
        disassembleBody(clause, fp);
    }

    /**
     * <p>Disassemble a goal list.</p>
     *
     * @param dire The directive.
     * @param fp   The disassemble printer.
     * @throws IOException     IO error.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    private static void disassembleBody(Directive dire,
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
                        disassembleBranch(help, fp, branch == ((Goal) temp).term);
                        branch = sc.args[1];
                    }
                    if (Directive.isGuard(branch)) {
                        SkelCompound sc = (SkelCompound) branch;
                        Directive help = (Directive) sc.args[0];
                        disassembleBranch(help, fp, branch == ((Goal) temp).term);
                    } else {
                        Writer wr = fp.pw.getWriter();
                        fp.friendlyCount();
                        wr.write(SpecialFriendly.CODE_FLOW_TRUST);
                        wr.write('\n');
                        wr.flush();
                        fp.level++;
                        disassembleBody((Directive) branch, fp);
                        fp.level--;
                    }
                } else if (Directive.isSequen(branch)) {
                    SkelCompound sc = (SkelCompound) branch;
                    Directive help = (Directive) sc.args[0];
                    disassembleBody(help, fp);
                } else if ((type = SpecialBody.controlType(branch)) == SpecialBody.TYPE_CTRL_BEGN
                        || type == SpecialBody.TYPE_CTRL_SBGN) {
                    /* */
                } else if (type == SpecialBody.TYPE_CTRL_CMMT
                        || type == SpecialBody.TYPE_CTRL_SCMT) {
                    Writer wr = fp.pw.getWriter();
                    fp.friendlyCount();
                    if (type == SpecialBody.TYPE_CTRL_CMMT) {
                        wr.write(SpecialFriendly.CODE_CUT_THEN);
                    } else {
                        wr.write(SpecialFriendly.CODE_CUT_SOFT_THEN);
                    }
                    wr.write('\n');
                    wr.flush();
                } else {
                    Writer wr = fp.pw.getWriter();
                    fp.friendlyCount();
                    if ((temp.flags & Goal.MASK_GOAL_CEND) != 0) {
                        wr.write(SpecialFriendly.CODE_GOAL_LAST);
                    } else {
                        wr.write(SpecialFriendly.CODE_GOAL_CALL);
                    }
                    wr.write(' ');
                    fp.pw.unparseStatement(branch, fp.ref);
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
     * @param fp    The firendly printer.
     * @param first The first flag.
     * @throws IOException     IO error.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    private static void disassembleBranch(Directive help,
                                          FriendlyPrinter fp,
                                          boolean first)
            throws IOException, EngineException, EngineMessage {
        Writer wr = fp.pw.getWriter();
        fp.friendlyCount();
        if (first) {
            wr.write(SpecialFriendly.CODE_FLOW_TRY);
        } else {
            wr.write(SpecialFriendly.CODE_FLOW_RETRY);
        }
        wr.write('\n');
        wr.flush();
        fp.level++;
        disassembleBody(help, fp);
        fp.level--;
    }

    /****************************************************************/
    /* Summary & Report                                             */
    /****************************************************************/

    /**
     * <p>Report the given predicate.</p>
     *
     * @param def    The abstract defined.
     * @param source The source.
     * @param map    The friendly report.
     * @param en     The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void collectPredicate(AbstractDefined def,
                                         AbstractSource source,
                                         AssocSorted<String, FriendlyReport> map,
                                         Engine en)
            throws EngineMessage {
        /* collect clauses */
        Clause[] list = def.listClauses(en);
        for (int i = 0; i < list.length; i++) {
            Clause clause = list[i];
            SkelAtom sa = StackElement.callableToName(clause.head);
            if (source != sa.scope)
                continue;
            collectClause(clause, map);
        }
    }

    /**
     * <p>Report a clause.</p>
     *
     * @param map    The friendly report.
     * @param clause The clause.
     */
    private static void collectClause(Clause clause,
                                      AssocSorted<String, FriendlyReport> map) {
        short[] arr = clause.intargs;
        if (arr == null) {
            FriendlyReport.increment(map, SpecialFriendly.CODE_UNIFY_HEAD);
        } else {
            int l = 0;
            while (l < arr.length) {
                switch (arr[l++]) {
                    case Optimization.UNIFY_TERM:
                        l++;
                        FriendlyReport.increment(map, SpecialFriendly.CODE_UNIFY_TERM);
                        break;
                    case Optimization.UNIFY_COMBO:
                        l += 2;
                        FriendlyReport.increment(map, SpecialFriendly.CODE_UNIFY_COMBO);
                        break;
                    default:
                        throw new IllegalArgumentException("illegal code");
                }
            }
        }
        collectBody(clause, map);
    }

    /**
     * <p>Report a goal list.</p>
     *
     * @param dire The directive.
     * @param map  The friendly report.
     */
    private static void collectBody(Directive dire,
                                    AssocSorted<String, FriendlyReport> map) {
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
                        collectBranch(help, map, branch == ((Goal) temp).term);
                        branch = sc.args[1];
                    }
                    if (Directive.isGuard(branch)) {
                        SkelCompound sc = (SkelCompound) branch;
                        Directive help = (Directive) sc.args[0];
                        collectBranch(help, map, branch == ((Goal) temp).term);
                    } else {
                        FriendlyReport.increment(map, SpecialFriendly.CODE_FLOW_TRUST);
                        collectBody((Directive) branch, map);
                    }
                } else if (Directive.isSequen(branch)) {
                    SkelCompound sc = (SkelCompound) branch;
                    Directive help = (Directive) sc.args[0];
                    collectBody(help, map);
                } else if ((type = SpecialBody.controlType(branch)) == SpecialBody.TYPE_CTRL_BEGN
                        || type == SpecialBody.TYPE_CTRL_SBGN) {
                    /* */
                } else if (type == SpecialBody.TYPE_CTRL_CMMT
                        || type == SpecialBody.TYPE_CTRL_SCMT) {
                    if (type == SpecialBody.TYPE_CTRL_CMMT) {
                        FriendlyReport.increment(map, SpecialFriendly.CODE_CUT_THEN);
                    } else {
                        FriendlyReport.increment(map, SpecialFriendly.CODE_CUT_SOFT_THEN);
                    }
                } else {
                    if ((temp.flags & Goal.MASK_GOAL_CEND) != 0) {
                        FriendlyReport.increment(map, SpecialFriendly.CODE_GOAL_LAST);
                    } else {
                        FriendlyReport.increment(map, SpecialFriendly.CODE_GOAL_CALL);
                    }
                }
            } while (temp != dire.last);
        }
    }

    /**
     * <p>Report a try/retry goal list.</p>
     *
     * @param help  The branch.
     * @param map   The friendly report.
     * @param first The first flag.
     */
    private static void collectBranch(Directive help,
                                      AssocSorted<String, FriendlyReport> map,
                                      boolean first) {
        if (first) {
            FriendlyReport.increment(map, SpecialFriendly.CODE_FLOW_TRY);
        } else {
            FriendlyReport.increment(map, SpecialFriendly.CODE_FLOW_RETRY);
        }
        collectBody(help, map);
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
     * <p>Cast a ptr.</p>
     *
     * @param m The skel.
     * @param d The display.
     * @return The ptr.
     * @throws EngineMessage Validation Error.
     */
    private static Clause derefAndCastClause(Object m, Display d)
            throws EngineMessage {
        m = SpecialUniv.derefAndCastRef(m, d);
        if (m instanceof Clause) {
            return (Clause) m;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    OP_DOMAIN_CLAUSE, m), d);
        }
    }

}
