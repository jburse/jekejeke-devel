package jekdev.reference.debug;

import jekpro.model.inter.*;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.PrologWriter;
import jekpro.model.rope.*;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

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
    final static int MASK_FRIEND_DEBUG = 0x00000001;

    private final static int SPECIAL_SYS_FRIENDLY = 0;
    private final static int SPECIAL_SYS_INSTRUMENTED = 1;

    private final static String CODE_UNIFY_TERM = " unify_term";
    private final static String CODE_UNIFY_VAR = " unify_var";

    private final static String CODE_CALL_GOAL = " call_goal";
    private final static String CODE_CALL_META = " call_meta";
    private final static String CODE_LAST_GOAL = " last_goal";
    private final static String CODE_LAST_META = " last_meta";

    private final static String CODE_TRY_FLOW = " try_flow";
    private final static String CODE_RETRY_FLOW = " retry_flow";
    private final static String CODE_TRUST_FLOW = " trust_flow";

    private final static String CODE_IF_FLOW = " if_flow";
    private final static String CODE_ELSE_FLOW = " else_flow";
    private final static String CODE_THEN_FLOW = " then_flow";

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
            case SPECIAL_SYS_FRIENDLY:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Predicate pick = SpecialPred.indicatorToProvable(temp[0], ref, en);
                if (pick == null)
                    return false;

                SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(temp[1], ref);
                AbstractSource source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                if (source == null)
                    return false;
                if (pick.getDef(source) == null)
                    return false;

                Object obj = en.visor.curoutput;
                LoadOpts.checkTextWrite(obj);
                Writer wr = (Writer) obj;
                PrologWriter pw = en.store.foyer.createWriter(Foyer.IO_TERM);
                pw.setSource(en.visor.peekStack());
                pw.setEngineRaw(en);
                pw.setFlags(pw.getFlags() | PrologWriter.FLAG_QUOT);
                pw.setSpez(PrologWriter.SPEZ_META);
                pw.setWriter(wr);
                SpecialFriendly.intermediatePredicate(pw, pick, source, 0, en);
                SpecialLoad.newLineFlush(wr);
                return true;
            case SPECIAL_SYS_INSTRUMENTED:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToProvable(temp[0], ref, en);
                if (pick == null)
                    return false;

                sa = SpecialUniv.derefAndCastStringWrapped(temp[1], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                if (source == null)
                    return false;
                if (pick.getDef(source) == null)
                    return false;
                obj = en.visor.curoutput;
                LoadOpts.checkTextWrite(obj);
                wr = (Writer) obj;
                pw = en.store.foyer.createWriter(Foyer.IO_TERM);
                pw.setSource(en.visor.peekStack());
                pw.setEngineRaw(en);
                pw.setFlags(pw.getFlags() | PrologWriter.FLAG_QUOT);
                pw.setSpez(PrologWriter.SPEZ_META);
                pw.setWriter(wr);
                SpecialFriendly.intermediatePredicate(pw, pick, source, MASK_FRIEND_DEBUG, en);
                SpecialLoad.newLineFlush(wr);
                return true;
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

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
    private static void intermediatePredicate(PrologWriter pw, Predicate pick,
                                              AbstractSource source, int flags,
                                              Engine en)
            throws EngineMessage, EngineException {
        if (!(pick.del instanceof AbstractDefined))
            return;
        FriendlyPrinter fp = new FriendlyPrinter();
        fp.flags = flags;
        fp.pw = pw;
        /* flesh out clauses */
        Clause[] list = ((AbstractDefined) pick.del).listClauses(en);
        for (int i = 0; i < list.length; i++) {
            Clause clause = list[i];
            SkelAtom sa = StackElement.callableToName(clause.term);
            if (source != sa.scope)
                continue;
            Object t = PreClause.interToClause(clause, en);
            pw.setSource(source);
            pw.setFlags(pw.getFlags() | (PrologWriter.FLAG_NEWL | PrologWriter.FLAG_MKDT));
            pw.setOffset(-1);
            Display ref = SpecialLoad.showClause(pw, t, clause.vars, en,
                    SpecialLoad.MASK_SHOW_NANO | SpecialLoad.MASK_SHOW_NRBD);
            pw.setSource(en.visor.peekStack());
            pw.setFlags(pw.getFlags() & ~(PrologWriter.FLAG_NEWL | PrologWriter.FLAG_MKDT));
            pw.setOffset(0);
            friendlyClause(clause, ref, fp);
        }
    }

    /**
     * <p>Disassemble a clause.</p>
     *
     * @param clause The clause.
     * @param ref    The display.
     * @throws EngineMessage IO error.
     */
    private static void friendlyClause(Clause clause, Display ref, FriendlyPrinter fp)
            throws EngineMessage, EngineException {
        try {
            fp.count = 0;
            Writer wr = fp.pw.getWriter();
            if (clause.intargs != null) {
                for (int l = 0; l < clause.intargs.length; l++) {
                    int n = clause.intargs[l];
                    if (n >= 0) {
                        if (n != Integer.MIN_VALUE) {
                            fp.friendlyCount();
                            wr.write(SpecialFriendly.CODE_UNIFY_TERM);
                            wr.write(" _");
                            wr.write(Integer.toString(n));
                            wr.write(", _");
                            wr.write(Integer.toString(l));
                            wr.write('\n');
                            wr.flush();
                        }
                    } else if (n == Optimization.UNIFY_TERM) {
                        fp.friendlyCount();
                        wr.write(SpecialFriendly.CODE_UNIFY_TERM);
                        wr.write(" _");
                        wr.write(Integer.toString(l));
                        wr.write(", ");
                        fp.pw.unparseStatement(((SkelCompound) clause.term).args[l], ref);
                        wr.write('\n');
                        wr.flush();
                    } else if (n == Optimization.UNIFY_VAR) {
                        fp.friendlyCount();
                        wr.write(SpecialFriendly.CODE_UNIFY_VAR);
                        wr.write(" _");
                        wr.write(Integer.toString(l));
                        wr.write(", ");
                        fp.pw.unparseStatement(((SkelCompound) clause.term).args[l], ref);
                        wr.write('\n');
                        wr.flush();
                    }
                }
            }
            friendlyBody(clause, ref, fp);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Write out a goal list.</p>
     *
     * @param dire The directive.
     * @param ref  The display.
     * @param fp   The firendly printer.
     * @throws IOException IO error.
     */
    private static void friendlyBody(Directive dire,
                                     Display ref,
                                     FriendlyPrinter fp)
            throws IOException, EngineException, EngineMessage {
        if (fp.lastDirective(dire) == null)
            return;
        Intermediate temp = fp.nextDirective(dire);
        for (; ; ) {
            if (Directive.isAlternative(temp.term)) {
                Object branch = temp.term;
                Writer wr = fp.pw.getWriter();
                do {
                    SkelCompound sc = (SkelCompound) branch;
                    Directive help = (Directive) sc.args[0];
                    fp.friendlyCount();
                    if (help.last != null && Directive.isBegin(help.next.term)) {
                        if (branch == temp.term) {
                            wr.write(SpecialFriendly.CODE_IF_FLOW);
                        } else {
                            wr.write(SpecialFriendly.CODE_ELSE_FLOW);
                        }
                    } else {
                        if (branch == temp.term) {
                            wr.write(SpecialFriendly.CODE_TRY_FLOW);
                        } else {
                            wr.write(SpecialFriendly.CODE_RETRY_FLOW);
                        }
                    }
                    wr.write('\n');
                    wr.flush();
                    fp.level++;
                    friendlyBody(help, ref, fp);
                    fp.level--;
                    branch = sc.args[1];
                } while (Directive.isAlternative(branch));
                fp.friendlyCount();
                wr.write(SpecialFriendly.CODE_TRUST_FLOW);
                wr.write('\n');
                wr.flush();
                fp.level++;
                friendlyBody((Directive) branch, ref, fp);
                fp.level--;
            } else if (Directive.isBegin(temp.term)) {
                /* */
            } else if (Directive.isCommit(temp.term)) {
                Writer wr = fp.pw.getWriter();
                fp.friendlyCount();
                wr.write(SpecialFriendly.CODE_THEN_FLOW);
                wr.write('\n');
                wr.flush();
            } else {
                Writer wr = fp.pw.getWriter();
                fp.friendlyCount();
                if ((temp.flags & Goal.MASK_GOAL_CEND) == 0) {
                    if ((temp.flags & Goal.MASK_GOAL_NAKE) == 0) {
                        wr.write(SpecialFriendly.CODE_CALL_GOAL);
                    } else {
                        wr.write(SpecialFriendly.CODE_CALL_META);
                    }
                } else {
                    if ((temp.flags & Goal.MASK_GOAL_NAKE) == 0) {
                        wr.write(SpecialFriendly.CODE_LAST_GOAL);
                    } else {
                        wr.write(SpecialFriendly.CODE_LAST_META);
                    }
                }
                wr.write(' ');
                fp.pw.unparseStatement(temp.term, ref);
                wr.write('\n');
                wr.flush();
            }
            if (fp.lastDirective(dire) == temp)
                break;
            temp = fp.nextGoal(temp);
        }
    }

}
