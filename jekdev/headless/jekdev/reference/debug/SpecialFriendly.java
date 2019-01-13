package jekdev.reference.debug;

import jekdev.model.bugger.ClauseTrace;
import jekdev.model.bugger.GoalTrace;
import jekpro.frequent.standard.EngineCopy;
import jekpro.model.inter.*;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.PrologWriter;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.model.rope.PreClause;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.proxy.FactoryAPI;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

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
    private final static int MASK_FRIEND_DEBUG = 0x00000001;

    private final static int SPECIAL_SYS_FRIENDLY = 0;
    private final static int SPECIAL_SYS_INSTRUMENTED = 1;

    private final static String CODE_NEW_BIND = " new_bind";
    private final static String CODE_DISPOSE_BIND = " dispose_bind";

    private final static String CODE_UNIFY_TERM = " unify_term";
    private final static String CODE_UNIFY_VAR = " unify_var";

    private final static String CODE_CALL_GOAL = " call_goal";
    private final static String CODE_LAST_GOAL = " last_goal";
    private final static String CODE_CALL_META = " call_meta";
    private final static String CODE_LAST_META = " last_meta";
    private final static String CODE_CALL_CONT = " call_cont";

    private final static int ALGN_NMBR = 5;

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
                FactoryAPI.checkTextWrite(obj);
                Writer wr = (Writer) obj;
                PrologWriter pw = Foyer.createWriter(Foyer.IO_TERM);
                pw.setWriteUtil(en.store);
                pw.setSource(en.store.user);
                pw.setEngineRaw(en);
                pw.setWriter(wr);
                SpecialFriendly.intermediatePredicate(pw, pick, source, 0, en);
                SpecialLoad.newLineFlush(wr);
                return en.getNextRaw();

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
                FactoryAPI.checkTextWrite(obj);
                wr = (Writer) obj;
                pw = Foyer.createWriter(Foyer.IO_TERM);
                pw.setWriteUtil(en.store);
                pw.setSource(en.store.user);
                pw.setEngineRaw(en);
                pw.setWriter(wr);
                SpecialFriendly.intermediatePredicate(pw, pick, source, MASK_FRIEND_DEBUG, en);
                SpecialLoad.newLineFlush(wr);
                return en.getNextRaw();
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
        /* flesh out clauses */
        Clause[] list = ((AbstractDefined) pick.del).listClauses(en);
        for (int i = 0; i < list.length; i++) {
            Clause clause = list[i];
            SkelAtom sa = StackElement.callableToName(clause.head);
            if (source != sa.scope)
                continue;
            Object t = PreClause.intermediateToClause(clause, en);
            pw.setFlags(PrologWriter.FLAG_QUOT | PrologWriter.FLAG_NEWL | PrologWriter.FLAG_MKDT);
            pw.setSpez(PrologWriter.SPEZ_META);
            pw.setOffset(-1);
            Display ref = SpecialLoad.showClause(pw, t, clause.vars, en,
                    SpecialLoad.MASK_SHOW_NANO | SpecialLoad.MASK_SHOW_NRBD);
            pw.setFlags(PrologWriter.FLAG_QUOT);
            pw.setSpez(PrologWriter.SPEZ_META);
            pw.setOffset(0);
            intermediateClause(pw, t, clause, ref, flags);
        }
    }

    /**
     * <p>Retrieve a variable from a clause term.</p>
     *
     * @param t The clause term.
     * @param i The variable index.
     * @return The variable, or null.
     */
    private static SkelVar getVar(Object t, int i) {
        Object var = EngineCopy.getVar(t);
        if (var == null)
            return null;
        if (var instanceof SkelVar) {
            SkelVar sv = (SkelVar) var;
            if (sv.id == i)
                return sv;
        } else {
            SkelVar[] temp = (SkelVar[]) var;
            for (int j = 0; j < temp.length; j++) {
                SkelVar sv = temp[j];
                if (sv.id == i)
                    return sv;
            }
        }
        return null;
    }

    /**
     * <p>Write out a new bind instruction.</p>
     *
     * @param t         The clause term.
     * @param lastalloc The last alloc.
     * @param endalloc  The end alloc.
     * @param pw        The prolog writer.
     * @param ref       The display.
     * @param count     The statement counter.
     * @return The statement counter.
     * @throws IOException IO error.
     */
    private static int intermediateNewBind(Object t, int lastalloc, int endalloc,
                                           PrologWriter pw, Display ref,
                                           int count)
            throws IOException, EngineException, EngineMessage {
        Writer wr = pw.getWriter();
        for (int j = lastalloc; j < endalloc; j++) {
            if (j == lastalloc) {
                count = intermediateCount(wr, count);
                wr.write(SpecialFriendly.CODE_NEW_BIND);
                wr.write(" ");
            } else {
                wr.write(", ");
            }
            pw.unparseStatement(getVar(t, j), ref);
            if (j == endalloc - 1) {
                wr.write('\n');
                wr.flush();
            }
        }
        return count;
    }

    /**
     * <p>Write a line number.</p>
     *
     * @param wr    The writer.
     * @param count The line number count.
     * @return The incremented line number count.
     */
    private static int intermediateCount(Writer wr, int count)
            throws IOException {
        wr.write(PrologWriter.align(Integer.toString(count), ALGN_NMBR, false));
        return count + 1;
    }

    /**
     * <p>Write out a dispose bind instruction.</p>
     *
     * @param t      The clause term.
     * @param lastgc The last alloc.
     * @param endgc  The end alloc.
     * @param clause The clause.
     * @param pw     The prolog writer.
     * @param ref    The display.
     * @param count  The statement counter.
     * @return The statement counter.
     * @throws IOException IO error.
     */
    private static int intermediateDisposeBind(Object t, int lastgc, int endgc,
                                               Clause clause,
                                               PrologWriter pw, Display ref,
                                               int count)
            throws IOException, EngineException, EngineMessage {
        Writer wr = pw.getWriter();
        for (int j = lastgc; j < endgc; j++) {
            if (j == lastgc) {
                count = intermediateCount(wr, count);
                wr.write(SpecialFriendly.CODE_DISPOSE_BIND);
                wr.write(" ");
            } else {
                wr.write(", ");
            }
            int k = clause.remtab[j];
            pw.unparseStatement(getVar(t, k), ref);
            if (j == endgc - 1) {
                wr.write('\n');
                wr.flush();
            }
        }
        return count;
    }

    /**
     * <p>Write out a call goal instruction.</p>
     *
     * @param body  The body goal.
     * @param pw    The prolog writer.
     * @param ref   The display.
     * @param count The statement counter.
     * @return The statement counter.
     * @throws IOException IO error.
     */
    private static int intermediateCallGoal(Goal body,
                                            PrologWriter pw, Display ref,
                                            int count)
            throws IOException, EngineException, EngineMessage {
        Writer wr = pw.getWriter();
        count = intermediateCount(wr, count);
        if ((body.flags & Intermediate.MASK_INTER_NLST) == 0) {
            if ((body.flags & Goal.MASK_GOAL_NAKE) == 0) {
                wr.write(SpecialFriendly.CODE_LAST_GOAL);
            } else {
                wr.write(SpecialFriendly.CODE_LAST_META);
            }
        } else {
            if ((body.flags & Goal.MASK_GOAL_NAKE) == 0) {
                wr.write(SpecialFriendly.CODE_CALL_GOAL);
            } else {
                wr.write(SpecialFriendly.CODE_CALL_META);
            }
        }
        wr.write(' ');
        pw.unparseStatement(body.goal, ref);
        wr.write('\n');
        wr.flush();
        return count;
    }

    /**
     * <p>Disassemble a clause.</p>
     *
     * @param pw     The prolog writer.
     * @param t      The clause term.
     * @param clause The clause.
     * @param ref    The display.
     * @param flags  The flags.
     * @throws EngineMessage IO error.
     */
    private static void intermediateClause(PrologWriter pw, Object t,
                                           Clause clause, Display ref,
                                           int flags)
            throws EngineMessage, EngineException {
        try {
            Writer wr = pw.getWriter();
            /* dissassemble the head */
            int count = 0;
            int lastalloc = 0;
            if (clause.intargs != null) {
                for (int l = 0; l < clause.intargs.length; l++) {
                    int n = clause.intargs[l];
                    if (n < 0) {
                        if (n != Integer.MIN_VALUE) {
                            count = intermediateCount(wr, count);
                            wr.write(SpecialFriendly.CODE_UNIFY_TERM);
                            wr.write(" _");
                            wr.write(Integer.toString(-n - 1));
                            wr.write(", _");
                            wr.write(Integer.toString(l));
                            wr.write('\n');
                            wr.flush();
                        }
                    } else {
                        count = intermediateNewBind(t, lastalloc, n, pw, ref, count);
                        lastalloc = n;
                        count = intermediateCount(wr, count);
                        wr.write(SpecialFriendly.CODE_UNIFY_TERM);
                        wr.write(" _");
                        wr.write(Integer.toString(l));
                        wr.write(", ");
                        pw.unparseStatement(((SkelCompound) clause.head).args[l], ref);
                        wr.write('\n');
                        wr.flush();
                    }
                }
            }
            Intermediate end = nextClause(clause, flags);
            int lastgc = 0;
            while (!(end instanceof Clause)) {
                Goal goal = (Goal) end;
                /* dissassemble a dispose */
                count = intermediateDisposeBind(t, lastgc, goal.endgc, clause, pw, ref, count);
                lastgc = goal.endgc;
                /* dissassemble a new */
                count = intermediateNewBind(t, lastalloc, goal.endalloc, pw, ref, count);
                lastalloc = goal.endalloc;
                if (goal.uniargs != null) {
                    int[] uniargs = goal.uniargs;
                    for (int l = 0; l < uniargs.length; l++) {
                        int k = uniargs[l];
                        count = intermediateCount(wr, count);
                        wr.write(SpecialFriendly.CODE_UNIFY_VAR);
                        wr.write(" _");
                        wr.write(Integer.toString(k));
                        wr.write(", ");
                        pw.unparseStatement(((SkelCompound) clause.head).args[k], ref);
                        wr.write('\n');
                        wr.flush();
                    }
                }
                /* dissassemble a goal */
                count = intermediateCallGoal(goal, pw, ref, count);
                end = nextGoal(end, flags);
            }
            /* dissassemble a dispose */
            int n = ((clause.flags & Clause.MASK_CLAUSE_NBDY) != 0 ? 0 : clause.dispsize);
            count = intermediateDisposeBind(t, lastgc, n, clause, pw, ref, count);
            /* dissassemble a continuation */
            intermediateCount(wr, count);
            wr.write(SpecialFriendly.CODE_CALL_CONT);
            wr.write('\n');
            wr.flush();
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Get the first literal of a clause.</p>
     *
     * @param clause The clause.
     * @param flags  The flags.
     * @return The first literal.
     */
    private static Intermediate nextClause(Clause clause, int flags) {
        if ((flags & MASK_FRIEND_DEBUG) != 0 &&
                clause instanceof ClauseTrace)
            return ((ClauseTrace) clause).nexttrace;
        return clause.next;
    }

    /**
     * <p>Get the next literal of a goal.</p>
     *
     * @param goal  The goal.
     * @param flags The flags.
     * @return The next literal.
     */
    private static Intermediate nextGoal(Intermediate goal, int flags) {
        if ((flags & MASK_FRIEND_DEBUG) != 0 &&
                goal instanceof GoalTrace)
            return ((GoalTrace) goal).nexttrace;
        return goal.next;
    }

}
