package jekpro.reference.runtime;

import jekpro.frequent.standard.EngineCopy;
import jekpro.frequent.stream.ForeignConsole;
import jekpro.model.builtin.Branch;
import jekpro.model.inter.*;
import jekpro.model.molec.*;
import jekpro.model.pretty.*;
import jekpro.model.rope.*;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.proxy.FactoryAPI;
import jekpro.tools.term.PositionKey;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.MapHashLink;
import matula.util.regex.ScannerError;
import matula.util.system.ConnectionReader;
import matula.util.system.OpenOpts;
import matula.util.wire.LangProperties;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.util.Locale;
import java.util.Properties;

/**
 * <p>Provides built-in predicates for sessions.</p>
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
public final class SpecialSession extends AbstractSpecial {
    private final static int SPECIAL_BREAK = 1;
    private final static int SPECIAL_SYS_QUOTED_VAR = 2;
    private final static int SPECIAL_SYS_GET_RAW_VARIABLES = 3;

    /**
     * <p>Create a session special.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialSession(int i) {
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
            case SPECIAL_BREAK:
                /* increase level */
                en.visor.breaklevel++;
                if (en.visor.breaklevel == 0)
                    en.visor.pushStack(en.store.user);
                try {
                    SpecialSession.sessionTerminal(en);
                    if (en.visor.breaklevel == 0)
                        LoadForce.checkModuleEnd(en);
                } catch (EngineMessage x) {
                    /* decrease level */
                    if (en.visor.breaklevel == 0) {
                        LoadForce.undoNonEmptyStack(en);
                        en.visor.popStack();
                    }
                    en.visor.breaklevel--;
                    throw x;
                } catch (EngineException x) {
                    /* decrease level */
                    if (en.visor.breaklevel == 0) {
                        LoadForce.undoNonEmptyStack(en);
                        en.visor.popStack();
                    }
                    en.visor.breaklevel--;
                    throw x;
                }
                /* decrease level */
                if (en.visor.breaklevel == 0) {
                    LoadForce.undoNonEmptyStack(en);
                    en.visor.popStack();
                }
                en.visor.breaklevel--;
                return en.getNextRaw();
            case SPECIAL_SYS_QUOTED_VAR:
                Object[] temp = ((SkelCompound) en.skel).args;
                BindCount[] ref = en.display;
                String fun = SpecialUniv.derefAndCastString(temp[0], ref);
                if (!en.unifyTerm(temp[1], ref, sysQuoteVar(fun, en), BindCount.DISPLAY_CONST))
                    return false;
                return en.getNextRaw();
            case SPECIAL_SYS_GET_RAW_VARIABLES:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                StackElement frame = en.visor.ref;
                BindCount[] ref2 = (frame != null ? frame.contdisplay.bind : null);
                Clause def = (frame != null ? frame.contskel.getClause() : null);
                en.skel = namedToAssoc((def != null ? def.vars : null), ref2, en.store);
                if (!en.unifyTerm(temp[0], ref, en.skel, ref2))
                    return false;
                return en.getNext();
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Ask the end-user for the session action.</p>
     *
     * @param en The engine.
     * @return The debugger action.
     * @throws EngineMessage Shit happens.
     */
    private static String askSessionAction(Engine en)
            throws EngineMessage {
        Object obj = en.visor.dispoutput;
        FactoryAPI.checkTextWrite(obj);
        Writer wr = (Writer) obj;

        obj = en.visor.dispinput;
        PrologReader.checkTextRead(obj);
        Reader lr = (Reader) obj;
        try {
            wr.write(" ");
            wr.flush();
            return ForeignConsole.readLine(lr);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Feedback for deterministic success.</p>
     *
     * @param en The engine trace.
     * @throws EngineMessage Shit happens.
     */
    private static void detFeedback(Engine en)
            throws EngineMessage {
        Object obj = en.visor.dispoutput;
        FactoryAPI.checkTextWrite(obj);
        Writer wr = (Writer) obj;
        try {
            wr.write('\n');
            wr.flush();
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Feedback for failure.</p>
     *
     * @param en The engine trace.
     * @throws EngineMessage Shit happens.
     */
    private static void failFeedback(Engine en)
            throws EngineMessage {
        Object obj = en.visor.dispoutput;
        FactoryAPI.checkTextWrite(obj);
        Writer wr = (Writer) obj;
        try {
            Locale locale = en.store.foyer.locale;
            Properties resources = LangProperties.getLang(
                    SpecialSession.class, "runtime", locale);
            wr.write(resources.getProperty("query.no"));
            wr.write('\n');
            wr.flush();
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Display the help text.</p>
     *
     * @param pos The position key.
     * @param en  The engine trace.
     * @throws EngineException Shit happens.
     */
    private static void helpText(PositionKey pos,
                                 Engine en)
            throws EngineException {
        try {
            Object obj = en.visor.dispoutput;
            FactoryAPI.checkTextWrite(obj);
            Writer wr = (Writer) obj;
            try {
                Locale locale = en.store.foyer.locale;
                Properties resources = LangProperties.getLang(
                        SpecialSession.class, "runtime", locale);
                wr.write(resources.getProperty("query.continue"));
                wr.write('\n');
                wr.write('\n');
                wr.flush();
            } catch (IOException x) {
                throw EngineMessage.mapIOException(x);
            }
        } catch (EngineMessage x) {
            EngineException y = new EngineException(x,
                    EngineException.fetchLoc(
                            EngineException.fetchStack(en), pos, en));
            en.display = null;
            throw y;
        }
    }

    /**
     * <p>Prompt for the query.</p>
     *
     * @param src The current source.
     * @param en  The engine trace.
     * @throws EngineMessage Shit happens.
     */
    private static void promptQuery(AbstractSource src, Engine en)
            throws EngineMessage, EngineException {
        Object obj = en.visor.dispoutput;
        FactoryAPI.checkTextWrite(obj);
        Writer wr = (Writer) obj;
        try {
            if (en.visor.breaklevel != 0) {
                wr.write("[");
                wr.write(Integer.toString(en.visor.breaklevel));
                wr.write("] ");
            }
            String s = src.getFullName();
            if (!Branch.OP_USER.equals(s)) {
                wr.write("(");
                Object res = SpecialDynamic.moduleToSlashSkel(s, en.store.user, en);
                PrologWriter.toString(res, BindCount.DISPLAY_CONST, wr, 0, en);
                wr.write(") ");
            }
            wr.write("?- ");
            wr.flush();
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Prolog top level.</p>
     *
     * @param en The engine trace.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void sessionTerminal(Engine en)
            throws EngineException, EngineMessage {
        Object obj = en.visor.dispinput;
        PrologReader.checkTextRead(obj);
        Reader lr = (Reader) obj;

        PrologReader rd = en.store.foyer.createReader(Foyer.IO_TERM);
        rd.getScanner().setReader(lr);
        rd.setEngineRaw(en);
        PositionKey pos = null;
        for (; ; ) {
            try {
                AbstractSource src = en.visor.peekStack();
                SpecialSession.promptQuery(src, en);
                int flags = 0;
                if ((en.store.foyer.getBits() & Foyer.MASK_STORE_CEXP) == 0 &&
                        (en.store.foyer.getBits() & Foyer.MASK_STORE_NBCV) != 0)
                    flags |= PrologReader.FLAG_NEWV;
                rd.setFlags(flags);
                rd.setReadUtil(en.store);
                rd.setSource(src);
                Object val;
                try {
                    val = rd.parseHeadStatement();
                } catch (ScannerError y) {
                    String line = ScannerError.linePosition(OpenOpts.getLine(lr), y.getPos());
                    rd.parseTailError(PrologReader.OP_PERIOD, y);
                    EngineMessage x = new EngineMessage(EngineMessage.syntaxError(y.getError()));
                    pos = (OpenOpts.getPath(lr) != null ?
                            new PositionKey(OpenOpts.getPath(lr), OpenOpts.getLineNumber(lr)) : null);
                    throw new EngineException(x,
                            EngineException.fetchPos(EngineException.fetchLoc(
                                    EngineException.fetchStack(en),
                                    pos, en), line, en)
                    );
                }
                pos = (OpenOpts.getPath(lr) != null ?
                        new PositionKey(OpenOpts.getPath(lr), rd.getClauseStart()) : null);
                if (val instanceof SkelAtom &&
                        ((SkelAtom) val).fun.equals(AbstractSource.OP_END_OF_FILE))
                    break;
                PreClause pre = expandGoalAndWrap(rd, val, pos, en);
                Clause clause = Clause.createClause(AbstractDefined.MASK_DEFI_NBDY |
                                AbstractDefined.MASK_DEFI_NLST |
                                AbstractDefined.MASK_DEFI_STOP, en);
                clause.analyzeBody(pre.molec, en);
                clause.vars = pre.vars;

                Intermediate r = en.contskel;
                Display u = en.contdisplay;
                AbstractBind mark = en.bind;
                int snap = en.number;
                StackElement backref = en.visor.ref;
                try {
                    Display ref = new Display();
                    ref.bind = BindCount.newBindClause(clause.dispsize);
                    en.visor.ref = new StackElement(clause, ref);
                    ref.setEngine(en);
                    en.contskel = clause.getNextRaw(en);
                    en.contdisplay = ref;
                    boolean found = en.runLoop(snap, true);
                    if (!found)
                        failFeedback(en);
                    while (found) {
                        en.skel = new SkelAtom("sys_show_vars");
                        en.display = BindCount.DISPLAY_CONST;
                        en.contskel = r;
                        en.contdisplay = u;
                        en.invokeChecked();
                        if (en.number != snap) {
                            String action = askSessionAction(en);
                            if (action == null) {
                                throw new EngineMessage(EngineMessage.systemError(
                                        EngineMessage.OP_SYSTEM_USER_EXIT));
                            } else if (";".equals(action)) {
                                found = en.runLoop(snap, false);
                                if (!found)
                                    failFeedback(en);
                            } else if ("?".equals(action)) {
                                helpText(pos, en);
                            } else {
                                try {
                                    if (parseAction(action, src, en)) {
                                        en.contskel = r;
                                        en.contdisplay = u;
                                        en.invokeChecked();
                                    } else {
                                        found = false;
                                    }
                                } catch (EngineException x) {
                                    systemSessionBreak(x, en);
                                }
                            }
                        } else {
                            detFeedback(en);
                            found = false;
                        }
                    }
                } catch (EngineMessage x) {
                    en.contskel = r;
                    en.contdisplay = u;
                    en.fault = new EngineException(x,
                            EngineException.fetchLoc(
                                    EngineException.fetchStack(en), pos, en));
                    en.releaseBind(mark);
                    en.visor.ref = backref;
                    throw en.fault;
                } catch (EngineException x) {
                    en.contskel = r;
                    en.contdisplay = u;
                    en.fault = x;
                    en.releaseBind(mark);
                    en.visor.ref = backref;
                    throw en.fault;
                }
                en.contskel = r;
                en.contdisplay = u;
                en.window = null;
                en.fault = null;
                en.cutChoices(snap);
                en.releaseBind(mark);
                en.visor.ref = backref;
                if (en.fault != null)
                    throw en.fault;
            } catch (EngineMessage x) {
                EngineException y = new EngineException(x, EngineException.fetchLoc(
                        EngineException.fetchStack(en), pos, en));
                if (systemQueryBreak(y, en))
                    break;
            } catch (EngineException x) {
                if (systemQueryBreak(x, en))
                    break;
            }
        }
    }

    /**
     * <p>Expand and wrap the given goal.</p>
     * <p>The variables are passed via the Prolog reader.</p>
     * <p>The variables are returned via the Prolog reader.</p>
     *
     * @param rd The prolog reader.
     * @param t  The directive skel.
     * @param en The engine.
     * @return The expanded goal.
     * @throws EngineException Shit happens.
     */
    private static PreClause expandGoalAndWrap(PrologReader rd, Object t,
                                               PositionKey pos, Engine en)
            throws EngineException, EngineMessage {
        if ((en.store.foyer.getBits() & Foyer.MASK_STORE_CEXP) == 0 &&
                (en.store.foyer.getBits() & Foyer.MASK_STORE_NBCV) != 0) {
            PreClause pre = new PreClause();
            pre.molec = new SkelCompound(new SkelAtom(
                    PreClause.OP_TURNSTILE), t);
            pre.vars = Named.makeNamed(rd.getVars());
            return pre;
        }

        /* expand goal */
        AbstractBind mark = en.bind;
        BindCount[] d;
        if ((en.store.foyer.getBits() & Foyer.MASK_STORE_CEXP) == 0) {
            int size = rd.getGensym();
            d = (size != 0 ? BindCount.newBind(size) : BindCount.DISPLAY_CONST);
            en.fault = null;
        } else {
            Intermediate r = en.contskel;
            Display u = en.contdisplay;
            SkelVar var = rd.atomToVariable(PrologReader.OP_ANON);
            SkelAtom sa = new SkelAtom("expand_goal", en.store.getRootSystem());
            Object molec = new SkelCompound(new SkelAtom(
                    PreClause.OP_TURNSTILE), new SkelCompound(sa, t, var));
            Clause clause = Clause.createClause(AbstractDefined.MASK_DEFI_NBDY |
                        AbstractDefined.MASK_DEFI_NLST |
                        AbstractDefined.MASK_DEFI_STOP, en);
            clause.analyzeBody(molec, en);
            clause.vars = Named.makeNamed(rd.getVars());

            int snap = en.number;
            StackElement backref = en.visor.ref;
            Display ref;
            try {
                ref = new Display();
                ref.bind = BindCount.newBindClause(clause.dispsize);
                en.visor.ref = new StackElement(clause, ref);
                ref.setEngine(en);
                en.contskel = clause.getNextRaw(en);
                en.contdisplay = ref;
                if (!en.runLoop(snap, true))
                    throw new EngineMessage(EngineMessage.syntaxError(
                            EngineMessage.OP_SYNTAX_EXPAND_FAILED));
            } catch (EngineMessage x) {
                en.contskel = r;
                en.contdisplay = u;
                en.fault = new EngineException(x, EngineException.fetchLoc(
                        EngineException.fetchStack(en), pos, en));
                en.releaseBind(mark);
                en.visor.ref = backref;
                throw en.fault;
            } catch (EngineException x) {
                en.contskel = r;
                en.contdisplay = u;
                en.fault = x;
                en.releaseBind(mark);
                en.visor.ref = backref;
                throw en.fault;
            }
            en.contskel = r;
            en.contdisplay = u;
            en.window = null;
            en.fault = null;
            en.cutChoices(snap);
            en.visor.ref = backref;
            t = var;
            d = ref.bind;
        }
        PreClause pre;
        try {
            if (en.fault != null)
                throw en.fault;
            pre = copyGoalVarsAndWrap(rd.getVars(), d, t, d, en);
        } catch (EngineMessage y) {
            en.fault = new EngineException(y, EngineException.fetchLoc(
                    EngineException.fetchStack(en), pos, en));
            en.releaseBind(mark);
            throw en.fault;
        } catch (EngineException x) {
            en.fault = x;
            en.releaseBind(mark);
            throw en.fault;
        }
        en.fault = null;
        en.releaseBind(mark);
        if (en.fault != null)
            throw en.fault;
        return pre;
    }

    /**
     * <p>Copy and wrap a goal from a term.</p>
     *
     * @param assoc The vars skeleton.
     * @param d2    The vars display.
     * @param t     The term skeleton.
     * @param d     The term display.
     * @param en    The engine.
     * @return The new clause.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static PreClause copyGoalVarsAndWrap(MapHashLink<String, SkelVar> assoc,
                                                 BindCount[] d2, Object t, BindCount[] d,
                                                 Engine en)
            throws EngineMessage, EngineException {
        PreClause pre = new PreClause();
        EngineCopy ec = en.enginecopy;
        if (ec == null) {
            ec = new EngineCopy();
            en.enginecopy = ec;
        }
        ec.vars = null;
        ec.flags = 0;
        if ((en.store.foyer.getBits() & Foyer.MASK_STORE_NBCV) != 0) {
            t = ec.copyRest(t, d);
        } else {
            t = ec.copyGoalAndWrap(t, d, en);
        }
        pre.molec = new SkelCompound(new SkelAtom(
                PreClause.OP_TURNSTILE), t);
        pre.vars = FileText.copyVars(assoc, d2, en, ec.vars);
        ec.vars = null;
        return pre;
    }

    /**********************************************************/
    /* Exception Handling                                     */
    /**********************************************************/

    /**
     * <p>Handle system exceptions in a top-level loop.</p>
     * <p>Will do the following:</p>
     * <ul>
     * <li><b>system_error(user_abort):</b> Print chain rest, do not break.</li>
     * <li><b>system_error(user_exit):</b> Print chain rest, do break.</li>
     * <li><b>system_error(memory_threshold):</b> Print exception, do not break.</li>
     * <li><b>system_error(import_deadlock):</b> Print exception, do not break.</li>
     * <li><b>system_error(_):</b> Re throw exception.</li>
     * <li><b>_:</b> Print exception, do not break.</li>
     * </ul>
     *
     * @param ex The exception.
     * @param en The engine.
     * @return True if break, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static boolean systemQueryBreak(EngineException ex,
                                            Engine en)
            throws EngineMessage, EngineException {
        EngineMessage m;
        Object t;
        boolean res;
        if ((m = ex.exceptionType(EngineException.OP_ERROR)) != null &&
                (t = m.messageType(EngineMessage.OP_SYSTEM_ERROR)) != null) {
            if (t instanceof SkelAtom &&
                    ((SkelAtom) t).fun.equals(EngineMessage.OP_SYSTEM_MEMORY_THRESHOLD)) {
                ex.printStackTrace(en);
                res = false;
            } else if (t instanceof SkelAtom &&
                    ((SkelAtom) t).fun.equals(EngineMessage.OP_SYSTEM_DEADLOCK_TIMEOUT)) {
                ex.printStackTrace(en);
                res = false;
            } else if (t instanceof SkelAtom &&
                    ((SkelAtom) t).fun.equals(EngineMessage.OP_SYSTEM_TIMELIMIT_EXCEEDED)) {
                ex.printStackTrace(en);
                res = false;
            } else {
                res = SpecialLoad.systemConsultBreak(ex, en, false);
            }
        } else {
            res = SpecialLoad.systemConsultBreak(ex, en, false);
        }
        return res;
    }

    /**
     * <p>Pass system exceptions, otherwise display only message.</p>
     *
     * @param ex The exception.
     * @param en The engine.
     * @throws EngineException Shit happens.
     */
    public static void systemSessionBreak(EngineException ex,
                                          Engine en)
            throws EngineMessage, EngineException {
        EngineMessage m;
        if ((m = ex.exceptionType(EngineException.OP_ERROR)) != null &&
                m.messageType(EngineMessage.OP_SYSTEM_ERROR) != null) {
            throw ex;
        } else {
            Object obj = en.visor.disperror;
            FactoryAPI.checkTextWrite(obj);
            Writer wr = (Writer) obj;
            try {
                wr.write(ex.getMessage(en));
                wr.write('\n');
                wr.flush();
            } catch (IOException x) {
                throw EngineMessage.mapIOException(x);
            }
        }
    }

    /**
     * <p>Parse the action.</p>
     *
     * @param action The action.
     * @param src    The scope.
     * @param en     The engine.
     * @return True if a action was parsed, otherwise false.
     * @throws EngineException Shit happens.
     */
    public static boolean parseAction(String action,
                                      AbstractSource src,
                                      Engine en)
            throws EngineMessage, EngineException {
        PrologReader rd = en.store.foyer.createReader(Foyer.IO_TERM);
        ConnectionReader cr = new ConnectionReader(new StringReader(action));
        cr.setLineNumber(1);
        rd.getScanner().setReader(cr);
        rd.setReadUtil(en.store);
        rd.setSource(src);
        rd.setEngineRaw(en);
        Object val;
        try {
            val = rd.parseHeadStatement();
        } catch (ScannerError y) {
            String line = ScannerError.linePosition(OpenOpts.getLine(cr), y.getPos());
            rd.parseTailError(PrologReader.OP_PERIOD, y);
            EngineMessage x = new EngineMessage(
                    EngineMessage.syntaxError(y.getError()));
            throw new EngineException(x,
                    EngineException.fetchPos(
                            EngineException.fetchStack(en), line, en));
        }
        if (val instanceof SkelAtom &&
                ((SkelAtom) val).fun.equals(AbstractSource.OP_END_OF_FILE))
            return false;
        en.skel = val;
        int size = rd.getGensym();
        en.display = (size != 0 ? BindCount.newBind(size) : BindCount.DISPLAY_CONST);
        return true;
    }

    /********************************************************************/
    /* Raw Variable Names                                               */
    /********************************************************************/

    /**
     * <p>Show a variable</p>
     *
     * @param fun The variable.
     * @param en  The engine.
     * @return The quoted variable.
     */
    private static SkelAtom sysQuoteVar(String fun, Engine en) {
        PrologWriter pw = new PrologWriter();
        pw.setWriteUtil(en.store);
        pw.setSource(en.store.user);
        pw.setEngineRaw(en);
        return new SkelAtom(pw.variableQuoted(fun));
    }

    /**
     * <p>Convert variable names.</p>
     * <p>Will not convert variables that have not yet been allocated.</p>
     * <p>Will not convert variables that have already been deallocated.</p>
     *
     * @param vars  The variable names.
     * @param store The store.
     * @return The Prolog association list.
     */
    private static Object namedToAssoc(Named[] vars, BindCount[] d,
                                       Store store) {
        Object end = store.foyer.ATOM_NIL;
        if (vars == null)
            return end;
        for (int i = vars.length - 1; i >= 0; i--) {
            Named cn = vars[i];
            SkelVar sv = cn.getEnt();
            if (d == null || sv.id >= d.length || d[sv.id] == null)
                continue;
            Object val = new SkelCompound(store.foyer.ATOM_EQUAL,
                    new SkelAtom(cn.getName()), sv);
            end = new SkelCompound(store.foyer.ATOM_CONS, val, end);
        }
        return end;
    }

}
