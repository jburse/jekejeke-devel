package jekpro.reference.runtime;

import jekpro.frequent.stream.ForeignConsole;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.Branch;
import jekpro.model.inter.*;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.PrologReader;
import jekpro.model.pretty.PrologWriter;
import jekpro.model.rope.*;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.tools.proxy.FactoryAPI;
import jekpro.tools.term.*;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SpecialSession extends AbstractSpecial {
    private final static int SPECIAL_VERSION = 0;
    private final static int SPECIAL_BREAK = 1;
    private final static int SPECIAL_SYS_WRITE_VAR = 2;

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
            case SPECIAL_VERSION:
                Object obj = en.visor.dispoutput;
                FactoryAPI.checkTextWrite(obj);
                Writer wr = (Writer) obj;
                showVersion(wr, en);
                return en.getNextRaw();
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
            case SPECIAL_SYS_WRITE_VAR:
                obj = en.visor.dispoutput;
                FactoryAPI.checkTextWrite(obj);
                wr = (Writer) obj;
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                String fun = EngineMessage.castString(en.skel, en.display);
                showVariable(wr, fun, en);
                return en.getNextRaw();
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Show a variable</p>
     *
     * @param wr  The writer.
     * @param fun The variable.
     * @param en  The engine.
     */
    private static void showVariable(Writer wr, String fun,
                                     Engine en)
            throws EngineMessage {
        try {
            PrologWriter pw = new PrologWriter();
            pw.setWriteUtil(en.store);
            pw.setSource(en.store.user);
            pw.setEngineRaw(en);
            wr.write(pw.variableQuoted(fun));
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Show a version banner.</p></�p>
     *
     * @param wr The writer.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void showVersion(Writer wr, Engine en)
            throws EngineMessage {
        try {
            AbstractBranch brand = en.store.foyer.getFactory().getBrandBranch();
            Locale locale = en.store.foyer.locale;
            Properties descr = brand.getDescriptionLang(locale);
            String family = descr.getProperty("family");
            String product = descr.getProperty("product") + " " + descr.getProperty("release");
            String company = descr.getProperty("company");
            wr.write(family + ", " + product);
            wr.write('\n');
            wr.flush();
            wr.write(company);
            wr.write('\n');
            wr.flush();
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
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
                Object res = Clause.moduleToSlashSkel(s, en.store.user, en);
                wr.write(PrologWriter.toString(res, Display.DISPLAY_CONST, 0, en));
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
                if ((en.store.foyer.getBits() & Foyer.MASK_STORE_CEXP) == 0 &&
                        (en.store.foyer.getBits() & Foyer.MASK_STORE_NBCV) != 0) {
                    rd.setFlags(PrologWriter.FLAG_SING);
                } else {
                    rd.setFlags(0);
                }
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
                Clause clause = en.store.foyer.createClause(AbstractDefined.MASK_DEFI_NBDY |
                        AbstractDefined.MASK_DEFI_NLST |
                        AbstractDefined.MASK_DEFI_STOP);
                clause.vars = pre.vars;
                clause.anon = pre.anon;
                clause.analyzeBody(pre.molec, en);

                Intermediate r = en.contskel;
                DisplayClause u = en.contdisplay;
                AbstractBind mark = en.bind;
                int snap = en.number;
                Frame backref = en.visor.ref;
                try {
                    DisplayClause ref = new DisplayClause(clause.dispsize);
                    en.visor.ref = new Frame(clause, ref);
                    ref.setEngine(en);
                    en.contskel = clause.getNextRaw(en);
                    en.contdisplay = ref;
                    boolean found = en.runFirst(snap);
                    if (!found)
                        failFeedback(en);
                    while (found) {
                        en.skel = new SkelAtom("sys_show_vars");
                        en.display = Display.DISPLAY_CONST;
                        en.contskel = r;
                        en.contdisplay = u;
                        en.unfoldCheckedIgnore();
                        if (en.number != snap) {
                            String action = askSessionAction(en);
                            if (action == null) {
                                throw new EngineMessage(EngineMessage.systemError(
                                        EngineMessage.OP_SYSTEM_USER_EXIT));
                            } else if (";".equals(action)) {
                                found = en.runNext(snap);
                                if (!found)
                                    failFeedback(en);
                            } else if ("?".equals(action)) {
                                helpText(pos, en);
                            } else {
                                try {
                                    if (parseAction(action, src, en)) {
                                        en.contskel = r;
                                        en.contdisplay = u;
                                        en.unfoldCheckedIgnore();
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
                    en.skel = new EngineException(x,
                            EngineException.fetchLoc(
                                    EngineException.fetchStack(en), pos, en));
                    en.releaseBind(mark);
                    en.visor.ref = backref;
                    throw (EngineException) en.skel;
                } catch (EngineException x) {
                    en.contskel = r;
                    en.contdisplay = u;
                    en.skel = x;
                    en.releaseBind(mark);
                    en.visor.ref = backref;
                    throw (EngineException) en.skel;
                }
                en.contskel = r;
                en.contdisplay = u;
                en.display = null;
                en.skel = null;
                en.cutChoices(snap);
                en.releaseBind(mark);
                en.visor.ref = backref;
                if (en.skel != null)
                    throw (EngineException) en.skel;
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
     * @throws EngineMessage   Shit happens.
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
            pre.anon = Named.makeNamed(rd.getAnon());
            return pre;
        }

        /* expand goal */
        AbstractBind mark = en.bind;
        Display d;
        if ((en.store.foyer.getBits() & Foyer.MASK_STORE_CEXP) == 0) {
            d = new Display(rd.getGensym());
            en.skel = null;
        } else {
            Intermediate r = en.contskel;
            DisplayClause u = en.contdisplay;
            SkelVar var = rd.atomToVariable(PrologReader.OP_ANON);
            SkelAtom sa = new SkelAtom("expand_goal",
                    en.store.foyer.SOURCE_SYSTEM);
            Clause clause = en.store.foyer.createClause(AbstractDefined.MASK_DEFI_NBDY |
                    AbstractDefined.MASK_DEFI_NLST |
                    AbstractDefined.MASK_DEFI_STOP);
            clause.vars = Named.makeNamed(rd.getVars());
            Object molec = new SkelCompound(new SkelAtom(
                    PreClause.OP_TURNSTILE), new SkelCompound(sa, t, var));
            clause.analyzeBody(molec, en);

            int snap = en.number;
            Frame backref = en.visor.ref;
            DisplayClause ref;
            try {
                ref = new DisplayClause(clause.dispsize);
                en.visor.ref = new Frame(clause, ref);
                ref.setEngine(en);
                en.contskel = clause.getNextRaw(en);
                en.contdisplay = ref;
                if (!en.runFirst(snap))
                    throw new EngineMessage(EngineMessage.syntaxError(
                            EngineMessage.OP_SYNTAX_EXPAND_FAILED));
            } catch (EngineMessage x) {
                en.contskel = r;
                en.contdisplay = u;
                en.skel = new EngineException(x, EngineException.fetchLoc(
                        EngineException.fetchStack(en), pos, en));
                en.releaseBind(mark);
                en.visor.ref = backref;
                throw (EngineException) en.skel;
            } catch (EngineException x) {
                en.contskel = r;
                en.contdisplay = u;
                en.skel = x;
                en.releaseBind(mark);
                en.visor.ref = backref;
                throw (EngineException) en.skel;
            }
            en.contskel = r;
            en.contdisplay = u;
            en.skel = null;
            en.display = null;
            en.cutChoices(snap);
            en.visor.ref = backref;
            t = var;
            d = ref;
        }
        PreClause pre;
        try {
            if (en.skel != null)
                throw (EngineException) en.skel;
            pre = copyGoalVarsAndWrap(rd.getVars(), d, t, d, en);
        } catch (EngineMessage y) {
            en.skel = new EngineException(y, EngineException.fetchLoc(
                    EngineException.fetchStack(en), pos, en));
            en.releaseBind(mark);
            throw (EngineException) en.skel;
        } catch (EngineException x) {
            en.skel = x;
            en.releaseBind(mark);
            throw (EngineException) en.skel;
        }
        en.skel = null;
        en.releaseBind(mark);
        if (en.skel != null)
            throw (EngineException) en.skel;
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
                                                 Display d2, Object t, Display d,
                                                 Engine en)
            throws EngineMessage, EngineException {
        PreClause pre = new PreClause();
        EngineAnon an = new EngineAnon();
        if ((en.store.foyer.getBits() & Foyer.MASK_STORE_NBCV) != 0) {
            t = an.copyTerm(t, d);
        } else {
            t = an.copyGoalAndWrap(t, d, en);
        }
        pre.molec = new SkelCompound(new SkelAtom(
                PreClause.OP_TURNSTILE), t);
        pre.vars = copyVars(assoc, d2, en, an.vars);
        pre.anon = copyVars(assoc, d2, en, an.anon);
        return pre;
    }

    /**
     * <p>Make a copy of the given variable names.</p>
     * <p>Only copy terms that are bound to a variable.</p>
     * <p>Only copy variables that already exist in rule.</p>
     *
     * @param vars The variable names molecs, can be null.
     * @param d    The variable names display.
     * @param en   The engine.
     * @param map  The variable map.
     * @return The named copy.
     */
    public static Named[] copyVars(MapHashLink<String, SkelVar> vars, Display d,
                                   Engine en, MapHash<TermVar, SkelVar> map) {
        if (vars == null)
            return null;
        ListArray<Named> copy = null;
        for (MapEntry<String, SkelVar> entry = vars.getLastEntry();
             entry != null; entry = vars.predecessor(entry)) {
            en.skel = entry.value;
            en.display = d;
            en.deref();
            if (!(en.skel instanceof SkelVar))
                continue;
            SkelVar sv = (SkelVar) en.skel;
            TermVar key = new TermVar(sv, en.display);
            sv = (map != null ? map.get(key) : null);
            if (sv == null)
                continue;
            if (copy == null)
                copy = new ListArray<Named>();
            copy.add(new Named(entry.key, sv));
        }
        if (copy == null)
            return null;
        Named[] res = new Named[copy.size()];
        int k = 0;
        for (int i = copy.size() - 1; i >= 0; i--) {
            res[k] = copy.get(i);
            k++;
        }
        return res;
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
        en.display = (rd.getGensym() != 0 ?
                new Display(rd.getGensym()) : Display.DISPLAY_CONST);
        return true;
    }

}
