package jekdev.reference.debug;

import jekdev.model.builtin.SupervisorTrace;
import jekdev.model.pretty.StoreTrace;
import jekdev.reference.inspection.SpecialFrame;
import jekdev.reference.system.SpecialMode;
import jekpro.frequent.standard.SupervisorCall;
import jekpro.frequent.stream.ForeignConsole;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.*;
import jekpro.model.molec.*;
import jekpro.model.pretty.*;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Intermediate;
import jekpro.model.rope.LoadOpts;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.runtime.SpecialDynamic;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.reference.runtime.SpecialSession;
import jekpro.reference.structure.SpecialUniv;
import jekpro.reference.structure.SpecialVars;
import jekpro.tools.term.*;
import matula.util.data.ListArray;
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
 * <p>Provides a special predicate for default debugger.</p>
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
public final class SpecialDefault extends AbstractSpecial {
    public final static String OP_STEP_IN = "step_in";
    public final static String OP_STEP_OVER = "step_over";
    public final static String OP_STEP_OUT = "step_out";
    public final static String OP_INHERIT = "inherit";

    private final static int SPECIAL_SPY = 0;
    private final static int SPECIAL_NOSPY = 1;
    private final static int SPECIAL_SYS_SPYING = 2;
    private final static int SPECIAL_SYS_NOTRACE_FRAME = 3;
    private final static int SPECIAL_SYS_LEASHED_PORT = 4;
    private final static int SPECIAL_SYS_TRACE = 5;
    private final static int SPECIAL_SYS_TRACE_PROMPT = 6;

    public final static int MASK_MODE_DEBG = 0x00000F00;
    public final static int MASK_MODE_LESH = 0x00FF0000;
    public final static int MASK_MODE_VIBL = 0xFF000000;

    public final static int MASK_DEBG_NOFL = 0x00000010;

    public final static int MASK_DEBG_DBOF = 0x00000000;
    public final static int MASK_DEBG_STIN = 0x00000100;
    public final static int MASK_DEBG_STVR = 0x00000200;
    public final static int MASK_DEBG_STOT = 0x00000300;
    public final static int MASK_DEBG_DBON = 0x00000400;
    public final static int MASK_DEBG_INHR = 0x00000F00;

    public final static int MASK_PORT_CALL = 0x00000001;
    public final static int MASK_PORT_EXIT = 0x00000002;
    public final static int MASK_PORT_REDO = 0x00000004;
    public final static int MASK_PORT_FAIL = 0x00000008;
    public final static int MASK_PORT_HEAD = 0x00000010;
    public final static int MASK_PORT_CHOP = 0x00000020;
    public final static int MASK_PORT_MASK = 0x0000000F;

    /**
     * <p>Create a default debugger special.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialDefault(int i) {
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
        try {
            switch (id) {
                case SPECIAL_SPY:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    Predicate pick = SpecialPred.indicatorToProvable(temp[0], ref, en);
                    Predicate.checkExistentProvable(pick, temp[0], ref);
                    ((StoreTrace) en.store).addSpyPoint(pick.getArity(), pick.getFun());
                    return true;
                case SPECIAL_NOSPY:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    pick = SpecialPred.indicatorToProvable(temp[0], ref, en);
                    Predicate.checkExistentProvable(pick, temp[0], ref);
                    ((StoreTrace) en.store).removeSpyPoint(pick.getArity(), pick.getFun());
                    return true;
                case SPECIAL_SYS_SPYING:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (!en.unifyTerm(temp[0], ref, currentSpyPoints(en), Display.DISPLAY_CONST))
                        return false;
                    return true;
                case SPECIAL_SYS_NOTRACE_FRAME:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    StackElement frame = SpecialFrame.derefAndCastStackElement(temp[0], ref);
                    if (!sysNotraceFrame(frame, en))
                        return false;
                    return true;
                case SPECIAL_SYS_LEASHED_PORT:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    int port = SpecialMode.atomToPort(temp[0], ref);
                    if (!sysLeashedPort(port, en))
                        return false;
                    return true;
                case SPECIAL_SYS_TRACE:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    port = SpecialMode.atomToPort(temp[0], ref);
                    frame = SpecialFrame.derefAndCastStackElement(temp[1], ref);
                    SpecialFrame.checkNotNull(frame);
                    sysTrace(port, frame, en);
                    return true;
                case SPECIAL_SYS_TRACE_PROMPT:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    port = SpecialMode.atomToPort(temp[0], ref);
                    frame = SpecialFrame.derefAndCastStackElement(temp[1], ref);
                    SpecialFrame.checkNotNull(frame);
                    sysTracePrompt(port, frame, en);
                    return true;
                default:
                    throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
            }
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
        }
    }

    /**************************************************************/
    /* Spypoint Enumeration                                       */
    /**************************************************************/

    /**
     * <p>Create a prolog list with the spy points.</p>
     *
     * @param en The engine.
     * @return The prolog list of the spy points.
     * @throws EngineMessage Shit happens.
     */
    private static Object currentSpyPoints(Engine en)
            throws EngineMessage {
        Object res = en.store.foyer.ATOM_NIL;
        ListArray<StoreKey> spypoints = ((StoreTrace) en.store).snapshotSpyPoints();
        for (int i = 0; i < spypoints.size; i++) {
            StoreKey sk = spypoints.get(i);
            Object decl = SpecialQuali.indicatorToColonSkel(sk.getFun(), en.store.user,
                    sk.getArity(), en);
            res = new SkelCompound(en.store.foyer.ATOM_CONS, decl, res);
        }
        return res;
    }

    /**
     * <p>Check whether the frame is a notrace.</p>
     *
     * @param frame The frame.
     * @param en    The engine.
     * @return True if the frame is notrace, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    private static boolean sysNotraceFrame(StackElement frame, Engine en)
            throws EngineException, EngineMessage {
        StackElement.callGoal(frame.contskel, frame.contdisplay, en);
        CachePredicate cp = StackElement.callableToPredicate(en.skel, en);
        if (cp != null && (cp.flags & CachePredicate.MASK_PRED_VISI) != 0 &&
                (cp.pick.getBits() & Predicate.MASK_PRED_NOTR) != 0)
            return true;
        return false;
    }

    /**
     * <p>Check whether the port is leashed.</p>
     *
     * @param port The port.
     * @param en   The engine.
     * @return True if the port is leashed, otherwise false.
     */
    private static boolean sysLeashedPort(int port, Engine en) {
        int tflags = en.visor.flags & SpecialDefault.MASK_MODE_LESH;
        int flags = ((StoreTrace)en.store).flags & SpecialDefault.MASK_MODE_LESH;
        return SpecialMode.isPort((tflags != 0 ? tflags : flags) >> 16, port);
    }

    /**************************************************************/
    /* Perform Action                                             */
    /**************************************************************/

    /**
     * <p>Display the port.</p>
     *
     * @param port  The port id.
     * @param frame The frame.
     * @param en    The engine trace.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void sysTrace(int port, StackElement frame,
                                 Engine en)
            throws EngineMessage, EngineException {
        int depth = calcDepth(frame, en);
        traceGoal(port, frame, depth, en);
        dontAsk(en);
        doContinue(port, frame, en);
    }

    /**
     * <p>Display the port and prompt and prompt.</p>
     *
     * @param port  The port id.
     * @param frame The frame.
     * @param en    The engine trace.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void sysTracePrompt(int port, StackElement frame,
                                       Engine en)
            throws EngineMessage, EngineException {
        int depth = SpecialDefault.calcDepth(frame, en);
        int back = (en.visor.flags & SpecialSession.MASK_MODE_PRMT);
        en.visor.setThreadPrompt(SpecialSession.MASK_PRMT_PDBG);
        do {
            SpecialDefault.traceGoal(port, frame, depth, en);
            String action = SpecialDefault.askDebugAction(en);
            if (action == null) {
                throw new EngineMessage(EngineMessage.systemError(
                        EngineMessage.OP_SYSTEM_USER_EXIT));
            } else if ("?".equals(action)) {
                SpecialDefault.helpText(en);
            } else {
                try {
                    if (SpecialDefault.parseAction(action, en)) {
                        SpecialDefault.invokeChecked(en);
                    } else {
                        en.visor.setThreadPrompt(SpecialSession.MASK_PRMT_PROF);
                    }
                } catch (EngineException x) {
                    SpecialDefault.systemSessionBreak(x, en);
                }
            }
        } while ((en.visor.flags & SpecialSession.MASK_MODE_PRMT)
                == SpecialSession.MASK_PRMT_PDBG);
        en.visor.setThreadPrompt(back);
        SpecialDefault.doContinue(port, frame, en);
    }

    /**
     * <p>Continue debugging in previous mode.</p>
     *
     * @param port  The port.
     * @param en    The engine.
     * @param frame The frame.
     */
    private static void doContinue(int port, StackElement frame,
                                   Engine en)
            throws EngineMessage, EngineException {
        int tflags = en.visor.flags & MASK_MODE_DEBG;
        int flags = ((StoreTrace)en.store).flags & MASK_MODE_DEBG;
        switch (tflags != SpecialDefault.MASK_DEBG_INHR ? tflags : flags) {
            case MASK_DEBG_DBOF:
            case MASK_DEBG_STIN:
                break;
            case MASK_DEBG_STVR:
                if (port == SpecialMode.CODE_CALL || port == SpecialMode.CODE_REDO)
                    ((SupervisorTrace) en.visor).setSkipFrame(frame);
                break;
            case MASK_DEBG_STOT:
                StackElement dc = StackElement.skipNoTrace(frame.contdisplay, en);
                if (dc != null)
                    ((SupervisorTrace) en.visor).setSkipFrame(dc);
                break;
            case MASK_DEBG_DBON:
                break;
            default:
                throw new IllegalArgumentException("illegal mode");
        }
    }

    /****************************************************************/
    /* Display Current Goal                                         */
    /****************************************************************/

    /**
     * <p>Compute the stack depth.</p>
     * <p>Only user frames are counted.</p>
     *
     * @param frame The frame.
     * @param en    The engine.
     * @return The depth.
     */
    private static int calcDepth(StackElement frame,
                                 Engine en)
            throws EngineMessage, EngineException {
        int depth = 0;
        StackElement dc = StackElement.skipNoTrace(frame.contdisplay, en);
        while (dc != null) {
            depth++;
            dc = StackElement.skipNoTrace(dc.contdisplay, en);
        }
        return depth;
    }

    /**
     * <p>Trace the given goal.</p>
     *
     * @param port  The port id.
     * @param frame The frame.
     * @param depth The current depth.
     * @param en    The engine trace.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void traceGoal(int port, StackElement frame,
                                  int depth, Engine en)
            throws EngineMessage, EngineException {
        Object obj = en.visor.curoutput;
        LoadOpts.checkTextWrite(obj);
        Writer wr = (Writer) obj;
        try {
            int tflags = en.visor.flags & MASK_MODE_DEBG;
            int flags = ((StoreTrace)en.store).flags & MASK_MODE_DEBG;
            switch (tflags != SpecialDefault.MASK_DEBG_INHR ? tflags : flags) {
                case MASK_DEBG_DBOF:
                    wr.write("- ");
                    break;
                case MASK_DEBG_STIN:
                    wr.write("  ");
                    break;
                case MASK_DEBG_STVR:
                    wr.write("= ");
                    break;
                case MASK_DEBG_STOT:
                    wr.write("> ");
                    break;
                case MASK_DEBG_DBON:
                    wr.write("* ");
                    break;
                default:
                    throw new IllegalArgumentException("illegal mode");
            }
            wr.write(Integer.toString(depth));
            wr.write(" ");
            PrologWriter pw = en.store.foyer.createWriter(Foyer.IO_TERM);
            pw.setSource(en.visor.peekStack());
            pw.setEngineRaw(en);
            pw.setSpez(PrologWriter.SPEZ_META);
            pw.setWriter(wr);
            StackElement.callGoal(frame.contskel, frame.contdisplay, en);
            en.skel = SpecialDynamic.callableToColonSkel(en.skel, en);
            showGoal(pw, port, en.skel, en.display, en);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Show a port.</p>
     *
     * @param pw The prolog writer.
     * @param sa The port.
     * @param en The engine.
     */
    private static void showPort(PrologWriter pw, SkelAtom sa, Engine en)
            throws EngineMessage {
        try {
            Locale locale = en.store.foyer.locale;
            Properties error = EngineMessage.getErrorLang(locale, en.store);
            String str = error.getProperty("debug." + sa.fun);
            pw.getWriter().write((str != null ? str : sa.fun));
            pw.getWriter().write(" ");
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Show a goal.</p>
     * <p>Bindings are not undone, so that variables are consistently displayed.</p>
     *
     * @param pw   The prolog writer.
     * @param port The port.
     * @param t    The goal skel.
     * @param d    The goal display.
     * @param en   The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void showGoal(PrologWriter pw, int port,
                                 Object t, Display d,
                                 Engine en)
            throws EngineMessage, EngineException {
        if ((en.store.foyer.getBits() & Foyer.MASK_FOYER_CEXP) == 0) {
            /* write goal */
            showPort(pw, SpecialMode.portToAtom(port, en), en);
            Object val = AbstractTerm.getSkel(en.visor.printmap);
            Display ref = AbstractTerm.getDisplay(en.visor.printmap);
            MapHashLink<Object, String> printmap = SpecialVars.assocToMap(val, ref, en);
            pw.setPrintMap(printmap);
            pw.unparseStatement(t, d);
            return;
        }
        Display d2 = new Display(2);
        d2.bind[0].bindUniv(t, d, en);

        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;
        SkelVar var1 = SkelVar.valueOf(0);
        SkelVar var3 = SkelVar.valueOf(1);
        t = new SkelCompound(new SkelAtom("rebuild_goal"), var1, var3);
        t = new SkelCompound(new SkelAtom(SpecialQuali.OP_COLON, en.store.system),
                new SkelAtom("experiment/simp"), t);
        Directive dire = Directive.createDirective(AbstractDefined.MASK_DEFI_CALL |
                Directive.MASK_DIRE_LTGC, en);
        dire.bodyToInterSkel(t, en, true);

        int snap = en.number;
        try {
            CallFrame ref = CallFrame.getFrame(d2, dire, en);
            en.contskel = dire;
            en.contdisplay = ref;
            if (!en.runLoop2(snap, true))
                throw new EngineMessage(EngineMessage.syntaxError(
                        EngineMessage.OP_SYNTAX_REBUILD_FAILED));
        } catch (EngineException x) {
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            throw en.fault;
        } catch (EngineMessage y) {
            EngineException x = new EngineException(y,
                    EngineException.fetchStack(en));
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            throw en.fault;
        }
        en.contskel = r;
        en.contdisplay = u;
        en.fault = null;
        en.cutChoices(snap);
        if (en.fault != null)
            throw en.fault;

        /* write goal */
        showPort(pw, SpecialMode.portToAtom(port, en), en);
        Object val = AbstractTerm.getSkel(en.visor.printmap);
        Display ref = AbstractTerm.getDisplay(en.visor.printmap);
        MapHashLink<Object, String> printmap = SpecialVars.assocToMap(val, ref, en);
        pw.setPrintMap(printmap);
        pw.unparseStatement(SkelVar.valueOf(0), d2);
    }

    /******************************************************************/
    /* Ask End-User                                                   */
    /******************************************************************/


    /**
     * <p>Parse the action.</p>
     *
     * @param action The action.
     * @param en     The engine.
     * @return True if a action was parsed, otherwise false.
     * @throws EngineException Shit happens.
     */
    private static boolean parseAction(String action,
                                       Engine en)
            throws EngineMessage, EngineException {
        PrologReader rd = en.store.foyer.createReader(Foyer.IO_TERM);
        ConnectionReader cr = new ConnectionReader(new StringReader(action));
        cr.setLineNumber(1);
        rd.getScanner().setReader(cr);
        rd.setSource(en.visor.peekStack());
        rd.setEngineRaw(en);
        Object val;
        try {
            try {
                val = rd.parseHeadStatement();
            } catch (ScannerError y) {
                String line = ScannerError.linePosition(OpenOpts.getLine(cr), y.getErrorOffset());
                rd.parseTailError(y);
                EngineMessage x = new EngineMessage(
                        EngineMessage.syntaxError(y.getMessage()));
                throw new EngineException(x,
                        EngineException.fetchPos(
                                EngineException.fetchStack(en), line, en));
            }
        } catch (IOException y) {
            throw EngineMessage.mapIOException(y);
        }
        if (val instanceof SkelAtom &&
                ((SkelAtom) val).fun.equals(AbstractSource.OP_END_OF_FILE))
            return false;
        en.skel = val;
        en.display = AbstractSkel.createDisplay(val);
        return true;
    }

    /**
     * <p>Don't ask the end-user, only new line flush the output.</p>
     *
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void dontAsk(Engine en) throws EngineMessage {
        Object obj = en.visor.curoutput;
        LoadOpts.checkTextWrite(obj);
        Writer wr = (Writer) obj;
        SpecialLoad.newLineFlush(wr);
    }

    /**
     * <p>Display the help text.</p>
     *
     * @param en The engine trace.
     * @throws EngineMessage Shit happens.
     */
    private static void helpText(Engine en) throws EngineMessage {
        Object obj = en.visor.curoutput;
        LoadOpts.checkTextWrite(obj);
        Writer wr = (Writer) obj;
        try {
            Locale locale = en.store.foyer.locale;
            Properties resources = LangProperties.getLang(
                    SpecialDefault.class, "debug", locale);
            wr.write(resources.getProperty("debug.continue"));
            wr.write('\n');
            wr.write('\n');
            wr.flush();
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Ask the end-user for the debugger action.</p>
     *
     * @param en The engine.
     * @return The debugger action.
     * @throws EngineMessage Shit happens.
     */
    private static String askDebugAction(Engine en)
            throws EngineMessage {
        Object obj = en.visor.curoutput;
        LoadOpts.checkTextWrite(obj);
        Writer wr = (Writer) obj;

        obj = en.visor.curinput;
        PrologReader.checkTextRead(obj);
        Reader lr = (Reader) obj;
        try {
            wr.write(" ? ");
            wr.flush();
            return ForeignConsole.readLine(lr);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /******************************************************************/
    /* Error Display                                                  */
    /******************************************************************/

    /**
     * <p>Pass system exceptions, otherwise display only message.</p>
     *
     * @param ex The exception.
     * @param en The engine.
     * @throws EngineException Shit happens.
     */
    private static void systemSessionBreak(EngineException ex,
                                           Engine en)
            throws EngineMessage, EngineException {
        EngineMessage m;
        if ((m = ex.exceptionType(EngineException.OP_ERROR)) != null &&
                m.messageType(EngineMessage.OP_SYSTEM_ERROR) != null) {
            throw ex;
        } else {
            Object obj = en.visor.curerror;
            LoadOpts.checkTextWrite(obj);
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

    /*******************************************************************/
    /* Mode Conversion                                                 */
    /*******************************************************************/

    /**
     * <p>Convert a debug mode to an atom.</p>
     *
     * @param m The mode.
     * @return The atom.
     */
    public static Object modeToAtom(int m) {
        switch (m) {
            case MASK_DEBG_INHR:
                return new SkelAtom(OP_INHERIT);
            case MASK_DEBG_DBOF:
                return new SkelAtom(AbstractFlag.OP_OFF);
            case MASK_DEBG_STIN:
                return new SkelAtom(OP_STEP_IN);
            case MASK_DEBG_STVR:
                return new SkelAtom(OP_STEP_OVER);
            case MASK_DEBG_STOT:
                return new SkelAtom(OP_STEP_OUT);
            case MASK_DEBG_DBON:
                return new SkelAtom(AbstractFlag.OP_ON);
            default:
                throw new IllegalArgumentException("illegal mode");
        }
    }

    /**
     * <p>Convert an atom to a debug mode.</p>
     *
     * @param t The atom skeleton.
     * @param d The atom display.
     * @return The debug mode.
     */
    public static int atomToMode(Object t, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(t, d);
        if (fun.equals(OP_INHERIT)) {
            return MASK_DEBG_INHR;
        } else if (fun.equals(AbstractFlag.OP_OFF)) {
            return MASK_DEBG_DBOF;
        } else if (fun.equals(OP_STEP_IN)) {
            return MASK_DEBG_STIN;
        } else if (fun.equals(OP_STEP_OVER)) {
            return MASK_DEBG_STVR;
        } else if (fun.equals(OP_STEP_OUT)) {
            return MASK_DEBG_STOT;
        } else if (fun.equals(AbstractFlag.OP_ON)) {
            return MASK_DEBG_DBON;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    "debug_mode", t), d);
        }
    }

    /*******************************************************************/
    /* Port List Conversion                                            */
    /*******************************************************************/

    /**
     * <p>Convert leash bits to leash atom list.</p>
     *
     * @param en    The engine.
     * @param flags The bits.
     * @return The atom list.
     */
    public static Object portsToList(Engine en, int flags) {
        Foyer foyer = en.store.foyer;
        Object res = foyer.ATOM_NIL;
        if ((flags & MASK_PORT_FAIL) == 0)
            res = new SkelCompound(foyer.ATOM_CONS, new SkelAtom(SpecialMode.OP_FAIL), res);
        if ((flags & MASK_PORT_REDO) == 0)
            res = new SkelCompound(foyer.ATOM_CONS, new SkelAtom(SpecialMode.OP_REDO), res);
        if ((flags & MASK_PORT_EXIT) == 0)
            res = new SkelCompound(foyer.ATOM_CONS, new SkelAtom(SpecialMode.OP_EXIT), res);
        if ((flags & MASK_PORT_CALL) == 0)
            res = new SkelCompound(foyer.ATOM_CONS, new SkelAtom(SpecialMode.OP_CALL), res);
        if ((flags & MASK_PORT_HEAD) != 0)
            res = new SkelCompound(foyer.ATOM_CONS, new SkelAtom(SpecialMode.OP_HEAD), res);
        if ((flags & MASK_PORT_CHOP) != 0)
            res = new SkelCompound(foyer.ATOM_CONS, new SkelAtom(SpecialMode.OP_CHOP), res);
        return res;
    }

    /**
     * <p>Convert leash atom list to leash bits.</p>
     *
     * @param t The skeleton.
     * @param d The display.
     * @return The bits.
     * @throws EngineMessage Shit happens.
     */
    public static int listToPorts(Object t, Display d) throws EngineMessage {
        int flags = MASK_PORT_MASK;
        BindUniv b;
        while (t instanceof SkelVar &&
                (b = d.bind[((SkelVar) t).id]).display != null) {
            t = b.skel;
            d = b.display;
        }
        while ((t instanceof SkelCompound) &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_CONS) &&
                ((SkelCompound) t).args.length == 2) {
            SkelCompound sc = (SkelCompound) t;
            Display ref = d;
            String fun = SpecialUniv.derefAndCastString(sc.args[0], d);
            if (fun.equals(SpecialMode.OP_CALL)) {
                flags &= ~MASK_PORT_CALL;
            } else if (fun.equals(SpecialMode.OP_EXIT)) {
                flags &= ~MASK_PORT_EXIT;
            } else if (fun.equals(SpecialMode.OP_REDO)) {
                flags &= ~MASK_PORT_REDO;
            } else if (fun.equals(SpecialMode.OP_FAIL)) {
                flags &= ~MASK_PORT_FAIL;
            } else if (fun.equals(SpecialMode.OP_HEAD)) {
                flags |= MASK_PORT_HEAD;
            } else if (fun.equals(SpecialMode.OP_CHOP)) {
                flags |= MASK_PORT_CHOP;
            } else {
                throw new EngineMessage(EngineMessage.domainError("leash", t));
            }
            t = sc.args[1];
            d = ref;
            while (t instanceof SkelVar &&
                    (b = d.bind[((SkelVar) t).id]).display != null) {
                t = b.skel;
                d = b.display;
            }
        }
        if (t instanceof SkelAtom &&
                ((SkelAtom) t).fun.equals(Foyer.OP_NIL)) {
            /* do nothing */
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_LIST, t), d);
        }
        return flags;
    }

    /****************************************************************************/
    /* Execution Helpers                                                        */
    /****************************************************************************/

    /**
     * <p>Search the given term once and close it.</p>
     * <p>Throw a warning when it fails.</p>
     * <p>The term is passed via skel and display.</p>
     *
     * @throws EngineException Shit happens.
     */
    private static void invokeChecked(Engine en)
            throws EngineException {
        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;
        boolean backignore = en.visor.setIgnore(false);
        boolean backverify = en.visor.setVerify(false);
        AbstractUndo mark = en.bind;
        int snap = en.number;
        try {
            Directive dire = SupervisorCall.callGoal(AbstractDefined.MASK_DEFI_CALL, en);
            Display d2 = en.display;

            CallFrame ref2 = CallFrame.getFrame(d2, dire, en);
            en.contskel = dire;
            en.contdisplay = ref2;
            if (!en.runLoop2(snap, true))
                throw new EngineMessage(EngineMessage.syntaxError(
                        EngineMessage.OP_SYNTAX_DIRECTIVE_FAILED));
        } catch (EngineException x) {
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            en. releaseBind(mark);
            en.visor.setVerify(backverify);
            en.visor.setIgnore(backignore);
            throw en.fault;
        } catch (EngineMessage y) {
            EngineException x = new EngineException(y,
                    EngineException.fetchStack(en));
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            en.releaseBind(mark);
            en.visor.setVerify(backverify);
            en.visor.setIgnore(backignore);
            throw en.fault;
        }
        en.contskel = r;
        en.contdisplay = u;
        en.fault = null;
        en.cutChoices(snap);
        en.releaseBind(mark);
        en.visor.setVerify(backverify);
        en.visor.setIgnore(backignore);
        if (en.fault != null)
            throw en.fault;
    }

}
