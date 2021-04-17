package jekpro.reference.bootload;

import matula.comp.sharik.LicenseError;
import jekpro.frequent.standard.SupervisorCopy;
import jekpro.frequent.system.ForeignLocale;
import jekpro.model.builtin.*;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.pretty.*;
import jekpro.model.rope.*;
import jekpro.reference.reflect.PropertyCallable;
import jekpro.reference.reflect.PropertySource;
import jekpro.reference.runtime.EvaluableLogic;
import jekpro.reference.structure.EngineVars;
import jekpro.reference.structure.SpecialUniv;
import jekpro.reference.structure.SpecialVars;
import jekpro.tools.term.*;
import matula.comp.sharik.AbstractTracking;
import matula.util.config.AbstractBundle;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;
import matula.util.data.MapHashLink;
import matula.util.regex.ScannerError;
import matula.util.system.OpenOpts;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

/**
 * <p>Provides built-in predicates for the load theory.</p>
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
public final class SpecialLoad extends AbstractSpecial {
    private final static int SPECIAL_SYS_LOAD_FILE = 0;
    private final static int SPECIAL_SYS_DETACH_FILE = 1;
    private final static int SPECIAL_SYS_IMPORT_FILE = 2;
    private final static int SPECIAL_SYS_REGISTER_FILE = 3;
    private final static int SPECIAL_SYS_SHOW_IMPORT = 4;
    private final static int SPECIAL_SYS_SHOW_BASE = 5;
    public final static int SPECIAL_SYS_BOOT_STREAM = 6;

    public final static String OP_MODULE = "module";
    public final static String OP_SET_PROLOG_FLAG = "set_prolog_flag";

    /**
     * <p>Create a load special.</p>
     *
     * @param i The id.
     */
    public SpecialLoad(int i) {
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
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case SPECIAL_SYS_LOAD_FILE:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                LoadOpts opts = new LoadOpts();
                opts.decodeLoadOpts(temp[1], ref, en);
                SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                AbstractSource source = (sa.scope != null ? sa.scope : en.store.user);
                opts.makeLoad(source, sa.fun, en);
                return true;
            case SPECIAL_SYS_DETACH_FILE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                opts = new LoadOpts();
                opts.decodeLoadOpts(temp[1], ref, en);
                sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                opts.makeUnload(source, sa.fun, en);
                return true;
            case SPECIAL_SYS_IMPORT_FILE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                opts = new LoadOpts();
                opts.decodeLoadOpts(temp[1], ref, en);
                sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                SpecialLoad.performImport(source, sa.fun, en, opts);
                return true;
            case SPECIAL_SYS_REGISTER_FILE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                registerFile(sa.scope, sa.fun, sa.getPosition(), en.store);
                return true;
            case SPECIAL_SYS_SHOW_IMPORT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;

                sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                if (source == null)
                    return false;

                SpecialLoad.listImport(source, en);
                return true;
            case SPECIAL_SYS_SHOW_BASE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;

                sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                if (source == null)
                    return false;

                showShortName(source, en);
                return true;
            case SPECIAL_SYS_BOOT_STREAM:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                Object obj = SpecialUniv.derefAndCastRef(temp[0], ref);
                PrologReader.checkTextRead(obj);
                SpecialLoad.bootStream((Reader) obj, en);
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>List the source.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param src The source.
     * @param en  The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void listImport(AbstractSource src, Engine en)
            throws EngineMessage, EngineException {
        Object obj = en.visor.curoutput;
        LoadOpts.checkTextWrite(obj);
        Writer wr = (Writer) obj;
        PrologWriter pw = en.store.foyer.createWriter(Foyer.IO_TERM);
        pw.setDefaults(en.visor.peekStack());
        pw.setEngine(en);
        pw.setFlags(pw.getFlags() | (PrologWriter.FLAG_QUOT | PrologWriter.FLAG_NEWL | PrologWriter.FLAG_MKDT));
        pw.setSpez(PrologWriter.SPEZ_META);
        pw.setOffset(-1);
        pw.setWriter(wr);

        /* show source comment */
        if (src != null &&
                (src.getBits() & AbstractSource.MASK_SRC_VISI) == 0 &&
                Branch.OP_USER.equals(src.getFullName())) {
            Object decl = new SkelCompound(new SkelAtom(OP_MODULE),
                    new SkelAtom(Branch.OP_USER),
                    new SkelAtom(Foyer.OP_NIL));
            decl = new SkelCompound(new SkelAtom(Foyer.OP_TURNSTILE), decl);
            decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
            pw.unparseStatement(decl, Display.DISPLAY_CONST);
            SpecialLoad.flushWriter(pw.getWriter());
        }

        if (src == null)
            return;

        /* flesh out properties */
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            MapHash<StoreKey, AbstractProperty<AbstractSource>> props = branch.getSrcProps();
            for (MapEntry<StoreKey, AbstractProperty<AbstractSource>> entry2 =
                 (props != null ? props.getLastEntry() : null);
                 entry2 != null; entry2 = props.predecessor(entry2)) {
                AbstractProperty<AbstractSource> prop = entry2.value;
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_SHOW) == 0)
                    continue;
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_DEFL) != 0 &&
                        Branch.OP_USER.equals(src.getFullName()))
                    continue;
                StoreKey sk = entry2.key;
                Object[] vals = prop.getObjProps(src, en);
                for (int j = 0; j < vals.length; j++) {
                    Object val = vals[j];
                    Object decl = srcDecl(sk, AbstractTerm.getSkel(val), src, en);
                    decl = new SkelCompound(new SkelAtom(Foyer.OP_TURNSTILE), decl);
                    decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
                    pw.unparseStatement(decl, AbstractTerm.getDisplay(val));
                    SpecialLoad.flushWriter(pw.getWriter());
                }
            }
        }

        if (src.utildouble != ReadOpts.UTIL_CODES) {
            Object val = ReadOpts.utilToAtom(src.utildouble);
            Object decl = new SkelCompound(new SkelAtom(OP_SET_PROLOG_FLAG),
                    new SkelAtom(Flag.OP_DOUBLE_QUOTES), val);
            decl = new SkelCompound(new SkelAtom(Foyer.OP_TURNSTILE), decl);
            decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
            pw.unparseStatement(decl, AbstractTerm.getDisplay(val));
            SpecialLoad.flushWriter(pw.getWriter());
        }
        if (src.utilback != ReadOpts.UTIL_ERROR) {
            Object val = ReadOpts.utilToAtom(src.utilback);
            Object decl = new SkelCompound(new SkelAtom(OP_SET_PROLOG_FLAG),
                    new SkelAtom(Flag.OP_BACK_QUOTES), val);
            decl = new SkelCompound(new SkelAtom(Foyer.OP_TURNSTILE), decl);
            decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
            pw.unparseStatement(decl, AbstractTerm.getDisplay(val));
            SpecialLoad.flushWriter(pw.getWriter());
        }
        if (src.utilsingle != ReadOpts.UTIL_ATOM) {
            Object val = ReadOpts.utilToAtom(src.utilsingle);
            Object decl = new SkelCompound(new SkelAtom(OP_SET_PROLOG_FLAG),
                    new SkelAtom(Flag.OP_SINGLE_QUOTES), val);
            decl = new SkelCompound(new SkelAtom(Foyer.OP_TURNSTILE), decl);
            decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
            pw.unparseStatement(decl, AbstractTerm.getDisplay(val));
            SpecialLoad.flushWriter(pw.getWriter());
        }
        if ((src.getBits() & AbstractSource.MASK_SRC_NSTY) != 0) {
            Object val = AbstractFlag.switchToAtom(false);
            Object decl = new SkelCompound(new SkelAtom(OP_SET_PROLOG_FLAG),
                    new SkelAtom(Flag.OP_STYLE_CHECK), val);
            decl = new SkelCompound(new SkelAtom(Foyer.OP_TURNSTILE), decl);
            decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
            pw.unparseStatement(decl, AbstractTerm.getDisplay(val));
            SpecialLoad.flushWriter(pw.getWriter());
        }
    }

    /**
     * <p>Show the short name of the source key.</p>
     *
     * @param src The source.
     */
    public static void showShortName(AbstractSource src, Engine en)
            throws EngineMessage {
        Object obj = en.visor.curoutput;
        LoadOpts.checkTextWrite(obj);
        Writer wr = (Writer) obj;

        try {
            wr.write("% ");
            wr.write(ForeignLocale.shortName(src.getPath()));
            wr.write('\n');
            wr.flush();
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Determine the declaration term.</p>
     *
     * @param sk     The property name.
     * @param skel   The property value.
     * @param source The source.
     * @param en     The engine.
     * @return The declaration term.
     */
    private static Object srcDecl(StoreKey sk, Object skel,
                                  AbstractSource source, Engine en)
            throws EngineMessage {
        if (sk.getFun().equals(PropertySource.OP_SYS_LINK) && sk.getArity() == 2)
            return PropertySource.shortLink(skel, source, en);
        if (sk.getFun().equals(PropertySource.OP_SYS_SOURCE_NAME) && sk.getArity() == 1)
            return PropertySource.shortModule(skel, en);
        return skel;
    }

    /**
     * <p>Flush the writer.</p>
     *
     * @param wr The writer.
     * @throws EngineMessage Shit happens.
     */
    public static void flushWriter(Writer wr)
            throws EngineMessage {
        try {
            wr.flush();
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /************************************************************/
    /* Perform Import                                           */
    /************************************************************/

    /**
     * <p>Perform an import.</p>
     *
     * @param scope The scope.
     * @param key   The source key.
     * @param en    The engine.
     * @param opts  The consult options.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void performImport(AbstractSource scope, String key,
                                      Engine en, LoadOpts opts)
            throws EngineMessage, EngineException {
        AbstractSource source = scope.getStore().getSourceDefined(key, false);
        Reader reader;
        if (!Branch.OP_USER.equals(source.getPath())) {
            reader = source.openReader(false, opts);
            scope.loadModule(reader, en);
            scope.closeReader(reader);
        } else {
            Object obj = en.visor.curinput;
            PrologReader.checkTextRead(obj);
            reader = (Reader) obj;
            scope.loadModule(reader, en);
        }
    }

    /****************************************************************/
    /* Resource Handling                                            */
    /****************************************************************/

    /**
     * <p>Register a file.</p>
     *
     * @param scope The call-site.
     * @param key   The source key.
     * @param pos   The position.
     * @param store The store.
     */
    private static void registerFile(AbstractSource scope, String key,
                                     PositionKey pos, Store store) {
        AbstractSource src = (scope != null ? scope : store.user);
        Resource rsc = store.foyer.createResource(key);
        rsc.setPosition(pos);
        src.addResource(rsc);
    }

    /**
     * <p>Handle system exceptions in a consult loop.</p>
     * <p>Will do the following:</p>
     * <ul>
     * <li><b>system_error(user_abort):</b> Print chain rest.</li>
     * <li><b>system_error(_):</b> Re-throw exception.</li>
     * <li><b>limit_error(_):</b> Re-throw exception.</li>
     * <li><b>_:</b> Print exception.</li>
     * </ul>
     *
     * @param ex The exception.
     * @param en The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static void systemConsultBreak(EngineException ex,
                                          Engine en)
            throws EngineMessage, EngineException {
        EngineMessage m;
        if ((m = ex.exceptionType(EngineException.OP_ERROR)) != null) {
            Object t;
            if ((t = m.messageType(EngineMessage.OP_SYSTEM_ERROR)) != null) {
                if (t instanceof SkelAtom &&
                        ((SkelAtom) t).fun.equals(EngineMessage.OP_SYSTEM_USER_ABORT)) {
                    EngineException rest = ex.causeChainRest();
                    if (rest != null)
                        rest.printStackTrace(en);
                } else {
                    throw ex;
                }
            } else if (m.messageType(EngineMessage.OP_LIMIT_ERROR) != null) {
                throw ex;
            } else {
                ex.printStackTrace(en);
            }
        } else {
            ex.printStackTrace(en);
        }
    }

    /*******************************************************************/
    /* Property Utilities                                              */
    /*******************************************************************/

    /**
     * <p>Check whether the two value lists contain the same elements.</p>
     *
     * @param vals  The first value list.
     * @param vals2 The second value list.
     * @return True if both value lists contain the same elements, otherwise false.
     */
    public static boolean sameValues(Object[] vals, Object[] vals2) {
        if (vals.length != vals2.length)
            return false;
        for (int i = 0; i < vals.length; i++) {
            Object val = firstArg(vals[i]);
            Object val2 = firstArg(vals2[i]);
            if (!val.equals(val2))
                return false;
        }
        return true;
    }

    /**
     * <p>Return the first arg of the value.</p>
     *
     * @param val The value.
     * @return The first arg.
     */
    public static Object firstArg(Object val) {
        SkelCompound sc = (SkelCompound) AbstractTerm.getSkel(val);
        return AbstractTerm.createMolec(sc.args[0], Display.DISPLAY_CONST);
    }

    /********************************************************************/
    /* Boot Stream                                                      */
    /********************************************************************/

    /**
     * <p>Consult a stream natively.</p>
     * <p>Term expansion is not provided.</p>
     *
     * @param lr  The buffered reader.
     * @param en  The interpreter.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void bootStream(Reader lr, Engine en)
            throws EngineException, EngineMessage {
        PrologReader rd = en.store.foyer.createReader(Foyer.IO_TERM);
        rd.setEngineRaw(en);
        for (; ; ) {
            try {
                Object val;
                rd.setDefaults(en.visor.peekStack(),
                        PrologReader.FLAG_SING | PrologReader.FLAG_NEWV);
                try {
                    try {
                        rd.getScanner().setReader(lr);
                        val = rd.parseHeadStatement();
                    } catch (ScannerError y) {
                        String line = ScannerError.linePosition(OpenOpts.getLine(lr), y.getErrorOffset());
                        rd.parseTailError(y);
                        EngineMessage x = new EngineMessage(EngineMessage.syntaxError(y.getMessage()));
                        throw new EngineException(x, EngineException.fetchPos(
                                EngineException.fetchStack(en), line, en)
                        );
                    }
                } catch (IOException y) {
                    throw EngineMessage.mapIOProblem(y);
                }
                if (val instanceof SkelAtom &&
                        ((SkelAtom) val).fun.equals(AbstractSource.OP_END_OF_FILE))
                    break;
                if (val instanceof SkelCompound &&
                        ((SkelCompound) val).args.length == 1 &&
                        ((SkelCompound) val).sym.fun.equals(Foyer.OP_TURNSTILE)) {
                    SkelCompound sc = (SkelCompound) val;
                    val = sc.args[0];
                    executeDirective(rd, val, en);
                } else {
                    Object term = Clause.clauseToHead(val, en);
                    PrologReader.checkSingleton(term, rd.getAnon(), en);
                    Clause clause = Clause.determineCompiled(
                            AbstractDefined.OPT_PERF_CNLT, term, val, en);
                    clause.vars = rd.getVars();
                    clause.assertRef(AbstractDefined.OPT_ACTI_BOTT, en);
                }
            } catch (EngineMessage x) {
                EngineException y = new EngineException(x,
                        EngineException.fetchStack(en));
                systemConsultBreak(y, en);
            } catch (EngineException x) {
                systemConsultBreak(x, en);
            }
        }
    }

    /**
     * <p>Execute a directive.</p>
     *
     * @param rd The Prolog reader.
     * @param molec The goal.
     * @param en  The engine.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    private static void executeDirective(PrologReader rd,
                                         Object molec, Engine en)
            throws EngineException, EngineMessage {
        Directive dire = Directive.createDirective(AbstractDefined.MASK_DEFI_CALL, en);
        int size = SupervisorCopy.displaySize(molec);
        dire.bodyToInterSkel(molec, en, true);
        AbstractUndo mark = en.bind;
        int snap = en.number;
        Object backref = en.visor.printmap;
        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;
        Display d2 = new Display(size);
        d2.vars = rd.getVars();
        try {
            Object val = hashToAssoc(rd.getVars(), d2, en);
            en.visor.printmap = AbstractTerm.createMolec(val, d2);
            CallFrame ref = CallFrame.getFrame(d2, dire, en);
            en.contskel = dire;
            en.contdisplay = ref;
            if (!en.runLoop(snap, true))
                throw new EngineMessage(EngineMessage.syntaxError(
                        EngineMessage.OP_SYNTAX_DIRECTIVE_FAILED));
        } catch (EngineException x) {
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            en.releaseBind(mark);
            en.visor.printmap = backref;
            throw en.fault;
        } catch (EngineMessage y) {
            EngineException x = new EngineException(y,
                    EngineException.fetchStack(en));
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            en.releaseBind(mark);
            en.visor.printmap = backref;
            throw en.fault;
        }
        en.contskel = r;
        en.contdisplay = u;
        en.fault = null;
        en.cutChoices(snap);
        en.releaseBind(mark);
        en.visor.printmap = backref;
        if (en.fault != null)
            throw en.fault;
    }

    /**
     * <p>Convert variable names.</p>
     * <p>Will not convert variables that have not yet been allocated.</p>
     * <p>Will not convert variables that have already been deallocated.</p>
     *
     * @param vars The var hash.
     * @param d    The term display.
     * @param en   The engine.
     * @return The Prolog association list.
     */
    public static Object hashToAssoc(MapHashLink<String, SkelVar> vars,
                                     Display d, Engine en) {
        Object end = en.store.foyer.ATOM_NIL;
        if (vars == null)
            return end;
        for (MapEntry<String, SkelVar> entry = vars.getLastEntry();
             entry != null; entry = vars.predecessor(entry)) {
            SkelVar sv = entry.value;
            if (sv.id >= d.bind.length || d.bind[sv.id] == null)
                continue;
            Object val = new SkelCompound(en.store.foyer.ATOM_EQUAL,
                    new SkelAtom(entry.key), sv);
            end = new SkelCompound(en.store.foyer.ATOM_CONS, val, end);
        }
        return end;
    }

}
