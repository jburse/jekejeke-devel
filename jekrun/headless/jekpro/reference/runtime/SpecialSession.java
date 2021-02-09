package jekpro.reference.runtime;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.pretty.*;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Intermediate;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;
import matula.util.regex.ScannerError;
import matula.util.system.OpenOpts;

import java.io.IOException;
import java.io.Reader;

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
    private final static int SPECIAL_SYS_QUOTED_VAR = 0;
    public final static int SPECIAL_SYS_BOOT_STREAM = 1;

    /**
     * <p>Create a session special.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialSession(int i) {
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
            case SPECIAL_SYS_QUOTED_VAR:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                String fun = SpecialUniv.derefAndCastString(temp[0], ref);
                if (!BindUniv.unifyTerm(sysQuoteVar(fun, en), Display.DISPLAY_CONST, temp[1], ref, en))
                    return false;
                return true;
            case SPECIAL_SYS_BOOT_STREAM:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                Object obj = SpecialUniv.derefAndCastRef(temp[0], ref);
                PrologReader.checkTextRead(obj);
                bootStream((Reader) obj, en);
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
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
        pw.setDefaults(en.visor.peekStack());
        pw.setFlags(pw.getFlags() | PrologWriter.FLAG_QUOT);
        return new SkelAtom(pw.variableQuoted(fun));
    }

    /**
     * <p>Consult a stream natively.</p>
     * <p>Term expansion is not provided.</p>
     *
     * @param lr  The buffered reader.
     * @param en  The interpreter.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static void bootStream(Reader lr, Engine en)
            throws EngineException, EngineMessage {
        PrologReader rd = en.store.foyer.createReader(Foyer.IO_TERM);
        rd.setEngineRaw(en);
        for (; ; ) {
            try {
                Object val;
                rd.setDefaults(en.visor.peekStack(), PrologReader.FLAG_SING | PrologReader.FLAG_NEWV);
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
                        ((SkelCompound) val).sym.fun.equals(Clause.OP_TURNSTILE)) {
                    SkelCompound sc = (SkelCompound) val;
                    val = sc.args[0];
                    SpecialSession.executeDirective(rd, val, en);
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
                SpecialLoad.systemConsultBreak(y, en);
            } catch (EngineException x) {
                SpecialLoad.systemConsultBreak(x, en);
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
            Object val = SpecialSession.hashToAssoc(rd.getVars(), d2, en);
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
