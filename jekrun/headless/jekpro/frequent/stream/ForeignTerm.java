package jekpro.frequent.stream;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.*;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.PositionKey;
import matula.util.regex.ScannerError;
import matula.util.system.OpenOpts;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

/**
 * The foreign predicates for the module term.
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
public final class ForeignTerm {

    /****************************************************************/
    /* AbstractTerm I/O                                             */
    /****************************************************************/

    /**
     * <p>Write a term respecting options to a stream.</p>
     *
     * @param inter The interpreter.
     * @param para  The write stream.
     * @param val   The write term.
     * @throws InterpreterMessage   Validation problem or option problem.
     * @throws InterpreterException Shit happens.
     */
    public static void sysWriteTerm(Interpreter inter,
                                    Writer para, AbstractTerm val)
            throws InterpreterMessage, InterpreterException {
        ForeignTerm.unparseTerm(inter, para, val, null, PrologWriter.FLAG_NUMV);
    }

    /**
     * <p>Write a term respecting options to a stream.</p>
     *
     * @param inter The interpreter.
     * @param para  The write stream.
     * @param val   The write term.
     * @param opt   The write options.
     * @throws InterpreterMessage   Validation problem or option problem.
     * @throws InterpreterException Shit happens.
     */
    public static void sysWriteTerm(Interpreter inter,
                                    Writer para, AbstractTerm val,
                                    Object opt)
            throws InterpreterMessage, InterpreterException {
        ForeignTerm.unparseTerm(inter, para, val, opt, 0);
    }

    /**
     * <p>Unparse the given term to a string.</p>
     *
     * @param inter The interpreter.
     * @param wr    The writer.
     * @param t     The term.
     * @param opt   The write options or null.
     * @param flags The default flags.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public static void unparseTerm(Interpreter inter, Writer wr,
                                   Object t, Object opt, int flags)
            throws InterpreterMessage, InterpreterException {
        Engine en = inter.getEngine();
        try {
            PrologWriter pw;
            if (opt != null && !opt.equals(Knowledgebase.OP_NIL)) {
                WriteOpts wo = new WriteOpts(en.visor.peekStack());
                if ((flags & PrologWriter.FLAG_QUOT) != 0)
                    wo.flags |= PrologWriter.FLAG_QUOT;
                if ((flags & PrologWriter.FLAG_NUMV) != 0)
                    wo.flags |= PrologWriter.FLAG_NUMV;
                wo.decodeWriteOptions(AbstractTerm.getSkel(opt),
                        AbstractTerm.getDisplay(opt), en);
                if ((wo.flags & PrologWriter.FLAG_FILL) == 0 &&
                        (wo.flags & PrologWriter.FLAG_NAVI) == 0) {
                    pw = en.store.foyer.createWriter(Foyer.IO_TERM);
                } else {
                    pw = en.store.foyer.createWriter(Foyer.IO_ANNO);
                }
                wo.setWriteOpts(pw);
            } else {
                pw = en.store.foyer.createWriter(Foyer.IO_TERM);
                pw.setSource(en.visor.peekStack());
                if ((flags & PrologWriter.FLAG_QUOT) != 0)
                    pw.flags |= PrologWriter.FLAG_QUOT;
                if ((flags & PrologWriter.FLAG_NUMV) != 0)
                    pw.flags |= PrologWriter.FLAG_NUMV;
            }
            pw.setEngineRaw(en);
            pw.setWriter(wr);
            pw.unparseStatement(AbstractTerm.getSkel(t), AbstractTerm.getDisplay(t));
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
    }

    /**
     * <p>Read a term and options from a stream.</p>
     *
     * @param inter The interpreter.
     * @param para  The read stream.
     * @return The read term.
     * @throws InterpreterMessage   Validation problem or option problem.
     * @throws InterpreterException Validation problem or option problem.
     */
    public static AbstractTerm sysReadTerm(Interpreter inter,
                                           Reader para)
            throws InterpreterMessage, InterpreterException {
        return (AbstractTerm) ForeignTerm.parseTerm(inter, para,
                null, PrologReader.FLAG_WRAP);
    }

    /**
     * <p>Read a term and options from a stream.</p>
     *
     * @param inter The interpreter.
     * @param para  The read stream.
     * @param opt   The read options.
     * @return The read term.
     * @throws InterpreterMessage   Validation problem or option problem.
     * @throws InterpreterException Validation problem or option problem.
     */
    public static AbstractTerm sysReadTerm(Interpreter inter,
                                           Reader para,
                                           Object opt)
            throws InterpreterMessage, InterpreterException {
        return (AbstractTerm) ForeignTerm.parseTerm(inter, para,
                opt, PrologReader.FLAG_WRAP);
    }

    /**
     * <p>Create a term from a line number reader.</p>
     * <p>Returns null when the output options don't unify.</p>
     *
     * @param inter The interpreter.
     * @param lr    The line number reader.
     * @param opt   The read options or null.
     * @param flags The flags.
     * @return The term or null.
     * @throws InterpreterException Shit happens.
     * @throws InterpreterMessage   Shit happens.
     */
    public static Object parseTerm(Interpreter inter, Reader lr,
                                   Object opt, int flags)
            throws InterpreterException, InterpreterMessage {
        Engine en = inter.getEngine();
        Object val;
        PrologReader rd;
        try {
            if (opt != null && !opt.equals(Knowledgebase.OP_NIL)) {
                ReadOpts ro = new ReadOpts(en.visor.peekStack());
                if ((flags & PrologReader.FLAG_TEOF) != 0)
                    ro.flags |= PrologReader.FLAG_TEOF;
                ro.decodeReadParameter(AbstractTerm.getSkel(opt), AbstractTerm.getDisplay(opt), en);
                if ((ro.flags & PrologWriter.FLAG_FILL) == 0) {
                    rd = en.store.foyer.createReader(Foyer.IO_TERM);
                } else {
                    rd = en.store.foyer.createReader(Foyer.IO_ANNO);
                }
                ro.setReadOpts(rd);
            } else {
                rd = en.store.foyer.createReader(Foyer.IO_TERM);
                rd.setDefaults(en.visor.peekStack());
                if ((flags & PrologReader.FLAG_TEOF) != 0)
                    rd.flags |= PrologReader.FLAG_TEOF;
            }
            rd.setEngineRaw(en);
            try {
                try {
                    rd.getScanner().setReader(lr);
                    val = rd.parseHeadStatement();
                } catch (ScannerError y) {
                    String line = ScannerError.linePosition(OpenOpts.getLine(lr), y.getErrorOffset());
                    rd.parseTailError(y);
                    EngineMessage x = new EngineMessage(
                            EngineMessage.syntaxError(y.getMessage()));
                    PositionKey pos = PositionKey.createPos(lr);
                    throw new EngineException(x,
                            EngineException.fetchPos(EngineException.fetchLoc(
                                    EngineException.fetchStack(en),
                                    pos, en), line, en));
                }
            } catch (IOException y) {
                throw EngineMessage.mapIOException(y);
            }
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
        if (val == null)
            return null;
        Display ref = AbstractSkel.createMarker(val, false);
        try {
            if (opt != null && !opt.equals(Knowledgebase.OP_NIL)) {
                if (!ReadOpts.decodeReadResults(AbstractTerm.getSkel(opt),
                        AbstractTerm.getDisplay(opt), val, ref, en, rd))
                    return null;
            }
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
        ref.setMarker(true);
        if ((flags & PrologReader.FLAG_WRAP) != 0) {
            return AbstractTerm.createTermWrapped(val, ref);
        } else {
            return AbstractTerm.createTerm(val, ref);
        }
    }

}
