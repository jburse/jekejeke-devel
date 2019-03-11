package jekpro.tools.call;

import jekpro.frequent.standard.EngineCopy;
import jekpro.model.inter.Engine;
import jekpro.model.inter.EngineYield;
import jekpro.model.inter.Supervisor;
import jekpro.model.molec.*;
import jekpro.model.pretty.*;
import jekpro.reference.bootload.ForeignEngine;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.PositionKey;
import matula.util.regex.ScannerError;
import matula.util.system.ConnectionReader;
import matula.util.system.OpenOpts;

import java.io.*;
import java.util.ArrayList;

/**
 * This class represents an interpreter. The interpreter object can be
 * obtained from a knowledge base by the iterable() method. The knowledge
 * base associated with an interpreter can be retrieved and set via
 * the methods getKnowledgebase() and setKnowledgebase().
 * <p>
 * The controller associated with an interpreter can be retrieved via
 * the method getController(). If multiple interactors are needed a
 * further interpreter object can be forked from an existing interpreter
 * object by the iterable() method. Multiple interactors share the
 * same controller.
 * <p>
 * To consistently compare variables their serial numbers are used.
 * Serial numbers are allocated and maintained similar to variable bindings
 * in the interpreter. Lexical comparison is therefore defined as a method
 * compare() of the class Interpreter. This method corresponds to the Prolog
 * predicate compare/3.
 * <p>
 * The interpreter further carries choice points and tail recursion
 * barriers. They are used when solving a goal. An interactor for a
 * solution set can be obtained via the method callin(). The interactor
 * can be controlled via the different methods of the interactor itself.
 * For more details see the CallIn class.
 * <p>
 * All Prolog flags can be accessed via the methods getProperty() and
 * setProperty(). The set of available Prolog flags depends on the
 * initialized capabilities and on the used toolkit. For properties
 * that represent streams the following Java classes are used:
 * <p>
 * <b>Table 5: Java Class ISO Stream Mapping</b>
 * <table>
 * <tr valign="baseline"><th>ISO Mode</th><th>ISO Type</th><th>Java Class</th></tr>
 * <tr valign="baseline"><td rowspan="2">Read</td><td>Text Stream</td><td>Reader</td></tr>
 * <tr valign="baseline"><td>Binary Stream</td><td>InputStream</td></tr>
 * <tr valign="baseline"><td rowspan="2">Write</td><td>Text Stream</td><td>Writer</td></tr>
 * <tr valign="baseline"><td>Binary Stream</td><td>OutputStream</td></tr>
 * </table>
 * <p>
 * The unparseTerm() methods convert a term to a string or stream. The
 * following flags are recognized. The method with an option term
 * recognizes all options from the write_term/2 predicate. The
 * following flags are available:
 * <p>
 * <ul>
 * <li><b>FLAG_QUOTED:</b> Quote atoms when necessary.</li>
 * <li><b>FLAG_NUMBERVARS:</b> Write $VAR(n) as a variable name.</li>
 * <li><b>FLAG_IGNORE_OPS:</b> Ignore operator definitions.</li>
 * <li><b>FLAG_IGNORE_MOD:</b>Ignore module prefixes.</li>
 * </ul>
 * <p>
 * The parseTerm() methods convert a string or stream to a term. The
 * method that takes a string doesn't require that the term is terminated
 * by a period and returns null when an empty string has been supplied.
 * The method with an option term recognizes all options from the
 * read_term/2 predicate and returns null when the option unification fails.
 * <p>
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
public final class Interpreter {
    private final Engine engine;

    public final static int FLAG_QUOTED = PrologWriter.FLAG_QUOT;
    public final static int FLAG_NUMBERVARS = PrologWriter.FLAG_NUMV;
    public final static int FLAG_IGNORE_OPS = PrologWriter.FLAG_IGNO;
    public final static int FLAG_IGNORE_MOD = PrologWriter.FLAG_IGNM;

    /**
     * <p>Retrieve the knowledge base.</p>
     *
     * @return The knowledge base.
     */
    public Knowledgebase getKnowledgebase() {
        return (Knowledgebase) engine.store.proxy;
    }

    /**
     * <p>Set the knowledge base.</p>
     *
     * @param k The knowledge base.
     */
    public void setKnowledgebase(Knowledgebase k) {
        Store store = (Store) k.getStore();
        engine.visor.popStack();
        engine.visor.pushStack(store.user);
        engine.store = store;
    }

    /**
     * <p>Retrieve the controller.</p>
     *
     * @return The controller.
     */
    public Controller getController() {
        return (Controller) engine.visor.proxy;
    }

    /*****************************************************************/
    /* Iterable Construction                                         */
    /*****************************************************************/

    /**
     * <p>Create an iterable with a reused controller.</p>
     *
     * @return The iterable.
     */
    public Interpreter iterable() {
        return new Interpreter(getKnowledgebase(), getController());
    }

    /*****************************************************************/
    /* Iterator Construction                                         */
    /*****************************************************************/

    /**
     * <p>Create a call-in.</p>
     *
     * @param goal The goal, non-null.
     * @return The call-in.
     */
    public CallIn iterator(Object goal) {
        return new CallIn(goal, this);
    }

    /*****************************************************************/
    /* Properties API                                                */
    /*****************************************************************/

    /**
     * <p>Retrieve the property names.</p>
     *
     * @return The property names.
     */
    public ArrayList<String> getProperties() {
        return ForeignEngine.listFlags(engine);
    }

    /**
     * <p>Retrieve an interpreter property.</p>
     *
     * @param flag The flag name.
     * @return The flag value.
     */
    public Object getProperty(String flag) {
        Object res = ForeignEngine.getFlag(flag, engine);
        return (res != null ? AbstractTerm.createTerm(res, Display.DISPLAY_CONST) : null);
    }

    /**
     * <p>Set an interpreter property.</p>
     *
     * @param flag The flag name.
     * @param val  The flag value.
     * @throws InterpreterMessage Shit happens.
     */
    public void setProperty(String flag, Object val)
            throws InterpreterMessage {
        try {
            ForeignEngine.setFlag(flag, AbstractTerm.getSkel(val),
                    AbstractTerm.getDisplay(val), engine);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /****************************************************************/
    /* AbstractTerm Writing                                         */
    /****************************************************************/

    /**
     * <p>Unparse the given term to a string.</p>
     *
     * @param t   The object.
     * @param opt The write options.
     * @return The string.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public String unparseTerm(AbstractTerm t, Object opt)
            throws InterpreterMessage, InterpreterException {
        StringWriter sw = new StringWriter();
        unparseTerm(sw, t, opt, true);
        return sw.toString();
    }

    /**
     * <p>Unparse the given term to a string.</p>
     *
     * @param wr  The writer.
     * @param t   The term.
     * @param opt The write options.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public void unparseTerm(Writer wr, AbstractTerm t, Object opt)
            throws InterpreterMessage, InterpreterException {
        unparseTerm(wr, t, opt, false);
    }

    /**
     * <p>Unparse the given term to a string.</p>
     *
     * @param wr  The writer.
     * @param t   The term.
     * @param opt The write options.
     * @param qf  The quoted flag.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    private void unparseTerm(Writer wr, AbstractTerm t, Object opt, boolean qf)
            throws InterpreterMessage, InterpreterException {
        Engine en = (Engine) getEngine();
        try {
            PrologWriter pw;
            if (!opt.equals(Knowledgebase.OP_NIL)) {
                WriteOpts wo = new WriteOpts(en);
                if (qf)
                    wo.flags |= PrologWriter.FLAG_QUOT;
                wo.decodeWriteOptions(AbstractTerm.getSkel(opt),
                        AbstractTerm.getDisplay(opt), en);
                if ((wo.flags & PrologWriter.FLAG_FILL) == 0 &&
                        (wo.flags & PrologWriter.FLAG_NAVI) == 0) {
                    pw = Foyer.createWriter(Foyer.IO_TERM);
                } else {
                    pw = Foyer.createWriter(Foyer.IO_ANNO);
                }
                wo.setWriteOpts(pw);
            } else {
                pw = Foyer.createWriter(Foyer.IO_TERM);
                pw.setSource(en.visor.peekStack());
                if (qf)
                    pw.flags |= PrologWriter.FLAG_QUOT;
            }
            pw.setEngineRaw(en);
            pw.setWriter(wr);
            pw.unparseStatement(AbstractTerm.getSkel(t),
                    AbstractTerm.getDisplay(t));
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
    }

    /***********************************************************/
    /* AbstractTerm Reading                                    */
    /***********************************************************/

    /**
     * <p>Create a term from a string.</p>
     * <p>The term doesn't need a period.</p>
     * <p>Returns null when an empty string has been supplied.</p>
     *
     * @param s   The string.
     * @param opt The read options.
     * @return The term or null.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public AbstractTerm parseTerm(String s, Object opt)
            throws InterpreterMessage, InterpreterException {
        ConnectionReader cr = new ConnectionReader(new StringReader(s));
        cr.setLineNumber(1);
        return parseTerm(cr, opt, true);
    }

    /**
     * <p>Create a term from a line number reader.</p>
     * <p>Returns null when the output options don't unify.</p>
     *
     * @param lr  The line number reader.
     * @param opt The read options.
     * @return The term or null.
     * @throws InterpreterException Shit happens.
     * @throws InterpreterMessage   Shit happens.
     */
    public AbstractTerm parseTerm(Reader lr, Object opt)
            throws InterpreterException, InterpreterMessage {
        return parseTerm(lr, opt, false);
    }

    /**
     * <p>Create a term from a line number reader.</p>
     * <p>Returns null when the output options don't unify.</p>
     *
     * @param lr  The line number reader.
     * @param opt The read options.
     * @param te  The end of line flag.
     * @return The term or null.
     * @throws InterpreterException Shit happens.
     * @throws InterpreterMessage   Shit happens.
     */
    private AbstractTerm parseTerm(Reader lr, Object opt, boolean te)
            throws InterpreterException, InterpreterMessage {
        Engine en = (Engine) getEngine();
        Object val;
        PrologReader rd;
        try {
            if (!opt.equals(Knowledgebase.OP_NIL)) {
                ReadOpts ro = new ReadOpts(en);
                if (te)
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
                rd.setSource(en.visor.peekStack());
                if (te)
                    rd.flags |= PrologReader.FLAG_TEOF;
            }
            rd.getScanner().setReader(lr);
            rd.setEngineRaw(en);
            try {
                try {
                    val = rd.parseHeadStatement();
                } catch (ScannerError y) {
                    String line = ScannerError.linePosition(OpenOpts.getLine(lr), y.getErrorOffset());
                    rd.parseTailError(y);
                    EngineMessage x = new EngineMessage(
                            EngineMessage.syntaxError(y.getMessage()));
                    PositionKey pos = (OpenOpts.getPath(lr) != null ?
                            new PositionKey(OpenOpts.getPath(lr), OpenOpts.getLineNumber(lr)) : null);
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
        Display ref = AbstractSkel.createDisplay(val);
        try {
            if (!ReadOpts.decodeReadOptions(AbstractTerm.getSkel(opt),
                    AbstractTerm.getDisplay(opt), val, ref, en, rd))
                return null;
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
        if (EngineCopy.displaySize(val) != 0)
            ref.flags |= Display.MASK_DPTM_MLTI;
        return AbstractTerm.createTermWrapped(val, ref);
    }

    /***********************************************************/
    /* For Internal Use Only                                   */
    /***********************************************************/

    /**
     * <p>Create an interpreter.</p>
     *
     * @param k The knowledgebase.
     * @param i The controller.
     */
    public Interpreter(Knowledgebase k, Controller i) {
        Store store = (Store) k.getStore();
        Supervisor visor = (Supervisor) i.getVisor();

        switch (store.foyer.getHint()) {
            case Foyer.HINT_WEB:
                engine = new EngineYield(store, visor);
                break;
            default:
                engine = new Engine(store, visor);
                break;
        }

        engine.proxy = this;
    }

    /**
     * <p>Retrieve the engine.</p>
     *
     * @return The engine.
     */
    public final Object getEngine() {
        return engine;
    }

}
