package jekpro.tools.call;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Supervisor;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.*;
import jekpro.model.rope.LoadForce;
import jekpro.reference.bootload.ForeignEngine;
import jekpro.reference.bootload.ForeignPath;
import jekpro.reference.structure.SpecialLexical;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.PositionKey;
import matula.util.regex.ScannerError;
import matula.util.system.ConnectionReader;
import matula.util.system.OpenOpts;

import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Comparator;

/**
 * This class represents an interpreter. The interpreter object can be
 * obtained from a knowledge base by the iterable() method. The knowledge
 * base associated with an interpreter can be retrieved and set via
 * the methods getKnowledgebase() and setKnowledgebase().
 *
 * The controller associated with an interpreter can be retrieved via
 * the method getController(). If multiple interactors are needed a
 * further interpreter object can be forked from an existing interpreter
 * object by the iterable() method. Multiple interactors share the
 * same controller.
 *
 * To consistently compare variables their serial numbers are used.
 * Serial numbers are allocated and maintained similar to variable bindings
 * in the interpreter. Lexical comparison is therefore defined as a method
 * compare() of the class Interpreter. This method corresponds to the Prolog
 * predicate compare/3.
 *
 * The interpreter further carries choice points and tail recursion
 * barriers. They are used when solving a goal. An interactor for a
 * solution set can be obtained via the method callin(). The interactor
 * can be controlled via the different methods of the interactor itself.
 * For more details see the CallIn class.
 *
 * All Prolog flags can be accessed via the methods getProperty() and
 * setProperty(). The set of available Prolog flags depends on the
 * initialized capabilities and on the used toolkit. For properties
 * that represent streams the following Java classes are used:
 *
 * <b>Table 5: Java Class ISO Stream Mapping</b>
 * <table>
 * <tr valign="baseline"><th>ISO Mode</th><th>ISO Type</th><th>Java Class</th></tr>
 * <tr valign="baseline"><td rowspan="2">Read</td><td>Text Stream</td><td>Reader</td></tr>
 * <tr valign="baseline"><td>Binary Stream</td><td>InputStream</td></tr>
 * <tr valign="baseline"><td rowspan="2">Write</td><td>Text Stream</td><td>Writer</td></tr>
 * <tr valign="baseline"><td>Binary Stream</td><td>OutputStream</td></tr>
 * </table>
 *
 * The methods findKey(), findPrefix() and keyToSpec() do relative file
 * name translation. The translation corresponds to the Prolog predicates
 * absolute_file_name/[2,3].
 *
 * The toString() methods convert a term to a string. The unparseTerm()
 * methods convert a term to a string or stream. The following flags are
 * recognized. The method with an option term recognizes all options from
 * the write_term/2 predicate. The following flags are available:
 *
 * <ul>
 * <li><b>FLAG_QUOTED:</b> Quote atoms when necessary.</li>
 * <li><b>FLAG_NUMBERVARS:</b> Write $VAR(n) as a variable name.</li>
 * <li><b>FLAG_IGNORE_OPS:</b> Ignore operator definitions.</li>
 * <li><b>FLAG_IGNORE_MOD:</b>Ignore module prefixes.</li>
 * </ul>
 *
 * The parseTerm() methods convert a string or stream to a term. The
 * method that takes a string doesn't require that the term is terminated
 * by a period and returns null when an empty string has been supplied.
 * The method with an option term recognizes all options from the
 * read_term/2 predicate and returns null when the option unification fails.
 *
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
public final class Interpreter implements Comparator<Object> {
    private final Engine engine;

    public final static int FLAG_QUOTED = PrologWriter.FLAG_QUOT;
    public final static int FLAG_NUMBERVARS = PrologWriter.FLAG_NUMV;
    public final static int FLAG_IGNORE_OPS = PrologWriter.FLAG_IGNO;
    public final static int FLAG_IGNORE_MOD = PrologWriter.FLAG_IGNM;

    /**
     * <p>Create an interpreter.</p>
     *
     * @param k The knowledgebase.
     * @param i The controller.
     */
    public Interpreter(Knowledgebase k, Controller i) {
        AbstractStore store = (AbstractStore) k.getStore();
        Supervisor visor = (Supervisor) i.getVisor();

        engine = new Engine(store, visor);
        engine.proxy = this;
    }

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
        AbstractStore store = (AbstractStore) k.getStore();
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
     * <p>Create a call-in. This amounts to the comprehension "{ void |
     * true, &lt;custom&gt; }" where "&lt;custom;&gt;" is the Java code
     * that is called during the interaction.</p>
     *
     * @return The call-in.
     */
    public CallIn iterator() {
        return new CallIn(AbstractTerm.VOID_OBJ,
                getKnowledgebase().getLobby().GOAL_TRUE, this);
    }

    /**
     * <p>Create a call-in. This amounts to the comprehension "{ void |
     * &lt;goal&gt;, &lt;custom&gt; }" where "&lt;custom&gt;" is the
     * Java code that is called during the interaction.</p>
     *
     * @param goal The goal, non-null.
     * @return The call-in.
     */
    public CallIn iterator(Object goal) {
        return new CallIn(AbstractTerm.VOID_OBJ, goal, this);
    }

    /**
     * <p>Create a call-in. This amounts to the comprehension "{ &lt;result&gt; |
     * &lt;goal&gt;, &lt;custom&gt; }" where "&lt;custom&gt;" is the
     * Java code that is called during the interaction.</p>
     *
     * @param result The result, non-null.
     * @param goal   The goal, non-null.
     * @return The call-in.
     */
    public CallIn iterator(Object result, Object goal) {
        return new CallIn(result, goal, this);
    }

    /*****************************************************************/
    /* AbstractTerm Comparison                                               */
    /*****************************************************************/

    /**
     * <p>Compare lexically this term with another term.</p>
     *
     * @param t1 The first term.
     * @param t2 The second term.
     * @return <0 this < t, 0 this = t, >0 this > t
     */
    public int compare(Object t1, Object t2)
            throws ArithmeticException {
        return SpecialLexical.compareTerm(AbstractTerm.getSkel(t1), AbstractTerm.getDisplay(t1),
                AbstractTerm.getSkel(t2), AbstractTerm.getDisplay(t2), engine);
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

    /*****************************************************************/
    /* File Probing                                                  */
    /*****************************************************************/

    /**
     * <p>Find a key according to the auto loader.</p>
     *
     * @param path The path.
     * @param key  The call-site.
     * @param mask The mask.
     * @return The source key.
     * @throws InterpreterMessage Shit happens.
     */
    public String findKey(String path, String key, int mask)
            throws InterpreterMessage {
        String res;
        try {
            AbstractSource src = (!"".equals(key) ?
                    AbstractSource.keyToSource(key, engine.store) : null);
            res = engine.findKey(path, src, mask);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
        return res;
    }

    /**
     * <p>Find a prefix according to the auto loader.</p>
     *
     * @param path The path.
     * @param key  The call-site.
     * @param mask The mask.
     * @return The prefixed path, or null.
     * @throws InterpreterMessage Shit happens.
     */
    public String findPrefix(String path, String key, int mask)
            throws InterpreterMessage {
        String res;
        try {
            AbstractSource src = (!"".equals(key) ?
                    AbstractSource.keyToSource(key, engine.store) : null);
            res = engine.findPrefix(path, src, mask);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
        return res;
    }

    /**
     * <p>Revert a path back to a spec.</p>
     *
     * @param path The source key.
     * @param key  The call-site.
     * @param mask The mask.
     * @return The spec.
     */
    public Object keyToSpec(String path, String key, int mask)
            throws InterpreterMessage {
        Object spec;
        try {
            AbstractSource src = (!"".equals(key) ?
                    AbstractSource.keyToSource(key, engine.store) : null);
            String fun = ((mask & ForeignPath.MASK_SUFX_RSCS) != 0 ?
                    LoadForce.OP_LINK_SYS_LOAD_RESOURCE : "");
            spec = AbstractSource.keyToSpec(path, fun, src, engine);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
        return spec;
    }

    /****************************************************************/
    /* Skeleton Writing                                             */
    /****************************************************************/

    /**
     * <p>Unparse the given term to a string.</p>
     */
    public static String toString(int flags, Object t) {
        return PrologWriter.toString(AbstractTerm.getSkel(t),
                AbstractTerm.getDisplay(t), flags);
    }

    /**
     * <p>Unparse the given term to a string.</p>
     *
     * @param wr    The writer.
     * @param flags The flags.
     * @param t     The object.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public static void toString(Writer wr, int flags, Object t)
            throws InterpreterMessage, InterpreterException {
        try {
            PrologWriter.toString(AbstractTerm.getSkel(t),
                    AbstractTerm.getDisplay(t), wr, flags, null);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
    }

    /****************************************************************/
    /* AbstractTerm Writing                                         */
    /****************************************************************/

    /**
     * <p>Unparse the given term to a string.</p>
     *
     * @param flags The flags.
     * @param t     The object.
     * @return The string.
     */
    public String unparseTerm(int flags, Object t) {
        Engine en = (Engine) getEngine();
        return PrologWriter.toString(AbstractTerm.getSkel(t),
                AbstractTerm.getDisplay(t), flags, en);
    }

    /**
     * <p>Unparse the given term to a string.</p>
     *
     * @param wr    The writer.
     * @param flags The flags.
     * @param t     The object.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public void unparseTerm(Writer wr, int flags, Object t)
            throws InterpreterMessage, InterpreterException {
        Engine en = (Engine) getEngine();
        try {
            PrologWriter.toString(AbstractTerm.getSkel(t),
                    AbstractTerm.getDisplay(t), wr, flags, en);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
    }

    /**
     * <p>Unparse the given term to a string.</p>
     *
     * @param wr  The writer.
     * @param opt The write options.
     * @param t   The term.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public void unparseTerm(Writer wr, Object opt, AbstractTerm t)
            throws InterpreterMessage, InterpreterException {
        Engine en = (Engine) getEngine();
        try {
            WriteOpts wo = new WriteOpts(en);
            wo.decodeWriteOptions(AbstractTerm.getSkel(opt),
                    AbstractTerm.getDisplay(opt), en);
            PrologWriter pw;
            if ((wo.flags & PrologWriter.FLAG_FILL) == 0 && (wo.flags & PrologWriter.FLAG_NAVI) == 0) {
                pw = Foyer.createWriter(Foyer.IO_TERM);
            } else {
                pw = Foyer.createWriter(Foyer.IO_ANNO);
            }
            pw.setEngineRaw(en);
            pw.setWriteOpts(wo);
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
     * @param s The string.
     * @return The term or null.
     * @throws InterpreterMessage Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public Object parseTerm(String s)
            throws InterpreterMessage, InterpreterException {
        Engine en = (Engine) getEngine();
        PrologReader rd = en.store.foyer.createReader(Foyer.IO_TERM);
        ConnectionReader cr = new ConnectionReader(new StringReader(s));
        cr.setLineNumber(1);
        rd.getScanner().setReader(cr);
        rd.setReadUtil(en.store);
        rd.setSource(en.store.user);
        rd.setEngineRaw(en);
        Object val;
        try {
            try {
                val = rd.parseHeadInternal();
            } catch (ScannerError y) {
                String line = ScannerError.linePosition(OpenOpts.getLine(cr), y.getPos());
                rd.parseTailError(PrologReader.OP_EOF, y);
                EngineMessage x = new EngineMessage(
                        EngineMessage.syntaxError(y.getError()));
                throw new EngineException(x,
                        EngineException.fetchPos(
                                EngineException.fetchStack(en), line, en));
            }
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
        if (val == null)
            return null;
        Display ref = (rd.getGensym() != 0 ?
                new Display(rd.getGensym()) : Display.DISPLAY_CONST);
        return AbstractTerm.createTerm(val, ref);
    }

    /**
     * <p>Create a term from a line number reader.</p>
     *
     * @param lr The line number reader.
     * @return The term or null.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public Object parseTerm(Reader lr)
            throws InterpreterMessage, InterpreterException {
        Engine en = (Engine) getEngine();
        PrologReader rd = en.store.foyer.createReader(Foyer.IO_TERM);
        rd.getScanner().setReader(lr);
        rd.setReadUtil(en.store);
        rd.setSource(en.store.user);
        rd.setEngineRaw(en);
        Object val;
        try {
            try {
                val = rd.parseHeadStatement();
            } catch (ScannerError y) {
                String line = ScannerError.linePosition(OpenOpts.getLine(lr), y.getPos());
                rd.parseTailError(PrologReader.OP_PERIOD, y);
                EngineMessage x = new EngineMessage(
                        EngineMessage.syntaxError(y.getError()));
                PositionKey pos = (OpenOpts.getPath(lr) != null ?
                        new PositionKey(OpenOpts.getPath(lr), OpenOpts.getLineNumber(lr)) : null);
                throw new EngineException(x,
                        EngineException.fetchPos(EngineException.fetchLoc(
                                EngineException.fetchStack(en),
                                pos, en), line, en));
            }
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
        Display ref = (rd.getGensym() != 0 ?
                new Display(rd.getGensym()) : Display.DISPLAY_CONST);
        return AbstractTerm.createTerm(val, ref);
    }

    /**
     * <p>Create a term from a line number reader.</p>
     * <p>Returns null when the output options don't unify.</p>
     *
     * @param lr  The line number reader.
     * @param opt The options.
     * @return The term or null.
     * @throws InterpreterException Shit happens.
     * @throws InterpreterMessage   Shit happens.
     */
    public AbstractTerm parseTerm(Reader lr, Object opt)
            throws InterpreterException, InterpreterMessage {
        Engine en = (Engine) getEngine();
        ReadOpts ro = new ReadOpts();
        try {
            ro.decodeReadParameter(AbstractTerm.getSkel(opt), AbstractTerm.getDisplay(opt), en);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
        PrologReader rd;
        if ((ro.flags & PrologWriter.FLAG_FILL) == 0) {
            rd = en.store.foyer.createReader(Foyer.IO_TERM);
        } else {
            rd = en.store.foyer.createReader(Foyer.IO_ANNO);
        }
        rd.getScanner().setReader(lr);
        ro.setReadOpts(rd);
        rd.setEngineRaw(en);
        Object val;
        try {
            try {
                val = rd.parseHeadStatement();
            } catch (ScannerError y) {
                String line = ScannerError.linePosition(OpenOpts.getLine(lr), y.getPos());
                rd.parseTailError(PrologReader.OP_PERIOD, y);
                EngineMessage x = new EngineMessage(
                        EngineMessage.syntaxError(y.getError()));
                PositionKey pos = (OpenOpts.getPath(lr) != null ?
                        new PositionKey(OpenOpts.getPath(lr), rd.getClauseStart()) : null);
                throw new EngineException(x,
                        EngineException.fetchPos(EngineException.fetchLoc(
                                EngineException.fetchStack(en),
                                pos, en), line, en));
            }
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
        Display ref = (rd.getGensym() != 0 ? new Display(rd.getGensym()) : Display.DISPLAY_CONST);
        try {
            if (!rd.decodeReadOptions(AbstractTerm.getSkel(opt),
                    AbstractTerm.getDisplay(opt), val, ref, en))
                return null;
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
        return AbstractTerm.createTermWrapped(val, ref);
    }

    /***********************************************************/
    /* For Internal Use Only                                   */
    /***********************************************************/

    /**
     * <p>Retrieve the engine.</p>
     *
     * @return The engine.
     */
    public final Object getEngine() {
        return engine;
    }

}