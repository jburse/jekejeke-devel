package jekpro.tools.call;

import jekpro.frequent.stream.ForeignTerm;
import jekpro.model.inter.Engine;
import jekpro.model.inter.EngineYield;
import jekpro.model.inter.Supervisor;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.PrologReader;
import jekpro.model.pretty.PrologWriter;
import jekpro.model.pretty.Store;
import jekpro.reference.bootload.ForeignEngine;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import matula.util.system.ConnectionReader;

import java.io.StringReader;
import java.io.StringWriter;
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
 * barriers. They are used when solving a term. An interactor for a
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
 * The parseTerm() respective parseTermWrapped() methods convert a string
 * to a term. The string must not be terminated by a period. The unparseTerm()
 * methods convert a term to a string. Atoms will be quoted during unparsed.
 * There are methods without and with an option list. The read and write option
 * lists are the same as the corresponding Prolog predicate:
 *
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
    /* Interpreter Factory                                           */
    /*****************************************************************/

    /**
     * <p>Create an iterable with a reused controller.</p>
     *
     * @return The iterable.
     */
    public Interpreter iterable() {
        return new Interpreter(getKnowledgebase(), getController());
    }

    /**
     * <p>Create a call-in.</p>
     *
     * @param goal The term, non-null.
     * @return The call-in.
     */
    public CallIn iterator(Object goal) {
        return new CallIn(goal, this);
    }

    /*****************************************************************/
    /* Properties API                                                */
    /*****************************************************************/

    /**
     * <p>Retrieve the interpreter property names.</p>
     *
     * @return The interpreter property names.
     */
    public ArrayList<String> getProperties() {
        return ForeignEngine.listPrologFlags(engine);
    }

    /**
     * <p>Retrieve an interpreter property.</p>
     *
     * @param flag The flag name.
     * @return The flag value.
     */
    public Object getProperty(String flag) {
        Object res = ForeignEngine.getPrologFlag(flag, engine);
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
            ForeignEngine.setPrologFlag(flag, AbstractTerm.getSkel(val),
                    AbstractTerm.getDisplay(val), engine);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /****************************************************************/
    /* Term Writing                                                 */
    /****************************************************************/

    /**
     * <p>Unparse the given term to a string.</p>
     *
     * @param t The object.
     * @return The string.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public String unparseTerm(Object t)
            throws InterpreterMessage, InterpreterException {
        StringWriter sw = new StringWriter();
        ForeignTerm.unparseTerm(this, sw, t, null, PrologWriter.FLAG_QUOT);
        return sw.toString();
    }

    /**
     * <p>Unparse the given term to a string.</p>
     *
     * @param t   The object.
     * @param opt The write options.
     * @return The string.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public String unparseTerm(Object t, Object opt)
            throws InterpreterMessage, InterpreterException {
        StringWriter sw = new StringWriter();
        ForeignTerm.unparseTerm(this, sw, t, opt, PrologWriter.FLAG_QUOT);
        return sw.toString();
    }

    /***********************************************************/
    /* Term Reading                                            */
    /***********************************************************/

    /**
     * <p>Create a term from a string.</p>
     * <p>The term doesn't need a period.</p>
     * <p>Returns null when an empty string has been supplied.</p>
     *
     * @param s The string.
     * @return The term or null.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public Object parseTerm(String s)
            throws InterpreterMessage, InterpreterException {
        ConnectionReader cr = new ConnectionReader(new StringReader(s));
        return ForeignTerm.parseTerm(this, cr,
                null, PrologReader.FLAG_TEOF);
    }

    /**
     * <p>Create a term from a string.</p>
     * <p>The term doesn't need a period.</p>
     * <p>Returns null when an empty string has been supplied.</p>
     *
     * @param s The string.
     * @return The term or null.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public AbstractTerm parseTermWrapped(String s)
            throws InterpreterMessage, InterpreterException {
        ConnectionReader cr = new ConnectionReader(new StringReader(s));
        return (AbstractTerm) ForeignTerm.parseTerm(this, cr,
                null, PrologReader.FLAG_TEOF + PrologReader.FLAG_WRAP);
    }

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
    public Object parseTerm(String s, Object opt)
            throws InterpreterMessage, InterpreterException {
        ConnectionReader cr = new ConnectionReader(new StringReader(s));
        return ForeignTerm.parseTerm(this, cr,
                opt, PrologReader.FLAG_TEOF);
    }

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
    public AbstractTerm parseTermWrapped(String s, Object opt)
            throws InterpreterMessage, InterpreterException {
        ConnectionReader cr = new ConnectionReader(new StringReader(s));
        return (AbstractTerm) ForeignTerm.parseTerm(this, cr,
                opt, PrologReader.FLAG_TEOF + PrologReader.FLAG_WRAP);
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
    public Object getEngine() {
        return engine;
    }

}
