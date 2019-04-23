package jekpro.tools.term;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.call.*;
import matula.util.config.FileExtension;
import matula.util.data.MapEntry;

import java.io.IOException;
import java.util.HashMap;
import java.util.Locale;
import java.util.Properties;

/**
 * This class represents a knowledge base. The constructor that takes
 * a toolkit creates a root knowledge base. The lobby can be accessed
 * by the method getLobby(). The constructor that takes another
 * knowledgebase creates a child knowledge base. The parent knowledge
 * base can be accessed by the method getParent(). An interactor can
 * be obtained from a knowledge base via the method iterable().
 * <p>
 * After creation, the knowledge base will be uninitialized. To initialize
 * the knowledge base the methods initKnowledgebase() should be used.
 * For a root knowledge base this will initialize the capabilities
 * already defined in the toolkit. The method finiKnowledgebase()
 * will in turn finish a knowledge base. For a root knowledgebase
 * this will finish the currently initialized capabilities of the
 * knowledge base and render the knowledge base unusable.
 * <p>
 * A knowledgebase provides a context for a base URL. The base URL is used
 * in resolving relative write paths. The GUI and the non-GUI console will
 * initialize the base URL to the current user directory. Otherwise the base
 * URL is left uninitialized. A knowledgebase also provides a context for
 * a locale. The GUI console will initialize the locale to the settings.
 * Otherwise the locale is set to the JVM locale.
 * <p>
 * A knowledge base also provides a context for a class path. The class
 * path is used in resolving path/1 and library/1 read paths. The class path
 * is also used to resolve class names. Classes can be loaded via the method
 * stringToClass(). The method respects short hands as defined in the foreign
 * function interface. The possibly shortened class name can be reconstructed
 * by the method classToString().
 * <p>
 * The current class loader can be retrieve via the method getLoader(). The
 * method addClassPath() allows adding further paths to the class loader. The
 * currently added paths can be queried with the method getClassPaths(). The
 * GUI console will automatically add the paths defined in the settings. The
 * GUI and the non-GUI console will also add the paths defined in the command line.
 * <p>
 * The method stringToCapability() allows retrieving a capability by name. The
 * capability need not already be defined in the toolkit. All that is need
 * is that the corresponding class is found in the added paths.
 * <p>
 * The getErrorProperties() allows retrieving the union of all error
 * properties that have been loaded and registered so far. The method might
 * return different properties for different locales. The method getCache()
 * gives the actual local cache for a given error property. To find a cache
 * the error property need not be registered but at least loaded.
 * <p>
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 * <p>
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 * <p>
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 * <p>
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public class Knowledgebase {
    private final Store store;

    public static final String OP_ON = AbstractFlag.OP_ON;
    public static final String OP_OFF = AbstractFlag.OP_OFF;
    public static final String OP_NIL = Foyer.OP_NIL;
    public static final String OP_CONS = Foyer.OP_CONS;
    public static final String OP_SUB = Foyer.OP_SUB;
    public static final String OP_NULL = AbstractFlag.OP_NULL;
    public static final String OP_TRUE = Foyer.OP_TRUE;

    /**
     * <p>Create a new knowledge base.</p>
     *
     * @param k The toolkit.
     */
    public Knowledgebase(Toolkit k) {
        Lobby l = new Lobby(k);
        Foyer foyer = (Foyer) l.getFoyer();

        store = foyer.createStore();
        store.proxy = this;
    }

    /**
     * <p>Create a new </p></o>
     *
     * @param k The toolkit.
     * @param c The caller.
     */
    public Knowledgebase(Toolkit k, Class c) {
        Lobby l = new Lobby(k);
        Foyer foyer = (Foyer) l.getFoyer();

        store = foyer.createStore(c);
        store.proxy = this;
    }

    /**
     * <p>Create a new knowledge base.</p>
     *
     * @param p The parent.
     */
    public Knowledgebase(Knowledgebase p) {
        Store parent = (Store) p.getStore();

        store = parent.createStore();
        store.proxy = this;
    }

    /**
     * <p>Retrieve the lobby.</p>
     *
     * @return The lobby.
     */
    public final Lobby getLobby() {
        return (Lobby) store.foyer.proxy;
    }

    /**
     * <p>Retrieve the parent.</p>
     *
     * @return The parent.
     */
    public final Knowledgebase getParent() {
        Store parent = store.parent;
        return (parent != null ? (Knowledgebase) parent.proxy : null);
    }

    /********************************************************************/
    /* Iterable Construction                                            */
    /********************************************************************/

    /**
     * <p>Create an iterable with a new controller.</p>
     *
     * @return The iterable.
     */
    public final Interpreter iterable() {
        return new Interpreter(this,
                new Controller(getLobby(), this));
    }

    /*******************************************************/
    /* Initialization API                                  */
    /*******************************************************/

    /**
     * <p>Init the predefined capabilities.</p>
     * <p>The prompt flag indicates whether user interaction is allowed.</p>
     *
     * @param inter  The call-in.
     * @param prompt The prompt flag.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public static void initKnowledgebase(Interpreter inter, boolean prompt)
            throws InterpreterMessage, InterpreterException {
        Engine en = (Engine) inter.getEngine();
        Engine backuse = en.visor.setInuse(en);
        Thread backthread = en.visor.setFence(Thread.currentThread());
        try {
            en.store.initStore((Engine) inter.getEngine(), prompt);
            en.visor.setFence(backthread);
            en.visor.setInuse(backuse);
        } catch (EngineMessage x) {
            en.visor.setFence(backthread);
            en.visor.setInuse(backuse);
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            en.visor.setFence(backthread);
            en.visor.setInuse(backuse);
            throw new InterpreterException(x);
        }
    }

    /**
     * <p>Init the predefined capabilities.</p>
     * <p>No user interaction is performed.</p>
     *
     * @param inter The call-in.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public static void initKnowledgebase(Interpreter inter)
            throws InterpreterMessage, InterpreterException {
        initKnowledgebase(inter, false);
    }

    /**
     * <p>Fini the registered capabilities.</p>
     *
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public final void finiKnowledgebase()
            throws InterpreterMessage, InterpreterException {
        try {
            store.finiStore();
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
    }

    /*******************************************************/
    /* Class Path                                          */
    /*******************************************************/

    /**
     * <p>Add a path.</p>
     *
     * @param path The path.
     * @throws InterpreterMessage Shit happens.
     */
    public final void addClassPath(String path)
            throws InterpreterMessage {
        try {
            store.addClassPath(path);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Retrieve the paths.</p>
     * <p>Returns a copy which should be treated immutable.</p>
     *
     * @return The paths.
     * @throws InterpreterMessage Shit happens.
     */
    public final String[] getClassPaths()
            throws InterpreterMessage {
        try {
            return store.snapshotClassPaths();
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Add a file extension.</p>
     *
     * @param e  The file extension.
     * @param fe The type and mime.
     */
    public final void addFileExtension(String e, FileExtension fe) {
        store.addFileExtension(e, fe);
    }

    /**
     * <p>Remove a file extension.</p>
     *
     * @param e The file extension.
     */
    public final void removeFileExtension(String e) {
        store.removeFileExtension(e);
    }

    /**
     * <p>Retrieve the file extensions.</p>
     * <p>Returns a copy which should be treated immutable.</p>
     *
     * @return The file extensions and their type.
     */
    public final MapEntry<String, FileExtension>[] getFileExtensions() {
        return store.snapshotFileExtensions();
    }

    /*******************************************************/
    /* Capabilities                                        */
    /*******************************************************/

    /**
     * <p>Find a capability.</p>
     *
     * @param name The name.
     * @return The capability.
     * @throws InterpreterMessage Shit happens.
     */
    public final Capability stringToCapability(String name)
            throws InterpreterMessage, InterpreterException {
        Store store = (Store) getStore();
        AbstractFactory factory = store.foyer.getFactory();
        AbstractBranch branch;
        try {
            branch = factory.getReflection().stringToBranch(name, store.loader);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
        Capability capa = (Capability) branch.proxy;
        if (capa == null)
            throw new NullPointerException("capability missing");
        return capa;
    }

    /***********************************************************/
    /* Error Properties                                        */
    /***********************************************************/

    /**
     * <p>Retrieve the error properties.</p>
     *
     * @param locale The locale.
     * @return The error properties.
     * @throws IOException Shit happens.
     */
    public final Properties getErrorProperties(Locale locale)
            throws IOException {
        return EngineMessage.getErrorLang(locale, store);
    }

    /**
     * <p>Retrieve the properties cache for the given resource bundle.</p>
     *
     * @param key The adr of the resource bundle.
     * @return The properties cache, or null.
     */
    public final HashMap<String, Properties> getCache(String key) {
        return EngineMessage.getCache(key, store);
    }

    /***********************************************************/
    /* For Internal Use Only                                   */
    /***********************************************************/

    /**
     * <p>Retrieve the store.</p>
     *
     * @return The store.
     */
    public final Object getStore() {
        return store;
    }

    /**
     * <p>Retrieve the class loader.</p>
     *
     * @return The class loader.
     */
    public ClassLoader getLoader() {
        return store.getLoader();
    }

}
