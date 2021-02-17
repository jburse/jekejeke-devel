package jekpro.model.pretty;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.Branch;
import jekpro.model.inter.Engine;
import jekpro.model.inter.LocalBlocking;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.LoadOpts;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.foreign.Tracking;
import matula.comp.sharik.AbstractTracking;
import matula.util.config.AbstractBundle;
import matula.util.config.AbstractFramework;
import matula.util.config.AbstractRecognizer;
import matula.util.config.FileExtension;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;
import matula.util.system.ForeignUri;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.Properties;

/**
 * <p>An object that encapsulates a knowledge base. An knowledge base
 * is a set of source, resource and canoncache. Source contain predicates
 * and operators.</p>
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
public class Store extends AbstractRecognizer {
    public final static int DEFAULT_MAX_STACK = 10;

    public Object proxy;
    public final Foyer foyer;
    public final Store parent;
    private final MapHash<String, AbstractSource> source = new MapHash<>();
    private MapEntry<String, AbstractSource>[] cachesource;
    public ClassLoader loader;
    private String[] cachepaths;
    private final ListArray<AbstractSource> roots = new ListArray<>();
    private AbstractSource[] cacheroots;
    public AbstractSource user;
    public String base;
    public AbstractSource system;
    public ListArray<LocalBlocking> privates;
    public String name;
    public Object belongsto;

    /* For autonumbering anonymous stores. */
    private static int storeInitNumber;

    private static synchronized int nextStoreNum() {
        return storeInitNumber++;
    }

    /**
     * <p>Create a new store.</p>
     *
     * @param f The foyer.
     * @param p The parent store.
     */
    protected Store(Foyer f, Store p) {
        parent = p;
        name = "Store-" + nextStoreNum();

        foyer = f;
        foyer.addStore(this);
        try {
            user = getSourceDefined(Branch.OP_USER, false);
            system = getSourceDefined(Branch.OP_SYSTEM, false);
        } catch (EngineMessage x) {
            throw new RuntimeException("shouldn't happen");
        }
    }

    /**
     * <p>Retrieve the parent.</p>
     *
     * @return The parent.
     */
    public Store getParent() {
        return parent;
    }

    /**
     * <p>Retrieve the loader.</p>
     *
     * @return The loader.
     */
    public ClassLoader getLoader() {
        return loader;
    }

    /**
     * <p>Retrieve the root system.</p>
     *
     * @return The root system.
     */
    public AbstractSource getRootSystem() {
        return ((Store) foyer.getRoot()).system;
    }

    /**
     * <p>Determine the capability for a path.</p>
     *
     * @param path The path.
     * @return The branch, or null.
     */
    public final AbstractBundle pathToDecoder(String path) {
        MapEntry<AbstractBundle, AbstractTracking> entry;
        if (ForeignUri.sysUriIsRelative(path)) {
            entry = Tracking.relativeURIstoRoots(path, foyer);
        } else {
            entry = Tracking.absoluteURIstoRoots(path, foyer);
        }
        return (entry != null ? entry.key : null);
    }

    /*****************************************************************/
    /* Variantion Points                                             */
    /*****************************************************************/

    /**
     * <p>Init the store.</p>
     * <p>Can be overridden by subclasses.</p>
     *
     * @param en     The engine.
     * @param prompt The prompt flag.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public void initStore(Engine en, boolean prompt)
            throws EngineMessage, EngineException {
        Store store = en.store;

        store.addRoot(user);
        store.addRoot(system);

        if (parent == null) {
            AbstractFramework framework = foyer.getFramework();
            try {
                setBase(framework.getRuntime().getBase());
            } catch (IOException x) {
                throw EngineMessage.mapIOException(x);
            }
            AbstractFactory factory = foyer.getFactory();
            factory.addMemListener(foyer);
            foyer.initFoyer(en, prompt);
        } else {
            setBase(parent.getBase());
        }
    }

    /**
     * <p>Fini the store.</p>
     * <p>Can be overridden by subclasses.</p>
     *
     * @throws EngineMessage Shit happens.
     */
    public void finiStore()
            throws EngineMessage, EngineException {
        clearStore(null);

        if (parent == null) {
            foyer.finiFoyer(this);
            AbstractFactory factory = foyer.getFactory();
            factory.removeMemListener(foyer);
        }

        removeRoot(user);
        removeRoot(system);

        foyer.removeStore(this);
    }

    /**
     * <p>Fini the store from this branch.</p>
     *
     * @param branch The branch.
     * @throws EngineMessage Shit happens.
     */
    public void clearStore(AbstractBranch branch)
            throws EngineMessage, EngineException {
        LoadOpts opts = null;
        MapEntry<String, AbstractSource>[] sources = snapshotSources();
        for (int i = 0; i < sources.length; i++) {
            AbstractSource source = sources[i].value;
            if (source.getBranch() != branch)
                continue;
            if (opts == null)
                opts = new LoadOpts();
            opts.performClear(source, foyer.getFactory().tooloutput);
            removeSource(source.getPath());
        }
    }

    /*****************************************************************/
    /* AbstractSource Access                                         */
    /*****************************************************************/

    /**
     * <p>Restrieve a source by source key.</p>
     *
     * @param key The source key.
     * @return The source.
     */
    public AbstractSource getSource(String key) {
        return getSource(key, this);
    }

    /**
     * <p>Restrieve a source by source key.</p>
     *
     * @param key   The source key.
     * @param store The store.
     * @return The source.
     */
    private static AbstractSource getSource(String key, Store store) {
        while (store != null) {
            AbstractSource res = store.getSourceDeclared(key);
            if (res != null)
                return res;
            store = store.parent;
        }
        return null;
    }

    /**
     * <p>Restrieve a source by source key.</p>
     *
     * @param key The source key.
     * @return The source.
     */
    public AbstractSource getSourceDeclared(String key) {
        synchronized (this) {
            return source.get(key);
        }
    }

    /**
     * <p>Define a source by source key.</p>
     * <p>Allocate the source if its not in the parent.</p>
     *
     * @param key The source key.
     * @param rsc The rscs flag.
     * @return The source.
     * @throws EngineMessage Shit happens.
     */
    public AbstractSource getSourceDefined(String key,
                                           boolean rsc)
            throws EngineMessage {
        if (parent != null) {
            AbstractSource res = parent.getSourceCheck(key, rsc);
            if (res != null)
                return res;
        }
        return addSource(key, rsc);
    }

    /**
     * <p>Define a source by source key.</p>
     * <p>Allocate the source if its not in the parent and if its in the class path.</p>
     *
     * @param key  The source key.
     * @param rscs The rscs flag.
     * @return The source.
     * @throws EngineMessage Shit happens.
     */
    private AbstractSource getSourceCheck(String key,
                                          boolean rscs)
            throws EngineMessage {
        if (parent != null) {
            AbstractSource res = parent.getSourceCheck(key, rscs);
            if (res != null)
                return res;
        }
        if (!AbstractSource.hasKey(key, this))
            return null;
        return addSource(key, rscs);
    }

    /**
     * <p>Define a source by source key.</p>
     * <p>Allocate the source if its in the class path.</p>
     *
     * @param key  The source key.
     * @param rscs The rscs flag.
     * @return The source.
     * @throws EngineMessage Shit happens.
     */
    private AbstractSource addSource(String key, boolean rscs)
            throws EngineMessage {
        AbstractSource src;
        synchronized (this) {
            src = source.get(key);
            if (src != null)
                return src;
            src = AbstractSource.makeSource(key, rscs, this);
            src.setStore(this);
            source.add(key, src);
            cachesource = null;
        }
        if (src.getBranch() != null &&
                (src.getBranch().getFlags() & AbstractBranch.MASK_BRAN_NOTR) != 0)
            src.setBit(AbstractSource.MASK_SRC_NOTR);
        src.initSource();
        return src;
    }

    /**
     * <p>Remove a source.</p>
     *
     * @param key The source key.
     */
    public boolean removeSource(String key) {
        AbstractSource src;
        synchronized (this) {
            src = source.get(key);
            if (src == null)
                return false;
            source.remove(key);
            cachesource = null;
        }
        return true;
    }

    /**
     * <p>Compute a snapshot of the sources.</p>
     *
     * @return The snapshot.
     */
    public MapEntry<String, AbstractSource>[] snapshotSources() {
        MapEntry<String, AbstractSource>[] res = cachesource;
        if (res != null)
            return res;
        synchronized (this) {
            res = cachesource;
            if (res != null)
                return res;
            res = new MapEntry[source.size()];
            source.toArray(res);
            cachesource = res;
        }
        return res;
    }

    /*****************************************************************/
    /* Class Path                                                    */
    /*****************************************************************/

    /**
     * <p>Add a path.</p>
     *
     * @param path The path.
     * @throws LicenseError Shit happens.
     */
    public void addClassPath(String path)
            throws LicenseError {
        if (path == null)
            throw new NullPointerException("path missing");
        Object data = foyer.getApplication();
        AbstractFramework framework = foyer.getFramework();
        synchronized (this) {
            ClassLoader stop = (parent != null ? parent.loader : null);
            loader = framework.getRuntime().addURL(loader, path, stop, data);
            cachepaths = null;
        }

        if (parent == null)
            Tracking.clearCanonCaches(foyer);
        foyer.notifyFixvers(this);
    }

    /**
     * <p>Take a snapshot of the class paths.</p>
     *
     * @return The class paths.
     * @throws LicenseError Shit happens.
     */
    public String[] snapshotClassPaths() throws LicenseError {
        String[] res = cachepaths;
        if (res != null)
            return res;
        Object data = foyer.getApplication();
        AbstractFramework framework = foyer.getFramework();
        synchronized (this) {
            res = cachepaths;
            if (res != null)
                return res;
            ListArray<String> paths;
            ClassLoader stop = (parent != null ? parent.loader : null);
            paths = framework.getRuntime().getURLs(loader, stop, data);
            res = new String[paths.size()];
            paths.toArray(res);
            cachepaths = res;
        }
        return res;
    }

    /*****************************************************************/
    /* File Extensions                                               */
    /*****************************************************************/

    /**
     * <p>Add a file extension.</p>
     *
     * @param e  The file extension.
     * @param fe The type and mime..
     */
    public void addFileExtension(String e, FileExtension fe) {
        super.addFileExtension(e, fe);
        if (getParent() == null)
            Tracking.clearCanonCaches(foyer);
        foyer.notifyFixvers(this);
    }

    /**
     * <p>Remove a file extension.</p>
     *
     * @param e The file extension.
     */
    public void removeFileExtension(String e) {
        super.removeFileExtension(e);
        if (getParent() == null)
            Tracking.clearCanonCaches(foyer);
        foyer.notifyFixvers(this);
    }

    /*****************************************************************/
    /* Source Roots                                                  */
    /*****************************************************************/

    /**
     * <p>Add a source root to the store.</p>
     *
     * @param source The source.
     */
    public void addRoot(AbstractSource source) {
        synchronized (this) {
            roots.add(source);
            cacheroots = null;
        }
    }

    /**
     * <p>Remove a source root from the store.</p>
     *
     * @param source The source.
     */
    public void removeRoot(AbstractSource source) {
        synchronized (this) {
            int k = roots.indexOf(source);
            if (k < 0)
                return;
            roots.remove(k);
            cacheroots = null;
        }
    }

    /**
     * <p>Take snapshot of the source roots for the store.</p>
     *
     * @return The source roots.
     */
    public AbstractSource[] snapshotRoots() {
        AbstractSource[] res = cacheroots;
        if (res != null)
            return res;
        synchronized (this) {
            res = cacheroots;
            if (res != null)
                return res;
            res = new AbstractSource[roots.size];
            roots.toArray(res);
            cacheroots = res;
        }
        return res;
    }

    /**********************************************************/
    /* Multifile Visibility                                   */
    /**********************************************************/

    /**
     * <p>Check whether a store is inside these ancestors.</p>
     *
     * @param what The store to check.
     * @return True if the store is inside the ancestors, false otherwise.
     */
    public static boolean ancestorStore(Store what, Store store) {
        while (store != null) {
            if (store == what)
                return true;
            store = store.parent;
        }
        return false;
    }

    /*******************************************************************/
    /* Base URL Access & Modification                                  */
    /*******************************************************************/

    /**
     * <p>Retrieve the base.</p>
     *
     * @return The base.
     */
    public String getBase() {
        return base;
    }

    /**
     * <p>Set the base.</p>
     *
     * @param b The base or null.
     * @throws EngineMessage Shit happens.
     */
    public final void setBase(String b)
            throws EngineMessage {
        if (b != null && ForeignUri.sysUriIsRelative(b))
            throw new EngineMessage(EngineMessage.syntaxError(
                    EngineMessage.OP_SYNTAX_RELATIVE_PATH));
        try {
            base = (b != null ? ForeignUri.sysCanonicalUri(b) : null);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /*******************************************************************/
    /* Max Store                                                       */
    /*******************************************************************/

    /**
     * <p>Retrieve the max stack setting.</p>
     *
     * @return The max stack setting.
     */
    public int getMaxStack() {
        return DEFAULT_MAX_STACK;
    }

    /**
     * <p>Set the max stack setting.</p>
     *
     * @param m The max stack setting.
     */
    public void setMaxStack(int m) {
        /* do nothing */
    }

    /*******************************************************************/
    /* Load Properties                                                 */
    /*******************************************************************/

    /**
     * <p>Load binary properties.</p>
     *
     * @param prop  The properties.
     * @param in    The input stream.
     * @param param The param or null.
     * @throws IOException Problem reading.
     */
    public void loadBinary(Properties prop, InputStream in,
                           Object param)
            throws IOException {
        prop.load(in);
    }

    /**
     * <p>Load text properties.</p>
     *
     * @param prop   The properties.
     * @param reader The reader.
     * @param param  The param or null.
     */
    public void loadText(Properties prop, Reader reader,
                         Object param) {
        throw new IllegalArgumentException("not supported");
    }

}
