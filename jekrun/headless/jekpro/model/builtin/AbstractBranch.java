package jekpro.model.builtin;

import matula.util.misc.LicenseError;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.LoadForce;
import jekpro.model.rope.LoadOpts;
import jekpro.model.rope.Operator;
import jekpro.model.rope.Resource;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.foreign.LookupResource;
import jekpro.tools.foreign.Tracking;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.TermAtomic;
import matula.comp.sharik.AbstractActivator;
import matula.comp.sharik.AbstractTracking;
import matula.util.config.AbstractBundle;
import matula.util.config.AbstractDescription;
import matula.util.data.ListArray;
import matula.util.data.MapHash;
import matula.util.data.MapHashLink;
import matula.util.wire.FileExtension;

import java.io.IOException;
import java.util.Date;

/**
 * <p>This class provides an abstract branch. Besides the properties API
 * and some capabilities primitives, it also provides a library loader.</p>
 *
 * @author Copyright 2011-2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.8.9 (a fast and small prolog interpreter)
 */
public abstract class AbstractBranch extends AbstractBundle {
    public static final int MASK_BRAN_NOTR = 0x00000100;

    public final static Object[] FALSE_PROPERTY = new Object[0];
    public final static Predicate[] FALSE_PREDS = new Predicate[0];

    public final static String OP_NEEDS_ACT = "needs_act";
    public final static String OP_ACT_STATUS = "act_status";
    public final static String OP_BUNDLE_PATH = "bundle_path";
    public final static String OP_EXPIRATION_DATE = "expiration_date";
    public final static String OP_LANGUAGE_CODE = "language_code";
    public final static String OP_INSTALL_CODE = "install_code";
    public final static String OP_LICENSE_CODE = "license_code";
    public final static String OP_SYS_NOTRACE = "sys_notrace";

    private final static String[] OP_BRANCH_PROPS = {
            OP_NEEDS_ACT,
            OP_ACT_STATUS,
            OP_EXPIRATION_DATE,
            OP_BUNDLE_PATH,
            OP_SYS_NOTRACE,
            OP_LANGUAGE_CODE,
            OP_INSTALL_CODE,
            OP_LICENSE_CODE};

    public Object proxy;
    private MapHash<String, AbstractFlag<Engine>> prologflags;
    private MapHash<String, AbstractFlag<Thread>> threadflags;
    private MapHash<StoreKey, AbstractProperty<Store>> storeprops;
    private MapHash<StoreKey, AbstractProperty<StackElement>> frameprops;
    private MapHashLink<StoreKey, AbstractProperty<Operator>> operprops;
    private MapHash<StoreKey, AbstractProperty<AbstractSource>> srcprops;
    private MapHash<StoreKey, AbstractProperty<Object>> callprops;
    private final ListArray<MapHashLink<StoreKey, AbstractProperty<Predicate>>> predprops
            = new ListArray<>();
    private final ListArray<MapHash<StoreKey, AbstractProperty<Object>>> streamprops
            = new ListArray<>();

    private String[] archiveroots;

    /**
     * <p>Retrieve the archive roots.</p>
     *
     * @return The archive roots.
     */
    public String[] getArchiveRoots() {
        return archiveroots;
    }

    /**
     * <p>Set the archive roots.</p>
     *
     * @param a The archive roots.
     */
    public void setArchiveRoots(String[] a) {
        archiveroots = a;
    }

    /**
     * <p>Create the tracking.</p>
     *
     * @return The tracking.
     */
    public final AbstractTracking createTracking() {
        Tracking tracking = new Tracking();
        if ((getFlags() & AbstractBundle.MASK_BNDL_NACT) == 0) {
            tracking.setLicense("DIST");
        } else {
            tracking.setLicense("");
        }
        return tracking;
    }

    /**********************************************************7
     /* Prolog & Thread Flags                                 */
    /**********************************************************7

     /**
     * <p>Retrieve the prolog flags.</p>
     *
     * @return The prolog flags.
     */
    public MapHash<String, AbstractFlag<Engine>> getPrologFlags() {
        return prologflags;
    }

    /**
     * <p>Set the prolog flags.</p>
     *
     * @param f The prolog flags.
     */
    public void setPrologFlags(MapHash<String, AbstractFlag<Engine>> f) {
        prologflags = f;
    }

    /**
     * <p>Retrieve the thread flags.</p>
     *
     * @return The thread flags.
     */
    public MapHash<String, AbstractFlag<Thread>> getThreadFlags() {
        return threadflags;
    }

    /**
     * <p>Set the thread flags.</p>
     *
     * @param f The thread flags.
     */
    public void setThreadFlags(MapHash<String, AbstractFlag<Thread>> f) {
        threadflags = f;
    }

    /**********************************************************7
     /* Store & Frame Properties                               */
    /**********************************************************7

     /**
     * <p>Retrieve the store properties.</p>
     *
     * @return The store properties.
     */
    public MapHash<StoreKey, AbstractProperty<Store>> getStoreProps() {
        return storeprops;
    }

    /**
     * <p>Set the store properties.</p>
     *
     * @param f The store properties.
     */
    public void setStoreProps(MapHash<StoreKey, AbstractProperty<Store>> f) {
        storeprops = f;
    }

    /**
     * <p>Retrieve the frame properties.</p>
     *
     * @return The frame properties.
     */
    public MapHash<StoreKey, AbstractProperty<StackElement>> getFrameProps() {
        return frameprops;
    }

    /**
     * <p>Set the frame properties.</p>
     *
     * @param f The frame properties.
     */
    public void setFrameProps(MapHash<StoreKey, AbstractProperty<StackElement>> f) {
        frameprops = f;
    }

    /**
     * <p>Retrieve the operator properties.</p>
     *
     * @return The operator properties.
     */
    public MapHashLink<StoreKey, AbstractProperty<Operator>> getOperProps() {
        return operprops;
    }

    /**
     * <p>Set the operator properties.</p>
     *
     * @param f The operator properties.
     */
    public void setOperProps(MapHashLink<StoreKey, AbstractProperty<Operator>> f) {
        operprops = f;
    }

    /**
     * <p>Retrieve the source properties.</p>
     *
     * @return The source properties.
     */
    public MapHash<StoreKey, AbstractProperty<AbstractSource>> getSrcProps() {
        return srcprops;
    }

    /**
     * <p>Set the source properties.</p>
     *
     * @param f The source properties.
     */
    public void setSrcProps(MapHash<StoreKey, AbstractProperty<AbstractSource>> f) {
        srcprops = f;
    }

    /**
     * <p>Retrieve the predicate properties.</p>
     *
     * @return The predicate properties.
     */
    public ListArray<MapHashLink<StoreKey, AbstractProperty<Predicate>>> getPredProps() {
        return predprops;
    }

    /**
     * <p>Set the predicate properties.</p>
     *
     * @param f The predicate properties.
     */
    public void addPredProps(MapHashLink<StoreKey, AbstractProperty<Predicate>> f) {
        if (f == null)
            throw new NullPointerException("properties missing");
        predprops.addFirst(f);
    }

    /**
     * <p>Retrieve the atom props.</p>
     *
     * @return The atom props.
     */
    public MapHash<StoreKey, AbstractProperty<Object>> getCallableProps() {
        return callprops;
    }

    /**
     * <p>Set the atom props.</p>
     *
     * @param r The atom props.
     */
    public void setCallableProps(MapHash<StoreKey, AbstractProperty<Object>> r) {
        callprops = r;
    }

    /**
     * <p>Retrieve the stream props.</p>
     *
     * @return The stream props.
     */
    public ListArray<MapHash<StoreKey, AbstractProperty<Object>>> getStreamProps() {
        return streamprops;
    }

    /**
     * <p>Set the stream props.</p>
     *
     * @param r The stream props.
     */
    public void addStreamProps(MapHash<StoreKey, AbstractProperty<Object>> r) {
        if (r == null)
            throw new NullPointerException("properties missing");
        streamprops.add(r);
    }

    /**********************************************************7
     /* Branch Lifecycle                                      */
    /**********************************************************7

     /**
     * <p>Init the store with this branch.</p>
     *
     * @param en     The engine.
     * @param prompt The prompt flag.
     * @param system The system flag.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public void initBranch(Engine en, boolean prompt, boolean system)
            throws EngineMessage, EngineException {
        checkNotToolkit(en.store, system);
        AbstractTracking tracking;
        try {
            tracking = en.store.foyer.putBundle(this, prompt, en);
        } catch (LicenseError x) {
            throw new EngineMessage(EngineMessage.licenseError(x.getMessage()));
        } catch (EngineMessage x) {
            throw x;
        } catch (EngineException x) {
            throw x;
        } catch (Exception x) {
            throw new IllegalArgumentException("illegal exception", x);
        }

        if (!(tracking instanceof Tracking))
            return;
        String[] roots = getArchiveRoots();
        if (roots == null)
            return;
        ListArray<String> res = new ListArray<>();
        try {
            for (int i = 0; i < roots.length; i++)
                rootToAbsolute(res, roots[i], en.store.foyer);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
        String[] uris = new String[res.size()];
        res.toArray(uris);
        ((Tracking) tracking).setArchiveURIs(uris);
    }

    /**
     * <p>Fini the store from this branch.</p>
     *
     * @param store  The store.
     * @param system The system flag.
     * @throws EngineMessage Shit happens.
     */
    public void finiBranch(Store store, boolean system)
            throws EngineMessage, EngineException {
        checkNotToolkit(store, system);
        store.clearStore(this);
        AbstractTracking tracking = store.foyer.getTracking(this);
        if (tracking instanceof Tracking)
            ((Tracking) tracking).clearCanonCache();
        store.foyer.removeBundle(this);
    }

    /***************************************************************************/
    /* Branch Properties                                                       */
    /***************************************************************************/

    /**
     * <p>Retrieve a branch property names.</p>
     *
     * @return The branch property names.
     */
    public static String[] getProperties() {
        return OP_BRANCH_PROPS;
    }

    /**
     * <p>Retrieve a branch property.</p>
     *
     * @param prop  The property name.
     * @param foyer The foyer.
     * @return The property value or null.
     */
    public Object getProperty(String prop, Foyer foyer) {
        if (OP_NEEDS_ACT.equals(prop)) {
            boolean val = (getFlags() & AbstractBundle.MASK_BNDL_NACT) != 0;
            return Flag.booleToAtom(val);
        } else if (OP_ACT_STATUS.equals(prop)) {
            AbstractTracking tracking = foyer.getTracking(this);
            String val = (tracking != null ? tracking.getError() : EngineMessage.OP_LICENSE_TRACKING_LOST);
            return new SkelAtom(val);
        } else if (OP_EXPIRATION_DATE.equals(prop)) {
            AbstractTracking tracking = foyer.getTracking(this);
            Date val = (tracking != null ? tracking.getExpiration() : null);
            return TermAtomic.normBigInteger(val != null ? val.getTime() : 0);
        } else if (OP_BUNDLE_PATH.equals(prop)) {
            AbstractActivator activator = foyer.getFramework().getActivator();
            ClassLoader loader = foyer.getRoot().getLoader();
            String path = activator.getBundlePath(loader, foyer.getFramework(), getDescription());
            return new SkelAtom(path);
        } else if (OP_SYS_NOTRACE.equals(prop)) {
            boolean val = (getFlags() & AbstractBranch.MASK_BRAN_NOTR) != 0;
            return Flag.booleToAtom(val);
        } else if (OP_LANGUAGE_CODE.equals(prop)) {
            String lang = getLang();
            return new SkelAtom(lang != null ? lang : "");
        } else if (OP_INSTALL_CODE.equals(prop)) {
            AbstractActivator activator = foyer.getFramework().getActivator();
            String install = activator.getInstall(foyer, getDescription());
            return new SkelAtom(install != null ? install : "");
        } else if (OP_LICENSE_CODE.equals(prop)) {
            AbstractTracking tracking = foyer.getTracking(this);
            String license = (tracking != null ? tracking.getLicense() : "");
            return new SkelAtom(license != null ? license : "");
        }
        return null;
    }

    /******************************************************/
    /* Init Utility                                       */
    /******************************************************/

    /**
     * <p>Load a Prolog text.</p>
     *
     * @param path The library path.
     * @param en   The engine.
     * @param init The interface init.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static void loadSystem(String path,
                                  Engine en, InterfaceInit init)
            throws EngineMessage, EngineException {
        LoadOpts opts = new LoadOpts();
        opts.setFlags(opts.getFlags() | LoadOpts.MASK_LOAD_COND);
        opts.setFlags(opts.getFlags() | LoadForce.MASK_LOAD_AUTO);
        opts.setFlags(opts.getFlags() | LoadOpts.MASK_LOAD_MASK);
        opts.setInit(init);
        String key = AbstractBranch.findPathLibrary(path, en);

        AbstractSource system = en.store.getRootSystem();
        opts.makeLoad(system, key, en);
    }

    /**
     * <p>Load a Prolog text.</p>
     *
     * @param lib The library.
     * @param en  The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static void loadSystem(String lib, Engine en)
            throws EngineMessage, EngineException {
        loadSystem(lib, en, null);
    }

    /**
     * <p>Add an error resource.</p>
     *
     * @param path  The library path.
     * @param scope The call-site, non-null.
     * @param en    The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void addResource(String path, AbstractSource scope,
                                   Engine en)
            throws EngineMessage, EngineException {
        LoadOpts opts = new LoadOpts();
        opts.setFlags(opts.getFlags() | LoadOpts.MASK_LOAD_COND);
        opts.setFlags(opts.getFlags() | LoadForce.MASK_LOAD_RSCS);
        opts.setFlags(opts.getFlags() | LoadOpts.MASK_LOAD_MASK);
        String key = AbstractBranch.findPathLibrary(path, en);
        opts.makeLoad(scope, key, en);

        Resource rsc = en.store.foyer.createResource(key);
        scope.addResource(rsc);
    }

    /**
     * <p>Find path in the class loaders resources.</p>
     * <p>Trying encrypted versions and throwing an existence error.</p>
     *
     * @param path The path, in slash notation.
     * @param en   The engine.
     * @return The source key, or null.
     */
    public static String findPathLibrary(String path, Engine en)
            throws EngineMessage {
        try {
            String key = LookupResource.findResource(path + FileExtension.ENCRYPTION_MARK, en.store);
            if (key != null)
                return key;
            key = LookupResource.findResource(path, en.store);
            if (key != null)
                return key;
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_LIBRARY, new SkelAtom(path)));
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /*******************************************************************/
    /* Not Toolkit                                                     */
    /*******************************************************************/

    /**
     * <p>Assure that the branch is not toolkit.</p>
     *
     * @param store  The store.
     * @param system The system flag.
     * @throws EngineMessage Shit happens.
     */
    public void checkNotToolkit(Store store, boolean system)
            throws EngineMessage {
        if (!isNotToolkit(store, system)) {
            AbstractFactory factory = store.foyer.getFactory();
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_TOOLKIT_CAPA,
                    new SkelAtom(factory.getReflection().branchToString(this))));
        }
    }

    /**
     * <p>Check whether the branch is not toolkit.</p>
     *
     * @param store  The store.
     * @param system The system flag.
     * @return True if branch is not toolkit, otherwise false.
     */
    private boolean isNotToolkit(Store store, boolean system) {
        AbstractBranch[] branches = (system ? null : store.foyer.getFactory().getInitBranches());
        if (branches != null) {
            for (int i = 0; i < branches.length; i++) {
                if (branches[i] == this)
                    return false;
            }
        }
        return true;
    }

    /***************************************************************/
    /* Capability Properties                                       */
    /***************************************************************/

    /**
     * <p>Precompute the uris of a root.</p>
     *
     * @param res   The target list.
     * @param root  The root.
     * @param foyer The enforced.
     * @throws IOException Shit happens.
     */
    public void rootToAbsolute(ListArray<String> res, String root, Foyer foyer)
            throws IOException {
        ClassLoader loader = foyer.getRoot().getLoader();
        String aspect = foyer.getFramework().getRuntime().getAspect();
        if (root.equals(getDescription().getMainRoot())) {
            Tracking.rootToAbsoluteCheck(res, root, loader,
                    AbstractDescription.MODEL_DEFAULT + ".propertiesx");
            Tracking.rootToAbsoluteCheck(res, root, loader,
                    AbstractDescription.PLATFORM_DIR + aspect + AbstractDescription.PLATFORM_FILE + ".propertiesx");
        } else {
            Tracking.rootToAbsoluteCheck(res, root, loader, "root.propertiesx");
        }
    }

}
