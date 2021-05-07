package jekdev.model.builtin;

import jekdev.reference.debug.PropertyTracePredicate;
import jekdev.reference.debug.PropertyTraceStream;
import jekdev.reference.inspection.PropertyTraceCallable;
import jekdev.reference.inspection.PropertyTraceFrame;
import jekdev.reference.inspection.PropertyTraceStore;
import jekdev.reference.system.FlagTrace;
import jekdev.reference.system.FlagTraceThread;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.InterfaceInit;
import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Store;
import matula.util.config.BaseBundle;
import matula.util.wire.LangProperties;

import java.util.Locale;
import java.util.Properties;

/**
 * <p>Implementation of an abstract branch.</p>
 *
 * @author Copyright 2010-2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.2 (a fast and small prolog interpreter)
 */
final class BranchTrace extends AbstractBranch {
    static BranchTrace DEFAULT = new BranchTrace();

    /**
     * <p>Retrieve the parameters of this branch.</p>
     *
     * @return The parameters of this brach.
     */
    public String[] getParams() {
        return BaseBundle.VOID_LIST;
    }

    /**
     * <p>Create a branch trace.</p>
     */
    private BranchTrace() {
        setRelease("JEKPR515DEV0");
        setDescription(DescriptionTrace.DEFAULT);
        Properties resources = LangProperties.getLang(BranchTrace.class, "description", Locale.getDefault());
        setLang(resources.getProperty(BaseBundle.PROP_PRODUCT_LANG));
//        setName(System.getProperty("user.name"));
//        setDate(BaseBundle.getInstallDate(SpecialDefault.class, "dummy.px"));
//        setFlags(AbstractBranch.MASK_BRAN_NOTR | BaseBundle.MASK_BNDL_NACT);
        setFlags(AbstractBranch.MASK_BRAN_NOTR);
        setArchiveRoots(new String[]{"jekdev/"});
        setPrologFlags(FlagTrace.DEFAULT);
        setThreadFlags(FlagTraceThread.DEFAULT);
        setStoreProps(PropertyTraceStore.DEFAULT);
        setFrameProps(PropertyTraceFrame.DEFAULT);
        addPredProps(PropertyTracePredicate.DEFAULT);
        setCallableProps(PropertyTraceCallable.DEFAULT);
        addStreamProps(PropertyTraceStream.DEFAULT);
    }

    /**
     * <p>Init the store with this branch.</p>
     *
     * @param en     The engine.
     * @param prompt The prompt flag.
     * @param system The system flag.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final void initBranch(Engine en,
                                 boolean prompt, boolean system)
            throws EngineMessage, EngineException {
        /* init the branch */
        super.initBranch(en, prompt, system);

        Store root = (Store) en.store.foyer.getRoot();
        root.system.addFix("jekdev/reference", AbstractSource.MASK_USES_LIBR);
        root.system.addFix("jekdev/frequent", AbstractSource.MASK_USES_LIBR);

        loadSystem("jekdev/reference/debug/dummy.p", en, new InterfaceInit() {
            public void init(AbstractSource scope, Engine en)
                    throws EngineMessage, EngineException {
                addResource("jekdev/reference/system/trace.properties", scope, en);
            }
        });
    }

    /**
     * <p>Fini the store from this branch.</p>
     *
     * @param store  The store.
     * @param system The system flag.
     * @throws EngineMessage Shit happens.
     */
    public final void finiBranch(Store store, boolean system)
            throws EngineMessage, EngineException {
        Store root = (Store) store.foyer.getRoot();
        root.system.removeFix("jekdev/reference", AbstractSource.MASK_USES_LIBR);
        root.system.removeFix("jekdev/frequent", AbstractSource.MASK_USES_LIBR);

        super.finiBranch(store, system);
    }

}
