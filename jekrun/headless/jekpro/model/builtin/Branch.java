package jekpro.model.builtin;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.reflect.*;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.term.SkelAtom;
import matula.util.config.AbstractBundle;
import matula.util.wire.FileExtension;
import matula.util.wire.LangProperties;

import java.util.Locale;
import java.util.Properties;

/**
 * <p>Refinement of a branch for the Jekejeke Runtime.</p>
 *
 * @author Copyright 2011-2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.2 (a fast and small prolog interpreter)
 */
public abstract class Branch extends AbstractBranch {
    public final static String OP_USER = "user";
    public final static String OP_SYSTEM = "system";

    /**
     * <p>Create the branch.</p>
     */
    public Branch() {
        setRelease("JEKPR515RUN1");
        Properties resources = LangProperties.getLang(Branch.class, "description", Locale.getDefault());
        setLang(resources.getProperty(AbstractBundle.PROP_PRODUCT_LANG));
        setFlags(AbstractBranch.MASK_BRAN_NOTR);
        setArchiveRoots(new String[]{"matula/", "derek/", "jekpro/"});
        setPrologFlags(Flag.DEFAULT);
        setThreadFlags(FlagThread.DEFAULT);
        setOperProps(PropertyOperator.DEFAULT);
        setSrcProps(PropertySource.DEFAULT);
        addPredProps(PropertyPredicate.DEFAULT);
        setCallableProps(PropertyCallable.DEFAULT);
        addStreamProps(PropertyStream.DEFAULT);
    }

    /**
     * <p>Init the store with this branch.</p>
     *
     * @param en The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public void initBranch(Engine en,
                           boolean prompt, boolean system)
            throws EngineMessage, EngineException {
        /* init the branch */
        super.initBranch(en, prompt, system);

        Store root = (Store) en.store.foyer.getRoot();
        root.addFileExtension(".class", new FileExtension(FileExtension.MASK_USES_BNRY, "application/java"));

        root.addFileExtension(".px", new FileExtension(FileExtension.MASK_USES_TEXT
                | FileExtension.MASK_DATA_ECRY, "text/prolog"));
        root.addFileExtension(".p", new FileExtension(FileExtension.MASK_USES_TEXT, "text/prolog"));
        root.addFileExtension(".pl", new FileExtension(FileExtension.MASK_USES_TEXT, "text/prolog"));
        root.addFileExtension(".pro", new FileExtension(FileExtension.MASK_USES_TEXT, "text/prolog"));

        root.addFileExtension(".propertiesx", new FileExtension(FileExtension.MASK_USES_RSCS
                | FileExtension.MASK_DATA_ECRY, "application/properties"));
        root.addFileExtension(".properties", new FileExtension(FileExtension.MASK_USES_RSCS, "application/properties"));
        root.addFileExtension(".txtx", new FileExtension(FileExtension.MASK_USES_RSCS
                | FileExtension.MASK_DATA_ECRY, "text/plain"));
        root.addFileExtension(".txt", new FileExtension(FileExtension.MASK_USES_RSCS, "text/plain"));
        root.addFileExtension(".gifx", new FileExtension(FileExtension.MASK_USES_RSCS
                | FileExtension.MASK_DATA_ECRY, "image/gif"));
        root.addFileExtension(".gif", new FileExtension(FileExtension.MASK_USES_RSCS, "image/gif"));
        root.addFileExtension(".htmx", new FileExtension(FileExtension.MASK_USES_RSCS
                | FileExtension.MASK_DATA_ECRY, "text/html"));
        root.addFileExtension(".htm", new FileExtension(FileExtension.MASK_USES_RSCS, "text/html"));

        en.store.foyer.ATOM_COMMA = new SkelAtom(Foyer.OP_COMMA, en.store.getRootSystem());
        en.store.foyer.ATOM_TRUE = new SkelAtom(Foyer.OP_TRUE, en.store.getRootSystem());
        en.store.foyer.ATOM_SLASH = new SkelAtom(Foyer.OP_SLASH, en.store.getRootSystem());
        en.store.foyer.ATOM_SEMICOLON = new SkelAtom(Foyer.OP_SEMICOLON, en.store.getRootSystem());
        en.store.foyer.ATOM_CONDITION = new SkelAtom(Foyer.OP_CONDITION, en.store.getRootSystem());
        en.store.foyer.ATOM_SOFT_CONDITION = new SkelAtom(Foyer.OP_SOFT_CONDITION, en.store.getRootSystem());
        en.store.foyer.ATOM_TURNSTYLE = new SkelAtom(Foyer.OP_TURNSTILE, en.store.getRootSystem());

        en.store.foyer.ATOM_SYS_ALTER = new SkelAtom(Foyer.OP_SYS_ALTER, en.store.getRootSystem());
        en.store.foyer.ATOM_SYS_GUARD = new SkelAtom(Foyer.OP_SYS_GUARD, en.store.getRootSystem());
        en.store.foyer.ATOM_SYS_SEQUEN = new SkelAtom(Foyer.OP_SYS_SEQUEN, en.store.getRootSystem());
        en.store.foyer.ATOM_SYS_BEGIN = new SkelAtom(Foyer.OP_SYS_BEGIN, en.store.getRootSystem());
        en.store.foyer.ATOM_SYS_COMMIT = new SkelAtom(Foyer.OP_SYS_COMMIT, en.store.getRootSystem());
        en.store.foyer.ATOM_SYS_SOFT_BEGIN = new SkelAtom(Foyer.OP_SYS_SOFT_BEGIN, en.store.getRootSystem());
        en.store.foyer.ATOM_SYS_SOFT_COMMIT = new SkelAtom(Foyer.OP_SYS_SOFT_COMMIT, en.store.getRootSystem());
        en.store.foyer.ATOM_CALL = new SkelAtom(Foyer.OP_CALL, en.store.getRootSystem());

        en.store.foyer.ATOM_COMPARE = new SkelAtom(Foyer.OP_COMPARE, en.store.getRootSystem());

        loadSystem("jekpro/model/builtin/special.p", en, new InterfaceInit() {
            public void init(AbstractSource scope, Engine en)
                    throws EngineMessage, EngineException {
                addResource("matula/util/regex/scanner.properties", scope, en);
                addResource("jekpro/reference/bootload/reader.properties", scope, en);
                addResource("jekpro/reference/runtime/engine.properties", scope, en);
                registerSpecials(scope, en);
            }
        });
        loadSystem("jekpro/model/builtin/body.p", en);
        loadSystem("jekpro/model/builtin/control.p", en);
    }

    /**
     * Register the special specials.
     *
     * @param scope The scope.
     * @param en    The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void registerSpecials(AbstractSource scope, Engine en)
            throws EngineMessage, EngineException {
        registerProvable(new SkelAtom("set_source_property", scope), 2,
                new SpecialSource(SpecialSource.SPECIAL_SET_SOURCE_PROPERTY), en);
        registerProvable(new SkelAtom("reset_source_property", scope), 2,
                new SpecialSource(SpecialSource.SPECIAL_RESET_SOURCE_PROPERTY), en);
        registerProvable(new SkelAtom("set_oper_property", scope), 2,
                new SpecialOper(SpecialOper.SPECIAL_SET_OPER_PROPERTY), en);
        registerProvable(new SkelAtom("sys_special", scope), 3,
                new SpecialSpecial(SpecialSpecial.SPECIAL_SYS_SPECIAL), en);
        registerProvable(new SkelAtom("sys_check_style_body", scope), 1,
                new SpecialSpecial(SpecialSpecial.SPECIAL_SYS_CHECK_STYLE_BODY), en);
        registerProvable(new SkelAtom("sys_neutral_oper", scope), 1,
                new SpecialOper(SpecialOper.SPECIAL_SYS_NEUTRAL_OPER), en);
        registerProvable(new SkelAtom("sys_callable_property_chk", scope), 3,
                new SpecialCall(SpecialCall.SPECIAL_SYS_CALLABLE_PROPERTY_CHK), en);
        registerProvable(new SkelAtom("sys_boot_stream", scope), 1,
                new SpecialLoad(SpecialLoad.SPECIAL_SYS_BOOT_STREAM), en);
    }

    /**
     * <p>Define a built-in based on name and length.</p>
     * <p>The store is passed via the engine.</p>
     *
     * @param sa    The predicate name.
     * @param arity The predicate length.
     * @param del   The delegate.
     * @param en    The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void registerProvable(SkelAtom sa, int arity,
                                         AbstractDelegate del,
                                         Engine en)
            throws EngineMessage, EngineException {
        CachePredicate cp = CachePredicate.getPredicateDefined(sa,
                arity, en, CachePredicate.MASK_CACH_CRTE);
        Predicate pick = cp.pick;
        Predicate.definePredicate(pick, del);
        Predicate.checkPredicateBody(pick, sa, en);
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
        Store root = (Store) store.foyer.getRoot();
        root.removeFileExtension(".class");

        root.removeFileExtension(".px");
        root.removeFileExtension(".p");
        root.removeFileExtension(".pl");
        root.removeFileExtension(".pro");

        root.removeFileExtension(".propertiesx");
        root.removeFileExtension(".properties");
        root.removeFileExtension(".txtx");
        root.removeFileExtension(".txt");
        root.removeFileExtension(".gifx");
        root.removeFileExtension(".gif");
        root.removeFileExtension(".htmx");
        root.removeFileExtension(".htm");

        store.foyer.ATOM_COMMA = null;
        store.foyer.ATOM_TRUE = null;
        store.foyer.ATOM_SLASH = null;
        store.foyer.ATOM_SEMICOLON = null;
        store.foyer.ATOM_CONDITION = null;
        store.foyer.ATOM_SOFT_CONDITION = null;
        store.foyer.ATOM_TURNSTYLE = null;

        store.foyer.ATOM_SYS_ALTER = null;
        store.foyer.ATOM_SYS_GUARD = null;
        store.foyer.ATOM_SYS_SEQUEN = null;
        store.foyer.ATOM_SYS_BEGIN = null;
        store.foyer.ATOM_SYS_COMMIT = null;
        store.foyer.ATOM_SYS_SOFT_BEGIN = null;
        store.foyer.ATOM_SYS_SOFT_COMMIT = null;
        store.foyer.ATOM_CALL = null;

        store.foyer.ATOM_COMPARE = null;

        /* reset the branch */
        super.finiBranch(store, system);
    }

}
