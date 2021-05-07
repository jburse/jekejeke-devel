package jekmin.model.builtin;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.InterfaceInit;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import jekpro.model.rope.Directive;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.config.BaseBundle;
import matula.util.wire.LangProperties;

import java.util.Locale;
import java.util.Properties;

/**
 * <p>Implementation of an abstract branch.</p>
 *
 * @author Copyright 2011-2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Minlog 0.1.0 (minimal logic capability)
 */
final class BranchForward extends AbstractBranch {
    static BranchForward DEFAULT = new BranchForward();

    /**
     * <p>Retrieve the parameters of this branch.</p>
     *
     * @return The parameters of this brach.
     */
    public String[] getParams() {
        return BaseBundle.VOID_LIST;
    }

    /**
     * <p>Create the minimal logic branch.</p>
     */
    private BranchForward() {
        setRelease("JEKPR512MIN1");
        setDescription(DescriptionForward.DEFAULT);
        Properties resources = LangProperties.getLang(BranchForward.class, "description", Locale.getDefault());
        setLang(resources.getProperty(BaseBundle.PROP_PRODUCT_LANG));
//        setName(System.getProperty("user.name"));
//        setDate(BaseBundle.getInstallDate(SpecialAttr.class, "dummy.px"));
//        setFlags(AbstractBranch.MASK_BRAN_NOTR | BaseBundle.MASK_BNDL_NACT);
        setFlags(AbstractBranch.MASK_BRAN_NOTR);
        setArchiveRoots(new String[]{"jekmin/"});
        setPrologFlags(FlagForward.DEFAULT);
    }

    /**
     * <p>Init the store with this branch.</p>
     *
     * @param en The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final void initBranch(Engine en,
                                 boolean prompt, boolean system)
            throws EngineMessage, EngineException {
        super.initBranch(en, prompt, system);

        Store root = (Store) en.store.foyer.getRoot();
        root.system.addFix("jekmin/reference", AbstractSource.MASK_USES_LIBR);
        root.system.addFix("jekmin/frequent", AbstractSource.MASK_USES_LIBR);

        /* set the attribute hook call */
        Object body = new SkelCompound(
                new SkelAtom(Foyer.OP_CALL, en.store.getRootSystem()),
                SkelVar.valueOf(0), SkelVar.valueOf(1), SkelVar.valueOf(2));
        Directive dire = Directive.createDirective(AbstractDefined.MASK_DEFI_CALL, en);
        dire.bodyToInterSkel(body, en, true);
        en.store.foyer.CLAUSE_HOOK = dire;

        loadSystem("jekmin/reference/experiment/dummy.p", en, new InterfaceInit() {
            public void init(AbstractSource scope, Engine en)
                    throws EngineMessage, EngineException {
                addResource("jekmin/reference/experiment/minlog.properties", scope, en);
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
        root.system.removeFix("jekmin/reference", AbstractSource.MASK_USES_LIBR);
        root.system.removeFix("jekmin/frequent", AbstractSource.MASK_USES_LIBR);

        /* reset the attribute hook call */
        store.foyer.CLAUSE_HOOK = null;

        super.finiBranch(store, system);
    }

}
