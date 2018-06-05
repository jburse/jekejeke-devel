package jekpro.model.pretty;

import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;

/**
 * <p>An elder store specialization of abstract stores.</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class StoreElder extends AbstractStore {

    /**
     * <p>Create a new store.</p>
     *
     * @param f The foyer.
     */
    public StoreElder(Foyer f) {
        super(f, null);
        loader = getClass().getClassLoader();
    }

    /**
     * <p>Init the store.</p>
     *
     * @param en     The engine.
     * @param prompt The prompt flag.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public void initStore(Engine en, boolean prompt)
            throws EngineMessage, EngineException {
        super.initStore(en, prompt);

        foyer.initFoyer(en, prompt);
    }

    /**
     * <p>Fini the store.</p>
     *
     * @param store The store.
     * @throws EngineMessage Shit happens.
     */
    public void finiStore(AbstractStore store)
            throws EngineMessage, EngineException {
        foyer.finiFoyer(store);

        super.finiStore(store);
    }

    /**
     * <p>Add a path.</p>
     *
     * @param path The path.
     * @throws EngineMessage Shit happens.
     */
    public void addClassPath(String path)
            throws EngineMessage {
        super.addClassPath(path);

        foyer.clearCanonCache();
        foyer.notifyFixvers(this);
    }

    /**
     * <p>Add a file extension.</p>
     *
     * @param ext  The file extension.
     * @param type The type.
     */
    public void addFileExtension(String ext, int type) {
        super.addFileExtension(ext, type);

        foyer.clearCanonCache();
        foyer.notifyFixvers(this);
    }

}