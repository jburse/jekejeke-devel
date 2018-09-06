package jekpro.model.pretty;

import jekpro.model.molec.EngineMessage;

/**
 * <p>An child store specialization of abstract stores.</p>
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
public final class StoreChild extends AbstractStore {

    /**
     * <p>Create a new store.</p>
     *
     * @param p The parent.
     */
    public StoreChild(AbstractStore p) {
        super(p.foyer, p);
        loader = p.loader;
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

        foyer.notifyFixvers(this);
    }

}