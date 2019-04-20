package jekpro.tools.term;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.call.Capability;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.call.Toolkit;
import matula.util.config.AbstractBundle;
import matula.comp.sharik.AbstractTracking;
import matula.util.data.MapEntry;
import matula.util.regex.ScannerError;

/*
 * With the introduction of hierarchical knowledge bases it became
 * apparent that we need to have a central location to manage the
 * licenses of the capabilities. Instances of this class serve as
 * such a place. A single instance is automatically created per
 * root knowledge base and then automatically used by all
 * child knowledge bases.
 *
 * The activation status of an initialized capability can be reevaluated
 * via the method checkLicense(). The overall activation status can
 * be reevaluated via the method checkLicenses(). The method
 * getCapabilities() allows querying the currently initialized
 * capabilities of the lobby.
 *
 * It is also possible to activate licenses programmatically. The
 * method activateCapability() does automatically activate a capability
 * over an internet service. If you are behind a firewall, you can
 * also query the install ID of a capability via the method
 * calcInstallID(). You can then use the install ID together with a
 * license key to obtain the license text. The license text can then
 * be stored via the method regLicenseText().
 *
 * The methods setHint() and setApplication() allow for advanced
 * configuration. The hint can indicate whether the lobby is used
 * in a GUI or non-GUI environment with interaction, or completely
 * without any interaction. The default is without any interaction.
 * The application is needed to be set in an interactive
 * Android environment.
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class Lobby {
    private final Foyer foyer;

    public final TermAtomic ATOM_CONS;
    public final TermAtomic ATOM_NIL;
    public final TermAtomic ATOM_SUB;

    /**
     * <p>Create a new lobby.</p>
     *
     * @param k The toolkit.
     */
    public Lobby(Toolkit k) {
        AbstractFactory factory = (AbstractFactory) k.getFactory();

        foyer = factory.createFoyer();
        foyer.proxy = this;

        ATOM_CONS = new TermAtomic(foyer.ATOM_CONS);
        ATOM_NIL = new TermAtomic(foyer.ATOM_NIL);
        ATOM_SUB = new TermAtomic(foyer.ATOM_SUB);
    }

    /**
     * <p>Retrieve the toolkit.</p>
     *
     * @return The toolkit.
     */
    public Toolkit getToolkit() {
        return (Toolkit) foyer.getFactory().proxy;
    }

    /**************************************************************/
    /* Activated Capabilties                                      */
    /**************************************************************/

    /**
     * <p>Check the license of the given capability.</p>
     *
     * @param c The capability.
     * @throws InterpreterMessage License error.
     */
    public void checkLicense(Capability c)
            throws InterpreterMessage {
        if (c == null)
            throw new NullPointerException("capability missing");
        AbstractBranch b = (AbstractBranch) c.getBranch();
        AbstractTracking tracking = foyer.getTracking(b);
        if (tracking == null)
            throw new InterpreterMessage(InterpreterMessage.licenseError(EngineMessage.OP_LICENSE_TRACKING_LOST));
        try {
            foyer.getFramework().getActivator().checkTracking(foyer, b, tracking);
        } catch (LicenseError x) {
            throw new InterpreterMessage(InterpreterMessage.licenseError(x.getError()));
        }
    }

    /**
     * <p>Check the licenses.</p>
     *
     * @throws InterpreterMessage License error.
     */
    public void checkLicenses()
            throws InterpreterMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = foyer.snapshotTrackings();
        try {
            foyer.getFramework().getActivator().checkEnforced(foyer, snapshot);
        } catch (LicenseError x) {
            throw new InterpreterMessage(InterpreterMessage.licenseError(x.getError()));
        }
    }

    /**
     * <p>Retrieve the capabilities.
     *
     * @return The capabilities.
     */
    public Capability[] getCapabilities() {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = foyer.snapshotTrackings();
        Capability[] res = new Capability[snapshot.length];
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            Capability capa = ((AbstractBranch) entry.key).capa;
            if (!(capa instanceof Capability))
                throw new NullPointerException("capability missing");
            res[i] = capa;
        }
        return res;
    }

    /********************************************************/
    /* The License File                                     */
    /********************************************************/

    /**
     * <p>Activate a capability.</p>
     *
     * @param c The capability.
     * @param h The license key.
     * @throws InterpreterMessage License error.
     */
    public void activateCapability(Capability c, String h)
            throws InterpreterMessage {
        if (c == null)
            throw new NullPointerException("capability missing");
        if (h == null)
            throw new NullPointerException("hash missing");
        AbstractBranch b = (AbstractBranch) c.getBranch();
        try {
            foyer.getFramework().getActivator().activateBundle(foyer, b, h);
        } catch (LicenseError x) {
            throw new InterpreterMessage(InterpreterMessage.licenseError(x.getError()));
        } catch (ScannerError x) {
            throw new InterpreterMessage(InterpreterMessage.syntaxError(x.getMessage()));
        }
    }

    /**
     * <p>Calculate the install ID.</p>
     *
     * @param c The capability.
     * @return The install ID.
     * @throws InterpreterMessage License error.
     */
    public String calcInstallID(Capability c)
            throws InterpreterMessage {
        if (c == null)
            throw new NullPointerException("capability missing");
        try {
            return foyer.getFramework().getActivator().calcInstallID(foyer, (AbstractBranch) c.getBranch());
        } catch (LicenseError x) {
            throw new InterpreterMessage(InterpreterMessage.licenseError(x.getError()));
        }
    }

    /**
     * <p>Register the license text.</p>
     *
     * @param capa The capability.
     * @param text The license text.
     * @throws InterpreterMessage License error.
     */
    public void regLicenseText(Capability capa, String text)
            throws InterpreterMessage {
        if (capa == null)
            throw new NullPointerException("capability missing");
        if (text == null)
            throw new NullPointerException("text missing");
        AbstractBranch branch = (AbstractBranch) capa.getBranch();
        try {
            foyer.getFramework().getActivator().regLicenseText(foyer, branch, text);
        } catch (LicenseError x) {
            throw new InterpreterMessage(InterpreterMessage.licenseError(x.getError()));
        }
    }

    /**
     * <p>The registered license text.</p>
     *
     * @param capa The capability.
     * @return The license text.
     * @throws InterpreterMessage License error.
     */
    public String regedLicenseText(Capability capa)
            throws InterpreterMessage {
        if (capa == null)
            throw new NullPointerException("capability missing");
        AbstractBranch branch = (AbstractBranch) capa.getBranch();
        String text;
        try {
            text = foyer.getFramework().getActivator().regedLicenseText(foyer, branch);
        } catch (LicenseError x) {
            throw new InterpreterMessage(InterpreterMessage.licenseError(x.getError()));
        }
        return text;
    }

    /***********************************************************/
    /* Advanced Configuration                                  */
    /***********************************************************/

    /**
     * <p>Set the hint.</p>
     *
     * @param h The hint.
     */
    public void setHint(int h) {
        AbstractFactory factory = foyer.getFactory();
        factory.setHint(foyer, h);
    }

    /**
     * <p>Set the application.</p>
     *
     * @param c The application.
     */
    public void setApplication(Object c) {
        foyer.setApplication(c);
    }

    /***********************************************************/
    /* For Internal Use Only                                   */
    /***********************************************************/

    /**
     * <p>Retrieve the store.</p>
     *
     * @return The store.
     */
    public final Object getFoyer() {
        return foyer;
    }

    /**
     * <p>Retrieve the root knowledge base.</p>
     *
     * @return The root knowledge base.
     */
    public Knowledgebase getRoot() {
        Store store = (Store) foyer.getRoot();
        return (store != null ? (Knowledgebase) store.proxy : null);
    }

}