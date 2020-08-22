package jekpro.tools.call;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.array.FlagFactory;
import jekpro.tools.term.Knowledgebase;
import matula.comp.sharik.AbstractTracking;
import matula.util.config.AbstractBundle;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;

/**
 * This class represents the base for all toolkits. Each toolkit predefines
 * a set of capabilities for a knowledge base. The list of predefined
 * capabilities can be retrieved via the method getInitCapabilities(). The
 * brand capability among the predefined capabilities can be retrieved via
 * the method getBrandCapability(). The lookup name of a capability
 * can be retrieve by the method capabilityToString().
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
public abstract class Toolkit {
    private final AbstractFactory factory;

    /* interpreter */
    public final static String PROP_SYS_CUR_INPUT = FlagFactory.OP_SYS_CUR_INPUT;
    public final static String PROP_SYS_CUR_OUTPUT = FlagFactory.OP_SYS_CUR_OUTPUT;
    public final static String PROP_SYS_CUR_ERROR = FlagFactory.OP_SYS_CUR_ERROR;
    public final static String PROP_SYS_ATTACHED_TO = FlagFactory.OP_SYS_ATTACHED_TO;

    /**
     * <p>Create a new toolkit.</p>
     *
     * @param f The factory.
     */
    protected Toolkit(AbstractFactory f) {
        factory = f;
        factory.proxy = this;
    }

    /**
     * <p>Retrieve the init capabilities.</p>
     *
     * @return The capabilities.
     */
    public final Capability[] getInitCapabilities() {
        AbstractBranch[] branches = factory.getInitBranches();
        Capability[] res = new Capability[branches.length];
        for (int i = 0; i < branches.length; i++) {
            Capability capa = (Capability) branches[i].proxy;
            if (capa == null)
                throw new NullPointerException("capability missing");
            res[i] = capa;
        }
        return res;
    }

    /**
     * <p>Retrieve the brand capability.</p>
     *
     * @return Then capability.
     */
    public final Capability getBrandCapability() {
        Capability capa = (Capability) factory.getBrandBranch().proxy;
        if (capa == null)
            throw new NullPointerException("capability missing");
        return capa;
    }

    /**
     * <p>Find a capability.</p>
     *
     * @param n The name.
     * @param k The knowledge base.
     * @return The capability.
     * @throws InterpreterMessage Shit happens.
     */
    public static Capability stringToCapability(String n, Knowledgebase k)
            throws InterpreterMessage, InterpreterException {
        if (n == null)
            throw new NullPointerException("name missing");
        AbstractFactory factory = k.getFoyer().getFactory();
        AbstractBranch branch;
        try {
            branch = factory.getReflection().stringToBranch(n, k.getLoader());
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


    /**
     * <p>Convert a capabilty to a string.</p>
     *
     * @param cap The capability.
     * @return The name.
     */
    public final String capabilityToString(Capability cap) {
        return factory.getReflection().branchToString((AbstractBranch) cap.getBranch());
    }


    /**************************************************************/
    /* Activated Capabilties                                      */
    /**************************************************************/

    /**
     * <p>Check the license of the given capability.</p>
     *
     * @param c The capability.
     * @param k The knowledge base.
     * @throws InterpreterMessage License error.
     */
    public static void checkLicense(Capability c, Knowledgebase k)
            throws InterpreterMessage {
        if (c == null)
            throw new NullPointerException("capability missing");
        Foyer foyer = k.getFoyer();
        AbstractBranch b = (AbstractBranch) c.getBranch();
        AbstractTracking tracking = foyer.getTracking(b);
        if (tracking == null)
            throw new InterpreterMessage(InterpreterMessage.licenseError(EngineMessage.OP_LICENSE_TRACKING_LOST));
        try {
            foyer.getFramework().getActivator().checkTracking(foyer, b, tracking);
        } catch (LicenseError x) {
            throw new InterpreterMessage(InterpreterMessage.licenseError(x.getMessage()));
        }
    }

    /**
     * <p>Check the licenses.</p>
     *
     * @param k The knowledge base.
     * @throws InterpreterMessage License error.
     */
    public static void checkLicenses(Knowledgebase k)
            throws InterpreterMessage {
        Foyer foyer = k.getFoyer();
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = foyer.snapshotTrackings();
        try {
            foyer.getFramework().getActivator().checkEnforced(foyer, snapshot);
        } catch (LicenseError x) {
            throw new InterpreterMessage(InterpreterMessage.licenseError(x.getMessage()));
        }
    }

    /**
     * <p>Retrieve the capabilities.
     *
     * @param k The knowledge base.
     * @return The capabilities.
     */
    public static Capability[] getCapabilities(Knowledgebase k) {
        Foyer foyer = k.getFoyer();
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = foyer.snapshotTrackings();
        Capability[] res = new Capability[snapshot.length];
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            Capability capa = (Capability) ((AbstractBranch) entry.key).proxy;
            if (capa == null)
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
     * @param k The knowledge base.
     * @throws InterpreterMessage License error.
     */
    public static void activateCapability(Capability c, String h,
                                          Knowledgebase k)
            throws InterpreterMessage {
        if (c == null)
            throw new NullPointerException("capability missing");
        if (h == null)
            throw new NullPointerException("hash missing");
        Foyer foyer = k.getFoyer();
        AbstractBranch b = (AbstractBranch) c.getBranch();
        try {
            foyer.getFramework().getActivator().activateBundle(foyer, b, h);
        } catch (LicenseError x) {
            throw new InterpreterMessage(InterpreterMessage.licenseError(x.getMessage()));
        }
    }

    /**
     * <p>Calculate the install ID.</p>
     *
     * @param c The capability.
     * @param k The knowledge base.
     * @return The install ID.
     * @throws InterpreterMessage License error.
     */
    public static String calcInstallID(Capability c, Knowledgebase k)
            throws InterpreterMessage {
        if (c == null)
            throw new NullPointerException("capability missing");
        Foyer foyer = k.getFoyer();
        try {
            return foyer.getFramework().getActivator().calcInstallID(foyer, (AbstractBranch) c.getBranch());
        } catch (LicenseError x) {
            throw new InterpreterMessage(InterpreterMessage.licenseError(x.getMessage()));
        }
    }

    /**
     * <p>Register the license text.</p>
     *
     * @param c The capability.
     * @param k The knowledge base.
     * @param t The license text.
     * @throws InterpreterMessage License error.
     */
    public static void regLicenseText(Capability c, String t, Knowledgebase k)
            throws InterpreterMessage {
        if (c == null)
            throw new NullPointerException("capability missing");
        if (t == null)
            throw new NullPointerException("text missing");
        Foyer foyer = k.getFoyer();
        AbstractBranch branch = (AbstractBranch) c.getBranch();
        try {
            foyer.getFramework().getActivator().regLicenseText(foyer, branch, t);
        } catch (LicenseError x) {
            throw new InterpreterMessage(InterpreterMessage.licenseError(x.getMessage()));
        }
    }

    /**
     * <p>The registered license text.</p>
     *
     * @param c The capability.
     * @param k The knowledge base.
     * @return The license text.
     * @throws InterpreterMessage License error.
     */
    public static String regedLicenseText(Capability c, Knowledgebase k)
            throws InterpreterMessage {
        if (c == null)
            throw new NullPointerException("capability missing");
        Foyer foyer = k.getFoyer();
        AbstractBranch branch = (AbstractBranch) c.getBranch();
        String text;
        try {
            text = foyer.getFramework().getActivator().regedLicenseText(foyer, branch);
        } catch (LicenseError x) {
            throw new InterpreterMessage(InterpreterMessage.licenseError(x.getMessage()));
        }
        return text;
    }

    /***********************************************************/
    /* For Internal Use Only                                   */
    /***********************************************************/

    /**
     * <p>Retrieve the factory.</p>
     *
     * @return The factory.
     */
    public final Object getFactory() {
        return factory;
    }

    /**
     * <p>Init a list of paths.</p>
     *
     * @param inter The interpreter.
     * @param cps   The class paths.
     * @throws InterpreterMessage Shit happens.
     */
    public static void initPaths(Interpreter inter, ListArray<String> cps)
            throws InterpreterMessage, InterpreterException {
        Knowledgebase know = inter.getKnowledgebase();
        for (int i = 0; i < cps.size(); i++) {
            String path = cps.get(i);
            if (path.startsWith("#"))
                continue;
            try {
                know.addClassPath(path);
            } catch (InterpreterMessage y) {
                InterpreterException x = new InterpreterException(y,
                        InterpreterException.fetchStack(inter));
                if (InterpreterException.systemConsultBreak(x, inter, false))
                    break;
            }
        }
    }

    /**
     * <p>Init a list of capabilities.</p>
     *
     * @param inter The call-in.
     * @param capas The list.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public static void initCapas(Interpreter inter, ListArray<String> capas)
            throws InterpreterException, InterpreterMessage {
        Knowledgebase know = inter.getKnowledgebase();
        for (int i = 0; i < capas.size(); i++) {
            String name = capas.get(i);
            if (name.startsWith("#"))
                continue;
            try {
                Capability capa = stringToCapability(name, know);
                capa.initCapability(inter, true);
            } catch (InterpreterMessage y) {
                InterpreterException x = new InterpreterException(y,
                        InterpreterException.fetchStack(inter));
                if (InterpreterException.systemConsultBreak(x, inter, false))
                    break;
            } catch (InterpreterException x) {
                if (InterpreterException.systemConsultBreak(x, inter, false))
                    break;
            }
        }
    }

}
