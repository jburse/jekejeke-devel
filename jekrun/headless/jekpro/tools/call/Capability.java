package jekpro.tools.call;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.Lobby;
import matula.util.config.AbstractBundle;

import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;
import java.util.Properties;

/**
 * <p>This class represents the base for all capabilities. A capability can
 * be associated to a knowledge base and initialized via the method
 * initCapability(). A capability can be finalized and de-associated from a
 * knowledge base via the method finiCapability().
 * </p>
 * <p>A capability has certain properties, some do exist already when the
 * capability has not yet been associated and some are specific to the
 * association of the capability to a knowledge base. The properties can
 * be retrieved via the method getProperty().
 * </p>
 * <p>Capabilities govern resources. To access a resource inside a
 * capability the method prepareStream() has to be used. Further the
 * method getDescriptionProperties() allows retrieving the description
 * properties of this capability. The method might return different
 * properties for different locales.</p>
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
public class Capability {
    public final static String PROP_NEEDS_ACT = AbstractBranch.OP_NEEDS_ACT;
    public final static String PROP_ACT_STATUS = AbstractBranch.OP_ACT_STATUS;
    public final static String PROP_EXPIRATION_DATE = AbstractBranch.OP_EXPIRATION_DATE;
    public final static String PROP_BUNDLE_DIR = AbstractBranch.OP_BUNDLE_DIR;
    public final static String PROP_LANGUAGE_CODE = AbstractBranch.OP_LANGUAGE_CODE;
    public final static String PROP_INSTALL_CODE = AbstractBranch.OP_INSTALL_CODE;
    public final static String PROP_LICENSE_CODE = AbstractBranch.OP_LICENSE_CODE;
    public final static String PROP_SYS_NOTRACE = AbstractBranch.OP_SYS_NOTRACE;

    private final AbstractBranch branch;

    /**
     * <p>Create a new capability.</p>
     *
     * @param b The abstract branch.
     */
    protected Capability(AbstractBranch b) {
        branch = b;
        branch.proxy = this;
    }

    /**
     * <p>Retrieve the hash code.</p>
     *
     * @return The hash code.
     */
    public int hashCode() {
        return branch.hashCode();
    }

    /**
     * <p>Check the identity.</p>
     *
     * @param o The other object.
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof Capability))
            return false;
        return branch.equals(((Capability)o).branch);
    }

    /**
     * <p>Init a capability.</p>
     * <p>No user interaction is performed.</p>
     *
     * @param inter The callin.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public void initCapability(Interpreter inter)
            throws InterpreterMessage, InterpreterException {
        initCapability(inter, false);
    }

    /**
     * <p>Init a capability.</p>
     * <p>The prompt flag indicates whether user interaction is allowed.</p>
     *
     * @param inter  The call-in.
     * @param prompt The prompt flag.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public void initCapability(Interpreter inter, boolean prompt)
            throws InterpreterMessage, InterpreterException {
        Engine en = (Engine) inter.getEngine();
        Engine backuse = en.visor.setInuse(en);
        Thread backthread = en.visor.setFence(Thread.currentThread());
        try {
            branch.initBranch(en, prompt, false);
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
     * <p>Fini a capability.</p>
     *
     * @param know The knowledge base.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public void finiCapability(Knowledgebase know)
            throws InterpreterMessage, InterpreterException {
        try {
            branch.finiBranch((Store) know.getStore(), false);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
    }

    /**
     * <p>Retrieve a capability property names.</p>
     *
     * @return The capability property names.
     */
    public static String[] getProperties() {
        return AbstractBranch.getProperties();
    }

    /**
     * <p>Retrieve a capability property.</p>
     *
     * @param prop  The property name.
     * @param lobby The lobby.
     * @return The property value or null.
     */
    public Object getProperty(String prop, Lobby lobby) {
        Object res = branch.getProperty(prop, (Foyer) lobby.getFoyer());
        return (res != null ? AbstractTerm.createTerm(res, Display.DISPLAY_CONST) : null);
    }

    /**
     * <p>Prepare a stream.</p>
     *
     * @param in   The input stream.
     * @param know The knowledgebase.
     * @return The prepared input stream.
     * @throws IOException  Shit happens.
     * @throws LicenseError Shit happens.
     */
    public InputStream prepareStream(InputStream in, Knowledgebase know)
            throws LicenseError, IOException {
        return branch.prepareStream(in, (Store) know.getStore());
    }

    /**
     * <p>Retrieve the bundle description.</p>
     *
     * @param locale The locale.
     * @param lobby  The lobby.
     * @return The properties.
     */
    public Properties getDescrModel(Locale locale, Lobby lobby) {
        Foyer foyer = (Foyer) lobby.getFoyer();
        return branch.getDescrModel(locale, foyer);
    }

    /**
     * <p>Retrieve the bundle description.</p>
     *
     * @param locale The locale.
     * @param lobby  The lobby.
     * @return The properties.
     */
    public Properties getDescrPlatform(Locale locale, Lobby lobby) {
        Foyer foyer = (Foyer) lobby.getFoyer();
        return branch.getDescrPlatform(locale, foyer);
    }

    /***********************************************************/
    /* For Internal Use Only                                   */
    /***********************************************************/

    /**
     * <p>Retrieve the branch.</p>
     *
     * @return The branch.
     */
    public final Object getBranch() {
        return branch;
    }

}
