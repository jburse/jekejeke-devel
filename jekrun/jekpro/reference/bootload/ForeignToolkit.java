package jekpro.reference.bootload;

import jekpro.tools.call.*;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;

/**
 * <p>The foreign predicates for the module toolkit.</p>
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
public final class ForeignToolkit {

    /**
     * <p>Activate a capability.</p>
     *
     * @param inter The call-in.
     * @param clazz The capability class.
     * @param hash  The license key.
     * @throws InterpreterMessage   Validation error.
     * @throws InterpreterException Validation error.
     */
    public static void sysActivateCapability(Interpreter inter,
                                             String clazz, String hash)
            throws InterpreterMessage, InterpreterException {
        Knowledgebase know = inter.getKnowledgebase();
        Capability capa = Toolkit.stringToCapability(clazz, know);
        Toolkit.activateCapability(capa, hash, know);
    }

    /**
     * <p>Calculate the install ID.</p>
     *
     * @param inter The call-in.
     * @param clazz The capability class.
     * @return The install ID.
     * @throws InterpreterMessage   Validation error.
     * @throws InterpreterException Validation error.
     */
    public static String sysCalcInstallID(Interpreter inter,
                                          String clazz)
            throws InterpreterMessage, InterpreterException {
        Knowledgebase know = inter.getKnowledgebase();
        Capability capa = Toolkit.stringToCapability(clazz, know);
        return Toolkit.calcInstallID(capa, know);
    }

    /**
     * <p>Register the license text.</p>
     *
     * @param inter The call-in.
     * @param clazz The capability class.
     * @param text  The license text.
     * @throws InterpreterMessage   Validation error.
     * @throws InterpreterException Validation error.
     */
    public static void sysRegLicenseText(Interpreter inter,
                                         String clazz, String text)
            throws InterpreterMessage, InterpreterException {
        Knowledgebase know = inter.getKnowledgebase();
        Capability capa = Toolkit.stringToCapability(clazz, know);
        Toolkit.regLicenseText(capa, text, know);
    }

    /**
     * <p>The registered license text.</p>
     *
     * @param inter The call-in.
     * @param clazz The capability class.
     * @return The license text.
     * @throws InterpreterMessage   Validation error.
     * @throws InterpreterException Validation error.
     */
    public static String sysRegedLicenseText(Interpreter inter,
                                             String clazz)
            throws InterpreterMessage, InterpreterException {
        Knowledgebase know = inter.getKnowledgebase();
        Capability capa = Toolkit.stringToCapability(clazz, know);
        return Toolkit.regedLicenseText(capa, know);
    }

    /**
     * <p>Init a capability.</p>
     * <p>No user interaction is performed.</p>
     *
     * @param inter The call-in.
     * @param clazz The capability class.
     * @throws InterpreterMessage   Validation error.
     * @throws InterpreterException Validation error.
     */
    public static void sysInitCapability(Interpreter inter,
                                         String clazz)
            throws InterpreterMessage, InterpreterException {
        Knowledgebase know = inter.getKnowledgebase();
        Capability capa = Toolkit.stringToCapability(clazz, know);
        capa.initCapability(inter);
    }

    /**
     * <p>Init a capability.</p>
     * <p>The prompt flag indicates whether user interaction is allowed.</p>
     *
     * @param inter The call-in.
     * @param clazz The capability class.
     * @param opt   The init options.
     * @throws InterpreterMessage   Validation error.
     * @throws InterpreterException Validation error.
     */
    public static void sysInitCapabilityOpt(Interpreter inter,
                                            String clazz, Object opt)
            throws InterpreterMessage, InterpreterException {
        Knowledgebase know = inter.getKnowledgebase();
        Capability capa = Toolkit.stringToCapability(clazz, know);
        InitOpts options = InitOpts.decodeInitOptions(opt);
        capa.initCapability(inter, options.getPrompt());
    }

    /**
     * <p>Fini a capability.</p>
     *
     * @param inter The call-in.
     * @param clazz The capability class.
     * @throws InterpreterMessage   Validation error.
     * @throws InterpreterException Validation error.
     */
    public static void sysFiniCapability(Interpreter inter,
                                         String clazz)
            throws InterpreterMessage, InterpreterException {
        Knowledgebase know = inter.getKnowledgebase();
        Capability capa = Toolkit.stringToCapability(clazz, know);
        capa.finiCapability(know);
    }

    /**
     * <p>Retrieve the capabilities.</p>
     *
     * @param inter The call-in.
     * @return The list of capabilities.
     */
    public static Object sysGetCapabilities(Interpreter inter) {
        Knowledgebase know = inter.getKnowledgebase();
        Toolkit toolkit = know.getToolkit();
        Capability[] objs = Toolkit.getCapabilities(inter.getKnowledgebase());
        Object end = know.getTermNil();
        for (int i = objs.length - 1; i >= 0; i--) {
            end = new TermCompound(know.getTermCons(),
                    toolkit.capabilityToString(objs[i]), end);
        }
        return end;
    }

    /**
     * <p>Get the properties of a capbility.</p>
     *
     * @param inter The call-in.
     * @param clazz The capability class.
     * @return The properties.
     * @throws InterpreterMessage   Validation error.
     * @throws InterpreterException Validation error.
     */
    public static Object sysGetCapabilityProperties(Interpreter inter,
                                                    String clazz)
            throws InterpreterMessage, InterpreterException {
        Knowledgebase know = inter.getKnowledgebase();
        Capability capa = Toolkit.stringToCapability(clazz, know);
        Object res = know.getTermNil();
        String[] props = Capability.getProperties();
        for (int i = 0; i < props.length; i++) {
            String prop = props[i];
            Object val = capa.getProperty(prop, inter.getKnowledgebase());
            val = new TermCompound(prop, val);
            res = new TermCompound(know.getTermCons(), val, res);
        }
        return res;
    }

    /**
     * <p>Get the property of a capbility.</p>
     *
     * @param inter The call-in.
     * @param clazz The capability class.
     * @param prop  The property name.
     * @return The properties.
     * @throws InterpreterMessage   Validation error.
     * @throws InterpreterException Validation error.
     */
    public static Object sysGetCapabilityProperty(Interpreter inter,
                                                  String clazz, String prop)
            throws InterpreterMessage, InterpreterException {
        Knowledgebase know = inter.getKnowledgebase();
        Capability capa = Toolkit.stringToCapability(clazz, know);
        Object val = capa.getProperty(prop, know);
        if (val == null)
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    "prolog_property", prop));
        return new TermCompound(prop, val);
    }

    /**
     * <p>Check the license of the given capability.</p>
     *
     * @param inter The call-in.
     * @param clazz The capability class.
     * @throws InterpreterMessage   Validation error.
     * @throws InterpreterException Validation error.
     */
    public static void sysCheckLicense(Interpreter inter,
                                       String clazz)
            throws InterpreterMessage, InterpreterException {
        Knowledgebase know = inter.getKnowledgebase();
        Capability capa = Toolkit.stringToCapability(clazz, know);
        Toolkit.checkLicense(capa, know);
    }

    /**
     * <p>Check the licenses.</p>
     *
     * @param inter The call-in.
     * @throws InterpreterMessage Validation error.
     */
    public static void sysCheckLicenses(Interpreter inter)
            throws InterpreterMessage {
        Knowledgebase know = inter.getKnowledgebase();
        Toolkit.checkLicenses(know);
    }

}
