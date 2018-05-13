package jekpro.reference.bootload;

import jekpro.tools.call.*;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;

import java.util.ArrayList;

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
     * @throws InterpreterMessage Validation error.
     */
    public static void sysActivateCapability(Interpreter inter,
                                             String clazz, String hash)
            throws InterpreterMessage {
        Capability capa = inter.getKnowledgebase().stringToCapability(clazz);
        inter.getKnowledgebase().getLobby().activateCapability(capa, hash);
    }

    /**
     * <p>Calculate the install ID.</p>
     *
     * @param inter The call-in.
     * @param clazz The capability class.
     * @return The install ID.
     * @throws InterpreterMessage Validation error.
     */
    public static String sysCalcInstallID(Interpreter inter,
                                          String clazz)
            throws InterpreterMessage {
        Capability capa = inter.getKnowledgebase().stringToCapability(clazz);
        return inter.getKnowledgebase().getLobby().calcInstallID(capa);
    }

    /**
     * <p>Register the license text.</p>
     *
     * @param inter The call-in.
     * @param clazz The capability class.
     * @param text  The license text.
     * @throws InterpreterMessage Validation error.
     */
    public static void sysRegLicenseText(Interpreter inter,
                                         String clazz, String text)
            throws InterpreterMessage {
        Capability capa = inter.getKnowledgebase().stringToCapability(clazz);
        inter.getKnowledgebase().getLobby().regLicenseText(capa, text);
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
        Capability capa = inter.getKnowledgebase().stringToCapability(clazz);
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
        Capability capa = inter.getKnowledgebase().stringToCapability(clazz);
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
        Capability capa = inter.getKnowledgebase().stringToCapability(clazz);
        capa.finiCapability(inter.getKnowledgebase());
    }

    /**
     * <p>Retrieve the capabilities.</p>
     *
     * @param inter The call-in.
     * @return The list of capabilities.
     */
    public static Object sysGetCapabilities(Interpreter inter) {
        Toolkit toolkit = inter.getKnowledgebase().getLobby().getToolkit();
        Capability[] objs = inter.getKnowledgebase().getLobby().getCapabilities();
        Object end = Knowledgebase.OP_NIL;
        for (int i = objs.length - 1; i >= 0; i--) {
            end = new TermCompound(Knowledgebase.OP_CONS,
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
     * @throws InterpreterMessage Validation error.
     */
    public static Object sysGetCapabilityProperties(Interpreter inter,
                                                    String clazz)
            throws InterpreterMessage {
        Capability capa = inter.getKnowledgebase().stringToCapability(clazz);
        Object res = Knowledgebase.OP_NIL;
        String[] props = Capability.getProperties();
        for (int i = 0; i < props.length; i++) {
            String prop = props[i];
            Object val = capa.getProperty(prop, inter.getKnowledgebase().getLobby());
            val = new TermCompound(prop, val);
            res = new TermCompound(Knowledgebase.OP_CONS, val, res);
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
     * @throws InterpreterMessage Validation error.
     */
    public static Object sysGetCapabilityProperty(Interpreter inter,
                                                  String clazz, String prop)
            throws InterpreterMessage {
        Capability capa = inter.getKnowledgebase().stringToCapability(clazz);
        Object val = capa.getProperty(prop, inter.getKnowledgebase().getLobby());
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
     * @throws InterpreterMessage Validation error.
     */
    public static void sysCheckLicense(Interpreter inter,
                                       String clazz)
            throws InterpreterMessage {
        Capability capa = inter.getKnowledgebase().stringToCapability(clazz);
        inter.getKnowledgebase().getLobby().checkLicense(capa);
    }

    /**
     * <p>Check the licenses.</p>
     *
     * @param inter The call-in.
     * @throws InterpreterMessage Validation error.
     */
    public static void sysCheckLicenses(Interpreter inter)
            throws InterpreterMessage {
        inter.getKnowledgebase().getLobby().checkLicenses();
    }

}
