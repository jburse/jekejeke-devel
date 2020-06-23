package jekpro.frequent.system;

import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.tools.call.*;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;
import matula.util.data.ListArray;
import matula.util.system.ForeignCache;
import matula.util.system.ForeignFile;
import matula.util.system.ForeignUri;
import matula.util.wire.LangProperties;

import java.io.IOException;
import java.util.HashMap;
import java.util.Locale;
import java.util.Properties;

/**
 * <p>The foreign predicates for the module system/locale.</p>
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
public final class ForeignLocale {

    /****************************************************************/
    /* Properties Lookup & Retrieval                                */
    /****************************************************************/

    /**
     * <p>Retrieve a properties.</p>
     *
     * @param inter  The interpreter.
     * @param pin    The pin.
     * @param locstr The locale.
     * @return The properties.
     * @throws IOException IO error.
     */
    public static Properties sysGetLang(Interpreter inter, String pin,
                                        String locstr)
            throws IOException {
        Knowledgebase know = inter.getKnowledgebase();
        HashMap<String, Properties> cache = know.getCache(pin);
        if (cache == null)
            return null;
        locstr = "_" + locstr;
        Properties prop = ForeignCache.getCached(cache, locstr);
        ForeignCache.getProp(prop, know.getStore(), pin, locstr);
        return (ForeignCache.isValid(prop) ? prop : null);
    }

    /**
     * <p>Retrieve the value for a key.</p>
     *
     * @param obj The properties.
     * @param key The key.
     * @return The value, or null.
     */
    public static String sysGetProperty(Properties obj, String key) {
        return obj.getProperty(key);
    }

    /**
     * <p>Retrieve the value for a key.</p>
     * <p>Return default value of key is not present</p>
     *
     * @param obj The properties.
     * @param key The key.
     * @return The value, or null.
     */
    public static String sysGetProperty(Properties obj, String key,
                                        String defaultValue) {
        return obj.getProperty(key, defaultValue);
    }

    /****************************************************************/
    /* Format Utilities                                             */
    /****************************************************************/

    /**
     * <p>Format an atom.</p>
     *
     * @param inter  The interpreter.
     * @param locstr The locale.
     * @param format The format.
     * @param list   The list of terms.
     * @throws InterpreterMessage   Not a list.
     * @throws InterpreterException Not a list.
     */
    public static String sysFormatToString(Interpreter inter, String locstr,
                                           String format, Object list)
            throws InterpreterMessage, InterpreterException {
        Locale locale = LangProperties.stringToLocale(locstr);
        Object[] args = ForeignLocale.prepareArguments(inter, list);
        return String.format(locale, format, args);
    }

    /**
     * <p>Unpack a list to an array of Java objects.</p>
     *
     * @param inter The interpreter.
     * @param term  The list of arguments.
     * @return The array of Java objects.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    private static Object[] prepareArguments(Interpreter inter, Object term)
            throws InterpreterMessage, InterpreterException {
        Engine en = inter.getEngine();
        ListArray<Object> vec = new ListArray<Object>();
        while (term instanceof TermCompound &&
                ((TermCompound) term).getArity() == 2 &&
                ((TermCompound) term).getFunctor().equals(
                        Knowledgebase.OP_CONS)) {
            TermCompound tc = (TermCompound) term;
            Object arg = tc.getArgMolec(0);
            try {
                vec.add(EngineMessage.prepareArgument(AbstractTerm.getSkel(arg),
                        AbstractTerm.getDisplay(arg), en));
            } catch (EngineMessage x) {
                throw new InterpreterMessage(x);
            } catch (EngineException x) {
                throw new InterpreterException(x);
            }
            term = tc.getArg(1);
        }
        if (term.equals(Foyer.OP_NIL)) {
            /* do nothing */
        } else {
            InterpreterMessage.checkInstantiated(term);
            throw new InterpreterMessage(InterpreterMessage.typeError(
                    InterpreterMessage.OP_TYPE_LIST, term));
        }
        Object[] args = new Object[vec.size()];
        vec.toArray(args);
        return args;
    }

    /****************************************************************/
    /* Message Utilities                                            */
    /****************************************************************/

    /**
     * <p>Format a term from properties.</p>
     *
     * @param inter  The interpreter.
     * @param locstr The locale.
     * @param obj    The properties.
     * @param term   The message term.
     * @return The formatted term.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public static String sysMessageMake(Interpreter inter, String locstr,
                                        Properties obj, Object term)
            throws InterpreterMessage, InterpreterException {
        try {
            Locale locale = LangProperties.stringToLocale(locstr);
            Engine en = inter.getEngine();
            return EngineMessage.messageMake(AbstractTerm.getSkel(term),
                    AbstractTerm.getDisplay(term), locale, obj, en);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
    }

    /****************************************************************/
    /* Known Properties                                             */
    /****************************************************************/

    /**
     * <p>Retrieve the error properties.</p>
     *
     * @param inter  The interpreter.
     * @param locstr The locale.
     * @return The properties.
     */
    public static Properties sysGetErrorProperties(Interpreter inter,
                                                   String locstr)
            throws IOException {
        Locale locale = LangProperties.stringToLocale(locstr);
        return inter.getKnowledgebase().getErrorProperties(locale);
    }

    /**
     * <p>Retrieve the description properties of a capability.</p>
     *
     * @param inter  The interpreter.
     * @param locstr The locale.
     * @param clazz  The capability.
     * @return The properties.
     * @throws InterpreterMessage Capability not found.
     */
    public static Properties sysGetDescrModel(Interpreter inter,
                                              String locstr, String clazz)
            throws InterpreterMessage, InterpreterException {
        Knowledgebase know = inter.getKnowledgebase();
        Capability capa = Toolkit.stringToCapability(clazz, know);
        Locale locale = LangProperties.stringToLocale(locstr);
        return capa.getDescrModel(locale, know.getRoot().getLoader());
    }

    /**
     * <p>Retrieve the description properties of a capability.</p>
     *
     * @param inter  The interpreter.
     * @param locstr The locale.
     * @param clazz  The capability.
     * @return The properties.
     * @throws InterpreterMessage Capability not found.
     */
    public static Properties sysGetDescrPlatform(Interpreter inter,
                                                 String locstr, String clazz)
            throws InterpreterMessage, InterpreterException {
        Knowledgebase know = inter.getKnowledgebase();
        Capability capa = Toolkit.stringToCapability(clazz, know);
        Locale locale = LangProperties.stringToLocale(locstr);
        return capa.getDescrPlatform(locale, know);
    }

    /**************************************************************/
    /* Path Utilities                                             */
    /**************************************************************/

    /**
     * <p>Compute short name from path.</p>
     *
     * @param path The path.
     * @return The short name.
     */
    public static String shortName(String path) {
        return ForeignFile.sysPathName(ForeignUri.sysUriSpec(path));
    }

}
