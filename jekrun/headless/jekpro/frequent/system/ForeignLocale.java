package jekpro.frequent.system;

import jekpro.tools.call.Capability;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.*;
import matula.util.system.ForeignCache;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.*;

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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class ForeignLocale {
    public final static int ARG_PRIMARY = 0;
    public final static int ARG_SECONDARY = 1;

    public static final String ARGTYPE_PARASQ = "parasq";
    public static final String ARGTYPE_PARAQ = "paraq";
    public static final String ARGTYPE_PARA = "para";
    public static final String ARGTYPE_ID = "id";

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
     * @throws InterpreterMessage Capability not found.
     */
    public static Properties sysGetLang(Interpreter inter, String pin,
                                        String locstr)
            throws InterpreterMessage, IOException {
        Knowledgebase know = inter.getKnowledgebase();
        HashMap<String, Properties> cache = know.getCache(pin);
        if (cache == null)
            return null;
        locstr = "_" + locstr;
        Properties prop = ForeignCache.getCached(cache, locstr);
        return ForeignCache.getLang(prop, know, pin, locstr);
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
     * @throws InterpreterMessage
     */
    public static String sysAtomFormat(Interpreter inter, String locstr,
                                       String format, Object list)
            throws InterpreterMessage, IOException {
        Locale locale = ForeignLocale.stringToLocale(locstr);
        Object[] args = ForeignLocale.prepareArguments(inter, list);
        return String.format(locale, format, args);
    }

    /**
     * <p>Unpack a list to an array of Java objects.</p>
     *
     * @param inter The interpreter.
     * @param term  The list of arguments.
     * @return The array of Java objects.
     * @throws InterpreterMessage Not a list.
     */
    private static Object[] prepareArguments(Interpreter inter, Object term)
            throws InterpreterMessage {
        ArrayList<Object> vec = new ArrayList<Object>();
        while (term instanceof TermCompound &&
                ((TermCompound) term).getArity() == 2 &&
                ((TermCompound) term).getFunctor().equals(
                        Knowledgebase.OP_CONS)) {
            TermCompound tc = (TermCompound) term;
            vec.add(prepareArgument(inter, tc.getArg(0)));
            term = tc.getArg(1);
        }
        if (term.equals(Knowledgebase.OP_NIL)) {
            /* do nothing */
        } else {
            InterpreterMessage.checkInstantiated(term);
            throw new InterpreterMessage(InterpreterMessage.typeError("list", term));
        }
        Object[] args = new Object[vec.size()];
        vec.toArray(args);
        return args;
    }

    /**
     * <p>Convert a string to a locale.</p>
     *
     * @param locstr The string.
     * @return The locale.
     */
    public static Locale stringToLocale(String locstr) {
        int k1 = locstr.indexOf('_');
        if (k1 == -1) {
            return new Locale(locstr);
        } else {
            int k2 = locstr.indexOf('_', k1 + 1);
            if (k2 == -1) {
                return new Locale(locstr.substring(0, k1),
                        locstr.substring(k1 + 1));
            } else {
                return new Locale(locstr.substring(0, k1),
                        locstr.substring(k1 + 1, k2), locstr.substring(k2 + 1));
            }
        }
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
     * @throws InterpreterMessage Validation error.
     */
    public static String sysMessageMake(Interpreter inter, String locstr,
                                        Properties obj, Object term)
            throws InterpreterMessage {
        Locale locale = ForeignLocale.stringToLocale(locstr);
        return messageMake(inter, term, locale, obj);
    }

    /**
     * <p>Format a term from properties.</p>
     * <p>The rules are as follows:</p>
     * <pre>
     *     prop == null: Formatted via Term.toString(FLAG_QUOTED,inter)
     *     pat == null: Error text and formatted via Term.toString(FLAG_QUOTED,inter)
     *     temp == null: Error text and formatted via Term.toString(FLAG_QUOTED,inter)
     *     otherwise: Formatted according to message template and message parameters.
     * </pre>
     *
     * @param inter  The interpreter or null.
     * @param term   The message term.
     * @param locale The locale.
     * @param prop   The properties file.
     * @return The formatted term.
     */
    public static String messageMake(Interpreter inter, Object term,
                                     Locale locale, Properties prop) {
        if (prop == null)
            return Term.toString(Term.FLAG_QUOTED, inter, term);
        ArrayList<String> pat = messagePattern(term, prop);
        if (pat == null) {
            StringBuilder buf = new StringBuilder();
            buf.append(prop.getProperty("term.pattern"));
            buf.append(": ");
            buf.append(Term.toString(Term.FLAG_QUOTED, inter, term));
            return buf.toString();
        }
        String temp = ForeignLocale.messageTemplate(term, pat, prop);
        if (temp == null) {
            StringBuilder buf = new StringBuilder();
            buf.append(prop.getProperty("term.template"));
            buf.append(": ");
            buf.append(Term.toString(Term.FLAG_QUOTED, inter, term));
            return buf.toString();
        }
        Object[] paras = ForeignLocale.messageParameters(inter, term, pat);
        return String.format(locale, temp, paras);
    }

    /**
     * <p>Retrieve the message pattern.</p>
     * <p>An message pattern is stored in the properties file as follows:</p>
     * <pre>
     *        pattern.functor.length=<pattern>
     * </pre>
     * <p>A pattern has the following syntax, whereby the number of
     * argument type specifiers must match the length:</p>
     * <pre>
     *        pattern :== { 'parasq'
     *                    | 'paraq'
     *                    | 'para'
     *                    | 'id' }
     * </pre>
     * <p>Will return null when the term is not a callable.</p>
     * <p>Will return null when the message pattern cannot be found.</p>
     * <p>Will throw an unchecked exception when the message pattern
     * does not conform.</p>
     *
     * @param term The message term.
     * @param prop The properties file.
     * @return The message pattern or null.
     */
    public static ArrayList<String> messagePattern(Object term,
                                                   Properties prop) {
        String fun;
        int arity;
        if (term instanceof String) {
            fun = (String) term;
            arity = 0;
        } else if (term instanceof TermCompound) {
            fun = ((TermCompound) term).getFunctor();
            arity = ((TermCompound) term).getArity();
        } else {
            return null;
        }
        String patstr = prop.getProperty("pattern." + fun + "." + arity);
        if (patstr == null)
            return null;
        ArrayList<String> pat = new ArrayList<String>();
        StringTokenizer st = new StringTokenizer(patstr);
        while (st.hasMoreTokens()) {
            String argtype = st.nextToken();
            if (argtype.equalsIgnoreCase(ForeignLocale.ARGTYPE_PARASQ)) {
                pat.add(ForeignLocale.ARGTYPE_PARASQ);
            } else if (argtype.equalsIgnoreCase(ForeignLocale.ARGTYPE_PARAQ)) {
                pat.add(ForeignLocale.ARGTYPE_PARAQ);
            } else if (argtype.equalsIgnoreCase(ForeignLocale.ARGTYPE_PARA)) {
                pat.add(ForeignLocale.ARGTYPE_PARA);
            } else if (argtype.equalsIgnoreCase(ForeignLocale.ARGTYPE_ID)) {
                pat.add(ForeignLocale.ARGTYPE_ID);
            } else {
                throw new IllegalArgumentException("illegal argument type");
            }
        }
        if (pat.size() != arity)
            throw new IllegalArgumentException("length mismatch");
        return pat;
    }

    /**
     * <p>Extract the message template.</p>
     * <p>An id is extracted according to the message pattern:</p>
     * <pre>
     *     id: The argument is converted with TermLiteral.toString()
     * </pre>
     *
     * @param term The message term.
     * @param pat  The message pattern.
     * @param prop The properties file.
     * @return The message template or null.
     */
    private static String messageTemplate(Object term, ArrayList<String> pat,
                                          Properties prop) {
        String fun;
        if (term instanceof String) {
            fun = (String) term;
        } else if (term instanceof TermCompound) {
            fun = ((TermCompound) term).getFunctor();
        } else {
            fun = null;
        }
        StringBuilder buf = new StringBuilder();
        buf.append(fun);
        for (int i = 0; i < pat.size(); i++) {
            if (ForeignLocale.ARGTYPE_ID.equals(pat.get(i))) {
                Object t = ((TermCompound) term).getArg(i);
                buf.append('.');
                buf.append(Term.toString(0, null, t));
            }
        }
        return prop.getProperty(buf.toString());
    }

    /**
     * <p>Extract the message parameters.</p>
     * <p>A parameter is extracted according to the messsage pattern:</p>
     * <pre>
     *     parasq: The argument is converted with AbstractSource.shortName() and TermLiteral.toString()
     *     paraq: The argument is converted with TermLiteral.toString(FLAG_QUOT,inter)
     *     para: The argument is converted with TermLiteral.toString(0,inter)
     * </pre>
     *
     * @param inter The interpreter.
     * @param term  The message term.
     * @param pat   The message pattern.
     * @return The message parameters.
     */
    private static Object[] messageParameters(Interpreter inter, Object term,
                                              ArrayList<String> pat) {
        ArrayList<Object> paravec = new ArrayList<Object>();
        for (int i = 0; i < pat.size(); i++) {
            String argtype = pat.get(i);
            if (!ForeignLocale.ARGTYPE_ID.equals(argtype)) {
                Object t = ((TermCompound) term).getArg(i);
                if (ForeignLocale.ARGTYPE_PARASQ.equals(argtype)) {
                    paravec.add(shortName((String) t));
                } else if (ForeignLocale.ARGTYPE_PARAQ.equals(argtype)) {
                    paravec.add(Term.toString(Term.FLAG_QUOTED, inter, t));
                } else {
                    paravec.add(ForeignLocale.prepareArgument(inter, t));
                }
            }
        }
        Object[] paras = new Object[paravec.size()];
        paravec.toArray(paras);
        return paras;
    }

    /**
     * <p>Unpack a term to a Java object.</p>
     *
     * @param inter The interpreter.
     * @param t     The argument.
     * @return The Java object.
     */
    public static Object prepareArgument(Interpreter inter, Object t) {
        if (t instanceof Double || t instanceof Float) {
            return t;
        } else if (t instanceof Long || t instanceof BigDecimal) {
            return TermAtomic.widenBigDecimal((Number) t);
        } else if (t instanceof Integer || t instanceof BigInteger) {
            return t;
        } else if (t instanceof String) {
            return t;
        } else if (!(t instanceof TermCompound) && !(t instanceof TermVar)) {
            return t;
        } else {
            return Term.toString(0, inter, t);
        }
    }

    /****************************************************************/
    /* Exception Utilities                                          */
    /****************************************************************/

    /**
     * <p>Format a term from properties.</p>
     *
     * @param inter  The interpreter.
     * @param locstr The locale.
     * @param obj    The properties.
     * @param term   The message term.
     * @return The formatted term.
     * @throws InterpreterMessage Validation error.
     */
    public static String sysErrorMake(Interpreter inter, String locstr,
                                      Properties obj, Object term)
            throws InterpreterMessage {
        Locale locale = ForeignLocale.stringToLocale(locstr);
        return errorMake(inter, term, locale, obj);
    }

    /**
     * <p>Create the user-friendly detail message from the exception term.</p>
     * <p>Will dynamically build the message each time the method is called.</p>
     * <p>The following rules apply.</p>
     * <pre>
     *      error(Message, Context):   property('exception.error') ": ", message(Message)
     *      warning(Message, Context): property('exception.warning') ": ", message(message)
     *      cause(Primary, Secondary): errorMake(Primary)
     *      TermLiteral                       property('exception.unknown') ": " string(TermLiteral)
     * </pre>
     *
     * @param inter  The interpreter.
     * @param term   The exception term.
     * @param locale The locale.
     * @param prop   The properties.
     * @return The exception message.
     */
    public static String errorMake(Interpreter inter, Object term,
                                   Locale locale, Properties prop) {
        for (; ; ) {
            if ((term instanceof TermCompound) &&
                    ((TermCompound) term).getArity() == 2 &&
                    ((TermCompound) term).getFunctor().equals("error")) {
                TermCompound tc = (TermCompound) term;
                StringBuilder buf = new StringBuilder();
                buf.append(prop.getProperty("exception.error"));
                buf.append(": ");
                buf.append(ForeignLocale.messageMake(inter, tc.getArg(0), locale, prop));
                return buf.toString();
            } else if ((term instanceof TermCompound) &&
                    ((TermCompound) term).getArity() == 2 &&
                    ((TermCompound) term).getFunctor().equals("warning")) {
                TermCompound tc = (TermCompound) term;
                StringBuilder buf = new StringBuilder();
                buf.append(prop.getProperty("exception.warning"));
                buf.append(": ");
                buf.append(ForeignLocale.messageMake(inter, tc.getArg(0), locale, prop));
                return buf.toString();
            } else if ((term instanceof TermCompound) &&
                    ((TermCompound) term).getArity() == 2 &&
                    ((TermCompound) term).getFunctor().equals("cause")) {
                TermCompound tc = (TermCompound) term;
                term = tc.getArg(ForeignLocale.ARG_PRIMARY);
            } else {
                StringBuilder buf = new StringBuilder();
                buf.append(prop.getProperty("exception.unknown"));
                buf.append(": ");
                buf.append(Term.toString(Term.FLAG_QUOTED, inter, term));
                return buf.toString();
            }
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
        Locale locale = ForeignLocale.stringToLocale(locstr);
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
    public static Properties sysGetDescriptionProperties(Interpreter inter,
                                                         String locstr, String clazz)
            throws InterpreterMessage {
        Capability capa = Knowledgebase.stringToCapability(clazz, inter);
        Locale locale = ForeignLocale.stringToLocale(locstr);
        return capa.getDescriptionProperties(locale);
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
        int k = path.lastIndexOf('/') + 1;
        return path.substring(k);
    }

}
