package jekpro.model.molec;

import derek.util.protect.LicenseError;
import jekpro.frequent.standard.EngineCopy;
import jekpro.frequent.system.ForeignLocale;
import jekpro.model.inter.Engine;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.AbstractStore;
import jekpro.model.pretty.PrologWriter;
import jekpro.model.pretty.SourceFileResource;
import jekpro.model.rope.Resource;
import jekpro.tools.term.*;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.regex.ScannerToken;
import matula.util.system.AbstractRecognizer;
import matula.util.system.ForeignCache;
import matula.util.wire.AbstractLivestock;
import matula.util.wire.PropertiesWithImport;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.MalformedURLException;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
import java.nio.channels.FileLockInterruptionException;
import java.nio.charset.CharacterCodingException;
import java.util.*;
import java.util.zip.ZipException;

/**
 * <p>This class defines an engine message which consists
 * of a message only. This is typically thrown by the built-ins
 * which do not have access to the stack. The constructor accepts
 * a skeleton and a display. It will make a copy of the given
 * exception term and internally store a new skeleton.</p>
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
public final class EngineMessage extends Exception {
    private static final String OP_INSTANTIATION_ERROR = "instantiation_error"; /* ISO */

    private static final String OP_TYPE_ERROR = "type_error"; /* ISO */
    public static final String OP_TYPE_INTEGER = "integer";
    public static final String OP_TYPE_NUMBER = "number";
    public static final String OP_TYPE_EVALUABLE = "evaluable";
    public static final String OP_TYPE_ATOM = "atom";
    public static final String OP_TYPE_PREDICATE_INDICATOR = "predicate_indicator";
    public static final String OP_TYPE_CALLABLE = "callable";
    public static final String OP_TYPE_CHARACTER = "character";
    public static final String OP_TYPE_LIST = "list";
    public static final String OP_TYPE_PAIR = "pair";
    public static final String OP_TYPE_ASSOC = "assoc";
    public static final String OP_TYPE_CAPABILITY = "capability";
    public static final String OP_TYPE_BYTE = "byte";
    public static final String OP_TYPE_REF = "ref";
    public static final String OP_TYPE_DECIMAL = "decimal";
    public static final String OP_TYPE_FLOAT = "float";
    public static final String OP_TYPE_SPECIAL = "special";
    public static final String OP_TYPE_METHOD = "method";
    public static final String OP_TYPE_CONSTRUCTOR = "constructor";
    public static final String OP_TYPE_OPER_INDICATOR = "oper_indicator";
    public static final String OP_TYPE_VALUE = "value";

    private static final String OP_DOMAIN_ERROR = "domain_error"; /* ISO */
    public static final String OP_DOMAIN_OPERATOR_SPECIFIER = "operator_specifier";
    public static final String OP_DOMAIN_OPERATOR_PRIORITY = "operator_priority";
    public static final String OP_DOMAIN_PROLOG_FLAG = "prolog_flag";
    public static final String OP_DOMAIN_FLAG_VALUE = "flag_value";
    public static final String OP_DOMAIN_NOT_LESS_THAN_ZERO = "not_less_than_zero";
    public static final String OP_DOMAIN_WRITE_OPTION = "write_option";
    public static final String OP_DOMAIN_READ_OPTION = "read_option";
    public static final String OP_DOMAIN_ASSERT_OPTION = "assert_option";
    public static final String OP_DOMAIN_IO_MODE = "io_mode";
    public static final String OP_DOMAIN_CONSULT_OPTION = "consult_option";
    public static final String OP_DOMAIN_REF = "ref";
    public static final String OP_DOMAIN_PROLOG_PROPERTY = "prolog_property";
    public static final String OP_DOMAIN_META_ARG = "meta_arg";
    public static final String OP_DOMAIN_PROPERTY_VALUE = "property_value";
    public static final String OP_DOMAIN_DIRECTIVE_CLAUSE = "directive_clause";
    public static final String OP_DOMAIN_VARIABLE_NAME = "variable_name";
    public static final String OP_DOMAIN_VERBOSE_OPTION = "verbose_option";
    public static final String OP_DOMAIN_LINK_OPTION = "link_option";
    public static final String OP_DOMAIN_ACTION_OPTION = "action_option";
    public final static String OP_DOMAIN_ARITY_MISMATCH = "arity_mismatch";
    public static final String OP_DOMAIN_FIX_OPTION = "fix_option";
    public static final String OP_DOMAIN_UNKNOWN_PROXY = "unknown_proxy";
    public static final String OP_DOMAIN_CLASS = "class";
    public static final String OP_DOMAIN_MODULE = "module";
    public static final String OP_DOMAIN_RECEIVER = "receiver";

    private static final String OP_REPRESENTATION_ERROR = "representation_error"; /* ISO */
    public static final String OP_REPRESENTATION_CHARACTER_CODE = "character_code";
    public final static String OP_REPRESENTATION_BYTE = "byte";
    public final static String OP_REPRESENTATION_SHORT = "short";
    public final static String OP_REPRESENTATION_INT = "int";
    public final static String OP_REPRESENTATION_LONG = "long";

    private static final String OP_EXISTENCE_ERROR = "existence_error"; /* ISO */
    public static final String OP_EXISTENCE_PROCEDURE = "procedure";
    public static final String OP_EXISTENCE_LIBRARY = "library";
    public static final String OP_EXISTENCE_SOURCE_SINK = "source_sink";
    public static final String OP_EXISTENCE_HOST = "host";
    public static final String OP_EXISTENCE_PORT = "port";
    public static final String OP_EXISTENCE_ENCODING = "encoding";
    public static final String OP_EXISTENCE_SOURCE = "source";
    public static final String OP_EXISTENCE_CLASS = "class";
    public static final String OP_EXISTENCE_METHOD = "method";
    public static final String OP_EXISTENCE_FIELD = "field";
    public static final String OP_EXISTENCE_OPERATOR = "operator";
    public static final String OP_EXISTENCE_SYNTAX = "syntax";
    public static final String OP_EXISTENCE_PROVABLE = "provable";
    public static final String OP_EXISTENCE_CONSTRUCTOR = "constructor";
    public static final String OP_EXISTENCE_PROXY = "proxy";
    public static final String OP_EXISTENCE_BODY = "body";
    public static final String OP_EXISTENCE_CODE = "code";

    private static final String OP_PERMISSION_ERROR = "permission_error"; /* ISO */
    public static final String OP_PERMISSION_ACCESS = "access"; /* ISO */
    public static final String OP_PERMISSION_MODIFY = "modify"; /* ISO */
    public static final String OP_PERMISSION_PRIVATE_PROCEDURE = "private_procedure"; /* ISO */
    public static final String OP_PERMISSION_STATIC_PROCEDURE = "static_procedure"; /* ISO */
    public static final String OP_PERMISSION_FLAG = "flag"; /* ISO */
    public static final String OP_PERMISSION_INPUT = "input";
    public static final String OP_PERMISSION_OUTPUT = "output";
    public static final String OP_PERMISSION_STREAM = "stream";
    public static final String OP_PERMISSION_BINARY_STREAM = "binary_stream";
    public static final String OP_PERMISSION_TEXT_STREAM = "text_stream";
    public static final String OP_PERMISSION_REPOSITION = "reposition";
    public static final String OP_PERMISSION_OPEN = "open";
    public static final String OP_PERMISSION_PROPERTY = "property";
    public static final String OP_PERMISSION_ADD = "add";
    public static final String OP_PERMISSION_VALUE = "value";
    public static final String OP_PERMISSION_OPERATOR = "operator"; /* ISO */
    public static final String OP_PERMISSION_CREATE = "create"; /* ISO */
    public static final String OP_PERMISSION_DIRECTIVE = "directive";
    public static final String OP_PERMISSION_LINK = "link";
    public static final String OP_PERMISSION_CLASS = "class";
    public static final String OP_PERMISSION_TOOLKIT_CAPA = "toolkit_capa";
    public static final String OP_PERMISSION_REDEFINE = "redefine";
    public static final String OP_PERMISSION_COERCE = "coerce";
    public static final String OP_PERMISSION_VIRTUAL = "virtual";
    public static final String OP_PERMISSION_PROCEDURE = "procedure";
    public final static String OP_PERMISSION_SOURCE_SINK = "source_sink";
    public final static String OP_PERMISSION_PROXY = "proxy";

    public static final String OP_SYSTEM_ERROR = "system_error"; /* ISO */
    public static final String OP_SYSTEM_USER_ABORT = "user_abort";
    public static final String OP_SYSTEM_USER_EXIT = "user_exit";
    public static final String OP_SYSTEM_USER_CLOSE = "user_close";
    public static final String OP_SYSTEM_MEMORY_THRESHOLD = "memory_threshold";
    public static final String OP_SYSTEM_DEADLOCK_TIMEOUT = "deadlock_timeout";
    public static final String OP_SYSTEM_TIMELIMIT_EXCEEDED = "timelimit_exceeded";

    private static final String OP_RESOURCE_ERROR = "resource_error"; /* ISO */
    public static final String OP_RESOURCE_SOCKET_TIMEOUT = "socket_timeout";
    public static final String OP_RESOURCE_CORRUPT_ARCHIVE = "corrupt_archive";
    public static final String OP_RESOURCE_BASEURL_MISSING = "baseurl_missing";

    public static final String OP_SYNTAX_EXPAND_FAILED = "expand_failed";
    public static final String OP_SYNTAX_REBUILD_FAILED = "rebuild_failed";

    public static final String OP_SYNTAX_MALFORMED_URL = "malformed_url";
    public static final String OP_SYNTAX_RELATIVE_PATH = "relative_path";
    public static final String OP_SYNTAX_MALFORMED_PATH = "malformed_path";

    public static final String OP_SYNTAX_NOT_LOCALE = "not_locale";
    public static final String OP_SYNTAX_WRONG_PARENT = "wrong_parent";
    public static final String OP_SYNTAX_SUPERFLOUS_END = "superflous_end";
    public static final String OP_SYNTAX_END_MISSING = "end_missing";

    public static final String OP_SYNTAX_OVERRIDE_PRED = "override_pred";
    public static final String OP_SYNTAX_DISCONTIGUOUS_PRED = "discontiguous_pred";
    public static final String OP_SYNTAX_MULTIFILE_PRED = "multifile_pred";
    public static final String OP_SYNTAX_PUBLIC_PRED = "public_pred";
    public static final String OP_SYNTAX_META_PREDICATE_PRED = "meta_predicate_pred";
    public static final String OP_SYNTAX_META_FUNCTION_PRED = "meta_function_pred";
    public static final String OP_SYNTAX_DYNAMIC_PRED = "dynamic_pred";
    public static final String OP_SYNTAX_THREAD_LOCAL_PRED = "thread_local_pred";
    public static final String OP_SYNTAX_IMPLEMENTATION_PRED = "implementation_pred";

    public static final String OP_SYNTAX_OVERRIDE_OPER = "override_oper";
    public static final String OP_SYNTAX_IMPLEMENTATION_OPER = "implementation_oper";

    public static final String OP_SYNTAX_MODULE_EMPTY = "module_empty";

    public static final String OP_SYNTAX_SINGLETON_VAR = "singleton_var";

    public static final String OP_SYNTAX_DIRECTIVE_FAILED = "directive_failed";
    public final static String OP_SYNTAX_SITE_MISSING = "site_missing";

    public final static String OP_EVALUATION_ERROR = "evaluation_error";
    public final static String OP_EVALUATION_ZERO_DIVISOR = "zero_divisor"; /* arithmetic */
    public final static String OP_EVALUATION_UNDEFINED = "undefined";
    public final static String OP_EVALUATION_FLOAT_OVERFLOW = "float_overflow";
    public final static String OP_EVALUATION_FLOAT_UNDERFLOW = "float_underflow";
    public final static String OP_EVALUATION_ORDERED = "ordered";
    public static final String OP_EVALUATION_PARTIAL_FUNCTION = "partial_function";

    public static final String OP_LICENSE_TRACKING_LOST = "tracking_lost";

    public static final String ARGTYPE_PARASQ = "parasq";
    public static final String ARGTYPE_PARAQ = "paraq";
    public static final String ARGTYPE_PARA = "para";
    public static final String ARGTYPE_ID = "id";

    private final Object template;

    /**
     * <p>No stack filling.</p>
     *
     * @return This throwable.
     * @see com.sun.org.apache.xerces.internal.parsers.AbstractDOMParser.Abort
     */
    public Throwable fillInStackTrace() {
        return this;
    }

    /**
     * <p>Non-copying constructor.</p>
     *
     * @param m The message skeleton.
     */
    public EngineMessage(Object m) {
        if (!EngineCopy.isGroundSkel(m))
            throw new IllegalArgumentException("needs display");
        template = m;
    }

    /**
     * <p>Copying constructor.</p>
     *
     * @param t The message skeleton.
     * @param d The message display.
     */
    public EngineMessage(Object t, Display d) {
        EngineCopy ec = new EngineCopy();
        template = ec.copyTerm(t, d);
    }

    /**
     * <p>Retrieve the template skeleton.</p>
     *
     * @return The template skeleton.
     */
    public Object getTemplate() {
        return template;
    }

    /**
     * <p>Retrieve the detailed message of a message.</p>
     * <p>Will dynamically build the message each time the method is called.</p>
     *
     * @return The detailed message.
     */
    public String getMessage() {
        return toString();
    }

    /**
     * <p>Retrieve the user-friendly detail message from the message term.</p>
     *
     * @return The messsage text.
     */
    public String toString() {
        int size = Display.displaySize(template);
        Display ref = (size != 0 ? new Display(size) : Display.DISPLAY_CONST);
        return EngineMessage.messageMake(template, ref, Locale.getDefault(), null, null);
    }

    /**
     * <p>Retrieve the user-friendly detail message form the message term.</p>
     *
     * @param store The store.
     * @return The message text.
     */
    public String toString(AbstractStore store) {
        try {
            Locale locale = store.foyer.locale;
            Properties error = getErrorLang(locale, store);
            int size = Display.displaySize(template);
            Display ref = (size != 0 ? new Display(size) : Display.DISPLAY_CONST);
            return EngineMessage.messageMake(template, ref, locale, error, null);
        } catch (IOException x) {
            throw new RuntimeException("shouldnt happen", x);
        }
    }

    /**
     * <p>Check the message type and return the message argument.</p>
     *
     * @param fun The message type.
     * @return The message argument or null.
     */
    public Object messageType(String fun) {
        Object m = getTemplate();
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 1 &&
                ((SkelCompound) m).sym.fun.equals(fun)) {
            return ((SkelCompound) m).args[0];
        }
        return null;
    }

    /*************************************************************/
    /* Error Constructors                                        */
    /*************************************************************/

    /**
     * <p>Create an instantiation error messsage.</p>
     *
     * @return The instantiation error message.
     */
    public static SkelAtom instantiationError() {
        return new SkelAtom(OP_INSTANTIATION_ERROR);
    }

    /**
     * <p>Create a type error message.</p>
     *
     * @param type The error type.
     * @param m    The culprit skel.
     * @return The type error message.
     */
    public static SkelCompound typeError(String type, Object m) {
        return new SkelCompound(new SkelAtom(OP_TYPE_ERROR),
                new SkelAtom(type), m);
    }

    /**
     * <p>Create a domain error message.</p>
     *
     * @param type The error type.
     * @param m    The culprit skel.
     * @return The domain error message.
     */
    public static SkelCompound domainError(String type, Object m) {
        return new SkelCompound(new SkelAtom(OP_DOMAIN_ERROR),
                new SkelAtom(type), m);
    }

    /**
     * <p>Create a representation error message.</p>
     *
     * @param type The error type.
     * @return The representation error message.
     */
    public static SkelCompound representationError(String type) {
        return new SkelCompound(new SkelAtom(OP_REPRESENTATION_ERROR),
                new SkelAtom(type));
    }

    /**
     * <p>Create an existence error message.</p>
     *
     * @param type The error type.
     * @param m    The culprit skel.
     * @return The existence error message.
     */
    public static SkelCompound existenceError(String type, Object m) {
        return new SkelCompound(new SkelAtom(OP_EXISTENCE_ERROR),
                new SkelAtom(type), m);
    }

    /**
     * <p>Create a syntax error message without culprit.</p>
     *
     * @param type The error type.
     * @return The syntax error message.
     */
    public static SkelCompound syntaxError(String type) {
        return new SkelCompound(new SkelAtom(ScannerToken.OP_SYNTAX_ERROR),
                new SkelAtom(type));
    }

    /**
     * <p>Create a syntax error message with culprit.</p>
     *
     * @param type The error type.
     * @param m    The culprit skel.
     * @return The syntax error message.
     */
    public static SkelCompound syntaxError(String type, Object m) {
        return new SkelCompound(new SkelAtom(ScannerToken.OP_SYNTAX_ERROR),
                new SkelAtom(type), m);
    }

    /**
     * <p>Create a permission error message.</p>
     *
     * @param operation The operation.
     * @param type      The error type.
     * @param m         The culprit skel.
     * @return The permission error message.
     */
    public static SkelCompound permissionError(String operation, String type, Object m) {
        return new SkelCompound(new SkelAtom(OP_PERMISSION_ERROR),
                new SkelAtom(operation), new SkelAtom(type), m);
    }

    /**
     * <p>Create a system error message.</p>
     *
     * @param type The error type.
     * @return The system error message.
     */
    public static SkelCompound systemError(String type) {
        return new SkelCompound(new SkelAtom(OP_SYSTEM_ERROR),
                new SkelAtom(type));
    }

    /**
     * <p>Create a license error message.</p>
     *
     * @param type The error type.
     * @return The system error message.
     */
    public static SkelCompound licenseError(String type) {
        return new SkelCompound(new SkelAtom(LicenseError.ERROR_LICENSE_ERROR),
                new SkelAtom(type));
    }

    /**
     * <p>Create a resource error message.</p>
     *
     * @param type The error type.
     * @return The resource error message.
     */
    public static SkelCompound resourceError(String type) {
        return new SkelCompound(new SkelAtom(OP_RESOURCE_ERROR), new SkelAtom(type));
    }

    /**
     * <p>Create an evaluation error message.</p>
     *
     * @param type The error type.
     * @return The evaluation error message.
     */
    public static SkelCompound evaluationError(String type) {
        return new SkelCompound(new SkelAtom(OP_EVALUATION_ERROR),
                new SkelAtom(type));
    }

    /*************************************************************/
    /* Error Checkers                                            */
    /*************************************************************/

    /**
     * <p>Check whether the given term is instantiated.</p>
     * <p>This check must be preceded by dereferencing.</p>
     *
     * @param t The term skel.
     * @throws EngineMessage Shit happens.
     */
    public static void checkInstantiated(Object t) throws EngineMessage {
        if (t instanceof SkelVar) {
            throw new EngineMessage(EngineMessage.instantiationError());
        } else {
            /* */
        }
    }

    /**
     * <p>Check whetehr the given term is a callable.</p>
     * <p>This check must be preceded by an instantiation check.</p>
     *
     * @param t The term skel.
     * @param d The term display.
     * @throws EngineMessage Shit happens.
     */
    public static void checkCallable(Object t, Display d) throws EngineMessage {
        if (t instanceof SkelCompound) {
            /* */
        } else if (t instanceof SkelAtom) {
            /* */
        } else {
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_CALLABLE, t), d);
        }
    }

    /**
     * <p>Check whether the given term is a reference.</p>
     * <p>This check must be preceded by an instantiation check.</p>
     *
     * @param t The term skel.
     * @param d The display skel.
     * @throws EngineMessage Not a reference.
     */
    public static void checkRef(Object t, Display d)
            throws EngineMessage {
        if (!(t instanceof AbstractSkel) && !(t instanceof Number)) {
            /* */
        } else {
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_REF, t), d);
        }
    }

    /**
     * <p>Check whether the given term is a number or a reference.</p>
     * <p>This check must be preceded by an instantiation check.</p>
     *
     * @param t The term skel.
     * @param d The display skel.
     * @throws EngineMessage Not a number or a reference.
     */
    public static void checkValue(Object t, Display d)
            throws EngineMessage {
        if (!(t instanceof AbstractSkel)) {
            /* */
        } else {
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_VALUE, t), d);
        }
    }

    /**
     * <p>Check whether the given number is not less than zero.</p>
     * <p>This check must be preceded by an integer check.</p>
     *
     * @param n The number, either Integer or BigInteger.
     * @throws EngineMessage Shit happens.
     */
    public static void checkNotLessThanZero(Number n) throws EngineMessage {
        if (n instanceof Integer) {
            if (n.intValue() < 0)
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_NOT_LESS_THAN_ZERO, n));
        } else {
            if (((BigInteger) n).compareTo(BigInteger.ZERO) < 0)
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_NOT_LESS_THAN_ZERO, n));
        }
    }

    /**
     * <p>Check whether the given int is a character code.</p>
     *
     * @param n The primitive int.
     * @throws EngineMessage Not a character code.
     */
    public static void checkCharacterCode(int n) throws EngineMessage {
        if (n < 0 || n > Character.MAX_CODE_POINT)
            throw new EngineMessage(EngineMessage.representationError(
                    EngineMessage.OP_REPRESENTATION_CHARACTER_CODE));
    }

    /**
     * <p>Check whether the given int is a byte.</p>
     *
     * @param n The primitive int.
     * @throws EngineMessage Not a byte.
     */
    public static void checkByte(int n) throws EngineMessage {
        if (n < 0 || n > 255)
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_BYTE, Integer.valueOf(n)));
    }

    /*************************************************************/
    /* Error Casts                                               */
    /*************************************************************/

    /**
     * <p>Check whether the given term is an atom.</p>
     * <p>This check must be preceded by an instantiation check.</p>
     *
     * @param t The term skel.
     * @param d The display skel.
     * @return The string.
     * @throws EngineMessage Shit happens.
     */
    public static String castString(Object t, Display d)
            throws EngineMessage {
        if (t instanceof SkelAtom) {
            return ((SkelAtom) t).fun;
        } else {
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_ATOM, t), d);
        }
    }

    /**
     * <p>Check whether the given term is an atom.</p>
     * <p>This check must be preceded by an instantiation check.</p>
     *
     * @param t The term skel.
     * @param d The display skel.
     * @return The wrapped string.
     * @throws EngineMessage Shit happens.
     */
    public static SkelAtom castStringWrapped(Object t, Display d)
            throws EngineMessage {
        if (t instanceof SkelAtom) {
            return (SkelAtom) t;
        } else {
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_ATOM, t), d);
        }
    }

    /**
     * <p>Check whether the given term is a Prolog number.</p>
     * <p>This check must be preceded by an instantiation check.</p>
     *
     * @param t The term skel.
     * @param d The display skel.
     * @return The number.
     * @throws EngineMessage Not a number.
     */
    public static Number castNumber(Object t, Display d)
            throws EngineMessage {
        if (t instanceof Number) {
            return (Number) t;
        } else {
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_NUMBER, t), d);
        }
    }

    /**
     * <p>Check whether the given term is a Prolog integer.</p>
     * <p>This check must be preceded by an instantiation check.</p>
     *
     * @param t The term skel.
     * @param d The display skel.
     * @return The integer, either Integer or BigInteger.
     * @throws EngineMessage Not a integer.
     */
    public static Number castInteger(Object t, Display d)
            throws EngineMessage {
        if (t instanceof Integer) {
            return (Integer) t;
        } else if (t instanceof BigInteger) {
            return (BigInteger) t;
        } else {
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_INTEGER, t), d);
        }
    }

    /**
     * <p>Check whether the given term is a Prolog decimal.</p>
     * <p>This check must be preceded by an instantiation check.</p>
     *
     * @param t The term skel.
     * @param d The display skel.
     * @return The decimal, either Long or BigDecimal.
     * @throws EngineMessage Not a integer.
     */
    public static Number castDecimal(Object t, Display d)
            throws EngineMessage {
        if (t instanceof Long) {
            return (Long) t;
        } else if (t instanceof BigDecimal) {
            return (BigDecimal) t;
        } else {
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_DECIMAL, t), d);
        }
    }

    /**
     * <p>Check whether the given value is a byte value.</p>
     * <p>This check must be preceded by an integer check.</p>
     *
     * @param t The number, either Integer or BigInteger.
     * @return The primitive byte value.
     * @throws EngineMessage Not a byte value.
     */
    public static byte castByteValue(Number t)
            throws EngineMessage {
        if (t instanceof Integer &&
                Byte.MIN_VALUE <= t.intValue() &&
                t.intValue() <= Byte.MAX_VALUE) {
            return (byte) t.intValue();
        } else {
            throw new EngineMessage(EngineMessage.representationError(
                    EngineMessage.OP_REPRESENTATION_BYTE));
        }
    }

    /**
     * <p>Check whether the given value is a short value.</p>
     * <p>This check must be preceded by an integer check.</p>
     *
     * @param t The number, either Integer or BigInteger.
     * @return The primitive short value.
     * @throws EngineMessage Not a short value.
     */
    public static short castShortValue(Number t)
            throws EngineMessage {
        if (t instanceof Integer &&
                Short.MIN_VALUE <= t.intValue() &&
                t.intValue() <= Short.MAX_VALUE) {
            return (short) t.intValue();
        } else {
            throw new EngineMessage(EngineMessage.representationError(
                    EngineMessage.OP_REPRESENTATION_SHORT));
        }
    }

    /**
     * <p>Check whether the given value is an int value.</p>
     * <p>This check must be preceded by an integer check.</p>
     *
     * @param t The number, either Integer or BigInteger.
     * @return The primitive int value.
     * @throws EngineMessage Not a int value.
     */
    public static int castIntValue(Number t)
            throws EngineMessage {
        if (t instanceof Integer) {
            return t.intValue();
        } else {
            throw new EngineMessage(EngineMessage.representationError(
                    EngineMessage.OP_REPRESENTATION_INT));
        }
    }

    /**
     * <p>Check whether the given value is a long value.</p>
     * <p>This check must be preceded by an integer check.</p>
     *
     * @param t The number, either Integer or BigInteger..
     * @return The primitive long value.
     * @throws EngineMessage Not a long value.
     */
    public static long castLongValue(Number t)
            throws EngineMessage {
        if (t instanceof Integer) {
            return t.intValue();
        } else if (t instanceof BigInteger &&
                TermAtomic.MIN_LONG.compareTo((BigInteger) t) <= 0 &&
                ((BigInteger) t).compareTo(TermAtomic.MAX_LONG) <= 0) {
            return t.longValue();
        } else {
            throw new EngineMessage(EngineMessage.representationError(
                    EngineMessage.OP_REPRESENTATION_LONG));
        }
    }

    /**
     * <p>Check whether the given atom is a character.</p>
     *
     * @param str The atom.
     * @return The code point.
     * @throws EngineMessage Not a character.
     */
    public static int castCharacter(String str)
            throws EngineMessage {
        int k;
        if (str.length() == 0 ||
                str.length() != Character.charCount(k = str.codePointAt(0)))
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_CHARACTER, new SkelAtom(str)));
        return k;
    }

    /**
     * <p>Check whether the given atom is a char value.</p>
     *
     * @param str The atom.
     * @return The char.
     * @throws EngineMessage Not a char value.
     */
    public static char castCharValue(String str)
            throws EngineMessage {
        int k;
        if (str.length() == 0 ||
                str.length() != Character.charCount(k = str.codePointAt(0)) ||
                k > 0xFFFF)
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_CHARACTER, new SkelAtom(str)));
        return (char) k;
    }

    /******************************************************************/
    /* Map IO                                                         */
    /******************************************************************/

    /**
     * <p>Map an IOException.</p>
     *
     * @param x The IO Exception.
     * @return The engine message.
     */
    public static EngineMessage mapIOException(IOException x) {
        if (x instanceof SocketTimeoutException) {
            return new EngineMessage(EngineMessage.resourceError(
                    EngineMessage.OP_RESOURCE_SOCKET_TIMEOUT));
        } else if (x instanceof InterruptedIOException) {
            return (EngineMessage) AbstractLivestock.sysThreadClear();
        } else if (x instanceof FileLockInterruptionException) {
            return (EngineMessage) AbstractLivestock.sysThreadClear();
        } else if (x instanceof UnsupportedEncodingException) {
            return new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_ENCODING,
                    new SkelAtom(x.getMessage())));
        } else if (x instanceof MalformedURLException) {
            return new EngineMessage(EngineMessage.syntaxError(
                    EngineMessage.OP_SYNTAX_MALFORMED_URL));
        } else if (x instanceof ZipException) {
            return new EngineMessage(EngineMessage.resourceError(
                    EngineMessage.OP_RESOURCE_CORRUPT_ARCHIVE));
        } else if (x instanceof CharacterCodingException) {
            return new EngineMessage(EngineMessage.syntaxError(
                    EngineMessage.OP_SYNTAX_MALFORMED_PATH));
        } else if (x instanceof FileNotFoundException) {
            return new EngineMessage(EngineMessage.existenceError(
                    OP_EXISTENCE_SOURCE_SINK, new SkelAtom(x.getMessage())));
        } else if (x instanceof UnknownHostException) {
            return new EngineMessage(EngineMessage.existenceError(
                    OP_EXISTENCE_HOST, new SkelAtom(x.getMessage())));
        } else if (x instanceof SocketException) {
            return new EngineMessage(EngineMessage.existenceError(
                    OP_EXISTENCE_PORT, new SkelAtom(x.getMessage())));
        } else {
            return new EngineMessage(EngineMessage.resourceError(
                    x.getMessage()));
        }
    }

    /******************************************************************/
    /* Message Utilities                                              */
    /******************************************************************/

    /**
     * <p>Format a term from properties.</p>
     * <p>The rules are as follows:</p>
     * <pre>
     *     prop == null: Formatted via AbstractTerm.unparseTerm(FLAG_QUOTED,inter)
     *     pat == null: Error text and formatted via AbstractTerm.unparseTerm(FLAG_QUOTED,inter)
     *     temp == null: Error text and formatted via AbstractTerm.unparseTerm(FLAG_QUOTED,inter)
     *     otherwise: Formatted according to message template and message parameters.
     * </pre>
     *
     * @param term   The message skel.
     * @param ref    The message display.
     * @param locale The locale
     * @param prop   The properties file.
     * @param en     The engine or null.
     * @return The formatted term.
     */
    public static String messageMake(Object term, Display ref,
                                     Locale locale, Properties prop,
                                     Engine en) {
        if (prop == null)
            return PrologWriter.toString(term, ref, PrologWriter.FLAG_QUOT, en);
        BindVar b;
        while (term instanceof SkelVar &&
                (b = ref.bind[((SkelVar) term).id]).display != null) {
            term = b.skel;
            ref = b.display;
        }
        ArrayList<String> pat = messagePattern(term, prop);
        if (pat == null) {
            StringBuilder buf = new StringBuilder();
            buf.append(prop.getProperty("term.pattern"));
            buf.append(": ");
            buf.append(PrologWriter.toString(term, ref, PrologWriter.FLAG_QUOT, en));
            return buf.toString();
        }
        String temp = EngineMessage.messageTemplate(term, ref, pat, prop);
        if (temp == null) {
            StringBuilder buf = new StringBuilder();
            buf.append(prop.getProperty("term.template"));
            buf.append(": ");
            buf.append(PrologWriter.toString(term, ref, PrologWriter.FLAG_QUOT, en));
            return buf.toString();
        }
        Object[] paras = EngineMessage.messageParameters(term, ref, pat, en);
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
     * @param term The message skeleton.
     * @param prop The properties file.
     * @return The message pattern or null.
     */
    private static ArrayList<String> messagePattern(Object term,
                                                    Properties prop) {
        String fun;
        int arity;
        if (term instanceof SkelAtom) {
            fun = ((SkelAtom) term).fun;
            arity = 0;
        } else if (term instanceof SkelCompound) {
            fun = ((SkelCompound) term).sym.fun;
            arity = ((SkelCompound) term).args.length;
        } else {
            fun = null;
            arity = -1;
        }
        String patstr = prop.getProperty("pattern." + fun + "." + arity);
        if (patstr == null)
            return null;
        ArrayList<String> pat = new ArrayList<String>();
        StringTokenizer st = new StringTokenizer(patstr);
        while (st.hasMoreTokens()) {
            String argtype = st.nextToken();
            if (argtype.equals(ARGTYPE_PARASQ)) {
                pat.add(ARGTYPE_PARASQ);
            } else if (argtype.equals(ARGTYPE_PARAQ)) {
                pat.add(ARGTYPE_PARAQ);
            } else if (argtype.equals(ARGTYPE_PARA)) {
                pat.add(ARGTYPE_PARA);
            } else if (argtype.equals(ARGTYPE_ID)) {
                pat.add(ARGTYPE_ID);
            } else {
                throw new IllegalArgumentException("illegal argument type");
            }
        }
        if (pat.size() != arity)
            throw new IllegalArgumentException("length mismatch");
        return pat;
    }

    /**
     * <p>Extract the message templatess.</p>
     * <p>An id is extracted according to the message pattern:</p>
     * <pre>
     *     id: The argument is converted with Molec.unparseTerm()
     * </pre>
     *
     * @param term The message skel.
     * @param ref  The message display.
     * @param pat  The message pattern.
     * @param prop The properties file.
     * @return The message template or null.
     */
    private static String messageTemplate(Object term, Display ref,
                                          ArrayList<String> pat, Properties prop) {
        String fun;
        if (term instanceof SkelAtom) {
            fun = ((SkelAtom) term).fun;
        } else if (term instanceof SkelCompound) {
            fun = ((SkelCompound) term).sym.fun;
        } else {
            fun = null;
        }
        StringBuilder buf = new StringBuilder();
        buf.append(fun);
        for (int i = 0; i < pat.size(); i++) {
            if (ARGTYPE_ID.equals(pat.get(i))) {
                Object t = ((SkelCompound) term).args[i];
                Display d = ref;
                BindVar b;
                while (t instanceof SkelVar &&
                        (b = d.bind[((SkelVar) t).id]).display != null) {
                    t = b.skel;
                    d = b.display;
                }
                buf.append('.');
                buf.append(PrologWriter.toString(t, d, 0, null));
            }
        }
        return prop.getProperty(buf.toString());
    }

    /**
     * <p>Extract the message parameters.</p>
     * <p>A parameter is extracted according to the messsage pattern:</p>
     * <pre>
     *     parasq: The argument is converted with AbstractSource.shortName() and Molec.unparseTerm()
     *     paraq: The argument is converted with Molec.unparseTerm(FLAG_QUOT,en)
     *     para: The argument is converted with Molec.unparseTerm(0,en)
     * </pre>
     *
     * @param term The message skel.
     * @param ref  The message display.
     * @param pat  The message pattern.
     * @param en   The engine.
     * @return The message parameters.
     */
    private static Object[] messageParameters(Object term, Display ref,
                                              ArrayList<String> pat,
                                              Engine en) {
        ListArray<Object> paravec = new ListArray<Object>();
        for (int i = 0; i < pat.size(); i++) {
            String argtype = pat.get(i);
            if (!ARGTYPE_ID.equals(argtype)) {
                Object t = ((SkelCompound) term).args[i];
                Display d = ref;
                BindVar b;
                while (t instanceof SkelVar &&
                        (b = d.bind[((SkelVar) t).id]).display != null) {
                    t = b.skel;
                    d = b.display;
                }
                if (argtype.equals(ARGTYPE_PARASQ)) {
                    String path = ((SkelAtom) t).fun;
                    t = new SkelAtom(ForeignLocale.shortName(path));
                    paravec.add(PrologWriter.toString(t, d, PrologWriter.FLAG_QUOT));
                } else if (argtype.equals(ARGTYPE_PARAQ)) {
                    paravec.add(PrologWriter.toString(t, d, PrologWriter.FLAG_QUOT, en));
                } else {
                    paravec.add(EngineMessage.prepareArgument(t, d, en));
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
     * @param t  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The Java object.
     */
    public static Object prepareArgument(Object t, Display d,
                                         Engine en) {
        if (t instanceof Float || t instanceof Double) {
            return t;
        } else if (t instanceof Long || t instanceof BigDecimal) {
            return TermAtomic.widenBigDecimal((Number) t);
        } else if (t instanceof Integer || t instanceof BigInteger) {
            return t;
        } else if (t instanceof SkelAtom) {
            return ((SkelAtom) t).fun;
        } else if (!(t instanceof AbstractSkel)) {
            return t;
        } else {
            return PrologWriter.toString(t, d, 0, en);
        }
    }

    /******************************************************************/
    /* Error Utilities                                                */
    /******************************************************************/

    /**
     * <p>Retrieve the properties file union.</p>
     *
     * @param locale The locale.
     * @param start  The store start.
     * @return The properties file union.
     * @throws IOException Shit happens.
     */
    public static Properties getErrorLang(Locale locale, AbstractStore start)
            throws IOException {
        PropertiesWithImport res = new PropertiesWithImport();
        String locstr = "_" + locale;
        AbstractStore store = start;
        while (store != null) {
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int j = 0; j < sources.length; j++) {
                AbstractSource base = sources[j].value;
                Resource[] rscs = base.snapshotResources();
                for (int i = 0; i < rscs.length; i++) {
                    Resource rsc = rscs[i];
                    String key = rsc.getKey();
                    HashMap<String, Properties> cache = getCache(key, start);
                    if (cache == null)
                        continue;
                    Properties prop = ForeignCache.getCached(cache, locstr);
                    prop = ForeignCache.getLang(prop, (AbstractRecognizer) start.proxy, key, locstr);
                    if (prop != null)
                        res.addImport(prop);
                }
            }
            store = store.parent;
        }
        return res;
    }

    /**
     * <p>Retrieve the properties cache for the given resource bundle.</p>
     *
     * @param key The source key.
     * @return The properties cache, or null.
     */
    public static HashMap<String, Properties> getCache(String key, AbstractStore store) {
        AbstractSource src = store.getSource(key);
        if (!(src instanceof SourceFileResource))
            return null;
        return ((SourceFileResource) src).getCache();
    }

}
