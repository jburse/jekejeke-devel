package jekpro.tools.call;

import jekpro.frequent.system.ForeignThread;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.*;

import java.io.IOException;

/**
 * <p>This class defines an interpreter message which consists of a
 * message only. This is typically thrown by the Java methods which
 * do not have access to the stack. The interpreter message class
 * cannot be sub-classed by the application programmer. But the
 * application programmer can create and also throw interpreter
 * messages.
 * </p>
 * <p>The application programmer can provide an arbitrary message term.
 * A copy will be created and stored when the constructor is invoked.
 * The copy can be retrieved via the method getValue().
 * </p>
 * <p>The methods unparseTerm() will un-parse the message term. The method
 * without a knowledge base argument will only display the skeleton. The
 * method without a knowledge base argument will attempt to produce a
 * user-friendly display by taking into account the message definitions
 * from the given toolkit.
 * </p>
 * <p>The xxxError() methods provide conveniences to create message terms
 * known from the ISO core standard. The checkXXX() methods provide
 * convenience methods to check terms for certain conditions, and if
 * these conditions are not met, a corresponding message is thrown. The
 * castXXX() methods also return a type casted or unboxed value.
 * </p>
 * <p>The method messageType() will return the Type when the original message
 * term does have the form <fun>(Type). Otherwise the method will return
 * null. The method mapIOException() will map an IOException to a
 * InterpreterMessage, possibly fetching the interpreter signal in case
 * of an interrupted exception.
 * </p>
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
public final class InterpreterMessage extends Exception {
    public final static String OP_TYPE_LIST = EngineMessage.OP_TYPE_LIST;
    public final static String OP_DOMAIN_FLAG_VALUE = EngineMessage.OP_DOMAIN_FLAG_VALUE;
    public static final String OP_PERMISSION_ACCESS = EngineMessage.OP_PERMISSION_ACCESS; /* ISO */

    private final EngineMessage message;

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
     * <p>Create an interpreter message from a term.</p>
     *
     * @param t The term.
     */
    public InterpreterMessage(Object t) {
        message = new EngineMessage(AbstractTerm.getSkel(t), AbstractTerm.getDisplay(t));
    }

    /**
     * <p>Create an interpreter message from an engine message.</p>
     *
     * @param m The engine message.
     */
    public InterpreterMessage(Exception m) {
        message = (EngineMessage) m;
    }

    /**
     * <p>Retrieve the message text.</p>
     *
     * @return The message text.
     */
    public String getMessage() {
        return message.getMessage();
    }

    /**
     * <p>Retrieve the message text.</p>
     * <p>Will use the Java default locale.</p>
     * <p>Will not use any error properties.</p>
     *
     * @return The message text.
     */
    public String toString() {
        return message.toString();
    }

    /**
     * <p>Retrieve the message text.</p>
     * <p>Will use the locale from the foyer.</p>
     * <p>Will use the error properties from the foyer.</p>
     *
     * @param f The foyer.
     * @return The message text.
     */
    public String toString(Foyer f) {
        return message.toString(f);
    }

    /**
     * <p>Retrieve the message text.</p>
     * <p>Will use the locale from the foyer of the store.</p>
     * <p>Will use the error properties from the store.</p>
     *
     * @param k The knowledgebase.
     * @return The message text.
     */
    public String toString(Knowledgebase k) {
        return message.toString(k.getStore());
    }

    /**
     * <p>Retrieve a copy of the message term.</p>
     *
     * @return The copy of the message term.
     */
    public Object getValue() {
        Object m = message.getTemplate();
        Display ref = AbstractSkel.createMarker(m);
        return AbstractTerm.createTerm(m, ref);
    }

    /**
     * <p>Retrieve the engine message.</p>
     *
     * @return The engine message.
     */
    public Exception getException() {
        return message;
    }

    /**
     * <p>Check the message type and return the message argument.</p>
     *
     * @param fun The message type.
     * @return The message argument or null.
     */
    public Object messageType(String fun) {
        Object m = message.getTemplate();
        Object o = message.messageType(fun);
        if (o == null)
            return null;
        Display ref = AbstractSkel.createMarker(m);
        return AbstractTerm.createTerm(o, ref);
    }

    /*************************************************************/
    /* Error Constructors                                        */
    /*************************************************************/

    /**
     * <p>Create an instantiation error messsage.</p>
     *
     * @return The instantiation error message.
     */
    public static String instantiationError() {
        return (String) AbstractTerm.createTerm(
                EngineMessage.instantiationError(), Display.DISPLAY_CONST);
    }

    /**
     * <p>Create a type error message.</p>
     *
     * @param type The error type.
     * @param t    The culprit term.
     * @return The type error message.
     */
    public static TermCompound typeError(String type, Object t) {
        return (TermCompound) AbstractTerm.createTerm(
                EngineMessage.typeError(type, AbstractTerm.getSkel(t)),
                AbstractTerm.getDisplay(t));
    }

    /**
     * <p>Create a domain error message.</p>
     *
     * @param type The error type.
     * @param t    The culprit term.
     * @return The domain error message.
     */
    public static TermCompound domainError(String type, Object t) {
        return (TermCompound) AbstractTerm.createTerm(
                EngineMessage.domainError(type, AbstractTerm.getSkel(t)),
                AbstractTerm.getDisplay(t));
    }

    /**
     * <p>Create a representation error message.</p>
     *
     * @param type The error type.
     * @return The representation error message.
     */
    public static TermCompound representationError(String type) {
        return (TermCompound) AbstractTerm.createTerm(
                EngineMessage.representationError(type), Display.DISPLAY_CONST);
    }

    /**
     * <p>Create a existence error message.</p>
     *
     * @param type The error type.
     * @param t    The culprit term.
     * @return The existence error message.
     */
    public static TermCompound existenceError(String type, Object t) {
        return (TermCompound) AbstractTerm.createTerm(
                EngineMessage.existenceError(type, AbstractTerm.getSkel(t)),
                AbstractTerm.getDisplay(t));
    }

    /**
     * <p>Create a syntax error message.</p>
     *
     * @param type The error type.
     * @return The syntax error message.
     */
    public static TermCompound syntaxError(String type) {
        return (TermCompound) AbstractTerm.createTerm(
                EngineMessage.syntaxError(type), Display.DISPLAY_CONST);
    }

    /**
     * <p>Create a syntax error message.</p>
     *
     * @param type The error type.
     * @param t    The culprit term.
     * @return The syntax error message.
     */
    public static TermCompound syntaxError(String type, Object t) {
        return (TermCompound) AbstractTerm.createTerm(
                EngineMessage.syntaxError(type, AbstractTerm.getSkel(t)),
                AbstractTerm.getDisplay(t));
    }

    /**
     * <p>Create a permission error message.</p>
     *
     * @param operation The operation.
     * @param type      The error type.
     * @param t         The culprit term.
     * @return The permission error message.
     */
    public static TermCompound permissionError(String operation, String type, Object t) {
        return (TermCompound) AbstractTerm.createTerm(
                EngineMessage.permissionError(operation, type, AbstractTerm.getSkel(t)),
                AbstractTerm.getDisplay(t));
    }

    /**
     * <p>Create a system error message.</p>
     *
     * @param type The error type.
     * @return The system error message.
     */
    public static TermCompound systemError(String type) {
        return (TermCompound) AbstractTerm.createTerm(
                EngineMessage.systemError(type), Display.DISPLAY_CONST);
    }

    /**
     * <p>Create a limit error message.</p>
     *
     * @param type The error type.
     * @return The limit error message.
     */
    public static TermCompound limitError(String type) {
        return (TermCompound) AbstractTerm.createTerm(
                EngineMessage.limitError(type), Display.DISPLAY_CONST);
    }

    /**
     * <p>Create a resource error message.</p>
     *
     * @param type The error type.
     * @return The resource error message.
     */
    public static TermCompound resourceError(String type) {
        return (TermCompound) AbstractTerm.createTerm(
                EngineMessage.resourceError(type), Display.DISPLAY_CONST);
    }

    /**
     * <p>Create a license error message.</p>
     *
     * @param type The error type.
     * @return The license error message.
     */
    public static TermCompound licenseError(String type) {
        return (TermCompound) AbstractTerm.createTerm(
                EngineMessage.licenseError(type),
                Display.DISPLAY_CONST);
    }

    /**
     * <p>Create an evaluation error message.</p>
     *
     * @param type The type.
     * @return The evaluation error message.
     */
    public static TermCompound evaluationError(String type) {
        return (TermCompound) AbstractTerm.createTerm(
                EngineMessage.evaluationError(type),
                Display.DISPLAY_CONST);
    }

    /*************************************************************/
    /* Error Checkers                                            */
    /*************************************************************/

    /**
     * <p>Check whether the given term is instantiated.</p>
     * <p>This check must be preceded by dereferencing.</p>
     *
     * @param t The term.
     * @throws InterpreterMessage Type error.
     */
    public static void checkInstantiated(Object t)
            throws InterpreterMessage {
        try {
            /* fast lane */
            if (t instanceof String)
                return;
            /* common lane */
            EngineMessage.checkInstantiated(AbstractTerm.getSkel(t));
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Check whether the given term is a callable.</p>
     *
     * @param t The term.
     * @throws InterpreterMessage Type error.
     */
    public static void checkCallable(Object t)
            throws InterpreterMessage {
        try {
            /* fast lane */
            if (t instanceof String)
                return;
            /* common lane */
            EngineMessage.checkCallable(AbstractTerm.getSkel(t),
                    AbstractTerm.getDisplay(t));
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /*************************************************************/
    /* Type Casts                                                */
    /*************************************************************/

    /**
     * <p>Check whether the given term is an atom.</p>
     *
     * @param t The external Prolog term.
     * @return The string.
     * @throws InterpreterMessage Type error.
     */
    public static String castString(Object t)
            throws InterpreterMessage {
        try {
            /* fast lane */
            if (t instanceof String)
                return (String) t;
            /* common lane */
            Object m = AbstractTerm.getSkel(t);
            Display d = AbstractTerm.getDisplay(t);
            return SpecialUniv.derefAndCastString(m, d);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Check whether the given term is an atom.</p>
     *
     * @param t The external Prolog term.
     * @return The wrapped string.
     * @throws InterpreterMessage Type error.
     */
    public static TermAtomic castStringWrapped(Object t)
            throws InterpreterMessage {
        try {
            /* fast lane */
            if (t instanceof TermAtomic &&
                    AbstractTerm.getSkel(t) instanceof SkelAtom)
                return (TermAtomic) t;
            /* common lane */
            Object m = AbstractTerm.getSkel(t);
            Display d = AbstractTerm.getDisplay(t);
            return new TermAtomic(SpecialUniv.derefAndCastStringWrapped(m, d));
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Check whether the given term is a number.</p>
     *
     * @param t The external Prolog term.
     * @return The number.
     * @throws InterpreterMessage Not a number.
     */
    public static Number castNumber(Object t)
            throws InterpreterMessage {
        try {
            /* fast lane */
            if (t instanceof String)
                throw new InterpreterMessage(InterpreterMessage.typeError(
                        EngineMessage.OP_TYPE_NUMBER, t));
            /* common lane */
            Object m = AbstractTerm.getSkel(t);
            Display d = AbstractTerm.getDisplay(t);
            return SpecialEval.derefAndCastNumber(m, d);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Check whether the given term is an integer.</p>
     *
     * @param t The external Prolog term.
     * @return The integer, either Integer or BigInteger.
     * @throws InterpreterMessage Not a integer.
     */
    public static Number castInteger(Object t)
            throws InterpreterMessage {
        try {
            /* fast lane */
            if (t instanceof String)
                throw new InterpreterMessage(InterpreterMessage.typeError(
                        EngineMessage.OP_TYPE_INTEGER, t));
            /* common lane */
            Object m = AbstractTerm.getSkel(t);
            Display d = AbstractTerm.getDisplay(t);
            return SpecialEval.derefAndCastInteger(m, d);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Check whether the given term is a Prolog decimal.</p>
     *
     * @param t The external Prolog term.
     * @return The decimal, either Long or BigDecimal.
     * @throws InterpreterMessage Not a integer.
     */
    public static Number castDecimal(Object t)
            throws InterpreterMessage {
        try {
            /* fast lane */
            if (t instanceof String)
                throw new InterpreterMessage(InterpreterMessage.typeError(
                        EngineMessage.OP_TYPE_DECIMAL, t));
            /* common lane */
            Object m = AbstractTerm.getSkel(t);
            Display d = AbstractTerm.getDisplay(t);
            return SpecialEval.derefAndCastDecimal(m, d);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Check whether the given term is a reference.</p>
     *
     * @param t The external Prolog term.
     * @return The reference.
     * @throws InterpreterMessage Not a reference.
     */
    public static Object castRef(Object t)
            throws InterpreterMessage {
        try {
            /* fast lane */
            if (t instanceof String)
                throw new InterpreterMessage(InterpreterMessage.typeError(
                        EngineMessage.OP_TYPE_REF, t));
            /* common lane */
            Object m = AbstractTerm.getSkel(t);
            Display d = AbstractTerm.getDisplay(t);
            return SpecialUniv.derefAndCastRef(m, d);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /******************************************************************/
    /* Map Exceptions                                                 */
    /******************************************************************/

    /**
     * <p>Map an IO Exception.</p>
     *
     * @param exception The IO Exception.
     * @return The interpreter message.
     */
    public static InterpreterMessage mapIOException(IOException exception) {
        return new InterpreterMessage(EngineMessage.mapIOException(exception));
    }

    /**
     * <p>Clear the signal.</p>
     *
     * @return The old signal, can be null.
     */
    public static InterpreterMessage sysThreadClear() {
        EngineMessage h = (EngineMessage) ForeignThread.sysThreadClear();
        return (h != null ? new InterpreterMessage(h) : null);
    }

}
