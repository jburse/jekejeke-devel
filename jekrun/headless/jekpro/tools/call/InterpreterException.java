package jekpro.tools.call;

import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.PositionKey;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>This class defines an interpreter exception which consists
 * of a message, eventually an text location and a back trace.
 * This is typically thrown by the interpreter which has access to
 * the text location and the back trace. The interpreter exception
 * class cannot be sub-classed by the application programmer. But the
 * application programmer can create and also throw interpreter
 * exceptions.
 * </p>
 * <p>The application programmer can provide an arbitrary exception
 * term to the basic constructor. A copy of the given term will
 * be created and stored in the exception. The copied exception term
 * can be retrieved via the method getValue(). The location of the
 * message term depends on the form of the exception term. The
 * following exception terms are recognized::
 * </p>
 * <pre>
 *      error(Message, Context)
 *      warning(Message, Context)
 *      cause(Primary, Secondary)
 *      Message
 * </pre>
 * <p>Alternatively the application programmer can provide a message
 * and a context. This time error(Message, Context) will be created.
 * Or the application programmer can provide a mes-sage, a context
 * and a type. This will create <fun>(Message, Context). Further
 * the constructor that takes two interpreter exceptions will
 * create a cause/1 exception term.
 * </p>
 * <p>There are a couple of convenience methods to create the context.
 * The method fetchStack() retrieves the back trace from the current
 * call chain. The method fetchPos() adds the text position to a back
 * trace. The method fetchLocation() adds the text location to a back
 * trace.
 * </p>
 * <p>The detailed message methods getMessage() will give the message
 * term without the context. The variant without an interpreter argument
 * can only display skeletons. The variant with an interpreter argument
 * will attempt a user-friendly display. The printStackTrace() methods
 * will attempt to produce a user-friendly display of the message
 * together with the context. The variant without a writer argument
 * will display to the error stream. The variant with a writer argument
 * will produce the stack trace to this writer.
 * </p>
 * <p>There are a couple of convenience methods to analyse the exception
 * term. The method exceptionType() will return the Message when the
 * original exception term does have the form &lt;fun&gt;(Message,_) or
 * cause(&lt;fun&gt;(Message,_),_). Otherwise the method will return null.
 * The method causeChainRest() will return the Rest when the original
 * exception term does have the form cause(_,Rest). Otherwise the method
 * will return null.
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
public final class InterpreterException extends Exception {
    private final EngineException exception;

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
     * <p>Create an interpreter exception from a term.</p>
     *
     * @param t The term.
     */
    public InterpreterException(Object t) {
        exception = new EngineException(AbstractTerm.getSkel(t), AbstractTerm.getDisplay(t));
    }

    /**
     * <p>Create an interpreter exception from an interpreter message.</p>
     *
     * @param message The interpreter message.
     * @param context The stack trace.
     */
    public InterpreterException(InterpreterMessage message, Object context) {
        exception = new EngineException((EngineMessage) message.getException(),
                AbstractTerm.getSkel(context));
    }

    /**
     * <p>Create an interpreter exception from an interpreter message.</p>
     *
     * @param message The interpreter message.
     * @param context The stack trace.
     * @param type    The type functor.
     */
    public InterpreterException(InterpreterMessage message, Object context, String type) {
        exception = new EngineException((EngineMessage) message.getException(),
                AbstractTerm.getSkel(context), type);
    }

    /**
     * <p>Create an interpreter exception from two interpreter exceptions.</p>
     *
     * @param e1 The first interpreter exception.
     * @param e2 The second interpreter exception.
     */
    public InterpreterException(InterpreterException e1, InterpreterException e2) {
        exception = new EngineException(e1.exception, e2.exception);
    }

    /**
     * <p>Create an interpreter exception from an engine exception.</p>
     *
     * @param e The engine exception.
     */
    public InterpreterException(Exception e) {
        exception = (EngineException) e;
    }

    /**
     * <p>Retrieve the engine exception.</p>
     *
     * @return The engine exception.
     */
    public Exception getException() {
        return exception;
    }

    /**
     * <p>Retrieve the detailed messsage for the interpreter exception.</p>
     * <p>Will dynamically build the message each time the method is called.</p>
     *
     * @return The detailed messsage.
     */
    public String getMessage() {
        return exception.getMessage();
    }

    /**
     * <p>Retrieve the detailed messsage for the interpreter exception.</p>
     * <p>Will dynamically build the message each time the method is called.</p>
     *
     * @param inter The call-in.
     * @return The detailed messsage.
     * @throws IOException          Shit happens.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public String getMessage(Interpreter inter)
            throws IOException, InterpreterMessage, InterpreterException {
        try {
            return exception.getMessage(inter.getEngine());
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
    }

    /**
     * <p>Print the stack trace.</p>
     * <p>Print on the disp output.</p>
     *
     * @param inter The call-in.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public void printStackTrace(Interpreter inter)
            throws InterpreterMessage, InterpreterException {
        try {
            exception.printStackTrace(inter.getEngine());
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
    }

    /**
     * <p>Print the stack trace.</p>
     *
     * @param wr    The writer.
     * @param inter The call-in.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public void printStackTrace(Writer wr, Interpreter inter)
            throws InterpreterMessage, InterpreterException {
        try {
            exception.printStackTrace(wr, inter.getEngine());
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
    }

    /**
     * <p>Retrieve a copy of the exception term.</p>
     *
     * @return The copy of the exception term.
     */
    public Object getValue() {
        Object m = exception.getTemplate();
        Display ref = AbstractSkel.createMarker(m);
        return AbstractTerm.createTerm(m, ref);
    }

    /**
     * <p>Create the back trace from the current call chain.</p>
     *
     * @param inter The call-in.
     * @return The exception skeleton.
     */
    public static Object fetchStack(Interpreter inter) {
        return AbstractTerm.createTerm(
                EngineException.fetchStack(inter.getEngine()),
                Display.DISPLAY_CONST);
    }

    /**
     * <p>Add the text location to a back trace.</p>
     *
     * @param t     The old back trace.
     * @param pos   The position key, or null.
     * @param inter The call-in.
     * @return The new back trace.
     */
    public static Object fetchLoc(Object t, PositionKey pos, Interpreter inter) {
        return AbstractTerm.createTerm(
                EngineException.fetchLoc(AbstractTerm.getSkel(t), pos,
                        inter.getEngine()),
                AbstractTerm.getDisplay(t));
    }

    /**
     * <p>Add the text position to a back trace.</p>
     *
     * @param t     The old back trace.
     * @param line  The position, or null.
     * @param inter The call-in.
     * @return The new back trace.
     */
    public static Object fetchPos(Object t, String line, Interpreter inter) {
        return AbstractTerm.createTerm(
                EngineException.fetchPos(AbstractTerm.getSkel(t), line,
                        inter.getEngine()),
                AbstractTerm.getDisplay(t));
    }

    /**
     * <p>Check the type of an exception term.</p>
     * <p>Means checking whether it is of the form:
     * <pre>
     *     <fun>(Message,_)
     *     cause(<fun>(Message,_),_)
     * </pre>
     *
     * @param fun The functor.
     * @return The Message, or null.
     */
    public InterpreterMessage exceptionType(String fun) {
        EngineMessage m = exception.exceptionType(fun);
        return (m != null ? new InterpreterMessage(m) : null);
    }

    /**
     * <p>Check whether the exception term is a cause chain.</p>
     * <p>Means checking whether it is of the form:
     * <pre>
     *     cause(_, Rest)
     * </pre>
     *
     * @return The type, or null,
     */
    public InterpreterException causeChainRest() {
        EngineException m = exception.causeChainRest();
        return (m != null ? new InterpreterException(m) : null);
    }

    /**
     * <p>Handle system exceptions in a consult loop.</p>
     * <p>Will do the following:</p>
     * <ul>
     * <li><b>system_error(user_abort):</b> Print chain rest.</li>
     * <li><b>system_error(_):</b> Re-throw exception.</li>
     * <li><b>limit_error(_):</b> Re-throw exception.</li>
     * <li><b>_:</b> Print exception.</li>
     * </ul>
     *
     * @param x     The exception.
     * @param inter The call-in.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public static void systemConsultBreak(InterpreterException x,
                                          Interpreter inter)
            throws InterpreterMessage, InterpreterException {
        try {
            SpecialLoad.systemConsultBreak(x.exception, inter.getEngine());
        } catch (EngineMessage y) {
            throw new InterpreterMessage(y);
        } catch (EngineException y) {
            throw new InterpreterException(y);
        }
    }

}
