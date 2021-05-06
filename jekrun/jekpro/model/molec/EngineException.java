package jekpro.model.molec;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.PrologWriter;
import jekpro.model.rope.LoadOpts;
import jekpro.reference.reflect.SpecialPred;
import jekpro.tools.term.*;
import matula.util.data.ListArray;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Locale;
import java.util.Properties;

/**
 * <p>This class defines an engine exception which consists
 * of a message, eventually a text location and a back trace.
 * This is typically thrown by the engine which has access to
 * the text location and the back trace. The private constructor
 * accepts a skeleton and a display. It will make a copy of the
 * given exception term and internally store a new skeleton.
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
public final class EngineException extends Exception {
    public final static String OP_ERROR = "error";
    public final static String OP_WARNING = "warning";
    public final static String OP_PRED = "pred";
    public final static String OP_PRED_MORE = "pred_more";
    public final static String OP_PRED_FILE_LINE = "pred_file_line";

    private final static String OP_CAUSE = "cause";
    private final static String OP_FILE_LINE = "file_line";
    private final static String OP_TEXT_POS = "text_pos";

    private final static int ARG_PRIMARY = 0;
    private final static int ARG_SECONDARY = 1;

    private final Object template;

    /**
     * <p>No stack filling.</p>
     *
     * @return This throwable.
     */
    public Throwable fillInStackTrace() {
        return this;
    }

    /**
     * <p>Non-copying constructor.</p>
     *
     * @param m The exception skeleton.
     */
    public EngineException(Object m) {
        if (SupervisorCopy.getVar(m) != null)
            throw new IllegalArgumentException("needs display");
        template = m;
    }

    /**
     * <p>Copying constructor.</p>
     *
     * @param t The exception skeleton.
     * @param d The exception display.
     */
    public EngineException(Object t, Display d) {
        SupervisorCopy ec = new SupervisorCopy();
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
     * <p>Create an engine exception from skeleton and size.</p>
     *
     * @param msg The engine message.
     * @param ctx The back trace.
     */
    public EngineException(EngineMessage msg, Object ctx) {
        if (SupervisorCopy.getVar(ctx) != null)
            throw new IllegalArgumentException("needs display");
        template = new SkelCompound(new SkelAtom(OP_ERROR), msg.getTemplate(), ctx);
        initCause(msg);
    }

    /**
     * <p>Create an engine exception from skeleton and size.</p>
     *
     * @param msg  The engine message.
     * @param ctx  The back trace.
     * @param type The type functor.
     */
    public EngineException(EngineMessage msg, Object ctx, String type) {
        if (SupervisorCopy.getVar(ctx) != null)
            throw new IllegalArgumentException("needs display");
        template = new SkelCompound(new SkelAtom(type), msg.getTemplate(), ctx);
        initCause(msg);
    }

    /**
     * <p>Make an engine exception from two engine exceptions.</p>
     * <p>Will do the following transformation:</p>
     * <ul>
     * <li>Before: First = cause(X1,..cause(Xn-1,Xn)), Second = Y.</li>
     * <li>After: Result = cause(X1,..cause(Xn-1,cause(Xn,Y))).</li>
     * </ul>
     *
     * @param e1 The first engine exception.
     * @param e2 The second engine exception.
     */
    public EngineException(EngineException e1, EngineException e2) {
        SupervisorCopy eb = new SupervisorCopy();
        /* unpack cause chain */
        ListArray<Object> list = new ListArray<>();
        Object m = e1.getTemplate();
        while (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 2 &&
                ((SkelCompound) m).sym.fun.equals(OP_CAUSE)) {
            list.add(((SkelCompound) m).args[ARG_PRIMARY]);
            m = ((SkelCompound) m).args[ARG_SECONDARY];
        }
        list.add(m);
        /* copy second */
        Display ref = AbstractSkel.createDisplay(e2.getTemplate());
        m = eb.copyTerm(e2.getTemplate(), ref);
        /* copy cause chain */
        ref = AbstractSkel.createDisplay(e1.getTemplate());
        for (int i = list.size() - 1; i >= 0; i--) {
            m = new SkelCompound(new SkelAtom(OP_CAUSE),
                    eb.copyTerm(list.get(i), ref), m);
        }
        template = m;
    }

    /**
     * <p>Create the back trace from the current call chain.</p>
     * <p>The following frame terms are used:</p>
     * <pre>
     *      frame       :== "file_line(" atom "," integer ")" |
     *                      "pred_file_line(" indicator "," atom "," integer ")" |
     *                      "pred(" indicator ")" |
     *                      "pred_more(" integer ")".
     *      indicator   :== [ path ":" ] name "/" integer.
     * </pre>
     *
     * @param en The engine.
     * @return The exception skeleton.
     */
    public static Object fetchStack(Engine en) {
        try {
            StackElement stack = StackElement.skipNoTrace(en, en);
            int k = 0;
            SkelCompound back = null;
            /* iterator and fetch pred_file_line, pred and pred_error */
            while (stack != null && k < en.store.getMaxStack()) {
                StackElement.callGoal(stack.contskel, stack.contdisplay, en);
                int arity = StackElement.callableToArity(en.skel);
                SkelAtom sa = StackElement.callableToName(en.skel);
                Object val;
                if (arity == 1 &&
                        (sa.fun.equals(AbstractSource.OP_SYS_LOAD_STREAM) ||
                                sa.fun.equals(AbstractSource.OP_SYS_BOOT_STREAM))) {
                    val = ((SkelCompound) en.skel).args[0];
                    PositionKey pos = PositionKey.createPos(val);
                    if (pos != null) {
                        val = new SkelCompound(new SkelAtom(OP_FILE_LINE),
                                new SkelAtom(pos.getOrigin()),
                                Integer.valueOf(pos.getLineNo()));
                    } else {
                        val = null;
                    }
                } else if (sa != null) {
                    val = SpecialPred.indicatorToColonSkel(sa, arity, en);
                    PositionKey pos = sa.getPosition();
                    if (pos != null) {
                        val = new SkelCompound(new SkelAtom(OP_PRED_FILE_LINE), val,
                                new SkelAtom(pos.getOrigin()),
                                Integer.valueOf(pos.getLineNo()));
                    } else {
                        val = new SkelCompound(new SkelAtom(OP_PRED), val);
                    }
                } else {
                    val = null;
                }
                if (val != null)
                    back = new SkelCompound(en.store.foyer.ATOM_CONS, val, back);
                k++;
                stack = StackElement.skipNoTrace(stack.contdisplay, en);
            }
            k = 0;
            /* count and fetch pred_more */
            while (stack != null) {
                k++;
                stack = StackElement.skipNoTrace(stack.contdisplay, en);
            }
            if (k != 0) {
                Object val = new SkelCompound(new SkelAtom(OP_PRED_MORE), Integer.valueOf(k));
                back = new SkelCompound(en.store.foyer.ATOM_CONS, val, back);
            }
            /* reverse list */
            Object t = en.store.foyer.ATOM_NIL;
            while (back != null) {
                SkelCompound jack = (SkelCompound) back.args[back.args.length - 1];
                back.args[back.args.length - 1] = t;
                t = back;
                back = jack;
            }
            return t;
        } catch (EngineException x) {
            throw new RuntimeException("give up", x);
        } catch (EngineMessage x) {
            throw new RuntimeException("give up", x);
        }
    }

    /**
     * <p>Add the text position to a back trace.</p>
     * <p>The following position terms are used:</p>
     * <pre>
     *     position     :== "text_pos(" atom ")"
     * </pre>
     *
     * @param res  The old back trace.
     * @param line The position, or null.
     * @param en   The engine.
     * @return The new back trace.
     */
    public static Object fetchPos(Object res, String line, Engine en) {
        if (line == null)
            return res;
        Object val = new SkelCompound(new SkelAtom(OP_TEXT_POS), new SkelAtom(line));
        return new SkelCompound(en.store.foyer.ATOM_CONS, val, res);
    }

    /**
     * <p>Add the text location to a back trace.</p>
     * <p>The following location terms are used:</p>
     * <pre>
     *      location    :== "file_line(" atom "," integer ")".
     * </pre>
     *
     * @param res The old back trace.
     * @param pos The position, can be null.
     * @param en  The engine.
     * @return The new back trace.
     */
    public static Object fetchLoc(Object res, PositionKey pos, Engine en) {
        if (pos == null)
            return res;
        Object val = new SkelCompound(new SkelAtom(OP_FILE_LINE),
                new SkelAtom(pos.getOrigin()), Integer.valueOf(pos.getLineNo()));
        return new SkelCompound(en.store.foyer.ATOM_CONS, val, res);
    }

    /*******************************************************/
    /* Stack Trace Printing                                */
    /*******************************************************/

    /**
     * <p>Retrieve the detailed messsage for the engine exception.</p>
     * <p>Will dynamically build the message each time the method is called.</p>
     *
     * @return The detailed message.
     */
    public String getMessage() {
        try {
            Display ref = AbstractSkel.createDisplay(template);
            return errorMake(template, ref, null, null, null);
        } catch (EngineMessage x) {
            throw new RuntimeException("shouldn't happen", x);
        } catch (EngineException x) {
            throw new RuntimeException("shouldn't happen", x);
        }
    }

    /**
     * <p>Retrieve the detailed messsage for the engine exception.</p>
     * <p>Will dynamically build the message each time the method is called.</p>
     *
     * @param en The engine.
     * @return The detailed message.
     * @throws IOException     Shit happens.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public String getMessage(Engine en)
            throws IOException, EngineException, EngineMessage {
        Locale locale = en.store.foyer.locale;
        Properties lang = EngineMessage.getErrorLang(locale, en.store);
        Display ref = AbstractSkel.createDisplay(template);
        return errorMake(template, ref, locale, lang, en);
    }


    /**
     * <p>Print the user-friendly stack trace.</p>
     * <p>Print on the disp output.</p>
     *
     * @param en The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public void printStackTrace(Engine en)
            throws EngineMessage, EngineException {
        Object obj = en.visor.curerror;
        LoadOpts.checkTextWrite(obj);
        Writer wr = (Writer) obj;
        try {
            Locale locale = en.store.foyer.locale;
            Properties lang = EngineMessage.getErrorLang(locale, en.store);
            Display ref = AbstractSkel.createDisplay(template);
            EngineException.printStackTrace(wr, template, ref, locale, lang, en);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Print the user-friendly stack trace.</p>
     * <p>Print on the disp output.</p>
     *
     * @param wr The writer.
     * @param en The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public void printStackTrace(Writer wr, Engine en)
            throws EngineMessage, EngineException {
        try {
            Locale locale = en.store.foyer.locale;
            Properties lang = EngineMessage.getErrorLang(locale, en.store);
            Display ref = AbstractSkel.createDisplay(template);
            EngineException.printStackTrace(wr, template, ref, locale, lang, en);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Create the user-friendly detail message from the exception term.</p>
     * <p>Will dynamically build the message each time the method is called.</p>
     * <p>The following rules apply.</p>
     * <pre>
     *      error(Message, Context):   property('exception.error') ": ", message(Message)
     *      warning(Message, Context): property('exception.warning') ": ", message(message)
     *      cause(Primary, Secondary): errorMake(Primary)
     *      AbstractTerm                       property('exception.unknown') ": " string(AbstractTerm)
     * </pre>
     *
     * @param term   The exception skel.
     * @param ref    The exception display.
     * @param locale The locale.
     * @param prop   The properties.
     * @param en     The engine.
     * @return The exception message.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static String errorMake(Object term, Display ref,
                                   Locale locale, Properties prop,
                                   Engine en)
            throws EngineMessage, EngineException {
        for (; ; ) {
            BindUniv b;
            while (term instanceof SkelVar &&
                    (b = ref.bind[((SkelVar) term).id]).display != null) {
                term = b.skel;
                ref = b.display;
            }
            if ((term instanceof SkelCompound) &&
                    ((SkelCompound) term).sym.fun.equals(EngineException.OP_ERROR) &&
                    ((SkelCompound) term).args.length == 2) {
                SkelCompound sc = (SkelCompound) term;
                StringBuilder buf = new StringBuilder();
                buf.append(prop != null ? prop.getProperty("exception.error") : "Error");
                buf.append(": ");
                buf.append(EngineMessage.messageMake(sc.args[0], ref, locale, prop, en));
                return buf.toString();
            } else if ((term instanceof SkelCompound) &&
                    ((SkelCompound) term).sym.fun.equals(EngineException.OP_WARNING) &&
                    ((SkelCompound) term).args.length == 2) {
                SkelCompound sc = (SkelCompound) term;
                StringBuilder buf = new StringBuilder();
                buf.append(prop != null ? prop.getProperty("exception.warning") : "Warning");
                buf.append(": ");
                buf.append(EngineMessage.messageMake(sc.args[0], ref, locale, prop, en));
                return buf.toString();
            } else if ((term instanceof SkelCompound) &&
                    ((SkelCompound) term).sym.fun.equals(OP_CAUSE) &&
                    ((SkelCompound) term).args.length == 2) {
                SkelCompound sc = (SkelCompound) term;
                term = sc.args[EngineException.ARG_PRIMARY];
            } else {
                StringWriter buf = new StringWriter();
                buf.append(prop != null ? prop.getProperty("exception.unknown") : "Unknown exception");
                buf.append(": ");
                PrologWriter.toString(term, ref, buf, PrologWriter.FLAG_QUOT, en);
                return buf.toString();
            }
        }
    }

    /**
     * <p>Print the user-friendly detailed message and stack trace from the exception term.</p>
     * <p>The following rules apply:</p>
     * <pre>
     *      error(Message, Context):   errorMake(This) "\n" printContext(Context)
     *      warning(Message, Context): errorMake(This) "\n" printContext(Context)
     *      cause(Primary, Secondary): printStackTrace(Primary} printStackTrace(Secondary)
     *      AbstractTerm                       errorMake(This)
     * </pre>
     *
     * @param wr     The writer.
     * @param term   The exception skel.
     * @param ref    The exception display.
     * @param locale The locale.
     * @param prop   The properties.
     * @param en     The engine.
     * @throws IOException   IO error.
     * @throws EngineMessage Not a number.
     */
    public static void printStackTrace(Writer wr, Object term, Display ref,
                                       Locale locale, Properties prop,
                                       Engine en)
            throws IOException, EngineMessage, EngineException {
        for (; ; ) {
            BindUniv b;
            while (term instanceof SkelVar &&
                    (b = ref.bind[((SkelVar) term).id]).display != null) {
                term = b.skel;
                ref = b.display;
            }
            if ((term instanceof SkelCompound) &&
                    ((SkelCompound) term).sym.fun.equals(OP_ERROR) &&
                    ((SkelCompound) term).args.length == 2) {
                SkelCompound sc = (SkelCompound) term;
                wr.write(errorMake(term, ref, locale, prop, en));
                wr.write('\n');
                wr.flush();
                EngineException.printContext(wr, sc.args[1], ref, locale, prop, en);
                return;
            } else if ((term instanceof SkelCompound) &&
                    ((SkelCompound) term).sym.fun.equals(OP_WARNING) &&
                    ((SkelCompound) term).args.length == 2) {
                SkelCompound sc = (SkelCompound) term;
                wr.write(errorMake(term, ref, locale, prop, en));
                wr.write('\n');
                wr.flush();
                EngineException.printContext(wr, sc.args[1], ref, locale, prop, en);
                return;
            } else if ((term instanceof SkelCompound) &&
                    ((SkelCompound) term).sym.fun.equals(OP_CAUSE) &&
                    ((SkelCompound) term).args.length == 2) {
                SkelCompound sc = (SkelCompound) term;
                EngineException.printStackTrace(wr,
                        sc.args[EngineException.ARG_PRIMARY], ref, locale, prop, en);
                term = sc.args[EngineException.ARG_SECONDARY];
            } else {
                wr.write(errorMake(term, ref, locale, prop, en));
                wr.write('\n');
                wr.flush();
                return;
            }
        }
    }

    /**
     * <p>Print the user-friendly exception context.</p>
     * <p>The following rules apply:</p>
     * <pre>
     *      [Message|Context]: message(Message) "\n" printContext(Context)
     *      []:                []
     *      AbstractTerm:              property('context.unknown') ": " string(AbstractTerm) "\n"
     * <pre>
     *
     * @param wr     The output stream writer.
     * @param term   The context skel.
     * @param ref    The context display.
     * @param locale The locale.
     * @param prop   The properties.
     * @param en     The engine.
     * @throws IOException   IO error.
     * @throws EngineMessage Not a number.
     */
    private static void printContext(Writer wr, Object term, Display ref,
                                     Locale locale, Properties prop,
                                     Engine en)
            throws IOException, EngineMessage, EngineException {
        for (; ; ) {
            BindUniv b;
            while (term instanceof SkelVar &&
                    (b = ref.bind[((SkelVar) term).id]).display != null) {
                term = b.skel;
                ref = b.display;
            }
            if (term instanceof SkelCompound &&
                    ((SkelCompound) term).sym.fun.equals(Foyer.OP_CONS) &&
                    ((SkelCompound) term).args.length == 2) {
                SkelCompound sc = (SkelCompound) term;
                wr.write(EngineMessage.messageMake(sc.args[0], ref, locale, prop, en));
                wr.write('\n');
                wr.flush();
                term = sc.args[1];
            } else if ((term instanceof SkelAtom) &&
                    ((SkelAtom) term).fun.equals(Foyer.OP_NIL)) {
                /* do nothing */
                return;
            } else {
                wr.write(prop != null ? prop.getProperty("context.unknown") : "Unknown context");
                wr.write(": ");
                PrologWriter.toString(term, ref, wr, PrologWriter.FLAG_QUOT, en);
                wr.write('\n');
                wr.flush();
                return;
            }
        }
    }

    /***********************************************************/
    /* Exception Checking                                      */
    /***********************************************************/

    /**
     * <p>Check the type of an exception term.</p>
     * <p>Means checking whether it is of the form:
     * <pre>
     *     <fun>(Type,_)
     *     cause(<fun>(Type,_),_)
     * </pre>
     *
     * @param fun The functor.
     * @return The type, or null.
     */
    public EngineMessage exceptionType(String fun) {
        if (template instanceof SkelCompound &&
                ((SkelCompound) template).args.length == 2 &&
                ((SkelCompound) template).sym.fun.equals(fun)) {
            Object o = ((SkelCompound) template).args[0];
            Display ref = AbstractSkel.createDisplay(template);
            return new EngineMessage(o, ref);
        } else if (template instanceof SkelCompound &&
                ((SkelCompound) template).args.length == 2 &&
                ((SkelCompound) template).sym.fun.equals(OP_CAUSE)) {
            Object m = ((SkelCompound) template).args[ARG_PRIMARY];
            if (m instanceof SkelCompound &&
                    ((SkelCompound) m).args.length == 2 &&
                    ((SkelCompound) m).sym.fun.equals(fun)) {
                Object o = ((SkelCompound) m).args[0];
                Display ref = AbstractSkel.createDisplay(template);
                return new EngineMessage(o, ref);
            }
        }
        return null;
    }

    /**
     * <p>Check whether the exception term is a cause chain.</p>
     * <p>Means checking whether it is of the form:
     * <pre>
     *     cause(_, Rest)
     * </pre>
     *
     * @return The rest, or null,
     */
    public final EngineException causeChainRest() {
        if (template instanceof SkelCompound &&
                ((SkelCompound) template).args.length == 2 &&
                OP_CAUSE.equals(((SkelCompound) template).sym.fun)) {
            Object o = ((SkelCompound) template).args[ARG_SECONDARY];
            Display ref = AbstractSkel.createDisplay(template);
            return new EngineException(o, ref);
        }
        return null;
    }

}
