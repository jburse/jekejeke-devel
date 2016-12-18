package jekpro.tools.term;

import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.pretty.*;
import jekpro.model.rope.Intermediate;
import jekpro.frequent.standard.EngineCopy;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import matula.util.regex.ScannerError;
import matula.util.system.OpenOpts;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;

/**
 * <p>This class provides writing, reading, unification and copying
 * of Prolog terms. The Java to Prolog API does provide specific
 * data types for all Prolog terms. But the end-user has choices,
 * he might have Prolog atomics unwrapped. Only compounds and
 * variables need always be wrapped, since the API data type
 * aggregates a skeleton and display.
 * </p>
 * <p>The external API roots the terms either in the Java Object
 * class or in the AbstractTerm class. In both cases the hashCode(), equals()
 * and unparseTerm() methods of the Java Object class can be used. The
 * realization is such that that even for non-ground Prolog terms
 * these methods correspond to the Prolog term_hash/2, ==/2
 * and write/1.
 * </p>
 * <p>The unparseTerm() methods convert a term to a string. If an
 * interpreter is supplied operator definitions will be available
 * and variable are dereferenced. The following flags are recognized.
 * The method with an option term recognizes all options from the
 * write_term/2 predicate, but cannot handle a null interpreter:</p>
 * <ul>
 * <li><b>FLAG_QUOTED:</b> Quote atoms when necessary.</li>
 * <li><b>FLAG_NUMBERVARS:</b> Write $VAR(n) as a variable name.</li>
 * <li><b>FLAG_IGNORE_OPS:</b> Ignore operator definitions.</li>
 * <li><b>FLAG_IGNORE_MOD:</b>Ignore module prefixes.</li>
 * </ul>
 * <p>The parseNumber() method converts a string to a number
 * term. The parseTerm() methods convert a string or stream to
 * a term. The method that takes a string doesn't require that
 * the term is terminated by a period and returns null when an
 * empty string has been supplied. The method with an option
 * term recognizes all options from the read_term/2 predicate
 * and returns null when the option unification fails.
 * </p>
 * <p>The method unifyTerm() attempts a unification. For performance
 * reasons a failed unification might leave variable bindings.
 * If the variable bindings need an undoing by the application
 * program it is bested to use unification in combination with
 * an empty interactor, see also the class Interpreter.
 * </p>
 * <p>The method copyTerm() creates a copy of the given term. The
 * method will create new compounds for those sub branches of the
 * original term that either contain variables or that need to
 * dereference variables and don't lead to atomics. The method
 * variant copyTermWrapped() returns a wrapped result.
 * </p>
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
public abstract class AbstractTerm {
    public final static Object VOID_OBJ = AbstractSkel.VOID_OBJ;

    /**
     * <p>Create a term by the given skeleton and display.</p>
     * <p>Will first determine the type of the skeleton and
     * then switch to the constructor of the apppropriate subclass.</p>
     * <p>Will unpack atoms and not wrap numbers and references.</p>
     *
     * @param m The skeleton.
     * @param d The display.
     * @return The term.
     */
    public static Object createTerm(Object m, Display d) {
        if (m instanceof SkelVar) {
            return new TermVar((SkelVar) m, d);
        } else if (m instanceof SkelCompound) {
            return new TermCompound((SkelCompound) m, d);
        } else if (m instanceof SkelAtom) {
            return ((SkelAtom) m).fun;
        } else if (m != null) {
            return m;
        } else {
            throw new NullPointerException("internal null");
        }
    }

    /**
     * <p>Create a molec by the given skeleton and display.</p>
     *
     * @param m The skeleton.
     * @param d The display.
     * @return The molec.
     */
    public static Object createMolec(Object m, Display d) {
        if (m instanceof SkelVar) {
            return new TermVar((SkelVar) m, d);
        } else if (m instanceof SkelCompound) {
            return new TermCompound((SkelCompound) m, d);
        } else {
            return m;
        }
    }

    /**
     * <p>Create a term by the given skeleton and display.</p>
     * <p>Will first determine the type of the skeleton and
     * then switch to the constructor of the apppropriate subclass.</p>
     * <p>Will wrap atoms, numbers and references.</p>
     *
     * @param m The skeleton.
     * @param d The display.
     * @return The term.
     */
    public static AbstractTerm createTermWrapped(Object m, Display d) {
        if (m instanceof SkelVar) {
            return new TermVar((SkelVar) m, d);
        } else if (m instanceof SkelCompound) {
            return new TermCompound((SkelCompound) m, d);
        } else if (m != null) {
            return new TermAtomic(m, false);
        } else {
            throw new NullPointerException("internal null");
        }
    }

    /**
     * <p>Retrieve the skeleton.</p>
     * <p>Works for wrapped and unwrapped data structure.</p>
     *
     * @param t The term.
     * @return The skeleton.
     */
    public static Object getSkel(Object t) {
        if (t instanceof TermVar) {
            return ((TermVar) t).skel;
        } else if (t instanceof TermCompound) {
            return ((TermCompound) t).skel;
        } else if (t instanceof String) {
            return new SkelAtom((String) t);
        } else if (t instanceof TermAtomic) {
            return ((TermAtomic) t).skel;
        } else if (t != null) {
            return t;
        } else {
            throw new NullPointerException("external null");
        }
    }

    /**
     * <p>Retrieve the display.</p>
     * <p>Works for wrapped and unwrapped data structure.</p>
     *
     * @param t The term.
     * @return The display.
     */
    public static Display getDisplay(Object t) {
        if (t instanceof TermVar) {
            return ((TermVar) t).display;
        } else if (t instanceof TermCompound) {
            return ((TermCompound) t).display;
        } else if (t != null) {
            return Display.DISPLAY_CONST;
        } else {
            throw new NullPointerException("external null");
        }
    }

    /**
     * <p>Return a string of a skeleton.</p>
     *
     * @return The string.
     */
    public String toString() {
        return PrologWriter.toString(getSkel(this), getDisplay(this), 0);
    }

    /****************************************************************/
    /* Prolog Operations                                            */
    /****************************************************************/

    /**
     * <p>Unify the this term for another term.</p>
     * <p>Occurs check is not performed.</p>
     *
     * @param inter The call-in.
     * @param fst   The first term.
     * @param snd   The second term.
     * @return True if the this term unifies with the other term, otherwise false.
     * @throws InterpreterException Shit happens.
     * @throws InterpreterMessage   Shit happens.
     */
    public static boolean unifyTerm(Interpreter inter, Object fst, Object snd)
            throws InterpreterException, InterpreterMessage {
        CallOut co = inter.getCallOut();
        Intermediate r;
        DisplayClause u;
        if (co == null) {
            r = null;
            u = null;
        } else {
            r = co.getGoalSkel();
            u = co.getGoalDisplay();
        }

        Engine en = (Engine) inter.getEngine();
        Engine backuse = en.visor.setInuse(en);
        Thread backthread = en.visor.setFence(Thread.currentThread());
        boolean res;
        try {
            res = en.unifyTerm(AbstractTerm.getSkel(fst), AbstractTerm.getDisplay(fst),
                    AbstractTerm.getSkel(snd), AbstractTerm.getDisplay(snd), r, u);
        } catch (EngineMessage x) {
            en.visor.setFence(backthread);
            en.visor.setInuse(backuse);
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            en.visor.setFence(backthread);
            en.visor.setInuse(backuse);
            throw new InterpreterException(x);
        }
        en.visor.setFence(backthread);
        en.visor.setInuse(backuse);
        return res;
    }

    /**
     * <p>Create a copy of this term.</p>
     *
     * @param inter The call-in.
     * @param t     The term.
     * @return The copy of this term.
     */
    public static Object copyTerm(Interpreter inter, Object t) {
        /* fast lane */
        if (t instanceof String)
            return t;
        /* common lane */
        Object m = AbstractTerm.getSkel(t);
        Display d = AbstractTerm.getDisplay(t);

        Engine en = (Engine) inter.getEngine();
        EngineCopy ec = en.enginecopy;
        if (ec == null) {
            ec = new EngineCopy();
            en.enginecopy = ec;
        }
        ec.vars = null;
        Object val = ec.copyTerm(m, d);
        ec.vars = null;
        int size = Display.displaySize(val);
        Display ref = (size != 0 ? new Display(size) : Display.DISPLAY_CONST);
        if (val != m || ref != d)
            return AbstractTerm.createTerm(val, ref);
        return t;
    }

    /**
     * <p>Create a copy of this term.</p>
     *
     * @param inter The call-in.
     * @param t     The term.
     * @return The copy of this term.
     */
    public static AbstractTerm copyTermWrapped(Interpreter inter, Object t) {
        /* common lane */
        Object m = AbstractTerm.getSkel(t);
        Display d = AbstractTerm.getDisplay(t);

        Engine en = (Engine) inter.getEngine();
        EngineCopy ec = en.enginecopy;
        if (ec == null) {
            ec = new EngineCopy();
            en.enginecopy = ec;
        }
        ec.vars = null;
        Object val = ec.copyTerm(m, d);
        ec.vars = null;
        int size = Display.displaySize(val);
        Display ref = (size != 0 ? new Display(size) : Display.DISPLAY_CONST);
        if (val != m || ref != d || !(t instanceof AbstractTerm))
            return AbstractTerm.createTermWrapped(val, ref);
        return (AbstractTerm) t;
    }

}
