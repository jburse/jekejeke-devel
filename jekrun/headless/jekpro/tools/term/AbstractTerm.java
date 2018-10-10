package jekpro.tools.term;

import jekpro.frequent.standard.EngineCopy;
import jekpro.model.inter.Engine;
import jekpro.model.molec.AbstractBind;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.PrologWriter;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;

import java.io.StringWriter;

/**
 * This class provides writing, reading, unification and copying
 * of Prolog terms. The Java to Prolog API does provide specific
 * data types for all Prolog terms. But the end-user has choices,
 * he might have Prolog atomics unwrapped. Only compounds and
 * variables need always be wrapped, since the API data type
 * aggregates a skeleton and display.
 * <p>
 * The external API roots the terms either in the Java Object
 * class or in the AbstractTerm class. In both cases the hashCode(), equals()
 * and unparseTerm() methods of the Java Object class can be used. The
 * realization is such that that even for non-ground Prolog terms
 * these methods correspond to the Prolog term_hash/2, ==/2
 * and write/1.
 * <p>
 * The method unifyTerm() attempts a unification. For performance
 * reasons a failed unification might leave variable bindings.
 * If the variable bindings need an undoing by the application
 * program it is bested to use unification in combination with
 * an empty interactor, see also the class Interpreter.
 * <p>
 * The method copyTerm() creates a copy of the given term. The
 * method will create new compounds for those sub branches of the
 * original term that either contain variables or that need to
 * dereference variables and don't lead to atomics. The method
 * variant copyTermWrapped() returns a wrapped result.
 * <p>
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
public abstract class AbstractTerm {
    public final static Object VOID_OBJ = AbstractSkel.VOID_OBJ;

    /**
     * <p>Create a term by the given skeleton and display.</p>
     * <p>Will first determine the type of the skeleton and
     * then switch to the constructor of the apppropriate subclass.</p>
     * <p>Will unpack atoms.</p>
     * <p>Will not wrap numbers and references.</p>
     *
     * @param m The skeleton, not null.
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
     * @param m The skeleton, not null.
     * @param d The display.
     * @return The term.
     */
    public static AbstractTerm createTermWrapped(Object m, Display d) {
        if (m instanceof SkelVar) {
            return new TermVar((SkelVar) m, d);
        } else if (m instanceof SkelCompound) {
            return new TermCompound((SkelCompound) m, d);
        } else {
            return new TermAtomic(m, false);
        }
    }

    /**
     * <p>Create a term by the given skeleton and display.</p>
     * <p>Will first determine the type of the skeleton and
     * then switch to the constructor of the apppropriate subclass.</p>
     * <p>Will keep atoms.</p>
     * <p>Will not wrap numbers and references.</p>
     *
     * @param m The skeleton, not null.
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
     * <p>Retrieve the skeleton.</p>
     * <p>Works for unwrapped, wrapped and molec data structure.</p>
     *
     * @param t The term, not null.
     * @return The skeleton.
     */
    public static Object getSkel(Object t) {
        if (t instanceof AbstractTerm) {
            return ((AbstractTerm)t).getSkel();
        } else if (t instanceof String) {
            return new SkelAtom((String) t);
        } else {
            return t;
        }
    }

    /**
     * <p>Retrieve the display.</p>
     * <p>Works for unwrapped, wrapped and molec data structure.</p>
     *
     * @param t The term, not null.
     * @return The display.
     */
    public static Display getDisplay(Object t) {
        if (t instanceof AbstractTerm) {
            return ((AbstractTerm)t).getDisplay();
        } else {
            return Display.DISPLAY_CONST;
        }
    }

    /************************************************************/
    /* Variation Points                                         */
    /************************************************************/

    /**
     * <p>Retrieve the skeleton.</p>
     *
     * @return The skeleton.
     */
    public abstract Object getSkel();

    /**
     * <p>Retrieve the display.</p>
     *
     * @return The display.
     */
    public abstract Display getDisplay();

    /************************************************************/
    /* Experimental Multi                                       */
    /************************************************************/

    /**
     * <p>Set the multi flag.</p>
     *
     * @param t The term.
     * @param m The multi flag.
     */
    public static void setMarker(Object t, Object m) {
        if (t instanceof TermVar) {
            ((TermVar) t).marker = m;
        } else if (t instanceof TermCompound) {
            ((TermCompound) t).marker = m;
        } else {
            /* */
        }
    }

    /**
     * <p>Retrieve the multi flag.</p>
     *
     * @param t The term.
     * @return The multi flag.
     */
    public static Object getMarker(Object t) {
        if (t instanceof TermVar) {
            return ((TermVar) t).marker;
        } else if (t instanceof TermCompound) {
            return ((TermCompound) t).marker;
        } else {
            return null;
        }
    }

    /************************************************************/
    /* String Generation                                        */
    /************************************************************/

    /**
     * <p>Return a string of a skeleton.</p>
     *
     * @return The string.
     */
    public String toString() {
        try {
            StringWriter sw = new StringWriter();
            PrologWriter.toString(getSkel(this), getDisplay(this), sw, 0, null);
            return sw.toString();
        } catch (EngineMessage x) {
            throw new RuntimeException("shouldn't happen", x);
        } catch (EngineException x) {
            throw new RuntimeException("shouldn't happen", x);
        }
    }

    /**
     * <p>Return a string of a skeleton.</p>
     *
     * @param flags The flags.
     * @return The string.
     */
    public String toString(int flags) {
        try {
            StringWriter sw = new StringWriter();
            PrologWriter.toString(getSkel(this), getDisplay(this), sw, flags, null);
            return sw.toString();
        } catch (EngineMessage x) {
            throw new RuntimeException("shouldn't happen", x);
        } catch (EngineException x) {
            throw new RuntimeException("shouldn't happen", x);
        }
    }

    /****************************************************************/
    /* Prolog Operations                                            */
    /****************************************************************/

    /**
     * <p>Retrieve the current bind.</p>
     *
     * @param inter The interpreter.
     * @return The current bind.
     */
    public static AbstractBind markBind(Interpreter inter) {
        Engine en = (Engine) inter.getEngine();
        return en.bind;
    }

    /**
     * <p>Unify the this term for another term.</p>
     * <p>Occurs check is not performed.</p>
     *
     * @param inter The call-in.
     * @param fst   The first term.
     * @param snd   The second term.
     * @return True if the this term unifies with the other term, otherwise false.
     * @throws InterpreterException Shit happens.
     */
    public static boolean unifyTerm(Interpreter inter, Object fst, Object snd)
            throws InterpreterException {
        Engine en = (Engine) inter.getEngine();
        Engine backuse = en.visor.setInuse(en);
        Thread backthread = en.visor.setFence(Thread.currentThread());
        boolean res;
        try {
            res = en.unifyTerm(AbstractTerm.getSkel(fst), AbstractTerm.getDisplay(fst),
                    AbstractTerm.getSkel(snd), AbstractTerm.getDisplay(snd));
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
     * <p>Release variable bindings done during unification.</p>
     *
     * @param inter The interpreter.
     * @param mark  The marked bind.
     * @throws InterpreterException Shit happens.
     */
    public static void releaseBind(Interpreter inter, AbstractBind mark)
            throws InterpreterException {
        Engine en = (Engine) inter.getEngine();
        Engine backuse = en.visor.setInuse(en);
        Thread backthread = en.visor.setFence(Thread.currentThread());
        try {
            en.skel = null;
            en.releaseBind(mark);
            if (en.skel != null)
                throw (EngineException) en.skel;
        } catch (EngineException x) {
            en.visor.setFence(backthread);
            en.visor.setInuse(backuse);
            throw new InterpreterException(x);
        }
        en.visor.setFence(backthread);
        en.visor.setInuse(backuse);
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
        if (val == m && !(t instanceof SkelAtom) && !(t instanceof TermAtomic))
            return t;
        int size = Display.displaySize(val);
        Display ref = (size != 0 ? new Display(size) : Display.DISPLAY_CONST);
        Object res = AbstractTerm.createTerm(val, ref);
        if (size != 0)
            AbstractTerm.setMarker(res, new MutableBit().setBit(true));
        return res;
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
        if (val == m && (t instanceof AbstractTerm))
            return (AbstractTerm) t;
        int size = Display.displaySize(val);
        Display ref = (size != 0 ? new Display(size) : Display.DISPLAY_CONST);
        AbstractTerm res = AbstractTerm.createTermWrapped(val, ref);
        if (size != 0)
            AbstractTerm.setMarker(res, new MutableBit().setBit(true));
        return res;
    }

    /**
     * <p>Create a copy of this term.</p>
     *
     * @param inter The call-in.
     * @param t     The term.
     * @return The copy of this term.
     */
    public static Object copyMolec(Interpreter inter, Object t) {
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
        if (val == m && !(t instanceof String) && !(t instanceof TermAtomic))
            return t;
        int size = Display.displaySize(val);
        Display ref = (size != 0 ? new Display(size) : Display.DISPLAY_CONST);
        Object res = AbstractTerm.createMolec(val, ref);
        if (size != 0)
            AbstractTerm.setMarker(res, new MutableBit().setBit(true));
        return res;
    }

}
