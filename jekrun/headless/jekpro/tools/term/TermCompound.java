package jekpro.tools.term;

import jekpro.frequent.standard.EngineCopy;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.reference.structure.SpecialLexical;
import jekpro.frequent.standard.SpecialSort;
import jekpro.tools.call.Interpreter;

/**
 * <p>This represents a compound term. There are two functor and argument
 * based constructor. The constructor without the interpreter parameter
 * assumes that the arguments belong to the same variable set. The
 * constructor with the interpreter parameter can deal with arguments
 * that belong to different variables sets.
 * </p>
 * <p>The constructors have been declared with variable arguments so that
 * multiple arguments can be easily specified by simply adding them to
 * the constructor call. The later constructor might create variable
 * bindings as a side effect.
 * </p>
 * <p>The functor and length can be retrieved by the methods getFunctor()
 * and getArity(). An individual argument can be retrieved by the method
 * getArg(). The later method will automatically do a dereferencing.
 * The method variants getFunctorWrapped() and getArgWrapped() return
 * wrapped results.
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
public final class TermCompound extends AbstractTerm {
    final SkelCompound skel;
    final Display display;

    /**
     * <p>Constructor for internal use only.</p>
     *
     * @param s The compound skel.
     * @param d The compound display.
     */
    public TermCompound(SkelCompound s, Display d) {
        skel = s;
        display = d;
    }

    /**
     * <p>Constructor for internal use only.</p>
     *
     * @param s The compound skel.
     */
    public TermCompound(SkelCompound s) {
        skel = s;
        display = Display.DISPLAY_CONST;
    }

    /**
     * <p>Create a compound from functor and arguments.</p>
     * <p>The arguments must have single display.</p>
     *
     * @param sym  The functor.
     * @param args The arguments.
     */
    public TermCompound(String sym, Object... args) {
        this(new SkelCompound(new SkelAtom(sym), makeArgs(args)),
                TermCompound.createCount(args));
    }

    /**
     * <p>Create a compound from functor and arguments.</p>
     * <p>The arguments must have single display.</p>
     *
     * @param sym  The functor.
     * @param args The arguments.
     */
    public TermCompound(TermAtomic sym, Object... args) {
        this(new SkelCompound(getFun(sym), makeArgs(args)),
                TermCompound.createCount(args));
    }

    /**
     * <p>Create a compound from interpreter, functor and arguments.</p>
     *
     * @param inter The call-in.
     * @param sym   The functor.
     * @param args  The arguments.
     */
    public TermCompound(Interpreter inter, String sym, Object... args) {
        this(new SkelCompound(new SkelAtom(sym),
                        makeArgs((Engine) inter.getEngine(), args)),
                ((Engine) inter.getEngine()).display);
    }

    /**
     * <p>Create a compound from interpreter, functor and arguments.</p>
     *
     * @param inter The call-in.
     * @param sym   The functor.
     * @param args  The arguments.
     */
    public TermCompound(Interpreter inter, TermAtomic sym, Object... args) {
        this(new SkelCompound(getFun(sym),
                        makeArgs((Engine) inter.getEngine(), args)),
                ((Engine) inter.getEngine()).display);
    }

    /**
     * <p>Compute the hash.</p>
     *
     * @return The hash value.
     */
    public int hashCode() {
        return SpecialSort.hashCode(skel, display, 0);
    }

    /**
     * <p>Check the identity to another molec compound.</p>
     *
     * @param o The other object.
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof TermCompound))
            return false;
        TermCompound m = (TermCompound) o;
        return SpecialLexical.equalTerm(skel, display, m.skel, m.display);
    }

    /**
     * <p>Retrieve the functor of the compound.</p>
     *
     * @return The functor.
     */
    public String getFunctor() {
        return skel.sym.fun;
    }

    /**
     * <p>Retrieve the functor of the compound.</p>
     *
     * @return The functor.
     */
    public TermAtomic getFunctorWrapped() {
        return new TermAtomic(skel.sym, false);
    }

    /**
     * <p>Retrieve the length of the compound.</p>
     *
     * @return The length.
     */
    public int getArity() {
        return skel.args.length;
    }

    /**
     * <p>Retrieve an argument of a compound.</p>
     * <p>The argument is dereferenced before returning it.</p>
     *
     * @param k The argument index, 0=first argument, length-1=last argument
     * @return The argument
     */
    public Object getArg(int k) {
        Object t = skel.args[k];
        Display d = display;
        BindVar b;
        while (t instanceof SkelVar &&
                (b = d.bind[((SkelVar) t).id]).display != null) {
            t = b.skel;
            d = b.display;
        }
        return AbstractTerm.createTerm(t, d);
    }

    /**
     * <p>Retrieve an argument of a compound.</p>
     * <p>The argument is dereferenced before returning it.</p>
     *
     * @param k The argument index, 0=first argument, length-1=last argument
     * @return The argument
     */
    public AbstractTerm getArgWrapped(int k) {
        Object t = skel.args[k];
        Display d = display;
        BindVar b;
        while (t instanceof SkelVar &&
                (b = d.bind[((SkelVar) t).id]).display != null) {
            t = b.skel;
            d = b.display;
        }
        return AbstractTerm.createTermWrapped(t, d);
    }

    /************************************************************************/
    /* Constructur Helpers                                                  */
    /************************************************************************/

    /**
     * <p>Retrieve the functor from an atomic.</p>
     *
     * @param ta The atomic.
     * @return The functor.
     */
    private static SkelAtom getFun(TermAtomic ta) {
        if (ta.skel instanceof SkelAtom)
            return (SkelAtom) ta.skel;
        throw new IllegalArgumentException("illegal fun");
    }

    /**
     * <p>Create the arguments for a compound.</p>
     * <p>The arguments must have single display.</p>
     *
     * @param args The term arguments.
     * @return The object arguments.
     */
    private static Object[] makeArgs(Object[] args) {
        boolean multi = TermCompound.isMulti(args);
        if (multi)
            throw new IllegalArgumentException("multi display");
        return TermCompound.createAlloc(args, false, null);
    }

    /**
     * <p>Create the arguments for a compound.</p>
     *
     * @param engine The engine.
     * @param args   The term arguments.
     * @return The object arguments.
     */
    private static Object[] makeArgs(Engine engine, Object[] args) {
        boolean multi = TermCompound.isMulti(args);
        engine.display = TermCompound.createCount(args);
        return TermCompound.createAlloc(args, multi, engine);
    }

    /**
     * <p>Check whether the arguments have multiple displays.</p>
     *
     * @param args The term array.
     * @return True if arguments have multiple displays, otherwise false.
     */
    private static boolean isMulti(Object[] args) {
        Display last = Display.DISPLAY_CONST;
        for (int i = 0; i < args.length; i++) {
            Object arg = args[i];
            /* fast lane */
            if (arg instanceof String)
                continue;
            /* common lane */
            Object t = AbstractTerm.getSkel(arg);
            Display d = AbstractTerm.getDisplay(arg);
            BindVar b;
            while (t instanceof SkelVar &&
                    (b = d.bind[((SkelVar) t).id]).display != null) {
                t = b.skel;
                d = b.display;
            }
            if (!EngineCopy.isGroundSkel(t)) {
                if (last == Display.DISPLAY_CONST) {
                    last = d;
                } else if (last != d) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * <p>Count the needed variable place holders.</p>
     * <p>New display is only created if multi.</p>
     *
     * @param args The arguments.
     * @return The new or reused display.
     */
    private static Display createCount(Object[] args) {
        int countvar = 0;
        boolean multi = false;
        Display last = Display.DISPLAY_CONST;
        for (int i = 0; i < args.length; i++) {
            Object arg = args[i];
            /* fast lane */
            if (arg instanceof String)
                continue;
            /* common lane */
            Object t = AbstractTerm.getSkel(arg);
            Display d = AbstractTerm.getDisplay(arg);
            BindVar b;
            while (t instanceof SkelVar &&
                    (b = d.bind[((SkelVar) t).id]).display != null) {
                t = b.skel;
                d = b.display;
            }
            if (!EngineCopy.isGroundSkel(t)) {
                countvar++;
                if (last == Display.DISPLAY_CONST) {
                    last = d;
                } else if (last != d) {
                    multi = true;
                }
            }
        }
        if (multi)
            last = new Display(countvar);
        return last;
    }

    /**
     * <p>Allocate the array and bind the place holders where necessary.</p>
     * <p>The display is passed via the engine.</p>
     *
     * @param args  The arguments.
     * @param multi The multi flag.
     * @param en    The engine copy.
     * @return The arguments.
     */
    private static Object[] createAlloc(Object[] args, boolean multi, Engine en) {
        Object[] newargs = new Object[args.length];
        int pos = 0;
        int countvar = 0;
        for (int i = 0; i < args.length; i++) {
            Object arg = args[i];
            Object t = AbstractTerm.getSkel(arg);
            Display d = AbstractTerm.getDisplay(arg);
            BindVar b;
            while (t instanceof SkelVar &&
                    (b = d.bind[((SkelVar) t).id]).display != null) {
                t = b.skel;
                d = b.display;
            }
            if (multi && !EngineCopy.isGroundSkel(t)) {
                SkelVar sv = SkelVar.valueOf(countvar);
                countvar++;
                en.display.bind[sv.id].bindVar(t, d, en);
                newargs[pos] = sv;
            } else {
                newargs[pos] = t;
            }
            pos++;
        }
        return newargs;
    }

}