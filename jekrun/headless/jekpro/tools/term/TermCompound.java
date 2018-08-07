package jekpro.tools.term;

import jekpro.frequent.standard.EngineCopy;
import jekpro.frequent.standard.SpecialSort;
import jekpro.model.inter.Engine;
import jekpro.model.molec.BindVar;
import jekpro.model.molec.Display;
import jekpro.reference.structure.SpecialLexical;
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
    TermCompound(SkelCompound s, Display d) {
        display = d;
        skel = s;
    }

    /**
     * <p>Constructor for internal use only.</p>
     *
     * @param s The compound skel.
     */
    public TermCompound(SkelCompound s) {
        display = Display.DISPLAY_CONST;
        skel = s;
    }

    /**
     * <p>Create a compound from functor and arguments.</p>
     * <p>The arguments must have single display.</p>
     *
     * @param sym  The functor.
     * @param args The arguments.
     */
    public TermCompound(String sym, Object... args) {
        display = TermCompound.makeSkel(args);
        skel = new SkelCompound(new SkelAtom(sym), args);
    }

    /**
     * <p>Create a compound from functor and arguments.</p>
     * <p>The arguments must have single display.</p>
     *
     * @param sym  The functor.
     * @param args The arguments.
     */
    public TermCompound(TermAtomic sym, Object... args) {
        display = TermCompound.makeSkel(args);
        skel = new SkelCompound(getFun(sym), args);
    }

    /**
     * <p>Create a compound from interpreter, functor and arguments.</p>
     *
     * @param inter The call-in.
     * @param sym   The functor.
     * @param args  The arguments.
     */
    public TermCompound(Interpreter inter, String sym, Object... args) {
        Engine en = (Engine) inter.getEngine();
        display = TermCompound.createCount(args, en);
        skel = TermCompound.createAlloc(new SkelAtom(sym), args, en);
    }

    /**
     * <p>Create a compound from interpreter, functor and arguments.</p>
     *
     * @param inter The call-in.
     * @param sym   The functor.
     * @param args  The arguments.
     */
    public TermCompound(Interpreter inter, TermAtomic sym, Object... args) {
        Engine en = (Engine) inter.getEngine();
        display = TermCompound.createCount(args, en);
        skel = TermCompound.createAlloc(getFun(sym), args, en);
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
     * <p>The object format is used.</p>
     *
     * @param k The argument index, 0=first argument, length-1=last argument
     * @return The argument in object format.
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
     * <p>The term format is used.</p>
     *
     * @param k The argument index, 0=first argument, length-1=last argument
     * @return The argument in term format.
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

    /**
     * <p>Retrieve an argument of a compound.</p>
     * <p>The argument is dereferenced before returning it.</p>
     * <p>The molec format is used.</p>
     *
     * @param k The argument index, 0=first argument, length-1=last argument
     * @return The argument in molec format.
     */
    public Object getArgMolec(int k) {
        Object t = skel.args[k];
        Display d = display;
        BindVar b;
        while (t instanceof SkelVar &&
                (b = d.bind[((SkelVar) t).id]).display != null) {
            t = b.skel;
            d = b.display;
        }
        return AbstractTerm.createMolec(t, d);
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
        Object obj = ta.skel;
        if (obj instanceof SkelAtom)
            return (SkelAtom) obj;
        throw new IllegalArgumentException("illegal fun");
    }

    /******************************************************************/
    /* Homogenous Case: One Pass                                      */
    /******************************************************************/

    /**
     * <p>Check that the homogenous display.</p>
     * <p>Repopulate array with skeleton only.</p>
     *
     * @param args The arguments.
     * @return The reused display.
     */
    private static Display makeSkel(Object[] args) {
        Display last = Display.DISPLAY_CONST;
        for (int i = 0; i < args.length; i++) {
            Object obj = args[i];
            Object t = AbstractTerm.getSkel(obj);
            Display d = AbstractTerm.getDisplay(obj);
            BindVar b;
            while (t instanceof SkelVar &&
                    (b = d.bind[((SkelVar) t).id]).display != null) {
                t = b.skel;
                d = b.display;
            }
            if (EngineCopy.getVar(t) != null) {
                if (last == Display.DISPLAY_CONST) {
                    last = d;
                } else if (last != d) {
                    throw new IllegalArgumentException("needs display");
                }
            }
            args[i] = t;
        }
        return last;
    }

    /******************************************************************/
    /* Inhomogenous Case: Two Pass                                    */
    /******************************************************************/

    /**
     * <p>Count the needed variable place holders.</p>
     * <p>New display is only created if multi.</p>
     *
     * @param args The arguments.
     * @param en   The engine, not null.
     * @return The new or reused display.
     */
    private static Display createCount(Object[] args, Engine en) {
        int countvar = 0;
        boolean multi = false;
        Display last = Display.DISPLAY_CONST;
        for (int i = 0; i < args.length; i++) {
            Object obj = args[i];
            /* fast lane */
            if (obj instanceof String)
                continue;
            /* common lane */
            Object t = AbstractTerm.getSkel(obj);
            Display d = AbstractTerm.getDisplay(obj);
            BindVar b;
            while (t instanceof SkelVar &&
                    (b = d.bind[((SkelVar) t).id]).display != null) {
                t = b.skel;
                d = b.display;
            }
            if (EngineCopy.getVar(t) != null) {
                countvar++;
                if (last == Display.DISPLAY_CONST) {
                    last = d;
                } else if (last != d) {
                    multi = true;
                }
            }
        }
        if (multi) {
            last = new Display(countvar);
            last.flags |= Display.MASK_DISP_MLTI;
        }
        en.skel = Boolean.valueOf(multi);
        en.display = last;
        return last;
    }

    /**
     * <p>Repopulate array with deref or variable.</p>
     * <p>The display is passed via the engine.</p>
     *
     * @param sa   The symbol.
     * @param args The arguments.
     * @param en   The engine.
     * @return The new compound.
     */
    private static SkelCompound createAlloc(SkelAtom sa, Object[] args, Engine en) {
        boolean multi = (en.skel == Boolean.TRUE);
        Display d3 = en.display;
        SkelVar[] vars;
        if (multi) {
            vars = SkelVar.valueOfArray(d3.bind.length);
        } else {
            vars = null;
        }
        int countvar = 0;
        for (int i = 0; i < args.length; i++) {
            Object obj = args[i];
            Object t = AbstractTerm.getSkel(obj);
            Display d = AbstractTerm.getDisplay(obj);
            BindVar b;
            while (t instanceof SkelVar &&
                    (b = d.bind[((SkelVar) t).id]).display != null) {
                t = b.skel;
                d = b.display;
            }
            if (multi && EngineCopy.getVar(t) != null) {
                SkelVar sv = vars[countvar];
                countvar++;
                d3.bind[sv.id].bindVar(t, d, en);
                if ((d.flags & Display.MASK_DISP_MLTI) != 0) {
                    d.remTab(en);
                    d.flags &= ~Display.MASK_DISP_MLTI;
                }
                args[i] = sv;
            } else {
                args[i] = t;
            }
        }
        if (multi) {
            return new SkelCompound(sa, args, vars);
        } else {
            return new SkelCompound(sa, args);
        }
    }

}