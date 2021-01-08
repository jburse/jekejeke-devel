package jekpro.tools.term;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.DisplayMarkable;
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
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
     * <p>Create a compound from functor and arguments.</p>
     * <p>The arguments must have single display.</p>
     *
     * @param sym  The functor.
     * @param args The arguments.
     */
    public TermCompound(String sym, Object... args) {
        display = makeCount(args);
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
        display = makeCount(args);
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
        Engine en = inter.getEngine();
        display = createCount(args, en);
        skel = createAlloc(new SkelAtom(sym), args, en);
    }

    /**
     * <p>Create a compound from interpreter, functor and arguments.</p>
     *
     * @param inter The call-in.
     * @param sym   The functor.
     * @param args  The arguments.
     */
    public TermCompound(Interpreter inter, TermAtomic sym, Object... args) {
        Engine en = inter.getEngine();
        display = createCount(args, en);
        skel = createAlloc(getFun(sym), args, en);
    }

    /************************************************************/
    /* Variation Points                                         */
    /************************************************************/

    /**
     * <p>Retrieve the skeleton.</p>
     *
     * @return The skeleton.
     */
    public Object getSkel() {
        return skel;
    }

    /**
     * <p>Retrieve the display.</p>
     *
     * @return The display.
     */
    public Display getDisplay() {
        return display;
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
        return new TermAtomic(skel.sym);
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
        BindUniv b;
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
        BindUniv b;
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
        BindUniv b;
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
     * @return The display.
     */
    private Display makeCount(Object[] args) {
        Display last = Display.DISPLAY_CONST;
        for (int i = 0; i < args.length; i++) {
            Object obj = args[i];
            Object t = AbstractTerm.getSkel(obj);
            if (SupervisorCopy.getVar(t) != null) {
                Display d = AbstractTerm.getDisplay(obj);
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
     * @param en   The engine.
     * @return The display.
     */
    private Display createCount(Object[] args, Engine en) {
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
            if (SupervisorCopy.getVar(t) != null) {
                Display d = AbstractTerm.getDisplay(obj);
                countvar++;
                if (last == Display.DISPLAY_CONST) {
                    last = d;
                } else if (last != d) {
                    multi = true;
                }
            }
        }
        if (multi)
            last = new DisplayMarkable(countvar);
        en.skel = Boolean.valueOf(multi);
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
    private SkelCompound createAlloc(SkelAtom sa, Object[] args, Engine en) {
        boolean multi = ((Boolean) en.skel).booleanValue();
        Display d3;
        SkelVar[] vars;
        if (multi) {
            d3 = display;
            vars = SkelVar.valueOfArray(d3.bind.length);
        } else {
            d3 = null;
            vars = null;
        }
        int countvar = 0;
        for (int i = 0; i < args.length; i++) {
            Object obj = args[i];
            Object t = AbstractTerm.getSkel(obj);
            if (multi && SupervisorCopy.getVar(t) != null) {
                Display d = AbstractTerm.getDisplay(obj);
                SkelVar sv = vars[countvar];
                countvar++;
                boolean ext = d.getAndReset();
                d3.bind[sv.id].bindUniv(t, d, en);
                if (ext)
                    d.remTab(en);
                args[i] = sv;
            } else {
                args[i] = t;
            }
        }
        if (multi) {
            SkelCompound sc2 = new SkelCompound(args, sa);
            sc2.var = (vars.length > 1 ? vars : vars[0]);
            return sc2;
        } else {
            return new SkelCompound(sa, args);
        }
    }

}