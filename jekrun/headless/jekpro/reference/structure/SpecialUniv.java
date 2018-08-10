package jekpro.reference.structure;

import jekpro.frequent.standard.EngineCopy;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.pretty.Foyer;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>Provides built-in predicates for univ ops.</p>
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
public final class SpecialUniv extends AbstractSpecial {
    private final static int SPECIAL_UNIV = 0;
    private final static int SPECIAL_ARG = 1;
    private final static int SPECIAL_SET_ARG = 2;
    private final static int SPECIAL_UNIFY = 3;
    private final static int SPECIAL_UNIFY_CHECKED = 4;
    private final static int SPECIAL_NOT_UNIFY = 5;
    private final static int SPECIAL_SYS_LIST_TO_TERM = 6;
    private final static int SPECIAL_SYS_TERM_TO_LIST = 7;

    /**
     * <p>Create a univ special.</p>
     *
     * @param i The id.
     */
    public SpecialUniv(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        try {
            switch (id) {
                case SPECIAL_UNIV:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    en.skel = temp[0];
                    en.display = ref;
                    en.deref();
                    if (!(en.skel instanceof SkelVar)) {
                        en.skel = SpecialUniv.termToList(en.skel, en);
                        if (!en.unifyTerm(temp[1], ref, en.skel, en.display))
                            return false;
                        return en.getNext();
                    }
                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    boolean multi = SpecialUniv.listToTerm(en);
                    Display d = en.display;
                    if (!en.unifyTerm(temp[0], ref, en.skel, d))
                        return false;
                    if (multi)
                        d.remTab(en);
                    return en.getNext();
                case SPECIAL_ARG:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    Number num = SpecialEval.derefAndCastInteger(temp[0], ref);
                    SpecialEval.checkNotLessThanZero(num);
                    int nth = SpecialEval.castIntValue(num);
                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    if (en.skel instanceof SkelCompound) {
                        Object[] cmp = ((SkelCompound) en.skel).args;
                        if (cmp.length < nth)
                            return false;
                        if (nth < 1)
                            return false;
                        if (!en.unifyTerm(temp[2], ref, cmp[nth - 1], en.display))
                            return false;
                        return en.getNext();
                    } else if (en.skel instanceof SkelAtom) {
                        return false;
                    } else {
                        EngineMessage.checkInstantiated(en.skel);
                        throw new EngineMessage(EngineMessage.typeError(
                                EngineMessage.OP_TYPE_CALLABLE,
                                en.skel), en.display);
                    }
                case SPECIAL_SET_ARG:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    num = SpecialEval.derefAndCastInteger(temp[0], ref);
                    SpecialEval.checkNotLessThanZero(num);
                    nth = SpecialEval.castIntValue(num);

                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    if (en.skel instanceof SkelCompound) {
                        SkelCompound sc = (SkelCompound) en.skel;
                        if (1 > nth || nth > sc.args.length)
                            return false;
                        nth--;
                        d = en.display;

                        en.skel = temp[2];
                        en.display = ref;
                        en.deref();
                        Object t2 = en.skel;
                        Display d2 = en.display;

                        multi = SpecialUniv.setCount(sc.args, d, t2, d2, nth, en);
                        sc = SpecialUniv.setAlloc(sc.sym, sc.args, d, t2, d2, nth, multi, en);
                        d = en.display;
                        if (!en.unifyTerm(temp[3], ref, sc, d))
                            return false;
                        if (multi)
                            d.remTab(en);
                        return en.getNext();
                    } else if (en.skel instanceof SkelAtom) {
                        return false;
                    } else {
                        EngineMessage.checkInstantiated(en.skel);
                        throw new EngineMessage(EngineMessage.typeError(
                                EngineMessage.OP_TYPE_CALLABLE,
                                en.skel), en.display);
                    }
                case SPECIAL_UNIFY:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (!en.unifyTerm(temp[1], ref, temp[0], ref))
                        return false;
                    return en.getNext();
                case SPECIAL_UNIFY_CHECKED:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (!SpecialUniv.unifyTermChecked(temp[0], ref, temp[1], ref, en))
                        return false;
                    return en.getNext();
                case SPECIAL_NOT_UNIFY:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    AbstractBind mark = en.bind;
                    if (en.unifyTerm(temp[1], ref, temp[0], ref))
                        return false;
                    en.skel = null;
                    en.releaseBind(mark);
                    if (en.skel != null)
                        throw (EngineException) en.skel;
                    return en.getNextRaw();
                case SPECIAL_SYS_LIST_TO_TERM:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.skel = temp[0];
                    en.display = ref;
                    en.deref();
                    multi = SpecialUniv.listToTerm(en);
                    d = en.display;
                    if (!en.unifyTerm(temp[1], ref, en.skel, d))
                        return false;
                    if (multi)
                        d.remTab(en);
                    return en.getNext();
                case SPECIAL_SYS_TERM_TO_LIST:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.skel = temp[0];
                    en.display = ref;
                    en.deref();
                    en.skel = SpecialUniv.termToList(en.skel, en);
                    if (!en.unifyTerm(temp[1], ref, en.skel, en.display))
                        return false;
                    return en.getNext();
                default:
                    throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
            }
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
        }
    }

    /******************************************************************/
    /* Set Arg                                                        */
    /******************************************************************/

    /**
     * <p>Count the needed variable place holders.</p>
     * <p>The reused or new display is returned in the engine display.</p>
     *
     * @param t2 The compound arguments.
     * @param d2 The compound display.
     * @param t  The set skel.
     * @param d  The set display.
     * @param k  The set position.
     * @param en The engine.
     * @return True if new display is returned, otherwise false.
     */
    public static boolean setCount(Object[] t2, Display d2,
                                   Object t, Display d, int k,
                                   Engine en) {
        int countvar = 0;
        Display last = Display.DISPLAY_CONST;
        boolean multi = false;
        for (int i = 0; i < t2.length; i++) {
            if (i != k) {
                en.skel = t2[i];
                en.display = d2;
                en.deref();
            } else {
                en.skel = t;
                en.display = d;
            }
            if (EngineCopy.getVar(en.skel) != null) {
                countvar++;
                if (last == Display.DISPLAY_CONST) {
                    last = en.display;
                } else if (last != en.display) {
                    multi = true;
                }
            }
        }
        if (multi)
            last = new Display(countvar);
        en.display = last;
        return multi;
    }

    /**
     * <p>Copy the arguments.</p>
     * <p>The reused or new display is passed via the engine display</p>
     * <p>The reused or new display is returned in the engine display.</p>
     *
     * @param sa    The symbol.
     * @param t2    The compound arguments.
     * @param d2    The compound display.
     * @param t     The set skel.
     * @param d     The set display.
     * @param k     The set position.
     * @param multi The multi flag.
     * @param en    The engine.
     * @return The new compound.
     */
    public static SkelCompound setAlloc(SkelAtom sa, Object[] t2, Display d2,
                                        Object t, Display d, int k,
                                        boolean multi, Engine en) {
        Display d4 = en.display;
        SkelVar[] vars;
        if (multi) {
            vars = SkelVar.valueOfArray(d4.bind.length);
        } else {
            vars = null;
        }
        Object[] args = new Object[t2.length];
        int countvar = 0;
        for (int i = 0; i < t2.length; i++) {
            if (i != k) {
                en.skel = t2[i];
                en.display = d2;
                en.deref();
            } else {
                en.skel = t;
                en.display = d;
            }
            if (multi && EngineCopy.getVar(en.skel) != null) {
                SkelVar sv = vars[countvar];
                countvar++;
                d4.bind[sv.id].bindVar(en.skel, en.display, en);
                args[i] = sv;
            } else {
                args[i] = en.skel;
            }
        }
        en.display = d4;
        if (multi) {
            return new SkelCompound(sa, args, vars);
        } else {
            return new SkelCompound(sa, args);
        }
    }

    /******************************************************************/
    /* Univ Predicate                                                 */
    /******************************************************************/

    /**
     * <p>Convert a term to a list.</p>
     * <p>The argument is passed via skel.</p>
     * <p>The result is returned via skel.</p>
     *
     * @param en Engine.
     * @return True if the argument was not a variable, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    private static Object termToList(Object t, Engine en)
            throws EngineMessage {
        Foyer foyer = en.store.foyer;
        Object res = foyer.ATOM_NIL;
        if (t instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) t;
            for (int i = sc.args.length - 1; i >= 0; i--)
                res = new SkelCompound(foyer.ATOM_CONS, sc.args[i], res);
            return new SkelCompound(foyer.ATOM_CONS, sc.sym, res);
        } else {
            EngineMessage.checkInstantiated(t);
            return new SkelCompound(foyer.ATOM_CONS, t, res);
        }
    }

    /**
     * <p>Convert a list to a term.</p>
     * <p>The argument is passed via skel and display.</p>
     * <p>The result is returned via skel and display.</p>
     * <p>Used by =../2.</p>
     *
     * @param en The interpreter.
     * @throws EngineMessage Shit happens.
     */
    private static boolean listToTerm(Engine en)
            throws EngineMessage {
        Object t = en.skel;
        Display d = en.display;
        if ((t instanceof SkelCompound) &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_CONS) &&
                ((SkelCompound) t).args.length == 2) {
            /* */
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_LIST, t), d);
        }
        SkelCompound sc = (SkelCompound) t;
        Object t2 = sc.args[0];
        Display d2 = d;

        en.skel = sc.args[1];
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;

        if (t instanceof SkelAtom &&
                ((SkelAtom) t).fun.equals(Foyer.OP_NIL)) {
            BindVar b;
            while (t2 instanceof SkelVar &&
                    (b = d2.bind[((SkelVar) t2).id]).display != null) {
                t2 = b.skel;
                d2 = b.display;
            }
            if (!(t2 instanceof SkelCompound) && !(t2 instanceof SkelVar)) {
                /* */
            } else {
                EngineMessage.checkInstantiated(t2);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_ATOMIC, t2), d2);
            }
            en.skel = t2;
            en.display = d2;
            return false;
        } else {
            SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(t2, d2);
            int mullen = univCount(t, d, en);
            en.skel = univAlloc(sa, t, d, mullen, en);
            return (mullen < 0);
        }
    }

    /**
     * <p>Count the needed variable place holders and the number of arguments.</p>
     * <p>The reused or new display is returned in the engine copy display.</p>
     *
     * @param t  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return True if new display is returned, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    private static int univCount(Object t, Display d, Engine en)
            throws EngineMessage {
        int length = 0;
        int countvar = 0;
        Display last = Display.DISPLAY_CONST;
        boolean multi = false;
        while (t instanceof SkelCompound &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_CONS) &&
                ((SkelCompound) t).args.length == 2) {
            SkelCompound sc = (SkelCompound) t;
            en.skel = sc.args[0];
            en.display = d;
            en.deref();
            if (EngineCopy.getVar(en.skel) != null) {
                countvar++;
                if (last == Display.DISPLAY_CONST) {
                    last = en.display;
                } else if (last != en.display) {
                    multi = true;
                }
            }
            en.skel = sc.args[1];
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
            length++;
        }
        if (t instanceof SkelAtom &&
                ((SkelAtom) t).fun.equals(Foyer.OP_NIL)) {
            /* */
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_LIST, t), d);
        }
        if (multi)
            last = new Display(countvar);
        en.display = last;
        return (multi ? -length - 1 : length);
    }

    /**
     * <p>Create a multi arguments and bind the needed variables place holders.</p>
     * <p>The reused or new display is passed via the engine display</p>
     * <p>The reused or new display is returned in the engine display.</p>
     *
     * @param sa     The symbol.
     * @param t      The term skeleton.
     * @param d      The term display.
     * @param mullen The multi flag and the length.
     * @param en     The engine.
     * @return The new compound.
     */
    private static SkelCompound univAlloc(SkelAtom sa, Object t, Display d,
                                          int mullen, Engine en) {
        Display d2 = en.display;
        boolean multi = (mullen < 0);
        SkelVar[] vars;
        if (multi) {
            vars = SkelVar.valueOfArray(d2.bind.length);
        } else {
            vars = null;
        }
        int length = (multi ? -mullen - 1 : mullen);
        Object[] args = new Object[length];
        int pos = 0;
        int countvar = 0;
        while (t instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) t;
            en.skel = sc.args[0];
            en.display = d;
            en.deref();
            if (multi && EngineCopy.getVar(en.skel) != null) {
                SkelVar sv = vars[countvar];
                countvar++;
                d2.bind[sv.id].bindVar(en.skel, en.display, en);
                args[pos] = sv;
            } else {
                args[pos] = en.skel;
            }
            pos++;
            en.skel = sc.args[1];
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
        }
        en.display = d2;
        if (multi) {
            return new SkelCompound(sa, args, vars);
        } else {
            return new SkelCompound(sa, args);
        }
    }

    /*****************************************************************/
    /* Checked Unification                                           */
    /*****************************************************************/

    /**
     * Unify two terms. As a side effect bindings are established.
     * Bindings are only created when the occurs check fails.
     *
     * @param alfa The first term.
     * @param d1   The display of the first term.
     * @param beta The second term.
     * @param d2   The display of the second term.
     * @param en   The engine.
     * @return True if the two terms unify, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    private static boolean unifyTermChecked(Object alfa, Display d1,
                                            Object beta, Display d2,
                                            Engine en)
            throws EngineException, EngineMessage {
        for (; ; ) {
            en.skel = alfa;
            en.display = d1;
            en.deref();
            alfa = en.skel;
            d1 = en.display;
            en.skel = beta;
            en.display = d2;
            en.deref();
            beta = en.skel;
            d2 = en.display;
            if (alfa instanceof SkelVar) {
                if (beta instanceof SkelVar) {
                    if (alfa == beta && d1 == d2)
                        return true;
                    if (hasVar(alfa, d1, (SkelVar) beta, d2))
                        return false;
                    return d2.bind[((SkelVar) beta).id].bindAttr(alfa, d1, d2, en);
                }
                if (hasVar(beta, d2, (SkelVar) alfa, d1))
                    return false;
                return d1.bind[((SkelVar) alfa).id].bindAttr(beta, d2, d1, en);
            }
            if (beta instanceof SkelVar) {
                if (hasVar(alfa, d1, (SkelVar) beta, d2))
                    return false;
                return d2.bind[((SkelVar) beta).id].bindAttr(alfa, d1, d2, en);
            }
            if (!(alfa instanceof SkelCompound))
                return alfa.equals(beta);
            if (!(beta instanceof SkelCompound))
                return false;
            Object[] t1 = ((SkelCompound) alfa).args;
            Object[] t2 = ((SkelCompound) beta).args;
            if (t1.length != t2.length)
                return false;
            if (!((SkelCompound) alfa).sym.equals(((SkelCompound) beta).sym))
                return false;
            int i = 0;
            for (; i < t1.length - 1; i++) {
                if (!unifyTermChecked(t1[i], d1, t2[i], d2, en))
                    return false;
            }
            alfa = t1[i];
            beta = t2[i];
        }
    }

    /**
     * <p>Check whether a variable occurs in a term.</p>
     * <p>Check is done from skeleton and display.</p>
     * <p>Uses the vars speed up structure of skel compouned.</p>
     * <p>Tail recursive implementation.</p>
     *
     * @param m  The term.
     * @param d  The display of the term.
     * @param t  The variable.
     * @param d2 The display of the variable.
     * @return True when the variable occurs in the term, false otherwise.
     */
    private static boolean hasVar(Object m, Display d, SkelVar t, Display d2) {
        for (; ; ) {
            Object var = EngineCopy.getVar(m);
            if (var == null)
                return false;
            SkelVar v;
            if (var instanceof SkelVar) {
                v = (SkelVar) var;
            } else {
                SkelVar[] temp = (SkelVar[]) var;
                int i = 0;
                for (; i < temp.length - 1; i++) {
                    v = temp[i];
                    BindVar b = d.bind[v.id];
                    if (b.display != null) {
                        if (hasVar(b.skel, b.display, t, d2))
                            return true;
                    } else {
                        if (v == t && d == d2)
                            return true;
                    }
                }
                v = temp[i];
            }
            BindVar b = d.bind[v.id];
            if (b.display != null) {
                m = b.skel;
                d = b.display;
            } else {
                return (v == t && d == d2);
            }
        }
    }

    /*****************************************************************/
    /* Deref And Cast                                                */
    /*****************************************************************/

    /**
     * <p>Check whether the given term is an atom.</p>
     *
     * @param t The term skel.
     * @param d The term display.
     * @return The string.
     * @throws EngineMessage Type error.
     */
    public static String derefAndCastString(Object t, Display d)
            throws EngineMessage {
        BindVar b;
        while (t instanceof SkelVar &&
                (b = d.bind[((SkelVar) t).id]).display != null) {
            t = b.skel;
            d = b.display;
        }
        if (t instanceof SkelAtom) {
            return ((SkelAtom) t).fun;
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_ATOM, t), d);
        }
    }

    /**
     * <p>Check whether the given term is an atom.</p>
     *
     * @param t The term skel.
     * @param d The term display.
     * @return The wrapped string.
     * @throws EngineMessage Type error.
     */
    public static SkelAtom derefAndCastStringWrapped(Object t, Display d)
            throws EngineMessage {
        BindVar b;
        while (t instanceof SkelVar &&
                (b = d.bind[((SkelVar) t).id]).display != null) {
            t = b.skel;
            d = b.display;
        }
        if (t instanceof SkelAtom) {
            return (SkelAtom) t;
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_ATOM, t), d);
        }
    }

    /**
     * <p>Check whether the given term is a reference.</p>
     *
     * @param t The term skel.
     * @param d The term display.
     * @return The reference.
     * @throws EngineMessage Type error.
     */
    public static Object derefAndCastRef(Object t, Display d)
            throws EngineMessage {
        BindVar b;
        while (t instanceof SkelVar &&
                (b = d.bind[((SkelVar) t).id]).display != null) {
            t = b.skel;
            d = b.display;
        }
        if (!(t instanceof AbstractSkel) && !(t instanceof Number)) {
            return t;
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_REF, t), d);
        }
    }

    /**
     * <p>Check whether the given term is a null atom or a reference.</p>
     *
     * @param t The term skel.
     * @param d The term display.
     * @return The reference or null.
     * @throws EngineMessage Type error.
     */
    public static Object derefAndCastRefOrNull(Object t, Display d)
            throws EngineMessage {
        BindVar b;
        while (t instanceof SkelVar &&
                (b = d.bind[((SkelVar) t).id]).display != null) {
            t = b.skel;
            d = b.display;
        }
        if (t instanceof SkelAtom &&
                ((SkelAtom) t).fun.equals(AbstractFlag.OP_NULL)) {
            return null;
        } else if (!(t instanceof AbstractSkel) && !(t instanceof Number)) {
            return t;
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_REF, t), d);
        }
    }

    /*************************************************************/
    /* String Casts                                              */
    /*************************************************************/

    /**
     * <p>Check whether the given atom is a character.</p>
     *
     * @param str The atom.
     * @return The code point.
     * @throws ClassCastException Not a character.
     */
    public static int castCharacter(String str)
            throws ClassCastException {
        int k;
        if (str.length() == 0 ||
                str.length() != Character.charCount(k = str.codePointAt(0)))
            throw new ClassCastException(EngineMessage.OP_REPRESENTATION_CHARACTER);
        return k;
    }

    /**
     * <p>Check whether the given atom is a char value.</p>
     *
     * @param str The atom.
     * @return The char.
     * @throws ClassCastException Not a char value.
     */
    public static char castCharValue(String str)
            throws ClassCastException {
        int k;
        if (str.length() == 0 ||
                str.length() != Character.charCount(k = str.codePointAt(0)) ||
                k > 0xFFFF)
            throw new ClassCastException(EngineMessage.OP_REPRESENTATION_CHAR);
        return (char) k;
    }

}
