package jekpro.reference.structure;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.pretty.Foyer;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.tools.array.Types;
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SpecialUniv extends AbstractSpecial {
    private final static int SPECIAL_ARG = 0;
    private final static int SPECIAL_SET_ARG = 1;
    private final static int SPECIAL_SYS_MUST_BE_ATOMIC = 2;
    private final static int SPECIAL_SYS_TERM_TO_ARITY = 3;
    private final static int SPECIAL_SYS_EXTEND_TERM = 4;
    private final static int SPECIAL_SYS_SHRINK_TERM = 5;
    private final static int SPECIAL_UNIFY_WITH_OCCURS_CHECK = 6;
    private final static int SPECIAL_NOT_UNIFY = 7;
    private final static int SPECIAL_COPY_TERM = 8;

    /**
     * <p>Create a univ special.</p>
     *
     * @param i The id.
     */
    public SpecialUniv(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a term in a list of goals for the first time.</p>
     * <p>The term is passed via the skel and display of the engine.</p>
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
                case SPECIAL_ARG:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
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
                        if (!en.unify(cmp[nth - 1], en.display, temp[2], ref))
                            return false;
                        return true;
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
                        Display d = en.display;

                        en.skel = temp[2];
                        en.display = ref;
                        en.deref();
                        Object t2 = en.skel;
                        Display d2 = en.display;

                        boolean multi = SpecialUniv.setCount(sc.args, d, t2, d2, nth, en);
                        sc = SpecialUniv.setAlloc(sc.sym, sc.args, d, t2, d2, nth, multi, en);
                        d = en.display;
                        if (!en.unify(sc, d, temp[3], ref))
                            return false;
                        if (multi)
                            d.remTab(en);
                        return true;
                    } else if (en.skel instanceof SkelAtom) {
                        return false;
                    } else {
                        EngineMessage.checkInstantiated(en.skel);
                        throw new EngineMessage(EngineMessage.typeError(
                                EngineMessage.OP_TYPE_CALLABLE,
                                en.skel), en.display);
                    }
                case SPECIAL_SYS_MUST_BE_ATOMIC:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.skel = temp[0];
                    en.display = ref;
                    en.deref();
                    if (!(en.skel instanceof SkelCompound) && !(en.skel instanceof SkelVar)) {
                        /* */
                    } else {
                        EngineMessage.checkInstantiated(en.skel);
                        throw new EngineMessage(EngineMessage.typeError(
                                EngineMessage.OP_TYPE_ATOMIC, en.skel), en.display);
                    }
                    return true;
                case SPECIAL_SYS_TERM_TO_ARITY:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.skel = temp[0];
                    en.display = ref;
                    en.deref();
                    if (en.skel instanceof SkelCompound) {
                        SkelCompound sc = (SkelCompound) en.skel;
                        num = Integer.valueOf(sc.args.length);
                    } else {
                        EngineMessage.checkInstantiated(en.skel);
                        num = Integer.valueOf(0);
                    }
                    if (!en.unify(num, Display.DISPLAY_CONST, temp[1], ref))
                        return false;
                    return true;
                case SPECIAL_SYS_EXTEND_TERM:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    boolean multi = SpecialUniv.listToTerm(temp[0], ref, temp[1], ref, en);
                    Display d = en.display;
                    if (!en.unify(en.skel, d, temp[2], ref))
                        return false;
                    if (multi)
                        d.remTab(en);
                    return true;
                case SPECIAL_SYS_SHRINK_TERM:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;

                    num = SpecialEval.derefAndCastInteger(temp[1], ref);
                    SpecialEval.checkNotLessThanZero(num);
                    nth = SpecialEval.castIntValue(num);

                    en.skel = temp[0];
                    en.display = ref;
                    en.deref();
                    d = en.display;
                    Object val = SpecialUniv.termToList(nth, en.skel, en);
                    if (val == null)
                        return false;
                    if (!en.unify(en.skel, d, temp[2], ref))
                        return false;
                    if (!en.unify(val, d, temp[3], ref))
                        return false;
                    return true;
                case SPECIAL_UNIFY_WITH_OCCURS_CHECK:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (!unifyChecked(temp[0], ref, temp[1], ref, en))
                        return false;
                    return true;
                case SPECIAL_NOT_UNIFY:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    AbstractUndo mark = en.bind;
                    if (en.unify(temp[1], ref, temp[0], ref))
                        return false;
                    en.fault = null;
                    en.releaseBind(mark);
                    if (en.fault != null)
                        throw en.fault;
                    return true;
                case SPECIAL_COPY_TERM:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    val = AbstractSkel.copySkel(temp[0], ref, en);
                    d = AbstractSkel.createMarker(val);
                    multi = d.getAndReset();
                    if (!en.unify(val, d, temp[1], ref))
                        return false;
                    if (multi)
                        d.remTab(en);
                    return true;
                default:
                    throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
            }
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
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
            if (SupervisorCopy.getVar(en.skel) != null) {
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
            if (multi && SupervisorCopy.getVar(en.skel) != null) {
                SkelVar sv = vars[countvar];
                countvar++;
                d4.bind[sv.id].bindUniv(en.skel, en.display, en);
                args[i] = sv;
            } else {
                args[i] = en.skel;
            }
        }
        en.display = d4;
        if (multi) {
            SkelCompound sc2 = new SkelCompound(args, sa);
            sc2.var = (vars.length > 1 ? vars : vars[0]);
            return sc2;
        } else {
            return new SkelCompound(sa, args);
        }
    }

    /******************************************************************/
    /* Univ Predicate                                                 */
    /******************************************************************/

    /**
     * <p>Convert a list to a term.</p>
     * <p>The argument is passed via skel and display.</p>
     * <p>The result is returned via skel and display.</p>
     * <p>Used by =../2.</p>
     *
     * @param t2 The term skeleton.
     * @param d2 The term skeleton.
     * @param t  The list skeleton.
     * @param d  The list display.
     * @param en The interpreter.
     * @throws EngineMessage Shit happens.
     */
    private static boolean listToTerm(Object t2, Display d2,
                                      Object t, Display d, Engine en)
            throws EngineMessage {
        en.skel = t2;
        en.display = d2;
        en.deref();
        t2 = en.skel;
        d2 = en.display;

        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;

        if (t instanceof SkelAtom &&
                ((SkelAtom) t).fun.equals(Foyer.OP_NIL)) {
            EngineMessage.checkInstantiated(t2);
            en.skel = t2;
            en.display = d2;
            return false;
        } else {
            SkelAtom sa;
            if (t2 instanceof SkelCompound) {
                sa = ((SkelCompound) t2).sym;
            } else if (t2 instanceof SkelAtom) {
                sa = (SkelAtom) t2;
            } else {
                EngineMessage.checkInstantiated(t2);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_CALLABLE, t2), d2);
            }
            int mullen = univCount(t2, d2, t, d, en);
            en.skel = univAlloc(sa, t2, d2, t, d, mullen, en);
            return (mullen < 0);
        }
    }

    /**
     * <p>Convert a term to a closure and list.</p>
     * <p>The closure is returned in the engine skeleton.</p>
     *
     * @param count The number of arguments.
     * @param t     The term skeleton.
     * @param en    Engine.
     * @return The list or null.
     * @throws EngineMessage Shit happens.
     */
    private static Object termToList(int count, Object t, Engine en)
            throws EngineMessage {
        Foyer foyer = en.store.foyer;
        Object val;
        if (t instanceof SkelCompound) {
            Object[] args = ((SkelCompound) t).args;
            if (count > args.length)
                return null;
            if (count == args.length) {
                en.skel = ((SkelCompound) t).sym;
            } else {
                Object[] args2 = new Object[args.length - count];
                System.arraycopy(args, 0, args2, 0, args2.length);
                en.skel = new SkelCompound(((SkelCompound) t).sym, args2);
            }
            Object res = foyer.ATOM_NIL;
            for (int i = 1; i <= count; i++)
                res = new SkelCompound(foyer.ATOM_CONS, args[args.length - i], res);
            val = res;
        } else {
            EngineMessage.checkInstantiated(t);
            if (count > 0)
                return null;
            en.skel = t;
            val = foyer.ATOM_NIL;
        }
        return val;
    }

    /**
     * <p>Count the needed variable place holders and the number of arguments.</p>
     * <p>The reused or new display is returned in the engine copy display.</p>
     *
     * @param t2 The term skeleton.
     * @param d2 The term skeleton.
     * @param t  The list skeleton.
     * @param d  The list display.
     * @param en The engine.
     * @return True if new display is returned, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    private static int univCount(Object t2, Display d2,
                                 Object t, Display d, Engine en)
            throws EngineMessage {
        int length = 0;
        int countvar = 0;
        Display last = Display.DISPLAY_CONST;
        boolean multi = false;
        if (t2 instanceof SkelCompound) {
            Object[] args = ((SkelCompound) t2).args;
            for (int i = 0; i < args.length; i++) {
                en.skel = args[i];
                en.display = d2;
                en.deref();
                if (SupervisorCopy.getVar(en.skel) != null) {
                    countvar++;
                    if (last == Display.DISPLAY_CONST) {
                        last = en.display;
                    } else if (last != en.display) {
                        multi = true;
                    }
                }
                length++;
            }
        }
        while (t instanceof SkelCompound &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_CONS) &&
                ((SkelCompound) t).args.length == 2) {
            SkelCompound sc = (SkelCompound) t;
            en.skel = sc.args[0];
            en.display = d;
            en.deref();
            if (SupervisorCopy.getVar(en.skel) != null) {
                countvar++;
                if (last == Display.DISPLAY_CONST) {
                    last = en.display;
                } else if (last != en.display) {
                    multi = true;
                }
            }
            length++;
            en.skel = sc.args[1];
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
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
     * @param t2     The term skeleton.
     * @param d2     The term skeleton.
     * @param t      The list skeleton.
     * @param d      The list display.
     * @param mullen The multi flag and the length.
     * @param en     The engine.
     * @return The new compound.
     */
    private static SkelCompound univAlloc(SkelAtom sa,
                                          Object t2, Display d2,
                                          Object t, Display d,
                                          int mullen, Engine en) {
        Display d3 = en.display;
        boolean multi = (mullen < 0);
        SkelVar[] vars;
        if (multi) {
            vars = SkelVar.valueOfArray(d3.bind.length);
        } else {
            vars = null;
        }
        int length = (multi ? -mullen - 1 : mullen);
        Object[] args = new Object[length];
        int pos = 0;
        int countvar = 0;
        if (t2 instanceof SkelCompound) {
            Object[] args2 = ((SkelCompound) t2).args;
            for (int i = 0; i < args2.length; i++) {
                en.skel = args2[i];
                en.display = d2;
                en.deref();
                if (multi && SupervisorCopy.getVar(en.skel) != null) {
                    SkelVar sv = vars[countvar];
                    countvar++;
                    d3.bind[sv.id].bindUniv(en.skel, en.display, en);
                    args[pos] = sv;
                } else {
                    args[pos] = en.skel;
                }
                pos++;
            }
        }
        while (t instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) t;
            en.skel = sc.args[0];
            en.display = d;
            en.deref();
            if (multi && SupervisorCopy.getVar(en.skel) != null) {
                SkelVar sv = vars[countvar];
                countvar++;
                d3.bind[sv.id].bindUniv(en.skel, en.display, en);
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
        en.display = d3;
        if (multi) {
            SkelCompound sc2 = new SkelCompound(args, sa);
            sc2.var = (vars.length > 1 ? vars : vars[0]);
            return sc2;
        } else {
            return new SkelCompound(sa, args);
        }
    }

    /*****************************************************************/
    /* Unify With Occurs Check                                       */
    /*****************************************************************/

    /**
     * <p>>Unify two terms. As a side effect bindings are established.</p
     * <p>Bindings are only created when the occurs check fails.<p>
     * <p>The verify hooks of attribute variables are called.</p>
     * <p>Tail recursive implementation.</p>
     *
     * @param alfa The first skeleton.
     * @param d1   The first display.
     * @param beta The second skeleton.
     * @param d2   The second display.
     * @param en   The engine.
     * @return True if the two terms unify, otherwise false.
     */
    private static boolean unifyChecked(Object alfa, Display d1,
                                       Object beta, Display d2,
                                       Engine en)
            throws EngineException {
        for (; ; ) {
            if (alfa instanceof SkelVar) {
                // combined check and deref
                BindUniv b1;
                if ((b1 = d1.bind[((SkelVar) alfa).id]).display != null) {
                    alfa = b1.skel;
                    d1 = b1.display;
                    continue;
                }
                for (; ; ) {
                    if (beta instanceof SkelVar) {
                        // combined check and deref
                        BindUniv b2;
                        if ((b2 = d2.bind[((SkelVar) beta).id]).display != null) {
                            beta = b2.skel;
                            d2 = b2.display;
                            continue;
                        }
                        if (b1 == b2)
                            return true;
                        if (b2.hasVar(alfa, d1, d2))
                            return false;
                        return b2.bindAttr(alfa, d1, en);
                    }
                    if (b1.hasVar(beta, d2, d1))
                        return false;
                    return b1.bindAttr(beta, d2, en);
                }
            }
            for (; ; ) {
                // combined check and deref
                if (beta instanceof SkelVar) {
                    BindUniv bc;
                    if ((bc = d2.bind[((SkelVar) beta).id]).display != null) {
                        beta = bc.skel;
                        d2 = bc.display;
                        continue;
                    }
                    if (bc.hasVar(alfa, d1, d2))
                        return false;
                    return bc.bindAttr(alfa, d1, en);
                }
                break;
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
                if (!unifyChecked(t1[i], d1, t2[i], d2, en))
                    return false;
            }
            alfa = t1[i];
            beta = t2[i];
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
        BindUniv b;
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
        BindUniv b;
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
        BindUniv b;
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
        BindUniv b;
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
