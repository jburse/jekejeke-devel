package jekpro.model.molec;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>Provides a state to do ISO body conversion.</p>
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
public final class SupervisorWrap {
    public final static int MASK_WRAP_MLTI = 0x00000001;
    public final static int MASK_WRAP_CHNG = 0x00000002;

    public int countvar;
    public Display last;
    public int flags;

    /**
     * <p>Prepare a term for execution.</p>
     * <p>Goal is updated in the skel and the display of this engine.</p>
     *
     * @return True if new display is returned, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public static boolean wrapGoal(Engine en)
            throws EngineException, EngineMessage {
        Object t = en.skel;
        Display d = en.display;
        EngineMessage.checkInstantiated(t);
        SupervisorWrap ew = en.visor.getWrap();
        ew.countvar = 0;
        ew.flags = 0;
        ew.last = Display.DISPLAY_CONST;
        ew.countGoal(t, d, en);
        if ((ew.flags & SupervisorWrap.MASK_WRAP_CHNG) == 0) {
            en.skel = t;
            en.display = d;
            return false;
        }
        if ((ew.flags & SupervisorWrap.MASK_WRAP_MLTI) != 0)
            ew.last = new Display(ew.countvar);
        ew.countvar = 0;
        en.skel = ew.replaceGoalAndWrap(t, d, en);
        en.display = ew.last;
        ew.last = Display.DISPLAY_CONST;
        return ((ew.flags & SupervisorWrap.MASK_WRAP_MLTI) != 0);
    }

    /**************************************************************************/
    /* Body Conversion Count                                                  */
    /**************************************************************************/

    /**
     * <p>Count the bridge variables of a term.</p>
     * <p>The reused or new display is updated in the engine copy display.</p>
     * <p>Tail recursive implementation.</p>
     *
     * @throws EngineMessage   Some non callable encountered.
     * @throws EngineException Shit happens.
     */
    public void countGoal(Object t, Display d,
                          Engine en)
            throws EngineMessage, EngineException {
        for (; ; ) {
            if (t instanceof SkelVar) {
                BindUniv b;
                if ((b = d.bind[((SkelVar) t).id]).display != null) {
                    t = b.skel;
                    d = b.display;
                    continue;
                }
                countvar++;
                if (last == Display.DISPLAY_CONST) {
                    last = d;
                } else if (last != d) {
                    flags |= MASK_WRAP_MLTI;
                }
                flags |= MASK_WRAP_CHNG;
                break;
            } else if (t instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) t;
                CachePredicate cp = CachePredicate.getPredicate(sc.sym, sc.args.length, en);
                Object[] decl = metaPredicateBody(cp);
                if (decl != null) {
                    for (int i = 0; i < sc.args.length - 1; i++) {
                        if (argZero(decl, i)) {
                            countGoal(sc.args[i], d, en);
                        } else if (argMinusOne(decl, i)) {
                            countTerm(sc.args[i], d, en);
                        } else {
                            countRest(sc.args[i], d);
                        }
                    }
                    if (argZero(decl, sc.args.length - 1)) {
                        t = sc.args[sc.args.length - 1];
                    } else if (argMinusOne(decl, sc.args.length - 1)) {
                        countTerm(sc.args[sc.args.length - 1], d, en);
                        break;
                    } else {
                        countRest(sc.args[sc.args.length - 1], d);
                        break;
                    }
                } else if (sc.var != null) {
                    countvar++;
                    if (last == Display.DISPLAY_CONST) {
                        last = d;
                    } else if (last != d) {
                        flags |= MASK_WRAP_MLTI;
                    }
                    break;
                } else {
                    break;
                }
            } else if (t instanceof SkelAtom) {
                break;
            } else {
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_CALLABLE, t), d);
            }
        }
    }

    /**
     * <p>Count the bridge variables of a term.</p>
     * <p>The reused or new display is updated in the engine copy display.</p>
     * <p>Tail recursive implementation.</p>
     *
     * @param t The term skel.
     * @param d The term display.
     * @throws EngineMessage   Some non callable encountered.
     * @throws EngineException Shit happens.
     */
    private void countTerm(Object t, Display d,
                           Engine en)
            throws EngineMessage, EngineException {
        for (; ; ) {
            if (t instanceof SkelVar) {
                BindUniv b;
                if ((b = d.bind[((SkelVar) t).id]).display != null) {
                    t = b.skel;
                    d = b.display;
                    continue;
                }
                countvar++;
                if (last == Display.DISPLAY_CONST) {
                    last = d;
                } else if (last != d) {
                    flags |= MASK_WRAP_MLTI;
                }
                break;
            } else if (t instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) t;
                CachePredicate cp = CachePredicate.getPredicate(sc.sym, sc.args.length, en);
                Object[] decl = metaPredicateRule(cp);
                if (decl != null) {
                    for (int i = 0; i < sc.args.length - 1; i++) {
                        if (argZero(decl, i)) {
                            countTerm(sc.args[i], d, en);
                        } else if (argMinusOne(decl, i)) {
                            countGoal(sc.args[i], d, en);
                        } else {
                            countRest(sc.args[i], d);
                        }
                    }
                    if (argZero(decl, sc.args.length - 1)) {
                        t = sc.args[sc.args.length - 1];
                    } else if (argMinusOne(decl, sc.args.length - 1)) {
                        countGoal(sc.args[sc.args.length - 1], d, en);
                        break;
                    } else {
                        countRest(sc.args[sc.args.length - 1], d);
                        break;
                    }
                } else if (sc.var != null) {
                    countvar++;
                    if (last == Display.DISPLAY_CONST) {
                        last = d;
                    } else if (last != d) {
                        flags |= MASK_WRAP_MLTI;
                    }
                    break;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    /**
     * <p>Count the bridge variables of the rest.</p>
     * <p>The reused or new display is updated in the engine copy display.</p>
     *
     * @param t The term skel.
     * @param d The term display.
     */
    private void countRest(Object t, Display d) {
        for (; ; ) {
            if (t instanceof SkelVar) {
                BindUniv b;
                if ((b = d.bind[((SkelVar) t).id]).display != null) {
                    t = b.skel;
                    d = b.display;
                    continue;
                }
                countvar++;
                if (last == Display.DISPLAY_CONST) {
                    last = d;
                } else if (last != d) {
                    flags |= MASK_WRAP_MLTI;
                }
                break;
            } else if (t instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) t;
                if (sc.var != null) {
                    countvar++;
                    if (last == Display.DISPLAY_CONST) {
                        last = d;
                    } else if (last != d) {
                        flags |= MASK_WRAP_MLTI;
                    }
                    break;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    /**************************************************************************/
    /* Body Conversion Replace                                                */
    /**************************************************************************/

    /**
     * <p>Replace the term skeleton and wrap naked calls.</p>
     * <p>The reused or new display is kept via the engine copy display.</p>
     * <p>Tail recursive implementation.</p>
     *
     * @param t  The term skeleton.
     * @param d  The term display..
     * @param en The engine.
     * @return The new term skeleton.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public Object replaceGoalAndWrap(Object t, Display d,
                                     Engine en)
            throws EngineMessage, EngineException {
        SkelCompound back = null;
        for (; ; ) {
            if (t instanceof SkelVar) {
                BindUniv b;
                if ((b = d.bind[((SkelVar) t).id]).display != null) {
                    t = b.skel;
                    d = b.display;
                    continue;
                }
                if ((flags & MASK_WRAP_MLTI) != 0) {
                    SkelVar sv = SkelVar.valueOf(countvar);
                    countvar++;
                    last.bind[sv.id].bindUniv(t, d, en);
                    t = sv;
                }
//                if (back != null &&
//                        back.args.length == 2 &&
//                        back.sym.fun.equals(SpecialQuali.OP_COLON)) {
//                    Object[] args = new Object[1];
//                    args[0] = back.args[1];
//                    SkelCompound help = new SkelCompound(new SkelAtom(Branch.OP_CALL,
//                            en.store.getRootSystem()), args, null);
//                    back.args[1] = help;
//                } else {
                t = new SkelCompound(en.store.foyer.ATOM_CALL, t);
//                }
                break;
            } else if (t instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) t;
                CachePredicate cp = CachePredicate.getPredicate(sc.sym, sc.args.length, en);
                Object[] decl = metaPredicateBody(cp);
                if (decl != null) {
                    Object[] args = new Object[sc.args.length];
                    for (int i = 0; i < sc.args.length - 1; i++) {
                        if (argZero(decl, i)) {
                            args[i] = replaceGoalAndWrap(sc.args[i], d, en);
                        } else if (argMinusOne(decl, i)) {
                            args[i] = replaceTermAndWrap(sc.args[i], d, en);
                        } else {
                            args[i] = replaceRest(sc.args[i], d, en);
                        }
                    }
                    if (argZero(decl, sc.args.length - 1)) {
                        args[sc.args.length - 1] = back;
                        back = new SkelCompound(sc.sym, args, null);
                        t = sc.args[sc.args.length - 1];
                    } else if (argMinusOne(decl, sc.args.length - 1)) {
                        args[sc.args.length - 1] = replaceTermAndWrap(sc.args[sc.args.length - 1], d, en);
                        t = new SkelCompound(sc.sym, args);
                        break;
                    } else {
                        args[sc.args.length - 1] = replaceRest(sc.args[sc.args.length - 1], d, en);
                        t = new SkelCompound(sc.sym, args);
                        break;
                    }
                } else if ((flags & MASK_WRAP_MLTI) != 0 && sc.var != null) {
                    SkelVar sv = SkelVar.valueOf(countvar);
                    countvar++;
                    last.bind[sv.id].bindUniv(t, d, en);
                    t = sv;
                    break;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        while (back != null) {
            SkelCompound help = (SkelCompound) back.args[back.args.length - 1];
            back.args[back.args.length - 1] = t;
            back.var = SkelCompound.makeExtra(back.args);
            t = back;
            back = help;
        }
        return t;
    }

    /**
     * <p>Replace the term skeleton and wrap naked calls.</p>
     * <p>The reused or new display is kept via the engine copy display.</p>
     * <p>Tail recursive implementation.</p>
     *
     * @param t  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The new term skeleton.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private Object replaceTermAndWrap(Object t, Display d,
                                      Engine en)
            throws EngineMessage, EngineException {
        SkelCompound back = null;
        for (; ; ) {
            if (t instanceof SkelVar) {
                BindUniv b;
                if ((b = d.bind[((SkelVar) t).id]).display != null) {
                    t = b.skel;
                    d = b.display;
                    continue;
                }
                if ((flags & MASK_WRAP_MLTI) != 0) {
                    SkelVar sv = SkelVar.valueOf(countvar);
                    countvar++;
                    last.bind[sv.id].bindUniv(t, d, en);
                    t = sv;
                }
                break;
            } else if (t instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) t;
                CachePredicate cp = CachePredicate.getPredicate(sc.sym, sc.args.length, en);
                Object[] decl = metaPredicateRule(cp);
                if (decl != null) {
                    Object[] args = new Object[sc.args.length];
                    for (int i = 0; i < sc.args.length - 1; i++) {
                        if (argZero(decl, i)) {
                            args[i] = replaceTermAndWrap(sc.args[i], d, en);
                        } else if (argMinusOne(decl, i)) {
                            args[i] = replaceGoalAndWrap(sc.args[i], d, en);
                        } else {
                            args[i] = replaceRest(sc.args[i], d, en);
                        }
                    }
                    if (argZero(decl, sc.args.length - 1)) {
                        args[sc.args.length - 1] = back;
                        back = new SkelCompound(sc.sym, args, null);
                        t = sc.args[sc.args.length - 1];
                    } else if (argMinusOne(decl, sc.args.length - 1)) {
                        args[sc.args.length - 1] = replaceGoalAndWrap(sc.args[sc.args.length - 1], d, en);
                        t = new SkelCompound(sc.sym, args);
                        break;
                    } else {
                        args[sc.args.length - 1] = replaceRest(sc.args[sc.args.length - 1], d, en);
                        t = new SkelCompound(sc.sym, args);
                        break;
                    }
                } else if ((flags & MASK_WRAP_MLTI) != 0 && sc.var != null) {
                    SkelVar sv = SkelVar.valueOf(countvar);
                    countvar++;
                    last.bind[sv.id].bindUniv(t, d, en);
                    t = sv;
                    break;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        while (back != null) {
            SkelCompound help = (SkelCompound) back.args[back.args.length - 1];
            back.args[back.args.length - 1] = t;
            back.var = SkelCompound.makeExtra(back.args);
            t = back;
            back = help;
        }
        return t;
    }

    /**
     * <p>Replace the term skeleton and wrap naked calls.</p>
     * <p>The reused or new display is passed via the engine display.</p>
     * <p>The reused or new display is returned in the engine display.</p>
     *
     * @param t  The rest skeleton.
     * @param d  The rest display.
     * @param en The engine.
     * @return The new rest skeleton.
     */
    private Object replaceRest(Object t, Display d, Engine en) {
        for (; ; ) {
            if (t instanceof SkelVar) {
                BindUniv b;
                if ((b = d.bind[((SkelVar) t).id]).display != null) {
                    t = b.skel;
                    d = b.display;
                    continue;
                }
                if ((flags & MASK_WRAP_MLTI) != 0) {
                    SkelVar sv = SkelVar.valueOf(countvar);
                    countvar++;
                    last.bind[sv.id].bindUniv(t, d, en);
                    t = sv;
                }
                break;
            } else if (t instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) t;
                if ((flags & MASK_WRAP_MLTI) != 0 && sc.var != null) {
                    SkelVar sv = SkelVar.valueOf(countvar);
                    countvar++;
                    last.bind[sv.id].bindUniv(t, d, en);
                    t = sv;
                    break;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        return t;
    }

    /*******************************************************/
    /* Body Conversion Helper                              */
    /*******************************************************/

    /**
     * <p>Retrieve the meta specifiers of a body predicate.</p>
     *
     * @param cp The cache predicate.
     * @return The meta spezifiers, or null.
     */
    private static Object[] metaPredicateBody(CachePredicate cp) {
        if (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0)
            return null;
        Predicate pick = cp.pick;
        if ((pick.getBits() & Predicate.MASK_PRED_BODY) == 0)
            return null;
        Object t = pick.meta_predicate;
        return (t != null ? ((SkelCompound) t).args : null);
    }

    /**
     * <p>Retrieve the meta specifiers of a rule predicate.</p>
     *
     * @param cp The cache predicate.
     * @return The meta spezifiers, or null.
     */
    private static Object[] metaPredicateRule(CachePredicate cp) {
        if (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0)
            return null;
        Predicate pick = cp.pick;
        if ((pick.getBits() & Predicate.MASK_PRED_RULE) == 0)
            return null;
        Object t = pick.meta_predicate;
        return (t != null ? ((SkelCompound) t).args : null);
    }

    /**
     * <p>Check whether the argument is a exactly 0.</p>
     *
     * @param args The meta declaration.
     * @param k    The index.
     * @return The argument spez.
     */
    private static boolean argZero(Object[] args, int k) {
        Object obj = args[k];
        if (!(obj instanceof Integer))
            return false;
        return ((Integer) obj).intValue() == 0;
    }

    /**
     * <p>Check whether the argument is a exactly -1.</p>
     *
     * @param args The meta declaration.
     * @param k    The index.
     * @return The argument spez.
     */
    private static boolean argMinusOne(Object[] args, int k) {
        Object obj = args[k];
        if (!(obj instanceof Integer))
            return false;
        return ((Integer) obj).intValue() == -1;
    }

}
