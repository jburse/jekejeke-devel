package jekpro.frequent.standard;

import jekpro.model.builtin.Branch;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.*;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import jekpro.tools.term.TermVar;
import matula.util.data.ListArray;
import matula.util.data.MapHash;

/**
 * <p>This class provides basic functions to copy terms.</p>
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
public class EngineCopy {
    public MapHash<BindCount, SkelVar> vars;

    /**
     * <p>Retrieve the variable or variable array.</p>
     *
     * @param t The term.
     * @return The variable or variable array.
     */
    public static Object getVar(Object t) {
        if (t instanceof SkelVar) {
            return t;
        } else if (t instanceof SkelCompound) {
            return ((SkelCompound) t).var;
        } else {
            return null;
        }
    }

    /*******************************************************/
    /* Variable Allocation                                 */
    /*******************************************************/

    /**
     * <p>Get a new variable for an old variable.</p>
     *
     * @param v The old variable skeleton.
     * @param d The old variable display.
     * @return The new variable.
     */
    public SkelVar getVar(SkelVar v, Display d) {
        BindCount key=d.bind[v.id];
        if (vars == null) {
            vars = new MapHash<BindCount, SkelVar>();
            v = null;
        } else {
            v = vars.get(key);
        }
        if (v == null) {
            v = new SkelVar(vars.size);
            vars.add(key, v);
        }
        return v;
    }

    /**
     * <p>Copy the given term.</p>
     * <p>Will change vars as a side effect.</p>
     * <p>Tail recursive solution.</p>
     *
     * @param t The term skel.
     * @param d The term display.
     * @return A copy of the term.
     */
    public final Object copyTerm(Object t, Display d) {
        SkelCompound back = null;
        for (; ; ) {
            if (t instanceof SkelVar) {
                SkelVar v = (SkelVar) t;
                BindVar b;
                if ((b = d.bind[v.id]).display != null) {
                    t = b.skel;
                    d = b.display;
                    continue;
                }
                t = getVar(v, d);
                break;
            } else if (t instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) t;
                if (sc.var != null) {
                    Object[] args = new Object[sc.args.length];
                    for (int i = 0; i < sc.args.length - 1; i++)
                        args[i] = copyTerm(sc.args[i], d);
                    args[sc.args.length - 1] = back;
                    back = new SkelCompound(sc.sym, args, null);
                    t = sc.args[sc.args.length - 1];
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        if (back == null)
            return t;
        do {
            SkelCompound jack = (SkelCompound) back.args[back.args.length - 1];
            back.args[back.args.length - 1] = t;
            back.var = SkelCompound.makeExtra(back.args);
            t = back;
            back = jack;
        } while (back != null);
        return t;
    }

    /**************************************************************************/
    /* Body Conversion Assert                                                 */
    /**************************************************************************/
    /**
     * <p>Copy the goal and wrap naked calls.</p>
     * <p>Implements the following rules:</p>
     * <pre>
     *      copyGoalAndWrap(V) --> call(V)
     *      meta_predicate(...G...) --> meta_predicate(...copyGoalAndWrap(G)...)
     *      meta_predicate(...T...) --> meta_predicate(...copyTermAndWrap(T)...)
     *      predicate(...A...) --> predicate(...copyTerm(A)...)
     *      A --> A
     *      C --> type_error(callable, C)
     * </pre>
     * <p>Tail recursive implementation.</p>
     *
     * @param t  The goal skel.
     * @param d  The goal display.
     * @param en The engine.
     * @return A copy of the goal with wrapped naked calls.
     * @throws EngineMessage   Some non callable encountered.
     * @throws EngineException Some non callable encountered.
     */
    public final Object copyGoalAndWrap(Object t, Display d,
                                  Engine en)
            throws EngineMessage, EngineException {
        SkelCompound back = null;
        for (; ; ) {
            if (t instanceof SkelVar) {
                SkelVar v = (SkelVar) t;
                BindVar b;
                if ((b = d.bind[v.id]).display != null) {
                    t = b.skel;
                    d = b.display;
                    continue;
                }
                t = getVar(v, d);
                t = new SkelCompound(new SkelAtom(Branch.OP_CALL), t);
                break;
            } else if (t instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) t;
                CachePredicate cp = CachePredicate.getPredicate(sc.sym, sc.args.length, en);
                Object[] decl = EngineCopy.metaPredicateBody(cp);
                if (decl != null) {
                    Object[] args = new Object[sc.args.length];
                    for (int i = 0; i < sc.args.length - 1; i++) {
                        if (EngineCopy.argZero(decl, i)) {
                            args[i] = copyGoalAndWrap(sc.args[i], d, en);
                        } else if (EngineCopy.argMinusOne(decl, i)) {
                            args[i] = copyTermAndWrap(sc.args[i], d, en);
                        } else {
                            args[i] = copyTerm(sc.args[i], d);
                        }
                    }
                    if (EngineCopy.argZero(decl, sc.args.length - 1)) {
                        args[sc.args.length - 1] = back;
                        back = new SkelCompound(sc.sym, args, null);
                        t = sc.args[sc.args.length - 1];
                    } else if (EngineCopy.argMinusOne(decl, sc.args.length - 1)) {
                        args[sc.args.length - 1] = copyTermAndWrap(sc.args[sc.args.length - 1], d, en);
                        t = new SkelCompound(sc.sym, args);
                        break;
                    } else {
                        args[sc.args.length - 1] = copyTerm(sc.args[sc.args.length - 1], d);
                        t = new SkelCompound(sc.sym, args);
                        break;
                    }
                } else if (sc.var != null) {
                    Object[] args = new Object[sc.args.length];
                    for (int i = 0; i < sc.args.length; i++)
                        args[i] = copyTerm(sc.args[i], d);
                    t = new SkelCompound(sc.sym, args);
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
        if (back == null)
            return t;
        do {
            SkelCompound help = (SkelCompound) back.args[back.args.length - 1];
            back.args[back.args.length - 1] = t;
            back.var = SkelCompound.makeExtra(back.args);
            t = back;
            back = help;
        } while (back != null);
        return t;
    }

    /**
     * <p>Copy the term and wrap naked calls.</p>
     * <p>Implements the following rules:</p>
     * <pre>
     *      copyTermAndWrap(V) --> V
     *      meta_predicate(...T...) --> meta_predicate(...copyTermAndWrap(T)...)
     *      meta_predicate(...G...) --> meta_predicate(...copyGoalAndWrap(G)...)
     *      predicate(...A...) --> predicate(...copyTerm(A)...)
     *      A --> A
     *      C --> C
     * </pre>
     * <p>Tail recursive implementation.</p>
     *
     * @param t  The term skel.
     * @param d  The term display.
     * @param en The engine.
     * @return A copy of the goal with wrapped naked calls.
     * @throws EngineMessage   Some non callable encountered.
     * @throws EngineException Some non callable encountered.
     */
    public final Object copyTermAndWrap(Object t, Display d,
                                  Engine en)
            throws EngineMessage, EngineException {
        SkelCompound back = null;
        for (; ; ) {
            if (t instanceof SkelVar) {
                SkelVar v = (SkelVar) t;
                BindVar b;
                if ((b = d.bind[v.id]).display != null) {
                    t = b.skel;
                    d = b.display;
                    continue;
                }
                t = getVar(v, d);
                break;
            } else if (t instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) t;
                CachePredicate cp = CachePredicate.getPredicate(sc.sym, sc.args.length, en);
                Object[] decl = EngineCopy.metaPredicateRule(cp);
                if (decl != null) {
                    Object[] args = new Object[sc.args.length];
                    for (int i = 0; i < sc.args.length - 1; i++) {
                        if (EngineCopy.argZero(decl, i)) {
                            args[i] = copyTermAndWrap(sc.args[i], d, en);
                        } else if (EngineCopy.argMinusOne(decl, i)) {
                            args[i] = copyGoalAndWrap(sc.args[i], d, en);
                        } else {
                            args[i] = copyTerm(sc.args[i], d);
                        }
                    }
                    if (EngineCopy.argZero(decl, sc.args.length - 1)) {
                        args[sc.args.length - 1] = back;
                        back = new SkelCompound(sc.sym, args, null);
                        t = sc.args[sc.args.length - 1];
                    } else if (EngineCopy.argMinusOne(decl, sc.args.length - 1)) {
                        args[sc.args.length - 1] = copyGoalAndWrap(sc.args[sc.args.length - 1], d, en);
                        t = new SkelCompound(sc.sym, args);
                        break;
                    } else {
                        args[sc.args.length - 1] = copyTerm(sc.args[sc.args.length - 1], d);
                        t = new SkelCompound(sc.sym, args);
                        break;
                    }
                } else if (sc.var != null) {
                    Object[] args = new Object[sc.args.length];
                    for (int i = 0; i < sc.args.length; i++)
                        args[i] = copyTerm(sc.args[i], d);
                    t = new SkelCompound(sc.sym, args);
                    break;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        if (back == null)
            return t;
        do {
            SkelCompound help = (SkelCompound) back.args[back.args.length - 1];
            back.args[back.args.length - 1] = t;
            back.var = SkelCompound.makeExtra(back.args);
            t = back;
            back = help;
        } while (back != null);
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
    public static Object[] metaPredicateBody(CachePredicate cp) {
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
    public static Object[] metaPredicateRule(CachePredicate cp) {
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
    public static boolean argZero(Object[] args, int k) {
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
    public static boolean argMinusOne(Object[] args, int k) {
        Object obj = args[k];
        if (!(obj instanceof Integer))
            return false;
        return ((Integer) obj).intValue() == -1;
    }

}
