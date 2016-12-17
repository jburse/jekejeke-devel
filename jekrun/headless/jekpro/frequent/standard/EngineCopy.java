package jekpro.frequent.standard;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.*;
import jekpro.model.pretty.Store;
import jekpro.model.rope.Intermediate;
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
    public MapHash<TermVar, SkelVar> vars;

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
        TermVar key = new TermVar(v, d);
        if (vars == null) {
            vars = new MapHash<TermVar, SkelVar>();
            v = null;
        } else {
            v = vars.get(key);
        }
        if (v == null) {
            v = new SkelVar(vars.size);
            vars.put(key, v);
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
    public Object copyTerm(Object t, Display d) {
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
                if (sc.vars != null) {
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
        ListArray<SkelVar> vec = SkelCompound.collectVars(t, null);
        SkelVar[] vars = null;
        do {
            SkelCompound jack = (SkelCompound) back.args[back.args.length - 1];
            back.args[back.args.length - 1] = t;
            vec = SkelCompound.prepareListButOne(back.args, vec);
            back.vars = (vars = SkelCompound.listToArray(vec, vars));
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
     * @param r  The continuation skeleton.
     * @param u  The continuation display.
     * @param en The engine.
     * @return A copy of the goal with wrapped naked calls.
     * @throws EngineMessage   Some non callable encountered.
     * @throws EngineException Some non callable encountered.
     */
    public Object copyGoalAndWrap(Object t, Display d,
                                  Intermediate r, DisplayClause u,
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
                t = new SkelCompound(new SkelAtom(Store.OP_CALL), t);
                break;
            } else if (t instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) t;
                Predicate pick = CachePredicate.getPredicate(sc.sym, sc.args.length, r, u, en);
                Object[] decl = EngineCopy.metaPredicateBody(pick);
                if (decl != null) {
                    Object[] args = new Object[sc.args.length];
                    for (int i = 0; i < sc.args.length - 1; i++) {
                        if (EngineCopy.argZero(decl, i)) {
                            args[i] = copyGoalAndWrap(sc.args[i], d, r, u, en);
                        } else if (EngineCopy.argMinusOne(decl, i)) {
                            args[i] = copyTermAndWrap(sc.args[i], d, r, u, en);
                        } else {
                            args[i] = copyTerm(sc.args[i], d);
                        }
                    }
                    if (EngineCopy.argZero(decl, sc.args.length - 1)) {
                        args[sc.args.length - 1] = back;
                        back = new SkelCompound(sc.sym, args, null);
                        t = sc.args[sc.args.length - 1];
                    } else if (EngineCopy.argMinusOne(decl, sc.args.length - 1)) {
                        args[sc.args.length - 1] = copyTermAndWrap(sc.args[sc.args.length - 1], d, r, u, en);
                        t = new SkelCompound(sc.sym, args);
                        break;
                    } else {
                        args[sc.args.length - 1] = copyTerm(sc.args[sc.args.length - 1], d);
                        t = new SkelCompound(sc.sym, args);
                        break;
                    }
                } else if (sc.vars != null) {
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
        ListArray<SkelVar> vec = SkelCompound.collectVars(t, null);
        SkelVar[] vars = null;
        do {
            SkelCompound help = (SkelCompound) back.args[back.args.length - 1];
            back.args[back.args.length - 1] = t;
            vec = SkelCompound.prepareListButOne(back.args, vec);
            back.vars = (vars = SkelCompound.listToArray(vec, vars));
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
     * @param r  The continuation skeleton.
     * @param u  The continuation display.
     * @param en The engine.
     * @return A copy of the goal with wrapped naked calls.
     * @throws EngineMessage   Some non callable encountered.
     * @throws EngineException Some non callable encountered.
     */
    public Object copyTermAndWrap(Object t, Display d,
                                  Intermediate r, DisplayClause u,
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
                Predicate pick = CachePredicate.getPredicate(sc.sym, sc.args.length, r, u, en);
                Object[] decl = EngineCopy.metaPredicateRule(pick);
                if (decl != null) {
                    Object[] args = new Object[sc.args.length];
                    for (int i = 0; i < sc.args.length - 1; i++) {
                        if (EngineCopy.argZero(decl, i)) {
                            args[i] = copyTermAndWrap(sc.args[i], d, r, u, en);
                        } else if (EngineCopy.argMinusOne(decl, i)) {
                            args[i] = copyGoalAndWrap(sc.args[i], d, r, u, en);
                        } else {
                            args[i] = copyTerm(sc.args[i], d);
                        }
                    }
                    if (EngineCopy.argZero(decl, sc.args.length - 1)) {
                        args[sc.args.length - 1] = back;
                        back = new SkelCompound(sc.sym, args, null);
                        t = sc.args[sc.args.length - 1];
                    } else if (EngineCopy.argMinusOne(decl, sc.args.length - 1)) {
                        args[sc.args.length - 1] = copyGoalAndWrap(sc.args[sc.args.length - 1], d, r, u, en);
                        t = new SkelCompound(sc.sym, args);
                        break;
                    } else {
                        args[sc.args.length - 1] = copyTerm(sc.args[sc.args.length - 1], d);
                        t = new SkelCompound(sc.sym, args);
                        break;
                    }
                } else if (sc.vars != null) {
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
        ListArray<SkelVar> vec = SkelCompound.collectVars(t, null);
        SkelVar[] vars = null;
        do {
            SkelCompound help = (SkelCompound) back.args[back.args.length - 1];
            back.args[back.args.length - 1] = t;
            vec = SkelCompound.prepareListButOne(back.args, vec);
            back.vars = (vars = SkelCompound.listToArray(vec, vars));
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
     * @param pick The predicate.
     * @return The meta spezifiers, or null.
     */
    public static Object[] metaPredicateBody(Predicate pick) {
        if (pick == null || (pick.getBits() & Predicate.MASK_PRED_BODY) == 0)
            return null;
        Object t = pick.meta_predicate;
        return (t != null ? ((SkelCompound) t).args : null);
    }

    /**
     * <p>Retrieve the meta specifiers of a rule predicate.</p>
     *
     * @param pick The predicate.
     * @return The meta spezifiers, or null.
     */
    public static Object[] metaPredicateRule(Predicate pick) {
        if (pick == null || (pick.getBits() & Predicate.MASK_PRED_RULE) == 0)
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
