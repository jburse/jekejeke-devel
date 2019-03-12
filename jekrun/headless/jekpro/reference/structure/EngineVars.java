package jekpro.reference.structure;

import jekpro.frequent.standard.EngineCopy;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.SetEntry;
import matula.util.data.SetHashLink;

/**
 * <p>This class provides basic functions to enumerate variables from molces.</p>
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
public final class EngineVars {
    public SetHashLink<Object> vars; /* input order */
    public SetHashLink<Object> anon; /* input order */
    public SetHashLink<BindUniv> visit; /* input order */

    /****************************************************************/
    /* Variable & Singletons                                        */
    /****************************************************************/

    /**
     * <p>Collect the vars of the given term in the given var set.</p>
     * <p>Makes use of the vars of skel compound.</p>
     * <p>The result is collected in vars.</p>
     * <p>Tail recursive solution.</p>
     *
     * @param t The term skel.
     * @param d The term display.
     */
    public void varInclude(Object t, Display d) {
        for (; ; ) {
            Object var = EngineCopy.getVar(t);
            if (var == null)
                break;
            SkelVar v;
            if (var instanceof SkelVar) {
                v = (SkelVar) var;
            } else {
                SkelVar[] temp = (SkelVar[]) var;
                int j = 0;
                for (; j < temp.length - 1; j++) {
                    v = temp[j];
                    BindUniv b = d.bind[v.id];
                    if (b.display != null) {
                        varInclude(b.skel, b.display);
                    } else {
                        varsAdd(v, d);
                    }
                }
                v = temp[j];
            }
            BindUniv b = d.bind[v.id];
            if (b.display != null) {
                t = b.skel;
                d = b.display;
            } else {
                varsAdd(v, d);
                break;
            }
        }
    }

    /**
     * <p>Add a variable to the variable list.</p>
     *
     * @param m The variable skeleton.
     * @param d The variable display.
     */
    private void varsAdd(Object m, Display d) {
        Object key = AbstractTerm.createMolec(m, d);
        if (vars == null) {
            vars = new SetHashLink<Object>();
            vars.add(key);
        } else {
            if (vars.getKey(key) == null)
                vars.add(key);
        }
    }

    /**
     * <p>Collect the vars of the given term in the given var set.</p>
     * <p>Makes use of the vars of skel compound.</p>
     * <p>The result is collected in vars.</p>
     * <p>Tail recursive solution.</p>
     *
     * @param t The term skel.
     * @param d The term display.
     */
    public void varExclude(Object t, Display d) {
        for (; ; ) {
            Object var = EngineCopy.getVar(t);
            if (var == null)
                break;
            SkelVar v;
            if (var instanceof SkelVar) {
                v = (SkelVar) var;
            } else {
                SkelVar[] temp = (SkelVar[]) var;
                int j = 0;
                for (; j < temp.length - 1; j++) {
                    v = temp[j];
                    BindUniv b = d.bind[v.id];
                    if (b.display != null) {
                        varExclude(b.skel, b.display);
                    } else {
                        Object key = AbstractTerm.createMolec(v, d);
                        if (vars != null) {
                            vars.remove(key);
                            if (vars.size == 0)
                                vars = null;
                        }
                    }
                }
                v = temp[j];
            }
            BindUniv b = d.bind[v.id];
            if (b.display != null) {
                t = b.skel;
                d = b.display;
            } else {
                Object key = AbstractTerm.createMolec(v, d);
                if (vars != null) {
                    vars.remove(key);
                    if (vars.size == 0)
                        vars = null;
                }
                break;
            }
        }
    }

    /**
     * <p>Collect the anonymous of the given term in the given var set.</p>
     * <p>Makes partial use of the vars of the skel compounds.</p>
     * <p>The result is collected in vars.</p>
     * <p>Tail recursive solution.</p>
     *
     * @param t The term skel.
     * @param d The term display.
     */
    public void singsOf(Object t, Display d) {
        for (; ; ) {
            if (t instanceof SkelVar) {
                SkelVar v = (SkelVar) t;
                BindUniv b;
                if ((b = d.bind[v.id]).display != null) {
                    t = b.skel;
                    d = b.display;
                    continue;
                }
                Object key = AbstractTerm.createMolec(v, d);
                boolean f;
                if (vars == null) {
                    vars = new SetHashLink<Object>();
                    f = false;
                } else {
                    f = vars.getKey(key) != null;
                }
                if (!f) {
                    vars.add(key);
                    if (anon == null)
                        anon = new SetHashLink<Object>();
                    anon.add(key);
                } else {
                    if (anon != null) {
                        anon.remove(key);
                        if (anon.size() == 0)
                            anon = null;
                    }
                }
                break;
            } else if (t instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) t;
                if (sc.var == null)
                    break;
                int i = 0;
                for (; i < sc.args.length - 1; i++)
                    singsOf(sc.args[i], d);
                t = sc.args[i];
            } else {
                break;
            }
        }
    }

    /****************************************************************/
    /* Cyclic Terms                                                 */
    /****************************************************************/

    /**
     * <p>Check whether the given term is acyclic.</p>
     * <p>Tail recursive solution.</p>
     *
     * @param t The term skel.
     * @param d The term display.
     * @return True if the term is acyclic, otherwise false.
     */
    public boolean isAcyclic(Object t, Display d) {
        int undo = 0;
        for (; ; ) {
            Object var = EngineCopy.getVar(t);
            if (var == null)
                break;
            SkelVar v;
            if (var instanceof SkelVar) {
                v = (SkelVar) var;
            } else {
                SkelVar[] temp = (SkelVar[]) var;
                int j = 0;
                for (; j < temp.length - 1; j++) {
                    v = temp[j];
                    BindUniv b = d.bind[v.id];
                    if (b.display != null) {
                        if (visitAdd(b))
                            return false;
                        if (!isAcyclic(b.skel, b.display))
                            return false;
                        visit.remove(b);
                    }
                }
                v = temp[j];
            }
            BindUniv b = d.bind[v.id];
            if (b.display != null) {
                if (visitAdd(b))
                    return false;
                undo++;
                t = b.skel;
                d = b.display;
            } else {
                break;
            }
        }
        while (undo > 0) {
            SetEntry<BindUniv> entry = visit.getLastEntry();
            visit.remove(entry.value);
            undo--;
        }
        return true;
    }

    /**
     * <p>Add a bind to the visit list.</p>
     *
     * @param b The bind.
     * @return True if the bind already exists, otherwise false.
     */
    private boolean visitAdd(BindUniv b) {
        if (visit == null) {
            visit = new SetHashLink<BindUniv>();
            visit.add(b);
        } else {
            if (visit.getKey(b) == null) {
                visit.add(b);
            } else {
                return true;
            }
        }
        return false;
    }

    /**
     * <p>Tail recursive solution.</p>
     *
     * @param t The term skel.
     * @param d The term display.
     */
    public void safeVars(Object t, Display d) {
        int undo = 0;
        for (; ; ) {
            Object var = EngineCopy.getVar(t);
            if (var == null)
                break;
            SkelVar v;
            if (var instanceof SkelVar) {
                v = (SkelVar) var;
            } else {
                SkelVar[] temp = (SkelVar[]) var;
                int j = 0;
                for (; j < temp.length - 1; j++) {
                    v = temp[j];
                    BindUniv b = d.bind[v.id];
                    if (b.display != null) {
                        if (visitAdd(b))
                            continue;
                        safeVars(b.skel, b.display);
                        visit.remove(b);
                    } else {
                        varsAdd(v, d);
                    }
                }
                v = temp[j];
            }
            BindUniv b = d.bind[v.id];
            if (b.display != null) {
                if (visitAdd(b))
                    break;
                undo++;
                t = b.skel;
                d = b.display;
            } else {
                varsAdd(v, d);
                break;
            }
        }
        while (undo > 0) {
            SetEntry<BindUniv> entry = visit.getLastEntry();
            visit.remove(entry.value);
            undo--;
        }
    }

}
