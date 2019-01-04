package jekpro.reference.structure;

import jekpro.frequent.standard.EngineCopy;
import jekpro.model.molec.BindCount;
import jekpro.model.molec.BindVar;
import jekpro.model.molec.Display;
import jekpro.model.pretty.NamedDistance;
import jekpro.model.pretty.PrologReader;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import jekpro.tools.term.TermAtomic;
import matula.util.data.*;

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

    /****************************************************************/
    /* Molec Operations                                             */
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
                    BindVar b = d.bind[v.id];
                    if (b.display != null) {
                        varInclude(b.skel, b.display);
                    } else {
                        Object key = AbstractTerm.createMolec(v, d);
                        if (vars == null) {
                            vars = new SetHashLink<Object>();
                            vars.add(key);
                        } else {
                            if (vars.getKey(key) == null)
                                vars.add(key);
                        }
                    }
                }
                v = temp[j];
            }
            BindVar b = d.bind[v.id];
            if (b.display != null) {
                t = b.skel;
                d = b.display;
            } else {
                Object key = AbstractTerm.createMolec(v, d);
                if (vars == null) {
                    vars = new SetHashLink<Object>();
                    vars.add(key);
                } else {
                    if (vars.getKey(key) == null)
                        vars.add(key);
                }
                break;
            }
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
                    BindVar b = d.bind[v.id];
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
            BindVar b = d.bind[v.id];
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
                BindVar b;
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
                    BindVar b = d.bind[v.id];
                    if (b.display != null) {
                        Object key = AbstractTerm.createMolec(v, d);
                        if (vars == null) {
                            vars = new SetHashLink<Object>();
                            vars.add(key);
                        } else {
                            if (vars.getKey(key) == null) {
                                vars.add(key);
                            } else {
                                return false;
                            }
                        }
                        if (!isAcyclic(b.skel, b.display))
                            return false;
                        vars.remove(key);
                    }
                }
                v = temp[j];
            }
            BindVar b = d.bind[v.id];
            if (b.display != null) {
                Object key = AbstractTerm.createMolec(v, d);
                if (vars == null) {
                    vars = new SetHashLink<Object>();
                    vars.add(key);
                } else {
                    if (vars.getKey(key) == null) {
                        vars.add(key);
                    } else {
                        return false;
                    }
                }
                undo++;
                t = b.skel;
                d = b.display;
            } else {
                break;
            }
        }
        while (undo > 0) {
            SetEntry<Object> entry = vars.getLastEntry();
            vars.remove(entry.key);
            undo--;
        }
        return true;
    }

}
