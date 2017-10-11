package jekpro.reference.structure;

import jekpro.model.molec.BindVar;
import jekpro.model.molec.Display;
import jekpro.model.pretty.PrologReader;
import jekpro.model.rope.NamedDistance;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import jekpro.tools.term.TermVar;
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class EngineVars {
    public SetHashLink<TermVar> vars; /* input order */
    public SetHashLink<TermVar> anon; /* input order */

    /****************************************************************/
    /* Molec Operations                                             */
    /****************************************************************/

    /**
     * <p>Check whether the given term is ground.</p>
     * <p>Tail recursive solution.</p>
     *
     * @param t The term skel.
     * @param d The term display.
     * @return True if the term is ground, otherwise false.
     */
    public static boolean isGround(Object t, Display d) {
        for (; ; ) {
            if (t instanceof SkelVar) {
                SkelVar v = (SkelVar) t;
                BindVar b = d.bind[v.id];
                if (b.display != null) {
                    t = b.skel;
                    d = b.display;
                } else {
                    return false;
                }
            } else if (t instanceof SkelCompound) {
                SkelVar[] vars = ((SkelCompound) t).vars;
                if (vars != null) {
                    int j = 0;
                    for (; j < vars.length - 1; j++) {
                        SkelVar v = vars[j];
                        BindVar b = d.bind[v.id];
                        if (b.display != null) {
                            if (!isGround(b.skel, b.display))
                                return false;
                        } else {
                            return false;
                        }
                    }
                    SkelVar v = vars[j];
                    BindVar b = d.bind[v.id];
                    if (b.display != null) {
                        t = b.skel;
                        d = b.display;
                    } else {
                        return false;
                    }
                } else {
                    return true;
                }
            } else {
                return true;
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
    public void varInclude(Object t, Display d) {
        for (; ; ) {
            if (t instanceof SkelVar) {
                SkelVar v = (SkelVar) t;
                BindVar b = d.bind[v.id];
                if (b.display != null) {
                    t = b.skel;
                    d = b.display;
                } else {
                    TermVar key = new TermVar(v, d);
                    if (vars == null) {
                        vars = new SetHashLink<TermVar>();
                        vars.putKey(key);
                    } else {
                        if (vars.getKey(key) == null)
                            vars.putKey(key);
                    }
                    break;
                }
            } else if (t instanceof SkelCompound) {
                SkelVar[] scvars = ((SkelCompound) t).vars;
                if (scvars != null) {
                    int j = 0;
                    for (; j < scvars.length - 1; j++) {
                        SkelVar v = scvars[j];
                        BindVar b = d.bind[v.id];
                        if (b.display != null) {
                            varInclude(b.skel, b.display);
                        } else {
                            TermVar key = new TermVar(v, d);
                            if (vars == null) {
                                vars = new SetHashLink<TermVar>();
                                vars.putKey(key);
                            } else {
                                if (vars.getKey(key) == null)
                                    vars.putKey(key);
                            }
                        }
                    }
                    SkelVar v = scvars[j];
                    BindVar b = d.bind[v.id];
                    if (b.display != null) {
                        t = b.skel;
                        d = b.display;
                    } else {
                        TermVar key = new TermVar(v, d);
                        if (vars == null) {
                            vars = new SetHashLink<TermVar>();
                            vars.putKey(key);
                        } else {
                            if (vars.getKey(key) == null)
                                vars.putKey(key);
                        }
                        break;
                    }
                } else {
                    break;
                }
            } else {
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
            if (t instanceof SkelVar) {
                SkelVar v = (SkelVar) t;
                BindVar b = d.bind[v.id];
                if (b.display != null) {
                    t = b.skel;
                    d = b.display;
                } else {
                    TermVar key = new TermVar(v, d);
                    if (vars != null) {
                        vars.remove(key);
                        if (vars.size == 0)
                            vars = null;
                    }
                    break;
                }
            } else if (t instanceof SkelCompound) {
                SkelVar[] scvars = ((SkelCompound) t).vars;
                if (scvars != null) {
                    int j = 0;
                    for (; j < scvars.length - 1; j++) {
                        SkelVar v = scvars[j];
                        BindVar b = d.bind[v.id];
                        if (b.display != null) {
                            varExclude(b.skel, b.display);
                        } else {
                            TermVar key = new TermVar(v, d);
                            if (vars != null) {
                                vars.remove(key);
                                if (vars.size == 0)
                                    vars = null;
                            }
                        }
                    }
                    SkelVar v = scvars[j];
                    BindVar b = d.bind[v.id];
                    if (b.display != null) {
                        t = b.skel;
                        d = b.display;
                    } else {
                        TermVar key = new TermVar(v, d);
                        if (vars != null) {
                            vars.remove(key);
                            if (vars.size == 0)
                                vars = null;
                        }
                        break;
                    }
                } else {
                    break;
                }
            } else {
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
                TermVar key = new TermVar(v, d);
                boolean f;
                if (vars == null) {
                    vars = new SetHashLink<TermVar>();
                    f = false;
                } else {
                    f = vars.getKey(key) != null;
                }
                if (!f) {
                    vars.putKey(key);
                    if (anon == null)
                        anon = new SetHashLink<TermVar>();
                    anon.putKey(key);
                } else {
                    anon.remove(key);
                }
                break;
            } else if (t instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) t;
                if (sc.vars != null) {
                    int i = 0;
                    for (; i < sc.args.length - 1; i++)
                        singsOf(sc.args[i], d);
                    t = sc.args[i];
                } else {
                    break;
                }
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
            if (t instanceof SkelVar) {
                SkelVar v = (SkelVar) t;
                BindVar b = d.bind[v.id];
                if (b.display != null) {
                    TermVar key = new TermVar(v, d);
                    if (vars == null) {
                        vars = new SetHashLink<TermVar>();
                        vars.putKey(key);
                    } else {
                        if (vars.getKey(key) == null) {
                            vars.putKey(key);
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
            } else if (t instanceof SkelCompound) {
                SkelVar[] scvars = ((SkelCompound) t).vars;
                if (scvars != null) {
                    int j = 0;
                    for (; j < scvars.length - 1; j++) {
                        SkelVar v = scvars[j];
                        BindVar b = d.bind[v.id];
                        if (b.display != null) {
                            TermVar key = new TermVar(v, d);
                            if (vars == null) {
                                vars = new SetHashLink<TermVar>();
                                vars.putKey(key);
                            } else {
                                if (vars.getKey(key) == null) {
                                    vars.putKey(key);
                                } else {
                                    return false;
                                }
                            }
                            if (!isAcyclic(b.skel, b.display))
                                return false;
                            vars.remove(key);
                        }
                    }
                    SkelVar v = scvars[j];
                    BindVar b = d.bind[v.id];
                    if (b.display != null) {
                        TermVar key = new TermVar(v, d);
                        if (vars == null) {
                            vars = new SetHashLink<TermVar>();
                            vars.putKey(key);
                        } else {
                            if (vars.getKey(key) == null) {
                                vars.putKey(key);
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
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        while (undo > 0) {
            SetEntry<TermVar> entry = vars.getLastEntry();
            vars.remove(entry.key);
            undo--;
        }
        return true;
    }

    /****************************************************************/
    /* Variable Naming                                              */
    /****************************************************************/

    /**
     * <p>Complement the variable names.</p>
     * @param mvs3  The var set.
     * @param mvs   The anon set, can be null.
     * @param vars  The old variable names.
     * @param print The new variable names.
     */
    public static void numberVariables(SetHashLink<TermVar> mvs3,
                                       SetHashLink<TermVar> mvs,
                                       MapHashLink<TermVar, NamedDistance> vars,
                                       MapHashLink<TermVar, NamedDistance> print) {
        SetHash<String> range = namedToCopy(mvs3, mvs, vars, print);
        restToCopy(mvs3, mvs, range, print);
    }

    /**
     * <p>Copy the variable names, anonymous get underscore ("_").</p>
     *
     * @param mvs3 The var set.
     * @param mvs  The anon set, can be null.
     * @param vars The variable names, can be null.
     * @param copy The new variable names.
     * @return The name range.
     */
    private static SetHash<String> namedToCopy(SetHashLink<TermVar> mvs3,
                                               SetHashLink<TermVar> mvs,
                                               MapHashLink<TermVar, NamedDistance> vars,
                                               MapHashLink<TermVar, NamedDistance> copy) {
        if (vars == null)
            return null;
        SetHash<String> range = null;
        for (MapEntry<TermVar, NamedDistance> entry = vars.getFirstEntry();
             entry != null; entry = vars.successor(entry)) {
            TermVar key = entry.key;
            NamedDistance nd = entry.value;
            if (mvs != null && mvs.getKey(key) != null) {
                copy.put(key, new NamedDistance(0, PrologReader.OP_ANON));
            } else {
                copy.put(key, nd);
            }
            mvs3.remove(key);
            if (range == null)
                range = new SetHash<String>();
            range.putKey(nd.getName());
        }
        return range;
    }

    /**
     * <p>Create names for the variables, anonymous get underscore ("_").</p>
     *
     * @param mvs3  The var set.
     * @param mvs   The anon set, can be null.
     * @param range The name range.
     * @param copy  The new variable names.
     */
    private static void restToCopy(SetHashLink<TermVar> mvs3,
                                   SetHashLink<TermVar> mvs,
                                   SetHash<String> range,
                                   MapHashLink<TermVar, NamedDistance> copy) {
        int k = 0;
        for (SetEntry<TermVar> entry = mvs3.getFirstEntry();
             entry != null; entry = mvs3.successor(entry)) {
            TermVar key = entry.key;
            if (mvs != null && mvs.getKey(key) != null) {
                copy.put(key, new NamedDistance(0, PrologReader.OP_ANON));
            } else {
                for (; ; ) {
                    StringBuilder buf = new StringBuilder();
                    buf.appendCodePoint(k % 26 + 'A');
                    if (k > 26)
                        buf.append(k / 26);
                    k++;
                    String name = buf.toString();
                    if (range == null || range.getKey(name) == null) {
                        copy.put(key, new NamedDistance(0, name));
                        break;
                    }
                }
            }
        }
    }

}
