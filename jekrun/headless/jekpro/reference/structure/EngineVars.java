package jekpro.reference.structure;

import jekpro.model.molec.BindVar;
import jekpro.model.molec.Display;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import jekpro.tools.term.TermVar;
import matula.util.data.SetEntry;
import matula.util.data.SetHash;
import matula.util.data.SetHashLink;

/**
 * <p>This class provides basic functions to enumerate variables.</p>
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
    /* Skel Operations                                              */
    /****************************************************************/

    /**
     * <p>Check whether the term needs is ground.</p>
     *
     * @param t The term.
     * @return True if the term needs a display.
     */
    public static boolean isGroundSkel(Object t) {
        if (t instanceof SkelVar) {
            return false;
        } else if (t instanceof SkelCompound) {
            return ((SkelCompound) t).vars == null;
        } else {
            return true;
        }
    }

    /**
     * <p>Collect the vars of the given term in the given var set.</p>
     * <p>Makes use of the vars of the skel compounds.</p>
     *
     * @param t   The term skel.
     * @param mvs The var set.
     */
    public static void varIncludeSkel(Object t, SetHash<SkelVar> mvs) {
        if (t instanceof SkelVar) {
            SkelVar sv = (SkelVar) t;
            if (mvs.getKey(sv) == null)
                mvs.putKey(sv);
        } else if (t instanceof SkelCompound) {
            SkelVar[] vars = ((SkelCompound) t).vars;
            if (vars != null) {
                for (int i = 0; i < vars.length; i++) {
                    SkelVar sv = vars[i];
                    if (mvs.getKey(sv) == null)
                        mvs.putKey(sv);
                }
            }
        }
    }

    /**
     * <p>Collect the vars of the given term in the given var set.</p>
     * <p>Makes use of the vars of the skel compounds.</p>
     *
     * @param t   The term skel.
     * @param mvs The var set.
     */
    public static void varExcludeSkel(Object t, SetHash<SkelVar> mvs) {
        if (t instanceof SkelVar) {
            SkelVar sv = (SkelVar) t;
            mvs.remove(sv);
        } else if (t instanceof SkelCompound) {
            SkelVar[] vars = ((SkelCompound) t).vars;
            if (vars != null) {
                for (int i = 0; i < vars.length; i++) {
                    SkelVar sv = vars[i];
                    mvs.remove(sv);
                }
            }
        }
    }

    /**
     * <p>Collect the anonymous of the given term in the given var set.</p>
     * <p>Makes partial use of the vars of the skel compounds.</p>
     * <p>Tail recursive solution.</p>
     *
     * @param t    The term skel.
     * @param vars The vars set.
     * @param anon The anon set.
     */
    public static void singsOfSkel(Object t,
                                   SetHash<SkelVar> vars,
                                   SetHash<SkelVar> anon) {
        for (; ; ) {
            if (t instanceof SkelVar) {
                SkelVar sv = (SkelVar) t;
                if (vars.getKey(sv) == null) {
                    vars.putKey(sv);
                    anon.putKey(sv);
                } else {
                    anon.remove(sv);
                }
                break;
            } else if (t instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) t;
                if (sc.vars != null) {
                    int i = 0;
                    for (; i < sc.args.length - 1; i++)
                        singsOfSkel(sc.args[i], vars, anon);
                    t = sc.args[i];
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

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

}
