package jekpro.frequent.standard;

import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelCompoundLineable;
import jekpro.tools.term.SkelVar;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;
import matula.util.data.MapHashLink;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SupervisorCopy {
    public final static int MASK_COPY_SING = 0x00000001;

    public MapHash<BindUniv, SkelVar> vars;
    public MapHash<BindUniv, SkelVar> anon;
    public int flags;

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

    /**
     * <p>Retrieve the linear flag.</p>
     *
     * @param t The term.
     * @return The linear flag.
     */
    public static boolean getLinear(Object t) {
        if (t instanceof SkelCompound) {
            return ((SkelCompound) t).getLinear();
        } else {
            return true;
        }
    }

    /**
     * <p>Determine the display size.</p>
     * <p>Beware, works only for the root copy and not for sub terms.</p>
     *
     * @param m The skeleton.
     * @return The display size.
     */
    public static int displaySize(Object m) {
        Object var = getVar(m);
        if (var == null)
            return 0;
        if (var instanceof SkelVar) {
            return 1;
        } else {
            return ((SkelVar[]) var).length;
        }
    }

    /*******************************************************/
    /* Ordinary Copy                                       */
    /*******************************************************/

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
                BindUniv b;
                if ((b = d.bind[v.id]).display != null) {
                    t = b.skel;
                    d = b.display;
                    continue;
                }
                t = getVarValue(b);
                break;
            } else if (t instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) t;
                if (sc.var != null) {
                    Object[] args = new Object[sc.args.length];
                    for (int i = 0; i < sc.args.length - 1; i++)
                        args[i] = copyTerm(sc.args[i], d);
                    args[sc.args.length - 1] = back;
                    back = new SkelCompound(args, sc.sym);
                    t = sc.args[sc.args.length - 1];
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        while (back != null) {
            SkelCompound jack = (SkelCompound) back.args[back.args.length - 1];
            back.args[back.args.length - 1] = t;
            back.makeExtra();
            t = back;
            back = jack;
        }
        return t;
    }

    /**
     * <p>Get a new variable for an old variable.</p>
     *
     * @param key The old variable key.
     * @return The new variable skeleton.
     */
    private SkelVar getVarValue(BindUniv key) {
        SkelVar v;
        if (vars == null) {
            vars = new MapHash<>();
            v = null;
        } else {
            v = vars.get(key);
        }
        if (v == null) {
            v = SkelVar.valueOf(vars.size);
            vars.add(key, v);
        }
        return v;
    }

    /*******************************************************/
    /* Rest Copy                                           */
    /*******************************************************/

    /**
     * <p>Copy the given term.</p>
     * <p>Will change vars as a side effect.</p>
     * <p>Tail recursive solution.</p>
     *
     * @param t The term skel.
     * @param d The term display.
     * @return A copy of the term.
     */
    public final Object copyTermNew(Object t, Display d) {
        SkelCompound back = null;
        for (; ; ) {
            if (t instanceof SkelVar) {
                SkelVar v = (SkelVar) t;
                BindUniv b;
                if ((b = d.bind[v.id]).display != null) {
                    t = b.skel;
                    d = b.display;
                    continue;
                }
                t = getVarNew(b);
                break;
            } else if (t instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) t;
                if (sc.var != null) {
                    Object[] args = new Object[sc.args.length];
                    for (int i = 0; i < sc.args.length - 1; i++)
                        args[i] = copyTermNew(sc.args[i], d);
                    args[sc.args.length - 1] = back;
                    back = new SkelCompoundLineable(args, sc.sym);
                    t = sc.args[sc.args.length - 1];
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        while (back != null) {
            SkelCompound jack = (SkelCompound) back.args[back.args.length - 1];
            back.args[back.args.length - 1] = t;
            back.makeExtra();
            t = back;
            back = jack;
        }
        return t;
    }

    /**
     * <p>Get a new variable for an old variable.</p>
     *
     * @param key The old variable key.
     * @return The new variable skeleton.
     */
    private SkelVar getVarNew(BindUniv key) {
        SkelVar v;
        if (vars == null) {
            vars = new MapHash<>();
            v = null;
        } else {
            v = vars.get(key);
        }
        if (v == null) {
            v = new SkelVar(vars.size);
            vars.add(key, v);
            if ((flags & MASK_COPY_SING) != 0) {
                if (anon == null)
                    anon = new MapHash<>();
                anon.add(key, v);
            }
        } else {
            if ((flags & MASK_COPY_SING) != 0) {
                if (anon != null) {
                    anon.remove(key);
                    if (anon.size == 0)
                        anon = null;
                }
            }
        }
        return v;
    }

    /***********************************************************/
    /* Second Pass                                             */
    /***********************************************************/

    /**
     * <p>Create variable map from variable names.</p>
     * <p>Non variable associations are skipped.</p>
     *
     * @param t  The variable names skel.
     * @param d  The variable names display.
     * @param en The engine.
     * @return The print map.
     * @throws EngineMessage Shit happens.
     */
    public static MapHash<BindUniv, String> assocToMapUniv(Object t, Display d,
                                                           Engine en)
            throws EngineMessage {
        MapHash<BindUniv, String> print = null;
        en.skel = t;
        en.display = d;
        en.deref();
        while (en.skel instanceof SkelCompound &&
                ((SkelCompound) en.skel).args.length == 2 &&
                ((SkelCompound) en.skel).sym.fun.equals(Foyer.OP_CONS)) {
            Object[] mc = ((SkelCompound) en.skel).args;
            d = en.display;
            en.skel = mc[0];
            en.deref();
            if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 2 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Foyer.OP_EQUAL)) {
                /* */
            } else {
                EngineMessage.checkInstantiated(en.skel);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_ASSOC,
                        en.skel), en.display);
            }
            Object[] mc2 = ((SkelCompound) en.skel).args;
            Display d2 = en.display;
            en.skel = mc2[1];
            en.deref();
            if (en.skel instanceof SkelVar) {
                BindUniv pair = en.display.bind[((SkelVar) en.skel).id];
                if (print == null)
                    print = new MapHash<>();
                String name = SpecialUniv.derefAndCastString(mc2[0], d2);
                addMapUniv(print, pair, name);
            }
            en.skel = mc[1];
            en.display = d;
            en.deref();
        }
        if (en.skel instanceof SkelAtom &&
                ((SkelAtom) en.skel).fun.equals(Foyer.OP_NIL)) {
            /* */
        } else {
            EngineMessage.checkInstantiated(en.skel);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_LIST,
                    en.skel), en.display);
        }
        return print;
    }

    /**
     * <p>Create a print map from variable names.</p>
     * <p>Will not convert variables that have not yet been allocated.</p>
     * <p>Will not convert variables that have already been deallocated.</p>
     *
     * @param vars The var hash.
     * @param d    The term display.
     * @param en   The engine.
     * @return The print map.
     */
    public static MapHash<BindUniv, String> hashToMapUniv(MapHashLink<String, SkelVar> vars,
                                                          Display d, Engine en) {
        if (vars == null)
            return null;
        MapHash<BindUniv, String> print = null;
        for (MapEntry<String, SkelVar> entry = vars.getFirstEntry();
             entry != null; entry = vars.successor(entry)) {
            SkelVar sv = entry.value;
            if (sv.id >= d.bind.length || d.bind[sv.id] == null)
                continue;
            en.skel = sv;
            en.display = d;
            en.deref();
            if (!(en.skel instanceof SkelVar))
                continue;
            BindUniv pair = en.display.bind[((SkelVar) en.skel).id];
            if (print == null)
                print = new MapHash<>();
            addMapUniv(print, pair, entry.key);
        }
        return print;
    }

    /**
     * <p>Add to the map hash.</p>
     *
     * @param print The print map.
     * @param key   The variable.
     * @param name  The variable name.
     */
    private static void addMapUniv(MapHash<BindUniv, String> print,
                                   BindUniv key,
                                   String name) {
        MapEntry<BindUniv, String> entry = print.getEntry(key);
        if (entry == null) {
            print.add(key, name);
        } else {
            entry.value = name;
        }
    }

    /**
     * <p>Make a copy of the given variable names.</p>
     * <p>Only copy terms that are bound to a variable.</p>
     * <p>Only copy variables that already exist in rule.</p>
     *
     * @param map   The variable map.
     * @param print The print map.
     * @return The named copy.
     */
    public static MapHashLink<String, SkelVar> copyVarsUniv(MapHash<BindUniv, SkelVar> map,
                                                            MapHash<BindUniv, String> print) {
        if (print == null || map == null)
            return null;
        MapHashLink<String, SkelVar> copy = null;
        for (MapEntry<BindUniv, SkelVar> entry = map.getFirstEntry();
             entry != null; entry = map.successor(entry)) {

            String name = print.get(entry.key);
            if (name == null)
                continue;
            if (copy == null)
                copy = new MapHashLink<>();
            copy.add(name, entry.value);
        }
        return copy;
    }

}
