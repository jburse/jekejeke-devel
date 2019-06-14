package jekpro.reference.structure;

import jekpro.frequent.standard.EngineCopy;
import jekpro.frequent.standard.SpecialSort;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.NamedDistance;
import jekpro.model.pretty.PrologReader;
import jekpro.model.pretty.PrologWriter;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.tools.term.*;
import matula.util.data.*;

/**
 * <p>Provides built-in predicates for variable ops.</p>
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
public final class SpecialVars extends AbstractSpecial {
    private final static String OP_EXISTENTIAL = "^";

    private final static int SPECIAL_TERM_VARIABLES = 0;
    private final static int SPECIAL_SYS_TERM_SINGELTONS = 1;
    private final static int SPECIAL_SYS_GOAL_KERNEL = 2;
    private final static int SPECIAL_SYS_GOAL_GLOBALS = 3;
    private final static int SPECIAL_NUMBERVARS = 4;
    private final static int SPECIAL_SYS_NUMBER_VARIABLES = 5;
    private final static int SPECIAL_SYS_GET_VARIABLE_NAMES = 6;
    private final static int SPECIAL_ACYCLIC_TERM = 7;
    private final static int SPECIAL_SAFE_TERM_VARIABLES = 8;

    /**
     * <p>Create a vars special.</p>
     *
     * @param i The id.
     */
    public SpecialVars(int i) {
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
                case SPECIAL_TERM_VARIABLES:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    EngineVars ev = new EngineVars();
                    ev.varInclude(temp[0], ref);
                    en.skel = temp[2];
                    en.display = ref;
                    en.deref();
                    SpecialSort.createSet(ev.vars, en);
                    Display d = en.display;
                    boolean multi = d.getAndReset();
                    if (!en.unifyTerm(temp[1], ref, en.skel, d))
                        return false;
                    if (multi)
                        d.remTab(en);
                    return true;
                case SPECIAL_SYS_TERM_SINGELTONS:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    ev = new EngineVars();
                    ev.singsOf(temp[0], ref);
                    en.skel = en.store.foyer.ATOM_NIL;
                    en.display = Display.DISPLAY_CONST;
                    SpecialSort.createSet(ev.anon, en);
                    d = en.display;
                    multi = d.getAndReset();
                    if (!en.unifyTerm(temp[1], ref, en.skel, d))
                        return false;
                    if (multi)
                        d.remTab(en);
                    return true;
                case SPECIAL_SYS_GOAL_KERNEL:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    SpecialVars.goalKernel(temp[0], ref, en);
                    if (!en.unifyTerm(temp[1], ref, en.skel, en.display))
                        return false;
                    return true;
                case SPECIAL_SYS_GOAL_GLOBALS:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    ev = new EngineVars();
                    SpecialVars.goalGlobals(temp[0], ref, ev);
                    en.skel = en.store.foyer.ATOM_NIL;
                    en.display = Display.DISPLAY_CONST;
                    SpecialSort.createSet(ev.vars, en);
                    d = en.display;
                    multi = d.getAndReset();
                    if (!en.unifyTerm(temp[1], ref, en.skel, d))
                        return false;
                    if (multi)
                        d.remTab(en);
                    return true;
                case SPECIAL_NUMBERVARS:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    Number num = SpecialEval.derefAndCastInteger(temp[1], ref);
                    SpecialEval.checkNotLessThanZero(num);
                    SpecialEval.castIntValue(num);
                    num = SpecialVars.numberVars(temp[0], ref, (Integer) num, en);
                    if (num == null)
                        return false;
                    if (!en.unifyTerm(temp[2], ref, num, Display.DISPLAY_CONST))
                        return false;
                    return true;
                case SPECIAL_SYS_NUMBER_VARIABLES:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    SpecialVars.numberVariables(temp, ref, en);
                    d = en.display;
                    multi = d.getAndReset();
                    if (!en.unifyTerm(temp[3], ref, en.skel, d))
                        return false;
                    if (multi)
                        d.remTab(en);
                    return true;
                case SPECIAL_SYS_GET_VARIABLE_NAMES:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    Display d2 = en.visor.query;
                    MapHashLink<Object, NamedDistance> print = SpecialVars.hashToMap(d2.vars, d2, en);
                    mapToAssoc(print, en);
                    d = en.display;
                    multi = d.getAndReset();
                    if (!en.unifyTerm(temp[0], ref, en.skel, d))
                        return false;
                    if (multi)
                        d.remTab(en);
                    return true;
                case SPECIAL_ACYCLIC_TERM:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    ev = new EngineVars();
                    if (!ev.isAcyclic(temp[0], ref))
                        return false;
                    return true;
                case SPECIAL_SAFE_TERM_VARIABLES:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    ev = new EngineVars();
                    ev.safeVars(temp[0], ref);
                    en.skel = temp[2];
                    en.display = ref;
                    en.deref();
                    SpecialSort.createSet(ev.vars, en);
                    d = en.display;
                    multi = d.getAndReset();
                    if (!en.unifyTerm(temp[1], ref, en.skel, d))
                        return false;
                    if (multi)
                        d.remTab(en);
                    return true;
                default:
                    throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
            }
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
        }
    }

    /*******************************************************************/
    /* Existential Quantifier                                          */
    /*******************************************************************/

    /**
     * <p>Strip the term from its quantifiers.</p>
     * <p>Result is return in skel and display of the engine.</p>
     *
     * @param t  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     */
    private static void goalKernel(Object t, Display d, Engine en) {
        while (t instanceof SkelVar) {
            BindUniv b;
            if ((b = d.bind[((SkelVar) t).id]).display == null)
                break;
            t = b.skel;
            d = b.display;
        }
        while (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(OP_EXISTENTIAL)) {
            SkelCompound sc = (SkelCompound) t;
            t = sc.args[1];
            while (t instanceof SkelVar) {
                BindUniv b;
                if ((b = d.bind[((SkelVar) t).id]).display == null)
                    break;
                t = b.skel;
                d = b.display;
            }
        }
        en.skel = t;
        en.display = d;
    }

    /**
     * <p>Compute the free variables of a term.</p>
     * <p>The result is returned in the engine copy vars.</p>
     *
     * @param t The term skeleton.
     * @param d The term display.
     */
    private static void goalGlobals(Object t, Display d, EngineVars ev) {
        while (t instanceof SkelVar) {
            BindUniv b;
            if ((b = d.bind[((SkelVar) t).id]).display == null)
                return;
            t = b.skel;
            d = b.display;
        }
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(OP_EXISTENTIAL)) {
            SkelCompound sc = (SkelCompound) t;
            goalGlobals(sc.args[1], d, ev);
            ev.varExclude(sc.args[0], d);
        } else {
            ev.varInclude(t, d);
        }
    }

    /*******************************************************************/
    /* Number Variables                                                */
    /*******************************************************************/

    /**
     * <p>Number the variables of a term.</p>
     * <p>Tail recursive implementation.</p>
     *
     * @param m   The skel.
     * @param d   The display.
     * @param val The start number.
     * @param en  The engine.
     * @return The end number or null.
     * @throws EngineException Shit happens.
     */
    private static Integer numberVars(Object m, Display d, Integer val,
                                      Engine en)
            throws EngineException {
        for (; ; ) {
            Object var = EngineCopy.getVar(m);
            if (var == null)
                return val;
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
                        val = numberVars(b.skel, b.display, val, en);
                        if (val == null)
                            return null;
                    } else {
                        Object t = new SkelCompound(new SkelAtom(PrologWriter.OP_DOLLAR_VAR), val);
                        if (!en.unifyTerm(v, d, t, Display.DISPLAY_CONST))
                            return null;
                        val = Integer.valueOf(val.intValue() + 1);
                    }
                }
                v = temp[j];
            }
            BindUniv b = d.bind[v.id];
            if (b.display != null) {
                m = b.skel;
                d = b.display;
            } else {
                Object t = new SkelCompound(new SkelAtom(PrologWriter.OP_DOLLAR_VAR), val);
                if (!en.unifyTerm(v, d, t, Display.DISPLAY_CONST))
                    return null;
                return Integer.valueOf(val.intValue() + 1);
            }
        }
    }

    /********************************************************************/
    /* Raw Variable Names                                               */
    /********************************************************************/

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
    public static MapHashLink<Object, NamedDistance> hashToMap(MapHashLink<String, SkelVar> vars,
                                                               Display d, Engine en) {
        if (vars == null)
            return null;
        MapHashLink<Object, NamedDistance> print = null;
        for (MapEntry<String, SkelVar> entry = vars.getFirstEntry();
             entry != null; entry = vars.successor(entry)) {
            SkelVar sv = entry.value;
            if (sv.id >= d.bind.length || d.bind[sv.id] == null)
                continue;
            en.skel = sv;
            en.display = d;
            int distance = NamedDistance.derefCount(en);
            if (!(en.skel instanceof SkelVar))
                continue;
            Object key = AbstractTerm.createMolec(en.skel, en.display);
            if (print == null)
                print = new MapHashLink<Object, NamedDistance>();
            NamedDistance.addPriorized(print, key, entry.key, distance);
        }
        return print;
    }

    /****************************************************************/
    /* Variable Naming                                              */
    /****************************************************************/

    /**
     * <p>Complement the variable names.</p>
     * <p>Result is returned in engine skel and display.</p>
     *
     * @param temp The term skeleton.
     * @param ref  The term display.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void numberVariables(Object[] temp, Display ref,
                                        Engine en)
            throws EngineMessage {
        SetHashLink<Object> vars = SpecialVars.arrayToSet(temp[0], ref, en);
        MapHashLink<Object, NamedDistance> print = SpecialVars.assocToMap(temp[1], ref, en);
        SetHashLink<Object> anon = SpecialVars.arrayToSet(temp[2], ref, en);
        MapHashLink<Object, NamedDistance> copy = SpecialVars.numberVars(vars, anon, print, 0);
        SpecialVars.mapToAssoc(copy, en);
    }

    /**
     * <p>Complement the variable names.</p>
     *
     * @param vars  The var set, can be null.
     * @param anon  The anon set, can be null.
     * @param print The old variable names, can be null.
     * @param flags The flags.
     * @return The new variable names, can be null.
     */
    public static MapHashLink<Object, NamedDistance> numberVars(SetHashLink<Object> vars,
                                                                SetHashLink<Object> anon,
                                                                MapHashLink<Object, NamedDistance> print,
                                                                int flags) {
        if (vars == null)
            return null;

        MapHashLink<Object, NamedDistance> copy = new MapHashLink<Object, NamedDistance>();
        int k = 0;
        SetHash<String> range = null;

        /* pass one, all non-anon */
        for (SetEntry<Object> entry = vars.getFirstEntry();
             entry != null; entry = vars.successor(entry)) {
            if (anon != null && anon.getKey(entry.value) != null)
                continue;
            NamedDistance nd;
            if (print != null && (nd = print.get(entry.value)) != null) {
                copy.add(entry.value, nd);
            } else {
                if (range == null)
                    range = NamedDistance.nameRange(print);
                String name = SkelVar.sernoToString(k, false);
                k++;
                while (range.getKey(name) != null) {
                    name = SkelVar.sernoToString(k, false);
                    k++;
                }
                NamedDistance.addAnon(copy, entry.value, name);
            }
        }

        /* pass two, all anon */
        for (SetEntry<Object> entry = vars.getFirstEntry();
             entry != null; entry = vars.successor(entry)) {
            if (anon == null || anon.getKey(entry.value) == null)
                continue;
            NamedDistance nd;
            if (print != null && (nd = print.get(entry.value)) != null) {
                copy.add(entry.value, nd);
            } else {
                if ((flags & SpecialLoad.MASK_SHOW_NANO) == 0) {
                    NamedDistance.addAnon(copy, entry.value, PrologReader.OP_ANON);
                } else {
                    if (range == null)
                        range = NamedDistance.nameRange(print);
                    String name = SkelVar.sernoToString(k, false);
                    k++;
                    while (range.getKey(name) != null) {
                        name = SkelVar.sernoToString(k, false);
                        k++;
                    }
                    NamedDistance.addAnon(copy, entry.value, name);
                }
            }
        }

        return copy;
    }

    /**
     * <p>Create variable set from variables.</p>
     * <p>Non variable associations are skipped.</p>
     *
     * @param t  The variable list skel.
     * @param d  The variable list display.
     * @param en The engine.
     * @return The variable list.
     * @throws EngineMessage Shit happens.
     */
    private static SetHashLink<Object> arrayToSet(Object t, Display d,
                                                  Engine en)
            throws EngineMessage {
        SetHashLink<Object> set = null;
        en.skel = t;
        en.display = d;
        en.deref();
        while (en.skel instanceof SkelCompound &&
                ((SkelCompound) en.skel).args.length == 2 &&
                ((SkelCompound) en.skel).sym.fun.equals(Foyer.OP_CONS)) {
            SkelCompound mc = (SkelCompound) en.skel;
            d = en.display;
            en.skel = mc.args[0];
            en.deref();
            if (en.skel instanceof SkelVar) {
                Object pair = AbstractTerm.createMolec(en.skel, en.display);
                if (set == null) {
                    set = new SetHashLink<Object>();
                    set.add(pair);
                } else {
                    if (set.getKey(pair) == null)
                        set.add(pair);
                }
            }
            en.skel = mc.args[1];
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
        return set;
    }

    /**
     * <p>Convert a variable map into variable names.</p>
     * <p>Will return the result in the skel and display of the engine.</p>
     *
     * @param mvs The variable map.
     * @param en  The engine.
     */
    public static void mapToAssoc(MapHashLink<Object, NamedDistance> mvs,
                                  Engine en) {
        int countvar = 0;
        Display last = Display.DISPLAY_CONST;
        boolean multi = false;
        for (MapEntry<Object, NamedDistance> entry =
             (mvs != null ? mvs.getFirstEntry() : null);
             entry != null; entry = mvs.successor(entry)) {
            Object t = AbstractTerm.getSkel(entry.key);
            if (EngineCopy.getVar(t) != null) {
                Display d = AbstractTerm.getDisplay(entry.key);
                countvar++;
                if (last == Display.DISPLAY_CONST) {
                    last = d;
                } else if (last != d) {
                    multi = true;
                }
            }
        }
        if (multi) {
            last = new Display(countvar);
            last.vars = Display.VARS_MARKER;
        }
        countvar = 0;
        Object m = en.store.foyer.ATOM_NIL;
        for (MapEntry<Object, NamedDistance> entry =
             (mvs != null ? mvs.getFirstEntry() : null);
             entry != null; entry = mvs.successor(entry)) {
            Object t = AbstractTerm.getSkel(entry.key);
            Object val;
            if (multi && EngineCopy.getVar(t) != null) {
                Display d = AbstractTerm.getDisplay(entry.key);
                SkelVar var = SkelVar.valueOf(countvar);
                countvar++;
                last.bind[var.id].bindUniv(t, d, en);
                val = var;
            } else {
                val = t;
            }
            val = new SkelCompound(en.store.foyer.ATOM_EQUAL,
                    new SkelAtom(entry.value.getName()), val);
            m = new SkelCompound(en.store.foyer.ATOM_CONS, val, m);
        }
        en.skel = m;
        en.display = last;
    }

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
    public static MapHashLink<Object, NamedDistance> assocToMap(Object t, Display d,
                                                                Engine en)
            throws EngineMessage {
        MapHashLink<Object, NamedDistance> print = null;
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
            int distance = NamedDistance.derefCount(en);
            if (en.skel instanceof SkelVar) {
                Object pair = TermAtomic.createMolec(en.skel, en.display);
                if (print == null)
                    print = new MapHashLink<Object, NamedDistance>();
                String name = SpecialUniv.derefAndCastString(mc2[0], d2);
                NamedDistance.addPriorized(print, pair, name, distance);
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

}
