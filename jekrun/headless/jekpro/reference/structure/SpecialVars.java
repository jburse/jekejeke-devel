package jekpro.reference.structure;

import jekpro.model.builtin.Property;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Frame;
import jekpro.model.inter.Special;
import jekpro.model.molec.*;
import jekpro.model.pretty.PrologWriter;
import jekpro.model.pretty.Store;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.model.rope.Named;
import jekpro.tools.term.*;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;
import matula.util.data.SetHashLink;

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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SpecialVars extends Special {
    private final static int SPECIAL_TERM_VARIABLES = 0;
    private final static int SPECIAL_TERM_VARIABLES_DIFF = 1;
    private final static int SPECIAL_SYS_TERM_SINGELTONS = 2;
    private final static int SPECIAL_SYS_GOAL_KERNEL = 3;
    private final static int SPECIAL_SYS_GOAL_GLOBALS = 4;
    private final static int SPECIAL_NUMBERVARS = 5;
    private final static int SPECIAL_SYS_NUMBER_VARIABLES = 6;
    private final static int SPECIAL_SYS_GET_VARIABLE_NAMES = 7;
    private final static int SPECIAL_ACYCLIC_TERM = 8;

    /**
     * <p>Create a vars special.</p>
     *
     * @param i The id.
     */
    public SpecialVars(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param r  The continuation skel.
     * @param u  The continuation display.
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean findFirst(Goal r, DisplayClause u,
                                   Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case SPECIAL_TERM_VARIABLES:
                Object[] t = ((SkelCompound) en.skel).args;
                Display d = en.display;
                EngineVars ev = new EngineVars();
                ev.varInclude(t[0], d);
                en.skel = en.store.ATOM_NIL;
                en.display = Display.DISPLAY_CONST;
                Property.consSet(ev.vars, en);
                if (!en.unifyTerm(t[1], d, en.skel, en.display, r, u))
                    return false;
                return r.getNext(u, en);
            case SPECIAL_TERM_VARIABLES_DIFF:
                t = ((SkelCompound) en.skel).args;
                d = en.display;
                ev = new EngineVars();
                ev.varInclude(t[0], d);
                en.skel = t[2];
                en.display = d;
                Property.consSet(ev.vars, en);
                if (!en.unifyTerm(t[1], d, en.skel, en.display, r, u))
                    return false;
                return r.getNext(u, en);
            case SPECIAL_SYS_TERM_SINGELTONS:
                t = ((SkelCompound) en.skel).args;
                d = en.display;
                ev = new EngineVars();
                ev.singsOf(t[0], d);
                en.skel = en.store.ATOM_NIL;
                en.display = Display.DISPLAY_CONST;
                Property.consSet(ev.anon, en);
                if (!en.unifyTerm(t[1], d, en.skel, en.display, r, u))
                    return false;
                return r.getNext(u, en);
            case SPECIAL_SYS_GOAL_KERNEL:
                t = ((SkelCompound) en.skel).args;
                d = en.display;
                SpecialVars.goalKernel(t[0], d, en);
                if (!en.unifyTerm(t[1], d, en.skel, en.display, r, u))
                    return false;
                return r.getNext(u, en);
            case SPECIAL_SYS_GOAL_GLOBALS:
                t = ((SkelCompound) en.skel).args;
                d = en.display;
                ev = new EngineVars();
                SpecialVars.goalGlobals(t[0], d, ev);
                en.skel = en.store.ATOM_NIL;
                en.display = Display.DISPLAY_CONST;
                Property.consSet(ev.vars, en);
                if (!en.unifyTerm(t[1], d, en.skel, en.display, r, u))
                    return false;
                return r.getNext(u, en);
            case SPECIAL_NUMBERVARS:
                t = ((SkelCompound) en.skel).args;
                d = en.display;
                en.skel = t[1];
                en.display = d;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                Number num = EngineMessage.castInteger(en.skel, en.display);
                EngineMessage.checkNotLessThanZero(num);
                EngineMessage.castIntValue(num);
                num = SpecialVars.numberVars(t[0], d, (Integer) en.skel, r, u, en);
                if (num == null)
                    return false;
                if (!en.unifyTerm(t[2], d, num, Display.DISPLAY_CONST, r, u))
                    return false;
                return r.getNext(u, en);
            case SPECIAL_SYS_NUMBER_VARIABLES:
                t = ((SkelCompound) en.skel).args;
                d = en.display;
                SpecialVars.numberVariables(t, d, en);
                if (!en.unifyTerm(t[3], d, en.skel, en.display, r, u))
                    return false;
                return r.getNext(u, en);
            case SPECIAL_SYS_GET_VARIABLE_NAMES:
                t = ((SkelCompound) en.skel).args;
                d = en.display;
                Frame frame = en.visor.ref;
                Display ref = (frame != null ? frame.getDisplay() : null);
                Clause def = (frame != null ? frame.getClause() : null);
                Object res = Named.namedToAssoc((def != null ? def.vars : null), ref, en.store);
                if (!en.unifyTerm(t[0], d, res, ref, r, u))
                    return false;
                return r.getNext(u, en);
            case SPECIAL_ACYCLIC_TERM:
                t = ((SkelCompound) en.skel).args;
                d = en.display;
                ev = new EngineVars();
                if (!ev.isAcyclic(t[0], d))
                    return false;
                return r.getNextRaw(u, en);
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /*******************************************************************/
    /* Existential Quantifier                                          */
    /*******************************************************************/

    /**
     * <p>Strip the goal from its quantifiers.</p>
     * <p>Result is return in skel and display of the engine.</p>
     *
     * @param t  The goal skeleton.
     * @param d  The goal display.
     * @param en The engine.
     */
    private static void goalKernel(Object t, Display d, Engine en) {
        while (t instanceof SkelVar) {
            BindVar b;
            if ((b = d.bind[((SkelVar) t).id]).display == null)
                break;
            t = b.skel;
            d = b.display;
        }
        while (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Clause.OP_EXISTENTIAL)) {
            SkelCompound sc = (SkelCompound) t;
            t = sc.args[1];
            while (t instanceof SkelVar) {
                BindVar b;
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
     * <p>Compute the free variables of a goal.</p>
     * <p>The result is returned in the engine copy vars.</p>
     *
     * @param t The goal skeleton.
     * @param d The goal display.
     */
    private static void goalGlobals(Object t, Display d, EngineVars ev) {
        while (t instanceof SkelVar) {
            BindVar b;
            if ((b = d.bind[((SkelVar) t).id]).display == null)
                return;
            t = b.skel;
            d = b.display;
        }
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Clause.OP_EXISTENTIAL)) {
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
     * @param r   The continuation skel.
     * @param u   The continuation display.
     * @param en  The engine.
     * @return The end number or null.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    private static Integer numberVars(Object m, Display d, Integer val,
                                      Intermediate r, DisplayClause u, Engine en)
            throws EngineException, EngineMessage {
        for (; ; ) {
            if (m instanceof SkelVar) {
                SkelVar v = (SkelVar) m;
                BindVar b = d.bind[v.id];
                if (b.display != null) {
                    m = b.skel;
                    d = b.display;
                } else {
                    Object t = new SkelCompound(new SkelAtom(PrologWriter.OP_DOLLAR_VAR), val);
                    if (!en.unifyTerm(v, d, t, Display.DISPLAY_CONST, r, u))
                        return null;
                    return Integer.valueOf(val.intValue() + 1);
                }
            } else if (m instanceof SkelCompound) {
                SkelCompound tc = (SkelCompound) m;
                if (tc.vars != null) {
                    for (int j = 0; j < tc.vars.length - 1; j++) {
                        SkelVar v = tc.vars[j];
                        BindVar b = d.bind[v.id];
                        if (b.display != null) {
                            val = numberVars(b.skel, b.display, val, r, u, en);
                            if (val == null)
                                return null;
                        } else {
                            Object t = new SkelCompound(new SkelAtom(PrologWriter.OP_DOLLAR_VAR), val);
                            if (!en.unifyTerm(v, d, t, Display.DISPLAY_CONST, r, u))
                                return null;
                            val = Integer.valueOf(val.intValue() + 1);
                        }
                    }
                    SkelVar v = tc.vars[tc.vars.length - 1];
                    BindVar b = d.bind[v.id];
                    if (b.display != null) {
                        m = b.skel;
                        d = b.display;
                    } else {
                        Object t = new SkelCompound(new SkelAtom(PrologWriter.OP_DOLLAR_VAR), val);
                        if (!en.unifyTerm(v, d, t, Display.DISPLAY_CONST, r, u))
                            return null;
                        return Integer.valueOf(val.intValue() + 1);
                    }
                } else {
                    return val;
                }
            } else {
                return val;
            }
        }
    }

    /**
     * <p>Complement the variable names.</p>
     * <p>Result is returned in engine skel and display.</p>
     *
     * @param temp The goal skeleton.
     * @param ref  The goal display.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void numberVariables(Object[] temp, Display ref, Engine en) throws EngineMessage {
        SetHashLink<TermVar> mvs2 = new SetHashLink<TermVar>();
        arrayToSet(temp[0], ref, mvs2, en);
        MapHashLink<TermVar, String> print = assocToMap(temp[1], ref, en);
        SetHashLink<TermVar> mvs = new SetHashLink<TermVar>();
        arrayToSet(temp[2], ref, mvs, en);
        MapHashLink<TermVar, String> print2 = new MapHashLink<TermVar, String>();
        EngineVars.numberVariables(mvs2, mvs, print, print2);
        mapToAssoc(print2, en);
    }

    /**
     * <p>Create variable set from variables.</p>
     * <p>Non variable associations are skipped.</p>
     *
     * @param t   The variable names skel.
     * @param d   The variable names display.
     * @param en  The engine.
     * @param set The print map.
     * @throws EngineMessage Shit happens.
     */
    private static void arrayToSet(Object t, Display d,
                                   SetHashLink<TermVar> set, Engine en)
            throws EngineMessage {
        en.skel = t;
        en.display = d;
        en.deref();
        while (en.skel instanceof SkelCompound &&
                ((SkelCompound) en.skel).args.length == 2 &&
                ((SkelCompound) en.skel).sym.fun.equals(Store.OP_CONS)) {
            SkelCompound mc = (SkelCompound) en.skel;
            d = en.display;
            en.skel = mc.args[0];
            en.deref();
            if (en.skel instanceof SkelVar) {
                TermVar pair = new TermVar((SkelVar) en.skel, en.display);
                if (set.getKey(pair) == null)
                    set.putKey(pair);
            }
            en.skel = mc.args[1];
            en.display = d;
            en.deref();
        }
        if (en.skel instanceof SkelAtom &&
                ((SkelAtom) en.skel).fun.equals(Store.OP_NIL)) {
            /* */
        } else {
            EngineMessage.checkInstantiated(en.skel);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_LIST,
                    en.skel), en.display);
        }
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
    public static MapHashLink<TermVar, String> assocToMap(Object t, Display d,
                                                          Engine en)
            throws EngineMessage {
        MapHashLink<TermVar, String> print = null;
        en.skel = t;
        en.display = d;
        en.deref();
        while (en.skel instanceof SkelCompound &&
                ((SkelCompound) en.skel).args.length == 2 &&
                ((SkelCompound) en.skel).sym.fun.equals(Store.OP_CONS)) {
            Object[] mc = ((SkelCompound) en.skel).args;
            d = en.display;
            en.skel = mc[0];
            en.deref();
            if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 2 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Store.OP_EQUAL)) {
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
                TermVar pair = new TermVar((SkelVar) en.skel, en.display);
                en.skel = mc2[0];
                en.display = d2;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                String name = EngineMessage.castString(en.skel, en.display);
                if (print == null)
                    print = new MapHashLink<TermVar, String>();
                if (print.get(pair) == null)
                    print.put(pair, name);
            }
            en.skel = mc[1];
            en.display = d;
            en.deref();
        }
        if (en.skel instanceof SkelAtom &&
                ((SkelAtom) en.skel).fun.equals(Store.OP_NIL)) {
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
     * <p>Convert a variable map into variable names.</p>
     * <p>Will return the result in the skel and display of the engine.</p>
     *
     * @param mvs The variable map.
     * @param en  The engine.
     */
    public static void mapToAssoc(MapHashLink<TermVar, String> mvs, Engine en) {
        int countvar = 0;
        Display last = Display.DISPLAY_CONST;
        boolean multi = false;
        for (MapEntry<TermVar, String> entry = mvs.getFirstEntry();
             entry != null; entry = mvs.successor(entry)) {
            TermVar key = entry.key;
            countvar++;
            if (last == Display.DISPLAY_CONST) {
                last = AbstractTerm.getDisplay(key);
            } else if (last != AbstractTerm.getDisplay(key)) {
                multi = true;
            }
        }
        if (multi) {
            last = new Display(countvar);
            countvar = 0;
        }
        Object m = en.store.ATOM_NIL;
        for (MapEntry<TermVar, String> entry = mvs.getFirstEntry();
             entry != null; entry = mvs.successor(entry)) {
            TermVar key = entry.key;
            Object val;
            if (multi) {
                SkelVar var = SkelVar.valueOf(countvar);
                countvar++;
                last.bind[var.id].bindVar(AbstractTerm.getSkel(key), AbstractTerm.getDisplay(key), en);
                val = var;
            } else {
                val = AbstractTerm.getSkel(key);
            }
            val = new SkelCompound(en.store.ATOM_EQUAL,
                    new SkelAtom(entry.value), val);
            m = new SkelCompound(en.store.ATOM_CONS, val, m);
        }
        en.skel = m;
        en.display = last;
    }

}
