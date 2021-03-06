package jekpro.reference.structure;

import jekpro.frequent.standard.SpecialSort;
import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.PrologReader;
import jekpro.model.pretty.PrologWriter;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.reflect.PropertyCallable;
import jekpro.reference.runtime.SpecialCollector;
import jekpro.tools.array.Types;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
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
    private final static int SPECIAL_TERM_SINGELTONS = 1;
    private final static int SPECIAL_NUMBERVARS = 2;
    private final static int SPECIAL_SYS_NUMBER_VARIABLES = 3;
    private final static int SPECIAL_NONGROUND = 4;
    private final static int SPECIAL_ACYCLIC_TERM = 5;

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
                    SpecialSort.createSet(ev.vars, en, false);
                    Display d = en.display;
                    boolean multi = d.getAndReset();
                    if (!en.unify(en.skel, d, temp[1], ref))
                        return false;
                    if (multi)
                        d.remTab(en);
                    return true;
                case SPECIAL_TERM_SINGELTONS:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    ev = new EngineVars();
                    ev.singsOf(temp[0], ref);
                    en.skel = en.store.foyer.ATOM_NIL;
                    en.display = Display.DISPLAY_CONST;
                    SpecialSort.createSet(ev.anon, en, false);
                    d = en.display;
                    multi = d.getAndReset();
                    if (!en.unify(en.skel, d, temp[1], ref))
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
                    if (!en.unify(num, Display.DISPLAY_CONST, temp[2], ref))
                        return false;
                    return true;
                case SPECIAL_SYS_NUMBER_VARIABLES:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    SpecialVars.numberVariables(temp, ref, en);
                    d = en.display;
                    multi = d.getAndReset();
                    if (!en.unify(en.skel, d, temp[3], ref))
                        return false;
                    if (multi)
                        d.remTab(en);
                    return true;
                case SPECIAL_NONGROUND:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (!firstVariable(temp[0], ref, en))
                        return false;
                    if (!en.unify(en.skel, en.display, temp[1], ref))
                        return false;
                    return true;
                case SPECIAL_ACYCLIC_TERM:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    ev = new EngineVars();
                    if (!ev.isAcyclic(temp[0], ref))
                        return false;
                    return true;
                default:
                    throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
            }
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
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
            Object var = SupervisorCopy.getVar(m);
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
                        if (!en.unify(t, Display.DISPLAY_CONST, v, d))
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
                if (!en.unify(t, Display.DISPLAY_CONST, v, d))
                    return null;
                return Integer.valueOf(val.intValue() + 1);
            }
        }
    }

    /********************************************************************/
    /* Print Map                                                        */
    /********************************************************************/

    /**
     * <p>Convert a variable map into variable names.</p>
     * <p>Will return the result in the skel and display of the engine.</p>
     *
     * @param mvs The variable map.
     * @param en  The engine.
     */
    public static void mapToAssoc(MapHashLink<Object, String> mvs,
                                  Engine en) {
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        for (MapEntry<Object, String> entry =
             (mvs != null ? mvs.getFirstEntry() : null);
             entry != null; entry = mvs.successor(entry)) {
            Object elem2 = entry.key;
            Object val2 = AbstractTerm.getSkel(elem2);
            Display ref2 = AbstractTerm.getDisplay(elem2);
            Object t4 = en.skel;
            Display d2 = en.display;
            SpecialCollector.pairValue(en.store.foyer.CELL_EQUAL,
                    new SkelAtom(entry.value), Display.DISPLAY_CONST,
                    val2, ref2, en);
            val2 = en.skel;
            ref2 = en.display;
            SpecialCollector.pairValue(en.store.foyer.CELL_CONS,
                    val2, ref2, t4, d2, en);
        }
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
        MapHash<BindUniv, String> print = PropertyCallable.assocToMapUniv(temp[1], ref, en);
        SetHashLink<Object> anon = SpecialVars.arrayToSet(temp[2], ref, en);
        MapHashLink<Object, String> map = SpecialVars.numberVars(vars, anon, print);
        SpecialVars.mapToAssoc(map, en);
    }

    /**
     * <p>Complement the variable names.</p>
     *
     * @param vars  The var set, can be null.
     * @param anon  The anon set, can be null.
     * @param print The old variable names, can be null.
     * @return The new variable names, can be null.
     */
    public static MapHashLink<Object, String> numberVars(SetHashLink<Object> vars,
                                                         SetHashLink<Object> anon,
                                                         MapHash<BindUniv, String> print) {
        if (vars == null)
            return null;

        MapHashLink<Object, String> copy = new MapHashLink<>();
        int k = 0;
        SetHash<String> range = null;

        for (SetEntry<Object> entry = vars.getFirstEntry();
             entry != null; entry = vars.successor(entry)) {
            SkelVar sv = (SkelVar) AbstractTerm.getSkel(entry.value);
            Display d = AbstractTerm.getDisplay(entry.value);
            String t;
            if (print != null && (t = print.get(d.bind[sv.id])) != null) {
                copy.add(entry.value, t);
            } else if (anon != null && anon.getEntry(entry.value) != null) {
                copy.add(entry.value, PrologReader.OP_ANON);
            } else {
                String name = SkelVar.sernoToString(k, false);
                k++;
                if (print != null) {
                    if (range == null)
                        range = nameRange(print);
                    while (range.getEntry(name) != null) {
                        name = SkelVar.sernoToString(k, false);
                        k++;
                    }
                }
                copy.add(entry.value, name);
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
                    set = new SetHashLink<>();
                    set.add(pair);
                } else {
                    if (set.getEntry(pair) == null)
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

    /**************************************************************/
    /* Name Utilities                                             */
    /**************************************************************/

    /**
     * <po>Compute the name range.</po>
     *
     * @param print The print map.
     * @return The name range.
     */
    public static SetHash<String> nameRange(MapHash<BindUniv, String> print) {
        if (print == null)
            return null;
        SetHash<String> range = new SetHash<>();
        for (MapEntry<BindUniv, String> entry = print.getFirstEntry();
             entry != null; entry = print.successor(entry))
            range.add(entry.value);
        return range;
    }

    /**
     * <p>Find the first variable.</p>
     * <p>Tail recursive solution.</p>
     * <p>The result in the engine skel and display</p>
     *
     * @param t The term skel.
     * @param d The term display.
     * @return True if the term is nonground, otherwise false.
     */
    private static boolean firstVariable(Object t, Display d, Engine en) {
        for (; ; ) {
            Object var = SupervisorCopy.getVar(t);
            if (var == null)
                return false;
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
                        if (firstVariable(b.skel, b.display, en))
                            return true;
                    } else {
                        en.skel = v;
                        en.display = d;
                        return true;
                    }
                }
                v = temp[j];
            }
            BindUniv b = d.bind[v.id];
            if (b.display != null) {
                t = b.skel;
                d = b.display;
            } else {
                en.skel = v;
                en.display = d;
                return true;
            }
        }
    }

}
