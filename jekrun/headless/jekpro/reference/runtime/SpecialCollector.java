package jekpro.reference.runtime;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.DisplayMarkable;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelCompound;
import matula.util.data.AbstractSet;
import matula.util.data.ListArray;
import matula.util.data.SetEntry;
import matula.util.data.SetTree;

/**
 * <p>Provides built-in predicates for the module collector.</p>
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
public final class SpecialCollector extends AbstractSpecial {
    private final static int SPECIAL_SYS_PIVOT_NEW = 0;
    private final static int SPECIAL_SYS_PIVOT_ADD = 1;
    private final static int SPECIAL_SYS_PIVOT_COLLECT = 2;
    private final static int SPECIAL_SYS_PIVOT_PUT = 3;
    private final static int SPECIAL_SYS_PIVOT_GATHER = 4;

    /**
     * <p>Create a collector special.</p>
     *
     * @param i The id.
     */
    public SpecialCollector(int i) {
        super(i);
    }

    /**
     * <p>Cons the value to the given term.</p>
     * <p>The result is returned in skeleton and display.</p>
     *
     * @param t2 The term skeleton.
     * @param d2 The term display.
     * @param t  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     */
    public static void pairValue(SkelCompound sc,
                                 Object t2, Display d2,
                                 Object t, Display d, Engine en) {
        Object v2 = SupervisorCopy.getVar(t2);
        Object v = SupervisorCopy.getVar(t);
        if (v2 == null) {
            Object[] args = new Object[2];
            args[0] = t2;
            args[1] = t;
            en.skel = new SkelCompound(sc.sym, args, v);
            en.display = d;
        } else if (v == null) {
            Object[] args = new Object[2];
            args[0] = t2;
            args[1] = t;
            en.skel = new SkelCompound(sc.sym, args, v2);
            en.display = d2;
        } else {
            Display d3 = new DisplayMarkable(2);
            boolean ext = d2.getAndReset();
            d3.bind[0].bindUniv(t2, d2, en);
            if (ext)
                d2.remTab(en);
            ext = d.getAndReset();
            d3.bind[1].bindUniv(t, d, en);
            if (ext)
                d.remTab(en);
            en.skel = sc;
            en.display = d3;
        }
    }

    /**
     * <p>Logically evaluate a term in a list of goals for the first time.</p>
     * <p>The term is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineException, EngineMessage {
        switch (id) {
            case SPECIAL_SYS_PIVOT_NEW:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                if (!en.unifyTerm(new SetEntry(), Display.DISPLAY_CONST, temp[0], ref))
                    return false;
                return true;
            case SPECIAL_SYS_PIVOT_ADD:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                SetEntry pivot = derefAndCastPivot(temp[0], ref);
                sysPivotAdd(pivot, temp[1], ref, en);
                return true;
            case SPECIAL_SYS_PIVOT_COLLECT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pivot = derefAndCastPivot(temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                sysPivotCollect(pivot, en);
                Display d = en.display;
                boolean multi = d.getAndReset();
                if (!en.unifyTerm(en.skel, d, temp[2], ref))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
            case SPECIAL_SYS_PIVOT_PUT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pivot = derefAndCastPivot(temp[0], ref);
                if (!sysPivotPut(pivot, temp[1], ref, en))
                    return false;
                return true;
            case SPECIAL_SYS_PIVOT_GATHER:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pivot = derefAndCastPivot(temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                sysPivotGather(pivot, en);
                d = en.display;
                multi = d.getAndReset();
                if (!en.unifyTerm(en.skel, d, temp[2], ref))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Add a pivot value.</p>
     *
     * @param pivot The pivot.
     * @param t     The element skeleton.
     * @param d     The element display.
     * @param en    The engine.
     */
    private static void sysPivotAdd(SetEntry<ListArray<Object>> pivot,
                                    Object t, Display d, Engine en) {
        ListArray<Object> help = pivot.value;
        if (help == null) {
            help = new ListArray<>();
            pivot.value = help;
        }
        help.add(AbstractSkel.copySkel(t, d, en));
    }

    /**
     * <p>Create a list from the pivot values.</p>
     * <p>The end is passed in skeleton and display.</p>
     * <p>The result is return in skeleton and display.</p>
     *
     * @param pivot The pivot.
     * @param en    The engine.
     */
    private static void sysPivotCollect(SetEntry<ListArray<Object>> pivot,
                                        Engine en) {
        ListArray<Object> help = pivot.value;
        if (help == null)
            return;
        for (int i = help.size() - 1; i >= 0; i--) {
            Object t = en.skel;
            Display d = en.display;
            Object val = help.get(i);
            Display ref = AbstractSkel.createMarker(val);
            pairValue(en.store.foyer.CELL_CONS,
                    val, ref, t, d, en);
        }
    }

    /**
     * <p>Put a pivot value.</p>
     *
     * @param pivot The pivot.
     * @param t     The element skeleton.
     * @param d     The element display.
     * @param en    The engine.
     */
    private static boolean sysPivotPut(SetEntry<AbstractSet<Object>> pivot,
                                       Object t, Display d, Engine en) {
        AbstractSet<Object> help = pivot.value;
        if (help == null) {
            help = new SetTree<>(AbstractSkel.DEFAULT);
            pivot.value = help;
        }
        t = AbstractSkel.copySkel(t, d, en);
        if (help.getEntry(t) == null) {
            help.add(t);
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Create a list from the pivot values.</p>
     * <p>The end is passed in skeleton and display.</p>
     * <p>The result is return in skeleton and display.</p>
     *
     * @param pivot The pivot.
     * @param en    The engine.
     */
    public static void sysPivotGather(SetEntry<AbstractSet<Object>> pivot,
                                      Engine en) {
        AbstractSet<Object> help = pivot.value;
        if (help == null)
            return;
        for (SetEntry<Object> entry = help.getLastEntry();
             entry != null; entry = help.predecessor(entry)) {
            Object t = en.skel;
            Display d = en.display;
            Object val = entry.value;
            Display ref = AbstractSkel.createMarker(val);
            pairValue(en.store.foyer.CELL_CONS,
                    val, ref, t, d, en);
        }
    }

    /**
     * <p>Cast a math context.</p>
     *
     * @param m The skel.
     * @param d The display.
     * @return The clause.
     * @throws EngineMessage Shit happens.
     */
    static SetEntry derefAndCastPivot(Object m, Display d)
            throws EngineMessage {
        m = SpecialUniv.derefAndCastRef(m, d);
        if (m instanceof SetEntry) {
            return (SetEntry) m;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    "pivot", m));
        }
    }
}