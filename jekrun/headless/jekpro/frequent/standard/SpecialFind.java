package jekpro.frequent.standard;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Intermediate;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelCompound;
import matula.util.data.ListArray;

/**
 * <p>Provides built-in predicates for the module bags.</p>
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
public final class SpecialFind extends AbstractSpecial {
    private final static int SPECIAL_FINDALL = 0;
    private final static int SPECIAL_FINDALL_END = 1;
    private final static int SPECIAL_COPY_TERM = 2;

    /**
     * <p>Create a set special.</p>
     *
     * @param i The id.
     */
    public SpecialFind(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
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
        switch (id) {
            case SPECIAL_FINDALL:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;

                en.skel = temp[1];
                en.display = ref;
                en.deref();
                ListArray<Object> list = iterFindAll(temp[0], ref, en);

                en.skel = en.store.foyer.ATOM_NIL;
                en.display = Display.DISPLAY_CONST;
                SpecialFind.createList(list, en);

                Display d = en.display;
                boolean multi = d.getAndReset();
                if (!en.unifyTerm(temp[2], ref, en.skel, d))
                    return false;
                if (multi)
                    BindUniv.remTab(d.bind, en);
                return en.getNext();
            case SPECIAL_FINDALL_END:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;

                en.skel = temp[1];
                en.display = ref;
                en.deref();
                list = iterFindAll(temp[0], ref, en);

                en.skel = temp[3];
                en.display = ref;
                en.deref();
                SpecialFind.createList(list, en);

                d = en.display;
                multi = d.getAndReset();
                if (!en.unifyTerm(temp[2], ref, en.skel, d))
                    return false;
                if (multi)
                    BindUniv.remTab(d.bind, en);
                return en.getNext();
            case SPECIAL_COPY_TERM:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                Object val = AbstractSkel.copySkel(temp[0], ref, en);
                int size = EngineCopy.displaySize(val);
                d = (size != 0 ? new Display(Display.newLexical(size)) :
                        Display.DISPLAY_CONST);
                if (!en.unifyTerm(temp[1], ref, val, d))
                    return false;
                if (size != 0)
                    BindUniv.remTab(d.bind, en);
                return en.getNext();
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /************************************************************************/
    /* Findall Predicate                                                    */
    /************************************************************************/

    /**
     * <p>Find all solutions.</p>
     * <p>The goal is passed in skel and display of the engine.</p>
     *
     * @param t2 The template term skel.
     * @param d2 The template term display.
     * @param en The engine.
     * @return The list of solutions.
     * @throws EngineException Shit happens.
     */
    private static ListArray<Object> iterFindAll(Object t2, Display d2,
                                                 Engine en)
            throws EngineException {
        Intermediate r = en.contskel;
        DisplayClause u = en.contdisplay;
        ListArray<Object> temp = null;
        AbstractBind mark = en.bind;
        int snap = en.number;
        try {
            boolean multi = en.wrapGoal();
            Display ref = en.display;
            Clause clause = en.store.foyer.CLAUSE_CALL;
            DisplayClause ref2 = new DisplayClause();
            ref2.bind = DisplayClause.newClause(clause.dispsize);
            ref2.def = clause;
            ref2.addArgument(en.skel, en.display, en);
            if (multi)
                BindUniv.remTab(ref.bind, en);
            ref2.setEngine(en);
            en.contskel = clause.getNextRaw(en);
            en.contdisplay = ref2;
            boolean found = en.runLoop(snap, true);
            while (found) {
                Object val = AbstractSkel.copySkel(t2, d2, en);
                if (temp == null)
                    temp = new ListArray<Object>();
                temp.add(val);
                found = en.runLoop(snap, false);
            }
        } catch (EngineMessage x) {
            en.contskel = r;
            en.contdisplay = u;
            throw new EngineException(x, EngineException.fetchStack(en));
        } catch (EngineException x) {
            en.contskel = r;
            en.contdisplay = u;
            throw x;
        }
        en.contskel = r;
        en.contdisplay = u;
        en.fault = null;
        en.releaseBind(mark);
        if (en.fault != null)
            throw en.fault;
        return temp;
    }

    /**
     * <p>Create the result list.</p>
     * <p>The end is passed in skel and display of the engine.</p>
     * <p>Result is returned in skel and display of the engine.</p>
     *
     * @param temp The list of solutions or null.
     * @param en   The engine.
     */
    private static void createList(ListArray<Object> temp, Engine en) {
        if (temp == null)
            return;
        for (int i = temp.size() - 1; i >= 0; i--) {
            Object t = en.skel;
            Display d = en.display;
            Object val = temp.get(i);
            Display ref = AbstractSkel.newDisplay(val);
            SpecialFind.pairValue2(en.store.foyer.CELL_CONS,
                    val, ref, t, d, en);
        }
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
    public static void pairValue2(SkelCompound sc,
                                  Object t2, Display d2,
                                  Object t, Display d, Engine en) {
        Object v2 = EngineCopy.getVar(t2);
        Object v = EngineCopy.getVar(t);
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
            Display d3 = new Display(Display.newLexical(2));
            boolean ext = d2.getAndReset();
            d3.bind[0].bindVar(t2, d2, en);
            if (ext)
                BindUniv.remTab(d2.bind, en);
            ext = d.getAndReset();
            d3.bind[1].bindVar(t, d, en);
            if (ext)
                BindUniv.remTab(d.bind, en);
            d3.flags |= Display.MASK_DPTM_MLTI;
            en.skel = sc;
            en.display = d3;
        }
    }

}
