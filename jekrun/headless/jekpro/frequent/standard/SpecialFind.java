package jekpro.frequent.standard;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Special;
import jekpro.model.molec.*;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.tools.term.SkelCompound;
import matula.util.data.ListArray;

/**
 * <p>Provides built-in predicates for the set theory.</p>
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
public final class SpecialFind extends Special {
    private final static int SPECIAL_FINDALL = 0;
    private final static int SPECIAL_COPY_TERM = 1;

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
     * @param r  The continuation skeleton.
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
            case SPECIAL_FINDALL:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                SpecialFind.findAll(temp, ref, r, u, en);
                if (!en.unifyTerm(temp[2], ref, en.skel, en.display, r, u))
                    return false;
                return r.getNext(u, en);
            case SPECIAL_COPY_TERM:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                EngineCopy ec = en.enginecopy;
                if (ec == null) {
                    ec = new EngineCopy();
                    en.enginecopy = ec;
                }
                ec.vars = null;
                Object temp2 = ec.copyTerm(temp[0], ref);
                ec.vars = null;
                int size = Display.displaySize(temp2);
                Display ref2 = (size != 0 ? new Display(size) : Display.DISPLAY_CONST);
                if (!en.unifyTerm(temp[1], ref, temp2, ref2, r, u))
                    return false;
                return r.getNext(u, en);
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /************************************************************************/
    /* Findall Predicate                                                    */
    /************************************************************************/

    /**
     * <p>Findall solutions.</p>
     * <p>Result is returned in skel and display of the engine.</p>
     *
     * @param temp The goal skel.
     * @param ref  The goal display.
     * @param r    The continuation skeleton.
     * @param u    The continuation display.
     * @param en   The engine.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Some non callable encountered.
     */
    private static void findAll(Object[] temp, Display ref,
                                Intermediate r, DisplayClause u, Engine en)
            throws EngineException, EngineMessage {
        en.skel = temp[1];
        en.display = ref;
        en.deref();
        ListArray<Object> list = iterFindAll(temp[0], ref, r, u, en);
        SpecialFind.createList(list, en);
    }

    /**
     * <p>Find all solutions.</p>
     * <p>The goal is passed in skel and display of the engine.</p>
     *
     * @param t2 The template term skel.
     * @param d2 The template term display.
     * @param r  The continuation skeleton.
     * @param u  The continuation display.
     * @param en The engine.
     * @return The list of solutions.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Some non callable encountered.
     */
    private static ListArray<Object> iterFindAll(Object t2, Display d2,
                                                 Intermediate r, DisplayClause u,
                                                 Engine en)
            throws EngineException, EngineMessage {
        EngineCopy ec = en.enginecopy;
        if (ec == null) {
            ec = new EngineCopy();
            en.enginecopy = ec;
        }

        ListArray<Object> temp = null;
        Bind mark = en.bind;
        int snap = en.number;
        try {
            en.wrapGoal(r, u);
            Clause clause = en.store.CLAUSE_CALL;
            DisplayClause ref = new DisplayClause(clause.dispsize);
            ref.addArgument(en.skel, en.display, en);
            ref.setEngine(r, u, en);
            clause.getNextRaw(ref, en);
            boolean found = en.runFirst(snap);
            while (found) {
                ec.vars = null;
                Object val = ec.copyTerm(t2, d2);
                ec.vars = null;
                if (temp == null)
                    temp = new ListArray<Object>();
                temp.add(val);
                found = en.runNext(snap);
            }
        } catch (EngineMessage x) {
            throw new EngineException(x, EngineException.fetchStack(r, u, en));
        }
        en.skel = null;
        en.releaseBind(r, u, mark);
        if (en.skel != null)
            throw (EngineException) en.skel;
        return temp;
    }

    /**
     * <p>Create the result list.</p>
     * <p>Result is returned in skel and display of the engine.</p>
     *
     * @param temp The list of solutions.
     * @param en   The engine.
     */
    private static void createList(ListArray<Object> temp, Engine en) {
        en.display = Display.DISPLAY_CONST;
        en.skel = en.store.ATOM_NIL;
        if (temp == null)
            return;
        for (int i = temp.size() - 1; i >= 0; i--) {
            Object val = temp.get(i);
            int size = Display.displaySize(val);
            Display ref = (size != 0 ? new Display(size) : Display.DISPLAY_CONST);
            consValue(val, ref, en.skel, en.display, en);
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
    public static void consValue(Object t2, Display d2, Object t, Display d, Engine en) {
        if (EngineCopy.isGroundSkel(t2)) {
            en.skel = new SkelCompound(en.store.ATOM_CONS, t2, t);
            en.display = d;
            return;
        }
        if (EngineCopy.isGroundSkel(t)) {
            en.skel = new SkelCompound(en.store.ATOM_CONS, t2, t);
            en.display = d2;
            return;
        }
        Display d3 = new Display(2);
        d3.bind[0].bindVar(t2, d2, en);
        d3.bind[1].bindVar(t, d, en);
        en.skel = en.store.CELL_CONS;
        en.display = d3;
    }

}
