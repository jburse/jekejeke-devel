package jekpro.model.builtin;

import jekpro.frequent.standard.SpecialSignal;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.*;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Intermediate;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides built-in predicates for control predicates.</p>
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
public final class SpecialControl extends AbstractSpecial {
    private final static int SPECIAL_FAIL = 0;
    private final static int SPECIAL_TRUE = 1;
    private final static int SPECIAL_CUT = 2;
    private final static int SPECIAL_SYS_FETCH_STACK = 3;
    private final static int SPECIAL_SYS_RAISE = 4;
    private final static int SPECIAL_SYS_TRAP = 5;

    /**
     * <p>Create a control special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialControl(int i) {
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
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineException, EngineMessage {
        switch (id) {
            case SPECIAL_FAIL:
                return false;
            case SPECIAL_TRUE:
                return true;
            case SPECIAL_CUT:
                CallFrame ref2 = en.contdisplay;
                while ((ref2.flags & Directive.MASK_DIRE_NOBR) != 0 &&
                        ref2.barrier == -1)
                    ref2 = ref2.contdisplay;
                int level = (ref2.barrier != -1 ?
                        ref2.barrier : ref2.number);
                if (level < en.number) {
                    ref2 = en.contdisplay;
                    while ((ref2.flags & Directive.MASK_DIRE_NOBR) != 0 &&
                            ref2.barrier == -1) {
                        en.window = ref2;
                        en.fault = null;
                        en.cutChoices(ref2.number);
                        en.window = null;
                        if (en.fault != null)
                            throw en.fault;
                        ref2.number = level;
                        ref2 = ref2.contdisplay;
                    }
                    en.window = ref2;
                    en.fault = null;
                    en.cutChoices(level);
                    en.window = null;
                    if (en.fault != null)
                        throw en.fault;
                    ref2 = en.contdisplay;
                    en.contdisplay = ref2.getFrame(en);
                }
                return true;
            case SPECIAL_SYS_FETCH_STACK:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                if (!en.unifyTerm(temp[0], ref,
                        EngineException.fetchStack(en),
                        Display.DISPLAY_CONST))
                    return false;
                return true;
            case SPECIAL_SYS_RAISE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                throw new EngineException(temp[0], ref);
            case SPECIAL_SYS_TRAP:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                return invokeTrap(en);
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Invoke a term and handle exceptions.</p>
     *
     * @param en The engine.
     * @return True fi the predicate succeeded, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    private boolean invokeTrap(Engine en)
            throws EngineException, EngineMessage {
        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;
        AbstractUndo mark = en.bind;
        int snap = en.number;
        try {
            boolean multi = en.wrapGoal();
            Display ref = en.display;
            Directive dire = en.store.foyer.CLAUSE_CALL;
            DisplayClause d2 = new DisplayClause(dire.size);
            d2.bind[0].bindUniv(en.skel, ref, en);
            if (multi)
                ref.remTab(en);
            CallFrame ref2 = CallFrame.getFrame(d2, dire, en);
            en.contskel = dire;
            en.contdisplay = ref2;
            if (!en.runLoop2(snap, true))
                return false;
            en.contskel = r;
            en.contdisplay = u;
            en.fault = null;
        } catch (EngineException x) {
            en.contskel = r;
            en.contdisplay = u;
            en.window = en.contdisplay;
            en.fault = x;
            en.cutChoices(snap);
            en.window = null;
            en.releaseBind(mark);
        } catch (EngineMessage y) {
            EngineException x = new EngineException(y,
                    EngineException.fetchStack(en));
            en.contskel = r;
            en.contdisplay = u;
            en.window = en.contdisplay;
            en.fault = x;
            en.cutChoices(snap);
            en.window = null;
            en.releaseBind(mark);
        }
        if (en.fault != null)
            return handleException(en);
        if (en.number != snap) {
            /* create choice point */
            en.choices = new ChoiceTrap(en.choices, snap, r, u, mark);
            en.number++;
        }
        return true;
    }

    /**
     * <p>Handle the exception.</p>
     * <p>The exception is passed in the skel of the engine.</p>
     *
     * @param en The engine.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public static boolean handleException(Engine en)
            throws EngineException, EngineMessage {
        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;
        EngineException y = en.fault;
        StackElement.callGoal(r, u, en);
        Object[] temp = ((SkelCompound) en.skel).args;
        Display ref = en.display;
        try {
            Object temp2 = y.getTemplate();
            Display ref2 = AbstractSkel.createMarker(temp2);
            boolean multi = ref2.getAndReset();
            if (!en.unifyTerm(temp[1], ref, temp2, ref2))
                throw y;
            if (multi)
                ref2.remTab(en);
        } catch (EngineException z) {
            throw new EngineException(y, z);
        }
        en.skel = temp[2];
        en.display = ref;
        en.deref();
        if (!SpecialSignal.invokeAtomic(en))
            return false;
        return true;
    }

}
