package jekpro.frequent.standard;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Supervisor;
import jekpro.model.molec.*;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Intermediate;
import jekpro.tools.term.SkelCompound;
import matula.util.wire.AbstractLivestock;

/**
 * <p>Provides built-in predicates for the signal predicates.</p>
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
public final class SpecialSignal extends AbstractSpecial {
    private final static int SPECIAL_SYS_ATOMIC = 0;
    private final static int SPECIAL_SYS_CLEANUP = 1;

    /**
     * <p>Create a signal special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialSignal(int i) {
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
            case SPECIAL_SYS_ATOMIC:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!SpecialSignal.invokeAtomic(en, ChoiceAtomic.MASK_FLAGS_MASK))
                    return false;
                return true;
            case SPECIAL_SYS_CLEANUP:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();

                boolean multi = en.wrapGoal();
                ref = en.display;
                Directive dire = en.store.foyer.CLAUSE_CALL;
                Display d2 = new Display(dire.size);
                d2.bind[0].bindUniv(en.skel, ref, en);
                if (multi)
                    ref.remTab(en);

                boolean mask = (en.visor.flags & AbstractLivestock.MASK_LIVESTOCK_NOSG) == 0;
                boolean verify = (en.visor.flags & Supervisor.MASK_VISOR_NOCNT) == 0;
                Intermediate r = en.contskel;
                CallFrame u = en.contdisplay;
                en.choices = new ChoiceCleanup(en.choices, r, u, en.bind, mask, verify, d2, dire);
                en.number++;
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Invoke the term with the signal mask changed.</p>
     * <p>The term is passed via the skeleton and display of the engine</p>
     *
     * @param en The engine.
     *           @param mask The mask.
     * @return True if the predicate succeeded, otherwise false
     * @throws EngineException Shit happens.
     */
    public static boolean invokeAtomic(Engine en, int mask)
            throws EngineException {
        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;
        int backup = ChoiceAtomic.clearFlags(mask, en);
        int snap = en.number;
        try {
            boolean multi = en.wrapGoal();
            Display ref = en.display;
            Directive dire = en.store.foyer.CLAUSE_CALL;
            Display d2 = new Display(dire.size);
            d2.bind[0].bindUniv(en.skel, ref, en);
            if (multi)
                ref.remTab(en);
            CallFrame ref2 = CallFrame.getFrame(d2, dire, en);
            en.contskel = dire;
            en.contdisplay = ref2;
            if (!en.runLoop2(snap, true)) {
                ChoiceAtomic.setFlags(mask, backup, en);
                return false;
            }
        } catch (EngineException x) {
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            ChoiceAtomic.setFlags(mask, backup, en);
            throw en.fault;
        } catch (EngineMessage y) {
            EngineException x = new EngineException(y,
                    EngineException.fetchStack(en));
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            ChoiceAtomic.setFlags(mask, backup, en);
            throw en.fault;
        }
        en.contskel = r;
        en.contdisplay = u;
        if (en.number != snap) {
            /* create choice point */
            en.choices = new ChoiceAtomic(en.choices, snap, r, u, mask);
            en.number++;
        }
        ChoiceAtomic.setFlags(mask, backup, en);
        return true;
    }

}
