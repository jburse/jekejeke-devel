package jekpro.frequent.experiment;

import jekpro.frequent.standard.SupervisorCall;
import jekpro.frequent.system.ForeignThread;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.CallFrame;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Intermediate;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides built-in predicates for monitor predicates.</p>
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
public class SpecialMonitor extends AbstractSpecial  {
    private final static int SPECIAL_SYS_OBJ = 0;
    private final static int SPECIAL_SYS_SYNC = 1;
    private final static int SPECIAL_SYS_WAIT = 2;
    private final static int SPECIAL_SYS_NOTIFY = 3;

    /**
     * <p>Create a monitor special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialMonitor(int i) {
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
     * @throws EngineMessage Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineException, EngineMessage {
        switch (id) {
            case SPECIAL_SYS_OBJ:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                if (!en.unifyTerm(temp[0], ref, new Object(), Display.DISPLAY_CONST))
                    return false;
                return true;
            case SPECIAL_SYS_SYNC:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                if (!SpecialMonitor.invokeSync(en, SpecialUniv.derefAndCastRef(temp[0], ref)))
                    return false;
                return true;
            case SPECIAL_SYS_WAIT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                try {
                    SpecialUniv.derefAndCastRef(temp[0], ref).wait();
                } catch (InterruptedException x) {
                    throw (EngineMessage) ForeignThread.sysThreadClear();
                }
                return true;
            case SPECIAL_SYS_NOTIFY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                SpecialUniv.derefAndCastRef(temp[0], ref).notify();
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);

        }
    }

    /**
     * <p>Invoke the term with the signal mask changed.</p>
     * <p>The term is passed via the skeleton and display of the engine</p>
     *
     * @param en   The engine.
     * @param sync The synchronization object.
     * @return True if the predicate succeeded, otherwise false
     * @throws EngineException Shit happens.
     */
    private static boolean invokeSync(Engine en, Object sync)
            throws EngineException {
        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;
        int snap = en.number;
        try {
            Directive dire = SupervisorCall.callGoal(AbstractDefined.MASK_DEFI_CALL, en);
            Display d2 = en.display;

            CallFrame ref2 = CallFrame.getFrame(d2, dire, en);
            en.contskel = dire;
            en.contdisplay = ref2;
            synchronized (sync) {
                if (!en.runLoop(snap, true))
                    return false;
            }
        } catch (EngineException x) {
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            synchronized (sync) {
                en.cutChoices(snap);
            }
            throw en.fault;
        } catch (EngineMessage y) {
            EngineException x = new EngineException(y,
                    EngineException.fetchStack(en));
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            synchronized (sync) {
                en.cutChoices(snap);
            }
            throw en.fault;
        }
        en.contskel = r;
        en.contdisplay = u;
        if (en.number != snap) {
            // create choice point
            en.choices = new ChoiceSync(en.choices, snap, r, u, sync);
            en.number++;
        }
        return true;
    }

}