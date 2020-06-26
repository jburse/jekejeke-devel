package jekpro.frequent.experiment;

import jekpro.model.inter.AbstractChoice;
import jekpro.model.inter.Engine;
import jekpro.model.molec.CallFrame;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Intermediate;

/**
 * <p>The class provides a choice point for synchronized calls.</p>
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
final class ChoiceSync extends AbstractChoice {
    private final Intermediate goalskel;
    private final int snap;
    private final Object sync;

    /**
     * <p>Create a choice context.</p>
     *
     * @param n The parent choice.
     * @param c The engine snap.
     * @param r The term list skeleton.
     * @param u The term list display.
     * @param s The synchronization object.
     */
    ChoiceSync(AbstractChoice n, int c, Intermediate r,
               CallFrame u, Object s) {
        super(n, u);
        goalskel = r;
        snap = c;
        sync = s;
    }

    /**
     * <p>Logically evaluate a term in a list of goals for an additional time.</p>
     * <p>The result is returned via the contskel and contdisplay of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineException Shit happens.
     */
    public final boolean moniNext(Engine en)
            throws EngineException {
        /* remove choice point */
        en.choices = next;
        en.number--;

        try {
            synchronized (sync) {
                if (!en.runLoop(snap, false))
                    return false;
            }
        } catch (EngineException x) {
            en.contskel = goalskel;
            en.contdisplay = goaldisplay;
            en.fault = x;
            synchronized (sync) {
                en.cutChoices(snap);
            }
            throw en.fault;
        } catch (EngineMessage y) {
            EngineException x = new EngineException(y,
                    EngineException.fetchStack(en));
            en.contskel = goalskel;
            en.contdisplay = goaldisplay;
            en.fault = x;
            synchronized (sync) {
                en.cutChoices(snap);
            }
            throw en.fault;
        }
        if (en.number != snap) {
            /* meta argument change */
            next = en.choices;
            /* reuse choice point */
            en.choices = this;
            en.number++;
        }
        en.contskel = goalskel;
        en.contdisplay = goaldisplay;
        return true;
    }

    /**
     * <p>Free data used to logically evaluate a term an additional time.</p>
     * <p>The current exception is passed via the engine fault.</p>
     * <p>The new current exception is returned via the engine fault.</p>
     * <p>The current contskel and contdisplay of the engine is not changed.</p>
     *
     * @param en The engine.
     */
    public final void moniCut(Engine en) {
        /* remove choice point */
        en.choices = next;
        en.number--;

        /* backup continuation */
        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;

        en.contskel = goalskel;
        en.contdisplay = goaldisplay;
        synchronized (sync) {
            en.cutChoices(snap);
        }

        /* restore continuation */
        en.contskel = r;
        en.contdisplay = u;
    }

}
