package jekpro.model.inter;

import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Store;

/**
 * <p>The class provides an engine that yields.</p>
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
public class EngineYield extends Engine {
    private static final int YIELD_MAX = 20000;

    private int yieldcount;

    /**
     * <p>Create a new engine.</p>
     *
     * @param s The store.
     * @param v The supervisor.
     */
    public EngineYield(Store s, Supervisor v) {
        super(s, v);
    }

    /*****************************************************************/
    /* Trampolin Interpreter with Yield                              */
    /*****************************************************************/

    /**
     * <p>Start searching solutions for the given term for the first time.</p>
     * <p>The term is passed via skel and display of this engine.</p>
     * <p>In case of exception, the choice points are already removed.</p>
     *
     * @param snap  The choice barrier.
     * @param found The backtracking flag.
     * @return True if the term list succeeded, otherwise false.
     * @throws EngineException Shit happens.
     */
    public final boolean runLoop(int snap, boolean found)
            throws EngineException {
        try {
            for (; ; ) {
                if ((yieldcount++) >= YIELD_MAX)
                    yieldReset();
                if (found) {
                    if (contskel != null) {
                        if (hasCont())
                            retireCont();
                        contskel = contskel.getNextRaw(this);
                        found = contskel.resolveNext(this);
                    } else {
                        break;
                    }
                } else {
                    if (snap < number) {
                        found = choices.moniNext(this);
                    } else {
                        break;
                    }
                }
            }
        } catch (EngineException x) {
            window = contdisplay;
            fault = x;
            cutChoices(snap);
            window = null;
            throw fault;
        } catch (EngineMessage y) {
            EngineException x = new EngineException(y,
                    EngineException.fetchStack(this));
            window = contdisplay;
            fault = x;
            cutChoices(snap);
            window = null;
            throw fault;
        }
        return found;
    }

    /**
     * <p>Yield the thread and reset the counter.</p>
     */
    private void yieldReset() {
        Thread.yield();
        yieldcount = 0;
    }

}