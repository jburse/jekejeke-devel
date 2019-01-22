package jekpro.tools.call;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Supervisor;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.Lobby;
import matula.util.wire.AbstractLivestock;

/**
 * The controller object cannot be manually created. It is automatically
 * created when an interpreter is obtained from a knowledge base, and
 * it is shared when an interpreter is forked from an interpreter. The
 * controller carries the basic input/output streams and thread local
 * predicates of an interpreter. For an instrumented interpreter it
 * might also carry spy points and break points.
 * <p/>
 * Interactors running in a primary thread might be controlled by a
 * secondary thread. A signal can be issued from a controlling thread
 * by calling setSignal(m) where m is non-null. A signal is an error term
 * wrapped into a class InterpreterMessage instance. It can be consumed
 * by calling throw setSignal(null) after catching InterruptedException.
 * It should be also consumed after catching InterruptedIOException
 * but not after SocketTimeoutException.
 * <p/>
 * The controlled thread will only be interrupted when the conjunctive
 * condition of a signal and a interrupt mask is satisfied. The current
 * signal can be retrieved by the method getSignal(). The interrupt
 * mask can be retrieved by the method getMask(). The interrupt mask
 * can be changed by the method setMask(). Both methods setSignal() and
 * setMask() set and clear the interrupted bit depending on whether the
 * conjunctive condition is satisfied or not.
 * <p/>
 * The methods setInuse() and getInuse() can be used to set and retrieve
 * the interpreter object a controller is currently in use. Further the
 * methods setFence() and currentController() can be used to set and
 * retrieve the thread a controller occupies. Both information pieces
 * are automatically tracked by the call-in interactor, so that the
 * application programmer usually doesn�t have to bother with
 * these methods.
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
public final class Controller {
    private final Supervisor visor;

    /*******************************************************/
    /* Signal Handling                                     */
    /*******************************************************/

    /**
     * <p>Retrieve the signal.</p>
     *
     * @return The old signal, can be null.
     */
    public final InterpreterMessage getSignal() {
        EngineMessage h = (EngineMessage) visor.signal;
        return (h != null ? new InterpreterMessage(h) : null);
    }

    /**
     * <p>Set the signal.</p>
     *
     * @param m The new signal, can be null.
     * @return The old signal, can be null.
     */
    public final InterpreterMessage setSignal(InterpreterMessage m) {
        EngineMessage h = (m != null ? (EngineMessage) m.getException() : null);
        h = (EngineMessage) visor.setSignal(h);
        return (h != null ? new InterpreterMessage(h) : null);
    }

    /**
     * <p>Retrieve the interrupt mask.</p>
     *
     * @return The old interrupt mask.
     */
    public boolean getMask() {
        return (visor.flags & AbstractLivestock.MASK_LIVESTOCK_NOSG) == 0;
    }

    /**
     * <p>Set the interrupt mask,</p>
     *
     * @param m The new interrupt mask.
     * @return The old interrupt mask.
     */
    public boolean setMask(boolean m) {
        return visor.setMask(m);
    }

    /*****************************************************************/
    /* Inuse Livecycle                                               */
    /*****************************************************************/

    /**
     * <p>Retrieve the inuse.</p>
     *
     * @return The inuse or null.
     */
    public final Interpreter getInuse() {
        Engine en = visor.inuse;
        return (en != null ? (Interpreter) en.proxy : null);
    }

    /**
     * <p>Set the inuse.</p>
     *
     * @param e The new inuse or null.
     * @return The old inuse or null.
     */
    public final Interpreter setInuse(Interpreter e) {
        Engine en = (e != null ? (Engine) e.getEngine() : null);
        en = visor.setInuse(en);
        return (en != null ? (Interpreter) en.proxy : null);
    }

    /*****************************************************************/
    /* Fence Livecycle                                               */
    /*****************************************************************/

    /**
     * <p>Retrieve the controller for a thread.</p>
     *
     * @param t The thread.
     * @return The controller or null.
     */
    public static Controller currentController(Thread t) {
        Supervisor s = (Supervisor) AbstractLivestock.currentLivestock(t);
        return (s != null ? (Controller) s.proxy : null);
    }

    /**
     * <p>Set the fence.</p>
     *
     * @param t The new thread or null.
     * @return The old thread or null.
     */
    public final Thread setFence(Thread t) {
        return visor.setFence(t);
    }

    /****************************************************************/
    /* Thread Statistics                                            */
    /****************************************************************/

    /**
     * <p>Retrieve the thread local length.</p>
     *
     * @return The thread local length.
     */
    public long getThreadLocalClauses() {
        return visor.getThreadLocalClauses();
    }

    /***********************************************************/
    /* For Internal Use Only                                   */
    /***********************************************************/

    /**
     * <p>Create a new interpreter.</p>
     *
     * @param l The lobby.
     */
    public Controller(Lobby l, Knowledgebase k) {
        Foyer foyer = (Foyer) l.getFoyer();
        Store store = (Store) k.getStore();

        visor = foyer.createSupervisor();
        visor.pushStack(store.user);
        visor.proxy = this;
    }

    /**
     * <p>Retrieve the supervisor.</p>
     *
     * @return The supervisor.
     */
    public final Object getVisor() {
        return visor;
    }

}
